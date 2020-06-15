+++
title = "Netlink IPCを使ってLinuxカーネルのネットワーク情報にアクセスする"
date = 2019-09-01
aliases = ["/posts/linux/99-communicating-netlink-with-ipc.html"]
[taxonomies]
tags = ["linux","network","netlink"]
+++

背景
---

Linux networkingには下記のような様々な構成要素があります。

* ネットワークデバイス
* 経路テーブル
* フィルタ
* etc.

これらを操作する必要がある場合、一般的には [iproute2][iproute2] などのコマンドラインツールから操作を行うことが多いかと思います。

しかしOpenStackやKubernetesなどの大規模なクラスタを管理する環境では、複数ホストにまたがってネットワークを動的にかつ一貫した方法で構成することが求められます。
こうしたニーズを実現するために、Linux networkingの構成要素を自作のプログラムから直接制御することが最も効率的な場合があります。
NetlinkというLinuxのサブシステムはLinuxカーネル内のネットワーク関連リソースを制御します。このNetlinkサブシステムを活用することでネットワーク管理を自動化することができます。

概要
---

NetlinkとはLinuxカーネルのサブシステムの名称です。
このサブシステムとユーザ空間のアプリケーションがやりとりするためのインタフェースとしてソケットベースのIPCが定義されています。
アプリケーションは一般的なソケットプログラミングの作法を通じてLinuxカーネル管理下のネットワーク関連リソースを操作することができます。

この記事ではNetlinkサブシステムとの典型的な通信方法について調査し説明します。
NetlinkサブシステムとIPC経由で通信するための典型的な作法については、 [Kernel Korner - Why and How to Use Netlink Socket][Kernel Korner - Why and How to Use Netlink Socket] こちらの記事でコードと共に解説されていますのでそれだけでも充分参考になります。

通信方法を示すための例としてここでは、ネットワークインタフェースの情報を取得するケースについて記述します。

全体のコードは [こちら](https://gist.github.com/utky/de8b37756f8664d0045f829292657ae5)

### この記事で使ったコードのリビジョン

* linux kernel : 4.19 `84df9525b0c27f3ebc2ebb1864fa62a97fdedb7d`
* iproute2: `260dc56ae3efbac6aa80e946d18eb0c66b95c5a4`

IPCによる操作
---

ここではNetlinkを使ったLinux networkingの設定を行うことを想定します。
特に簡単な例としてネットワークデバイスの作成・削除を例にとります。

IPCメッセージの送受信の流れは概して下記のようになります。

* 1) socketの確保
* 2) アドレスの確保
* 3) 要求メッセージの作成
* 4) 要求メッセージの送信
* 5) 応答メッセージの受信
* 6) 応答メッセージの解析

以降ではこれらのステップごとにコードの例を示しながら説明をします。

1) socketの確保
---

ソケットの作成には `socket` を使います。

SOCKET(3P)
```
SYNOPSIS
       #include <sys/socket.h>

       int socket(int domain, int type, int protocol);
```

この関数はソケット作成に必要なプロトコルや通信方式を表す定数を渡します。

ソケット作成が成功するとソケットのファイルディスクリプタが返却されます。
失敗した場合は `-1` が返却されます。

以降では指定するべき引数について説明します。

### domain

ソケット通信に使うプロトコルファミリーを指定します。

ADDRESS_FAMILIES(7)には指定可能なファミリーの定義が記載されています。
一般的によく使うのは

* `AF_INET` : IPv4通信
* `AF_LOCAL` : Unix domain socket

### type

データ送受信の通信方式を指定します。
TCPのような信頼性が高くストリーム指向の通信をおこなうか、UDPのようなデータグラム転送方式で通信をおこなうか選択できます。

### protocol

domainで指定した種別に対応する具体的な利用プロトコルを指定します。
0を指定するとデフォルトの実装が使われます。

### Netlinkの場合

Netlinkの操作を行う場合、この関数に渡す引数はほぼ決まっています。

* `domain` :  `AF_NETLINK`
* `type` : `SOCK_DGRAM`
* `protocol` : `NETLINK_ROUTE`

protocolにはNetlinkサブシステム上に定義されたいくつかの機能から選びます。
定義はNETLINK(7)で確認することができます。

ネットワークデバイスや経路テーブルの制御には `NETLINK_ROUTE` を指定します。

NETLINK(7)
```
       NETLINK_ROUTE
              Receives routing and link updates and may be used to modify
              the routing tables (both IPv4 and IPv6), IP addresses, link
              parameters, neighbor setups, queueing disciplines,  traffic
              classes and packet classifiers (see rtnetlink(7)).
```

### コード例

```c
    int fd = socket(AF_NETLINK, SOCK_DGRAM, NETLINK_ROUTE);
    if (fd < 0) {
        perror("Opening socket failed");
        return errno;
    }
```

負値が返却された場合はソケット作成に失敗しているので、エラーハンドリングを行います。

2) アドレスの確保
---

ソケットを通信の端点として利用可能にするためにはソケットに特定のアドレスを与える必要があります。
使う `domain` によってアドレスを表すデータ構造は異なります。

例えばIPv4用ソケットのアドレスは下記のような定義になります。

include/uapi/linux/in.h
```c
struct sockaddr_in {
  __kernel_sa_family_t  sin_family;     /* Address family               */
  __be16                sin_port;       /* Port number                  */
  struct in_addr        sin_addr;       /* Internet address             */

  /* Pad to size of `struct sockaddr'. */
  unsigned char         __pad[__SOCK_SIZE__ - sizeof(short int) -
                        sizeof(unsigned short int) - sizeof(struct in_addr)];
};
```
IPv4のプロトコルにおけるアドレスとして

* `sin_port` : ポート番号
* `sin_addr` : IPアドレス

を指定する必要があることが分かります。
またここでも `sin_family` のフィールドに `socket` の `domain` と同じプロトコルファミリーを指定する必要があります。

Netlinkの場合は先頭のプロトコルファミリー以外にもいくつか固有のフィールドがあります。

include/uapi/linux/netlink.h
```c
struct sockaddr_nl {
        __kernel_sa_family_t    nl_family;      /* AF_NETLINK   */
        unsigned short  nl_pad;         /* zero         */
        __u32           nl_pid;         /* port ID      */
        __u32           nl_groups;      /* multicast groups mask */
};
```

* `nl_pad` : パディング用の領域
* `nl_pid` : ポート番号
* `nl_groups` : このソケットで受信を許可するマルチキャストメッセージのグループ

アドレスが作れたらソケットとアドレスを対応付けます。
そのために使うのが関数 `bind` です。

BIND(3P)
```
SYNOPSIS
       #include <sys/socket.h>

       int bind(int socket, const struct sockaddr *address,
           socklen_t address_len);
```

対応付けに成功する場合は `0` を返し、失敗した場合は負値を返します。

* `socket` : 対応付けるソケットのファイルディスクリプタ
* `address` : プロトコルに対応したアドレス構造体のポインタ
* `address_len` : アドレス構造体のデータ長

### コード例

```c
    int pid = getpid();
    struct sockaddr_nl sa;
    memset(&sa, 0, sizeof(sa));
    sa.nl_family = AF_NETLINK;
    sa.nl_pid = pid;

    if (bind(fd, (struct sockaddr*)&sa, sizeof(sa)) < 0) {
        close(fd);
        perror("Binding socket to address failed");
        return errno;
    }
```
`pid` にはアドレスを利用するプロセスのPIDを渡すこともできます。
用途はあくまでもポート番号であるため、識別性があればPIDである必要はありません。

3) 要求メッセージの作成
---

ソケット経由で送信するNetlinkメッセージは大きく2つの層に分離することができます。

1. Netlink固有ヘッダ `nlmsghdr`
2. データグラム通信共通ヘッダ `msghdr`

メッセージを作成する場合はまず `nlmsghdr` とそのボディを先に作ってから、
`msghdr` でカプセル化した構造体を送信用関数に入力することになります。

### nlmsghdr

Netlink固有ヘッダは下記のような構造になっています。

include/uapi/linux/netlink.h
```c
struct nlmsghdr {
	__u32		nlmsg_len;	/* Length of message including header */
	__u16		nlmsg_type;	/* Message content */
	__u16		nlmsg_flags;	/* Additional flags */
	__u32		nlmsg_seq;	/* Sequence number */
	__u32		nlmsg_pid;	/* Sending process port ID */
};
```

このデータ構造はあくまでもヘッダです。
実際の詳細な要求内容などのパラメータはボディとしてこのヘッダに続くデータ構造でエンコードされます。
ヘッダに続くデータ構造は `nlmsg_type` に応じて変わります。
ネットワークインタフェースを操作するメッセージや経路テーブルを操作するメッセージなど、目的に応じてこのヘッダの後に続けるデータの構造を返る必要があります。

Netlinkの `NETLINK_ROUTE` プロトコルでインタフェースを操作する場合は典型的にこのヘッダの後に

* メッセージ種別固有のメッセージボディ `ifinfomsg`
* 属性 `rtattr` 可変長リスト

が続きます。

```
| nlmsghdr | ifinfomsg | rtattr 1 | rtattr 2 | rtattr 3 | ... | rtattr N |
```

このため要求メッセージに必要な構造体を合成して一つの構造体とする下記のようなデータを定義する方法があります。

```c
struct iplink_req {
	struct nlmsghdr		n;
	struct ifinfomsg	i;
	char			buf[1024];
}
```

#### フィールド nlmsgn_len

このヘッダ + ボディ の長さを指定します。
マクロ `NLMSG_LENGTH` にボディのデータ長を渡すと正しい値が計算されます。

include/uapi/linux/netlink.h
```c
#define NLMSG_LENGTH(len) ((len) + NLMSG_HDRLEN)
```

#### フィールド nlmsg_type

送信するメッセージの種類を指定します。
ソケット作成時に `protocol` に指定したNetlinkの機能ごとにメッセージ種別が定義されています。
今回の例であれば `NETLINK_ROUTE` に対応するメッセージ種別はRTNETLINK(7)にドキュメンテーションされています。

例えばヘッダファイル `rtnetlink.h` の定数定義ではリンクの操作として下記のように4種類のメソッドが定義されています。

include/uapi/linux/rtnetlink.h
```c
	RTM_NEWLINK	= 16,
#define RTM_NEWLINK	RTM_NEWLINK
	RTM_DELLINK,
#define RTM_DELLINK	RTM_DELLINK
	RTM_GETLINK,
#define RTM_GETLINK	RTM_GETLINK
	RTM_SETLINK,
#define RTM_SETLINK	RTM_SETLINK
```

ちょうどCRUDのような操作種別が定義されているのが分かると思います。
この記事の想定しているネットワークインタフェース情報の取得であれば `RTM_GETLINK` を指定することになります。

今回は `RTM_GETLINK` を指定してインタフェース情報を取得してみようと思います。

#### フィールド nlmsg_flags

Netlinkに共通するメッセージのオプションをフラグ形式で指定します。

NETLINK(7)
```
       Standard flag bits in nlmsg_flags
       ──────────────────────────────────────────────────────────────────
       NLM_F_REQUEST   Must be set on all request messages.
       NLM_F_MULTI     The message is part of a multipart message termi‐
                       nated by NLMSG_DONE.
       NLM_F_ACK       Request for an acknowledgment on success.
       NLM_F_ECHO      Echo this request.

       Additional flag bits for GET requests
       ────────────────────────────────────────────────────────────────────
       NLM_F_ROOT     Return the complete table instead of a single entry.
       NLM_F_MATCH    Return  all entries matching criteria passed in mes‐
                      sage content.  Not implemented yet.

       NLM_F_ATOMIC   Return an atomic snapshot of the table.
       NLM_F_DUMP     Convenience macro; equivalent to
                      (NLM_F_ROOT|NLM_F_MATCH).
```

共通のフラグだけでなくGET系メッセージ固有のフラグも定義されていることが分かります。
今回のネットワークインタフェースの参照リクエストではこのGET系メッセージ用のフラグもつけます。

* `NLM_F_REQUEST` : リクエストであることを明示する
* `NLM_F_ACK` : メッセージ処理成功時にACKが返ることを要求する
* `NLM_F_DUMP` : 条件にマッチする内容をすべて取得する

今回は `NLM_F_REQUEST` と `NLM_F_DUMP` をフラグとして渡します。
`NLM_F_ACK` を指定するとACKメッセージの解析も必要になって手間が増えそうなので今回はやめておきます。
Netlinkサブシステムに確実にメッセージが届いたことを確認する必要があれば `NLM_F_ACK` をフラグを指定してACKメッセージを受け取るようにしてください。

#### フィールド nlmsg_seq

このシーケンス番号は要求メッセージと応答メッセージの対応を追跡するために使われます。


#### フィールド nlmsg_pid

アドレスに指定したポート番号と同じ数値を指定します。
名称はPIDとありますがプロセスIDを要求されているわけではなく、あくまでも通信端点の識別に必要な番号です。

#### インタフェース操作用メッセージ ifinfomsg

ネットワークインタフェースの操作に用いる要求メッセージには `ifinfomsg` という構造体を用います。
これはあくまでのインタフェース操作用のメッセージであるため、他の用途には使えません。
他にも経路制御用の構造体として `rtmsg` などがあります。

`ifinfomsg` はインタフェース作成・削除時などに使用するため、
どのインタフェースを操作するのかを特定するインデックスなどのフィールドを持ちます。

include/uapi/linux/rtnetlink.h
```c
/* struct ifinfomsg
 * passes link level specific information, not dependent
 * on network protocol.
 */

struct ifinfomsg {
	unsigned char	ifi_family;
	unsigned char	__ifi_pad;
	unsigned short	ifi_type;		/* ARPHRD_* */
	int		ifi_index;		/* Link index	*/
	unsigned	ifi_flags;		/* IFF_* flags	*/
	unsigned	ifi_change;		/* IFF_* change mask */
};
```

コードだけでは少し説明不足なのでmanも確認します。

RTNETLINK(7)
```
       RTM_NEWLINK, RTM_DELLINK, RTM_GETLINK
              Create, remove or get information about a specific network interface.  These messages contain an ifinfomsg structure  followed  by  a
              series of rtattr structures.

              struct ifinfomsg {
                  unsigned char  ifi_family; /* AF_UNSPEC */
                  unsigned short ifi_type;   /* Device type */
                  int            ifi_index;  /* Interface index */
                  unsigned int   ifi_flags;  /* Device flags  */
                  unsigned int   ifi_change; /* change mask */
              };

```

* `ifi_family` : `AF_UNSPEC` を指定します
* `ifi_type` : `ARPHDR_*` で定義されたリンクの種類
* `ifi_index` : インタフェースのインデックス
* `ifi_flags` : NETDFEVICE(7) にある `SIOCGIFFLAGS` の節を見よとのこと
* `ifi_change` :  manによると将来に向けて予約されているフィールドでありひとまず `0xFFFFFFFF` を指定しておくとのこと

##### ifi_typeについて

ソースコードのコメントに `ARPHRD_*` と書いてあるとおりARP関連ヘッダファイルに利用可能な定数の定義があります。

include/uapi/linux/if_arp.h
```c
/* ARP protocol HARDWARE identifiers. */
#define ARPHRD_NETROM	0		/* from KA9Q: NET/ROM pseudo	*/
#define ARPHRD_ETHER 	1		/* Ethernet 10Mbps		*/
#define	ARPHRD_EETHER	2		/* Experimental Ethernet	*/
#define	ARPHRD_AX25	3		/* AX.25 Level 2		*/
#define	ARPHRD_PRONET	4		/* PROnet token ring		*/
#define	ARPHRD_CHAOS	5		/* Chaosnet			*/
#define	ARPHRD_IEEE802	6		/* IEEE 802.2 Ethernet/TR/TB	*/
#define	ARPHRD_ARCNET	7		/* ARCnet			*/
#define	ARPHRD_APPLETLK	8		/* APPLEtalk			*/
#define ARPHRD_DLCI	15		/* Frame Relay DLCI		*/
#define ARPHRD_ATM	19		/* ATM 				*/
#define ARPHRD_METRICOM	23		/* Metricom STRIP (new IANA id)	*/
#define	ARPHRD_IEEE1394	24		/* IEEE 1394 IPv4 - RFC 2734	*/
#define ARPHRD_EUI64	27		/* EUI-64                       */
#define ARPHRD_INFINIBAND 32		/* InfiniBand			*/
```

一般的なEthernetのデバイスに対するクエリであれば `ARPHRD_ETHER` を指定します。

##### ifi_indexについて

システム上でインタフェースを一意に特定できるインデックスです。
インタフェース名とペアになっているため下記の関数によって名前からインデックスに解決することができます。

IF_NAMETOINDEX(3)
```
NAME
       if_nametoindex,  if_indextoname  - mappings between network inter‐
       face names and indexes

SYNOPSIS
       #include <net/if.h>

       unsigned int if_nametoindex(const char *ifname);

       char *if_indextoname(unsigned int ifindex, char *ifname);
```

### msghdr

ソケットで送信するデータを収める構造体である `msghdr` は下記のようになっています。

include/linux/socket.h
```c
/*
 *	As we do 4.4BSD message passing we use a 4.4BSD message passing
 *	system, not 4.3. Thus msg_accrights(len) are now missing. They
 *	belong in an obscure libc emulation or the bin.
 */
 
struct msghdr {
	void		*msg_name;	/* ptr to socket address structure */
	int		msg_namelen;	/* size of socket address structure */
	struct iov_iter	msg_iter;	/* data */
	void		*msg_control;	/* ancillary data */
	__kernel_size_t	msg_controllen;	/* ancillary data buffer length */
	unsigned int	msg_flags;	/* flags on received message */
	struct kiocb	*msg_iocb;	/* ptr to iocb for async requests */
};
```

しかし後方互換のために下記の構造体を渡すことができます。ここでは `iovec` というバッファを単純に扱えるこの互換構造を使います。

```c
struct compat_msghdr {
    compat_uptr_t   msg_name;   /* void * */
    compat_int_t    msg_namelen;
    compat_uptr_t   msg_iov;    /* struct compat_iovec * */
    compat_size_t   msg_iovlen;
    compat_uptr_t   msg_control;    /* void * */
    compat_size_t   msg_controllen;
    compat_uint_t   msg_flags;
};
```

* `msg_name` : ソケットアドレスへのポインタ
* `msg_namelen` : ソケットアドレスのデータ長
* `msg_iov` : 送信するリクエストのデータを格納するバッファ

#### iov_iter

`msghdr` にペイロードを埋め込むためのコンテナとして使います。

include/uapi/linux/uio.h
```c
struct iovec
{
        void __user *iov_base;  /* BSD uses caddr_t (1003.1g requires void>
        __kernel_size_t iov_len; /* Must be size_t (1003.1g) */
};
```

* `iov_base` : データの領域を指すポインタ
* `iov_len` : データ長

[The iov_iter interface][] 

4) 要求メッセージを送信
---

送信するメッセージとオープンしたソケットのファイルディスクリプタを使います。

SENDMSG(3P)
```
NAME
       sendmsg — send a message on a socket using a message structure

SYNOPSIS
       #include <sys/socket.h>

       ssize_t sendmsg(int socket, const struct msghdr *message, int flags);
```

* `socket` : ソケットのファイルディスクリプタ
* `messge` : 送信するメッセージを指すポインタ
* `flags` : 送信時の振る舞いを細かく制御するフラグ

返却値が負値の場合は送信に失敗しているため、 `errno` を参照してエラーハンドリングをします。


### コード例

```c
    if (sendmsg(fd, &msg, 0) < 0) {
        close(fd);
        perror("Sending message failed");
        return errno;
    }
```

5) 応答メッセージの受信
---

カーネルから応答を受け取る際には要求送信時に利用したソケットのファイルディスクリプタとヘッダ `msghdr` を使い回すようです。
[iproute2のlibnetlink.c __rtnl_talk_iov](https://github.com/shemminger/iproute2/blob/master/lib/libnetlink.c#L863)を参照。

### バッファの初期化

送信時に使った `msghdr` に埋め込まれている `iovec` を初期化することで受信データをカーネルからユーザ空間にコピーする空き領域を作ります。

```c
#define MAX_RECV_BUF_LEN 32768

    char recv_buf[MAX_RECV_BUF_LEN];
    struct iovec riov = {
        .iov_base = &recv_buf,
        .iov_len = MAX_RECV_BUF_LEN
    };
	msg.msg_iov = &riov;
	msg.msg_iovlen = 1;
```

`msg` は送信時に使った `msghdr` を使い回します。
このヘッダ構造のインスタンスにはソケットのアドレスが紐付けられているため、
使い回すことで引き続き同じ通信アドレスを利用できます。

ただし `msg` のペイロード部分には新しく受信したデータを格納する領域が必要であるため、
`iovec` を新しく確保して `msg` にそのポインタを埋め込みます。
`iovec` 内部のデータ格納用領域にはバイト配列である `recv_buf` を使います。

### メッセージの受信

ソケットから受信したデータを取り出すには `recvmsg` を使います。

RECVMSG(3P)
```
NAME
       recvmsg — receive a message from a socket

SYNOPSIS
       #include <sys/socket.h>

       ssize_t recvmsg(int socket, struct msghdr *message, int flags);
```

* `socket` : ソケットのファイルディスクリプタ
* `messge` : 受信するメッセージを指すポインタ
* `flags` : 受信時の振る舞いを細かく制御するフラグ

`sendmsg` と同様に負値

### コード例

```c
        int recv_len = recvmsg(fd, &msg, 0);
        if (recv_len < 0) {
            close(fd);
            perror("Receiving message failed");
            return errno;
        }
```

6) 応答メッセージの解析
---

メッセージの受信に成功するとNetlinkサブシステムから受信したデータを格納したアドレスを指すポインタが手に入ります。
メッセージの解析はこのポインタを起点としてアドレスを移動しながらデータを読み出していきます。

いい感じの解析方法の大枠は `man` ページに書いてあるのでそれを参考にします。

NETLINK(7)
```
           for (nh = (struct nlmsghdr *) buf; NLMSG_OK (nh, len);
                nh = NLMSG_NEXT (nh, len)) {
               /* The end of multipart message */
               if (nh->nlmsg_type == NLMSG_DONE)
                   return;

               if (nh->nlmsg_type == NLMSG_ERROR)
                   /* Do some error handling */
               ...

               /* Continue with parsing payload */
               ...
           }
```

メッセージの解析にはマクロを活用します。

### NLMSG_OK

`NLMSG_OK` は指定された `nlmsghdr` のアドレスと `recvmsg` で得られたデータ長から正しい長さのデータが受信できているかを判定します。

NETLINK(3)
```
       int NLMSG_OK(struct nlmsghdr *nlh, int len);
```

ソケットから読み込んだメッセージが正しい長さである場合に解析処理を続行するための判定に使われます。

```c
           for (nh = (struct nlmsghdr *) buf; NLMSG_OK (nh, len);
                nh = NLMSG_NEXT (nh, len)) {
```

### NLMSG_NEXT

`NLMSG_NEXT` は指定された `nlmsghdr` のアドレスと `recvmsg` で得られたデータ長を元に次の `mlmsghdr` を指すアドレスを返します。

NETLINK(3)
```
       struct nlmsghdr *NLMSG_NEXT(struct nlmsghdr *nlh, int len);
```

このマクロはソケットから一度に複数のメッセージを読み出せた場合に
2つめ以降のメッセージの先頭のポインタを取得するために使います。

### コード例

```c
    struct nlmsghdr *rnh = recv_buf;
    if (!NLMSG_OK(rnh, recved)) {
        perror("Maybe received multi-part message in response");
        goto handle_failure;
    }
```

### 解析部分のコード例

下記のコード例では解析したデータをただ印字するだけになっています。

```c
    struct nlmsghdr *rnh;
    for (rnh = (struct nlmsghdr *) recv_buf; NLMSG_OK (rnh, recv_len); rnh = NLMSG_NEXT (rnh, recv_len)) {
        /* The end of multipart message */
        if (rnh->nlmsg_type == NLMSG_DONE) {
            printf("NLMSG_DONE detected\n");
            break;
        }
        if (rnh->nlmsg_type == NLMSG_ERROR) {
            perror("Receiving message failed");
            return errno;
        }

        printf("Received nlmsghdr\n");
        printf("\tnlhdr address: %p\n", rnh);
        printf("\tnlmsg_len: %d\n", rnh->nlmsg_len);
        printf("\tnlmsg_type: %d\n", rnh->nlmsg_type);
        printf("\tnlmsg_flags: %x\n", rnh->nlmsg_flags);
        printf("\tnlmsg_seq: %d\n", rnh->nlmsg_seq);
        printf("\tnlmsg_pid: %d\n", rnh->nlmsg_pid);

        struct ifinfomsg *riim = NLMSG_DATA(rnh);
        printf("Received ifinfomsg\n");
        printf("\tifi_type: %d\n", riim->ifi_type);
        printf("\tifi_index: %d\n", riim->ifi_index);
        printf("\tifi_flags: %x\n", riim->ifi_flags);
        printf("\tifi_change: %x\n", riim->ifi_change);
    }
```

全体のコードは [こちら](https://gist.github.com/utky/de8b37756f8664d0045f829292657ae5)

参考
---

Netlinkに関する情報を掴むのには下記が有用でした。

1. Linux Kernalのソースコード
2. [iproute2][iproute2] のソースコード
3. man
4. 下記のリンク

* [Kernel Korner - Why and How to Use Netlink Socket][Kernel Korner - Why and How to Use Netlink Socket]
* [Manipulating the Networking Environment Using RTNETLINK][Manipulating the Networking Environment Using RTNETLINK]
* [Understanding And Programming With Netlink Sockets][Understanding And Programming With Netlink Sockets]
* [The iov_iter interface][The iov_iter interface]
* [Linux, Netlink, and Go — Part 1: netlink][Linux, Netlink, and Go — Part 1: netlink]

[Kernel Korner - Why and How to Use Netlink Socket]: https://www.linuxjournal.com/article/7356
[Manipulating the Networking Environment Using RTNETLINK]: https://www.linuxjournal.com/article/8498
[Understanding And Programming With Netlink Sockets]: https://people.redhat.com/nhorman/papers/netlink.pdf
[iproute2]: https://github.com/shemminger/iproute2
[The iov_iter interface]: https://lwn.net/Articles/625077/
[Linux, Netlink, and Go — Part 1: netlink]: https://medium.com/@mdlayher/linux-netlink-and-go-part-1-netlink-4781aaeeaca8

