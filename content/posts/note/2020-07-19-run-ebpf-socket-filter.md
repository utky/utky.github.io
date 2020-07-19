+++
title = "eBPFプログラムの実行方法を調べる"
date = 2020-07-19
[taxonomies]
tags = ["linux","ebpf","network"]
+++

## はじめに

eBPFがLinuxカーネル内で実行されるプログラムの仕組みであることは知っている。
けれどそれ以上の情報をよく知らないので実際に動かしてみて解った振りをしたい。

いろいろeBPFプログラムを書いて試したいので実行方法をまずは確認することにした。

## その前に

eBPFプログラムの実行方法に関するサンプルは既にLinuxカーネルのリポジトリに大量に提供されている。

[Linux 5.7.7 の samples/bpf]

なのでC言語で遊びたい人はそこを読んで、manでリファレンス引けば特に苦労せずにeBPFプログラムを実行することができるはず。
つまりここから先は読む必要ないので読まなくても良いと思う。

## 問題: eBPFプログラムを実行する小さいプログラムが欲しい

eBPFプログラムを実行するにはどうすればよいのだろうか。
といってもHello Worldを印字するプログラムを作って実行できればよい、という風にはいかない。

eBPFプログラムはカーネル内のイベントに反応して実行されるように設計されているためだ。
つまりeBPFプログラムはカーネルに組み込んでカーネルから実行される。Hello Worldプログラムのように独立して実行できるものではない。eBPFプログラムを作る人はカーネルからどのイベントを契機にして呼び出されて欲しいかを、あらかじめ決定しておく必要がある。

というよりは特定のイベントから情報を集めたいがためにeBPFプログラムを書くという方が実態に近いはずなので、この記事のように「eBPFプログラムをいじりたいので使えそうなイベントを決める」というは順序が逆。

イベント発生元とeBPFプログラムを関連付けることをアタッチという。
eBPFプログラムをどのイベントにアタッチするかによってその方法は異なる。

代表的なアタッチ方法として下記の二つが [bpf(2)] でも言及されている。

1) socketへのアタッチ (Linux 3.19 以降)

```c
setsockopt(sockfd, SOL_SOCKET, SO_ATTACH_BPF, &prog_fd, sizeof(prog_fd));
```

2) perf eventへのアタッチ (Linux 4.1 以降)

```c
ioctl(event_fd, PERF_EVENT_IOC_SET_BPF, prog_fd);
```

他にもカーネルのバージョンアップにつれて対応するイベントは増えている。
これはeBPFプログラムのtypeとして定義されており、下記のヘッダファイルに一覧されている。

[bpf_prog_type]

この定義を利用したサンプルプログラムもカーネルのリポジトリには含まれているので、
より詳細に実例を知りたい場合は見ると参考になる。

[samples/bpf/bpf_load.c]

アタッチされたプログラムをカーネルが呼び出すためにはカーネルのメモリにプログラムがロードされている必要がある。これをeBPFプログラムのロードと呼ぶ。
カーネルにロードを指示するためのシステムコールは当然用意されている。

```c
int bpf(int cmd, union bpf_attr *attr, unsigned int size);
```

[bpf(2)]

このシステムコールはロードだけでなく、様々なeBPFに関する操作を提供している。

今回はeBPFプログラムをsocketへアタッチするようなごく単純なプログラムをRustで記述してみることにした。

socketは自ホストのループバックインタフェースからのパケットを取り出せるようにする。
eBPFプログラムはこのsocketから取り出されるパケットをフィルタするためにアタッチされる。
フィルタに合致したパケットだけがsocketから取り出せるようになるわけだが、
この記事ではeBPFプログラムの実行にフォーカスするためeBPFプログラムは単純なものにしておく。
いずれ別の記事の中でより複雑なeBPFプログラムを組んでいくことにする。

## 環境

Linux Kernel 5.7.7
```
 % uname -a
Linux archlinux 5.7.7-arch1-1 #1 SMP PREEMPT Wed, 01 Jul 2020 14:53:16 +0000 x86_64 GNU/Linux
```

正直、新しすぎて仕事場で使う際の参考にならないのだが。。。
できるだけ新しい機能よりも3.19あたりから入っている機能のみを使っていくように心がける。

Rust 1.44.1
```
[dependencies]
libc = "0.2"
```

libc crateだけだとbpf系のヘッダもないし、bpfシステムコールもないのだが、
この記事の目的がbpfを知ることなのであえて自分で調べて実装する道を選んでおく。

## socketへeBPFプログラムをアタッチする流れ

プログラムは大きく4つのステップに分解される。

1. ループバックインタフェースからパケットを受け取るためのsocket作成
2. eBPFプログラムのロード
3. eBPFプログラムのアタッチ
4. socketからフィルタ済みパケットの読み出し

C言語でこれらを書くならどうすれば良いのか、これも充分なサンプルが用意されている。

1. [socket作成]
2. [ロード]
3. [アタッチ]

C言語での参考実装は上記にあるのでそれを見れば充分だったりする。
今回はRustで書いてみることにしているが、libc crateを使ってunsafeばかりやってるのは許してほしい。

### socket作成

socketのセットアップは普通にlibc crateから提供されるAPIを使う。

#### socketを作る

ループバックインタフェースからEthernetのパケットを抽出したいので、
AF_PACKETのsocketを作る。

_要注意_

`ETH_P_ALL`はlibc crateとしてはc_intで定義されているが、APIとして要求するのはc_ushortであるため、あらかじめ下記を定義しておいてそちらを使うことにする。
```rust
const ETH_P_ALL_NS: c_ushort = (libc::ETH_P_ALL as c_ushort).to_be();
```

これを利用してsocket作成を行うコードは下記になる。
```rust
  let sock = socket(
     libc::AF_PACKET,
     libc::SOCK_RAW | libc::SOCK_CLOEXEC,
     ETH_P_ALL_NS).expect("Failed to create socket");
```

[packet(7)] を読むとフラグの立て方がよく書かれているのでほぼそのまま。

ただし`libc::socket`はunsafeだしエラーもハンドルしたいので、若干のラッパーをはさんでいる。
```rust
fn socket(domain: c_int, ty: c_int, protocol: c_int) -> Result<RawFd> {
  let sock_fd = unsafe { libc::socket(domain, ty, protocol) };
  if sock_fd < 0 {
    Err(Error::last_os_error())
  }
  else {
    Ok(sock_fd)
  }
}
```

errnoを取ってResultとして伝搬させたい、くらいの意図。

#### `lo` デバイス用のsockaddr_ll作る

socketを`bind`する前にどのインタフェースからパケットを抽出するかをカーネルに伝えるために、
`sockaddr_ll`を作る

```rust
  let sockaddr = new_sockaddr_ll("lo");
```

`new_sockaddr_ll`は下記のように構造体の初期化を行う。

```rust
fn new_sockaddr_ll(devname: &str) -> sockaddr_ll {
 let devname_cstring = CString::new(devname.clone()).expect("Failed to allocate device name");
 let mut sll: sockaddr_ll = unsafe { zeroed() };
 sll.sll_family = libc::AF_PACKET as c_ushort;
 sll.sll_ifindex = unsafe { libc::if_nametoindex(devname_cstring.as_ptr()) as c_int };
 sll.sll_protocol = ETH_P_ALL_NS;
 sll
}
```

`if_nametoindex`でループバックデバイスの名称`"lo"`からインタフェースをシステム内で識別するインデックスを取得して`sockaddr_ll`に設定する。これによってパケットを抽出する対象のインタフェースを指定する。

#### bindする

```rust
  bind(&sock, sockaddr).expect("Failed to bind socket to lo device");
```

先ほどの`socket`関数と同様にResultラッパーをかませている。
```rust
fn bind<S>(sock: &RawFd, sockaddr: S) -> Result<()>
  where S: SocketAddress {
  let result = unsafe { libc::bind(*sock, sockaddr.as_sockaddr(), sockaddr.length()) };
  if result < 0 {
    Err(Error::last_os_error())
  }
  else {
    Ok(())
  }
}
```

`bind`時に`sockaddr`の構造体へのポインタを渡すが、作ったsocketの種類によって実際に参照される構造体は異なる。上記の場合ではネットワークデバイスからEthernetフレームを取り出す用途に適した構造体`sockaddr_ll`を利用する。

この`sockaddr`は多態性を持たせてあげる必要がある、ということなのでtraitを使う。
`sockaddr`のような性質をもつ`sockaddr_ll`という振る舞いをtraitで表す。

```rust
trait SocketAddress {
  fn length(&self) -> socklen_t;
  fn as_sockaddr(&self) -> *const sockaddr;
}

impl SocketAddress for sockaddr_ll {
  fn length(&self) -> socklen_t {
    size_of::<sockaddr_ll>() as socklen_t
  }
  fn as_sockaddr(&self) -> *const sockaddr {
    unsafe { transmute::<*const sockaddr_ll, *const sockaddr>(self) }
  }
}
```
`transmute`を使って`sockaddr_ll`のポインタを`sockaddr`のポインタの型へと翻訳しておく。
こうすることで`sockaddr_ll`を`sockaddr`と見せて、[bind(2)]のインタフェースを満たすことができる。
実装する構造体の分だけ`size_of`や`transmute`を呼び出すことになってしまうので、もっといいやり方がありそうな気はする。

### ロード

割とここが一番面倒だった。

#### 命令列を作る

カーネル内で実行されるeBPFの命令列を一式用意する。
命令のバイナリデータをアセンブラぽいものから出力するためのツールも既に結構揃っていたりする。

[tools/bpf]

などにbpf_asmといったアセンブラも実装されている。
今回はRustで環境を作っているので、とりあえずRustで命令を作る。
ちなみにC言語では[bpf_insn]という構造体で一つの命令を表す。

Rustで命令を表す構造体はlibc crateで未定義なので自前で定義する。
```rust
// op:8, dst_reg:4, src_reg:4, off:16, imm:32
#[derive(Debug)]
#[repr(C)]
struct BpfInsn {
  op: u8,
  reg: u8,
  off: u16,
  imm: u32
}
```

レジスタ8bitは上位4ビットと下位4ビットでそれぞれsrc/dstが使い分けされる。
C言語ではbitfieldが構文上使えるがRustではどうやら組み込みでなさそうなので、
ひとまず8ビットに丸め込んでおく。

このデータ構造を活用して小さめのeBPFプログラムを作っていきたいが、小さなプログラムとは何だろうか。
「ただexitする」プログラムをまずは最低限として考えているのがちょうどよさそうだ。
なので返却値を設定して終了する、という2つの命令だけをここでは記述する。

C言語で書くならマクロと列挙型を使って下記のように書ける。
```c
struct bpf_insn prog[] = {
  BPF_MOV64_IMM(BPF_REG_0, -1),
  BPF_EXIT_INSN()
};
```

※ 命令生成のマクロは[include/linux/filter.h]を参照

ここで返却値に負値を設定しているとまるで異常終了かのようにみえるがsocketフィルタリングのプログラムではその意味は異なる。
socketのフィルタリングにおける返却値-1は「すべてのパケットをsocketに通す」ということを示す。
0以上の値の場合はそのバイト数分だけパケットのデータをsocketに渡す。
つまり0であれば0バイト分なので「パケットをsocketに通さない」となる。
今回は単純のために全てのパケットを通したいので常に-1を返すプログラムを作成する。

libc crateのみではそれも無いのでとりあえず手書きで命令を作る。

```rust
  let insns = vec![
    // BPF_MOV64_IMM(BPF_REG_0, -1),
    BpfInsn{ op: 0x07 | 0xb0 | 0x00, reg: 0, off: 0, imm: (!1 + 1)},
    // BPF_EXIT_INSN()
    BpfInsn{ op: 0x05 | 0x90, reg: 0, off: 0, imm: 0 },
  ];
```
(immはu32なので1に対して2の補数をとって負値をunsigned上で作っている)

opcodeを合成している箇所で命令を決定している。
それぞれ書きのようにコードを組み合わせている。

R0に-1を格納する。R0は返却値として使われるので、この場合はeBPFのプログラムの終了コードを表すことになる。
[BPF_MOV64_IMM()](https://elixir.bootlin.com/linux/v5.7.7/source/include/linux/filter.h#L151)

* `0x07` : BPF_ALU64 64ビット算術演算命令
* `0xb0` : BPF_MOV srcレジスタからdstレジスタへのコピー
* `0x00` : BPF_K 即値モード

eBPFプログラムを終了させる。
[BPF_EXIT_INSN()](https://elixir.bootlin.com/linux/v5.7.7/source/include/linux/filter.h#L366)

* `0x05` : BPF_JMP jump系の命令
* `0x90` : BPF_EXIT プログラムの終了

#### ロード用パラメータを作る

eBPFプログラムをカーネルにロードさせる場合には下記のような構造体を作って渡す必要がある。
といってもsocketのフィルタの場合は全部埋めなくても良いのでまだ楽。

```rust
 #[derive(Debug)]
#[repr(C)]
struct BpfLoadAttr {
  prog_type: u32,
  insn_cnt: u32,
  insns: *const BpfInsn,
  license: *const c_char,
  log_level: u32,
  log_size: u32,
  log_buf: *mut c_char,
  kern_version: u32,
  prog_flags: u32,
  prog_name: *const c_char,
  prog_ifindex: u32,
  expected_attach_type: u32
}
```

C言語では共用体として定義されており、メンバーはそのままRustでも流用できる。

[bpf_attr for BPF_PROG_LOAD]

カーネルの進化に応じていろいろメンバーが増えている。
しかし全てのメンバーをRust側に実装することはせず、
過去のサンプルなどを見つつ最低限のメンバーを定義することにした。
それが上記のRust版になる。

各メンバーの意味は未調査のところもあるものの下記のようになっているようだった。

* `prog_type: u32` : [bpf_prog_type] を指定する。こうしてみるとめちゃくちゃ種類あるな。
* `insn_cnt: u32` : 命令列の長さ。上記の例だと2命令なので2になる。
* `insns: *const BpfInsn` : 命令列へのポインタ。
* `license: *const c_char` : プログラムのライセンスを表す長さ128のサイズの文字列。
* `log_level: u32` : ログ出力のレベル、とりあえず1にしておけばある程度出る。2にするともうちょっと出るっぽいが未検証。
* `log_size: u32` : 次に述べるバッファの長さ。
* `log_buf: *mut c_char` : eBPFプログラムの検査器が出力するログを格納する文字列バッファ
* `kern_version: u32` : kprobeのイベントを扱う時は対応カーネルのバージョンを指定する。それ以外の時は0でも大丈夫そう。
* `prog_flags: u32` : 未調査。
* `prog_name: *const c_char` : ユーザ側で指定する任意のプログラム名(最大16バイト)。空文字でもいい。
* `prog_ifindex: u32` : XDPを使う場合のifindexのようだが、今回は使わない。
* `expected_attach_type: u32` : 未調査。

これらのメンバーを埋めるために色々さぼりながら書いたのが下記のコード。

```rust
    let mut log_buf: [c_char; LOG_BUF_FIZE] = [0; LOG_BUF_FIZE];
    // GPLの文字列を c_char の固定長ベクタに詰め込んで後で配列のポインタを渡す
    let license = String::from("GPL");
    let mut license_buf: Vec<c_char> = Vec::with_capacity(LICENSE_BUF_SIZE);
    license.chars().into_iter().take(LICENSE_BUF_SIZE).enumerate().for_each({|e|
      license_buf.insert(e.0, e.1 as c_char)
    });

    let attr = BpfLoadAttr {
      prog_type: prog_type,
      insn_cnt: insns.len() as u32,
      insns: insns.as_ptr(),
      license: license_buf.as_ptr() as *const c_char,
      log_level: 1, /* 1 = debug, 2 = trace */
      log_size: LOG_BUF_FIZE as u32,
      log_buf: log_buf.as_mut_ptr(),
      kern_version: 0,
      prog_flags: 0,
      // ここは面倒なのでとりあえず空の文字列にしてしまっている
      prog_name: std::ptr::null(),
      prog_ifindex: 0,
      expected_attach_type: 0
    };
```

### bpfシステムコールを呼び出す

libc crateにはbpfシステムコール用APIがなかったので`libc::syscall`を直接呼ぶことにした。

```rust
    let result = unsafe {
      libc::syscall(
         libc::SYS_bpf,
         5, // BPF_PROG_LOAD
         &attr as *const BpfLoadAttr,
         size_of::<BpfLoadAttr>() as c_long
      )
    };
```

ちなみにこのsyscallはC言語では可変長引数を使って定義されている。
しかしRustの関数には原則的に可変長引数は使えない。
FFI用の関数のみ特別に使えるらしい。

したがってRust自身では可変長引数が使えないために`syscall`呼び出し部分がrust-analyzerではエラー扱いとなってしまうようだった。
しかし`cargo build`するとコンパイルはできてしまうのだった。

bpfシステムコールのインタフェースは3つの引数を取るようになっている。
```c
int bpf(int cmd, union bpf_attr *attr, unsigned int size);
```

とりあえず何かと`bpf_attr`に詰め込んでカーネルに渡すスタイル。

`cmd`は下記に定義ある[bpf_cmd]から選択する。
今回の場合はプログラムのロードなので`BPF_PROG_LOAD = 5`を指定している。

というかこうしてみるとコマンドがものすごく増えている。。。

`BPF_PROG_LOAD`の実行結果はロードされたプログラムのfdが返ってくる。
これがプロセス内でeBPFプログラムを識別するキーになるのでどこかに記憶しておくことになるだろう。

### アタッチ

ロードまで終わったら後は下り坂となる。
socketのオプションとしてeBPFプログラムのfdを渡してやるだけでよい。

下記ではeBPFプログラムのfd`prog.fd`を持つsocketオプションを作ってsocketに設定する。

```rust
  let opt = AttachBpf { prog_fd: prog.fd };
  setsockopt(&sock, opt).expect("Failed to set sockopt");
```

socketオプションも設定するデータの種類によって多態性を持つのでやはり`SocketOption`というtraitを定義して抽象化している。
オプション`SO_ATTACH_BPF`にはeBPFプログラムのfdを渡せばよいのでそれに合わせてtraitの実装をした。
```rust
trait SocketOption {
  fn level(&self) -> c_int;
  fn name(&self) -> c_int;
  fn length(&self) -> socklen_t;
  fn value(&self) -> *const c_void;
}

struct AttachBpf {
  prog_fd: RawFd
}

impl SocketOption for AttachBpf {
  fn level(&self) -> c_int {
    libc::SOL_SOCKET
  }
  fn name(&self) -> c_int {
    libc::SO_ATTACH_BPF
  }
  fn length(&self) -> socklen_t {
    size_of::<c_int>() as socklen_t
  }
  fn value(&self) -> *const c_void {
    unsafe { transmute::<&c_int, *const c_void>(&(self.prog_fd)) }
  }
}

fn setsockopt<O>(sock: &RawFd, opt: O) -> Result<()>
  where O: SocketOption {
  let result = unsafe { libc::setsockopt(*sock, opt.level(), opt.name(), opt.value(), opt.length()) };
  if result < 0 {
    Err(Error::last_os_error())
  }
  else {
    Ok(())
  }
}
```

## socketからフィルタ済みパケットの読み出し

既に使えるsocketのfdは手元にあるわけなので、あとはfdからパケットのデータを読み出せればよい。
パケットはストリームというよりデータグラムっぽい読み出しの方が適切そうなので`UnixDatagram#recv`を使ってバッファに読み込むようにしている。

なお、生のfdからハンドラを作るのはunsafeな操作であり、別途`std::os::unix::io::FromRawFd`をuseしてスコープに持ってきておく必要がある。

```rust
  let mut buf: [u8; 1500] = [0; 1500];
  let s = unsafe { std::os::unix::net::UnixDatagram::from_raw_fd(sock) };
  loop {
    match s.recv(&mut buf) {
      Err(e) => {
        println!("Failed to read from device: {:?}", e);
        break;
      },
      Ok(read) => {
        println!("read {} bytes", read);
        std::thread::sleep(std::time::Duration::from_secs(1));
      }
    }
  }
```

## プログラムの全体像

gistにおいた。

[run_ebpf_socket_filter_main.rs](https://gist.github.com/utky/474e95a4834aa6c78ec3be4b96d4048d)

実行してとりあえずパケットが取れることを確認するには、このプログラムをビルドして実行しつつ、別のターミナルから通信を発生させればよい。

あらかじめ localhost 宛てに通信を発生させておく。
```sh
$ ping localhost
PING localhost(localhost (::1)) 56 data bytes
64 bytes from localhost (::1): icmp_seq=1 ttl=64 time=0.011 ms
64 bytes from localhost (::1): icmp_seq=2 ttl=64 time=0.022 ms
```

ビルドしたプログラムをsudoつきで走らせるとデータが読めていることが分かる。
```sh
$ sudo ./target/debug/run_ebpf_socket_filter
read 66 bytes
read 66 bytes
read 132 byte
```

eBPFプログラムが確かに作用していることを確認するために1つ目の命令のimmを0にしてみる。
```rust
    BpfInsn{ op: 0x07 | 0xb0 | 0x00, reg: 0, off: 0, imm: 0},
```

ビルドされたプログラムは何もパケットを受信できずバッファへの読み込みでブロックしてしまう。
```sh
$ sudo ./target/debug/run_ebpf_socket_filter

```

## まとめ

eBPFプログラム自体は概ね下記の3ステップを準備するとカーネルから実行されるようにできた。

1. 命令列の作成
2. eBPFプログラムのロード
3. eBPFプログラムのアタッチ

これでeBPFプログラムの実行環境が整ったので、より複雑なコードを実装して試すことができるようになった。
今後はこれを下地にしてsocketフィルタリングを中心としてeBPFプログラミングを試してみたい。

Rustについては所有権に応じたデータの変換をするので一杯いっぱいで、まだまだ練習の余地がかなりある。。。

[Linux 5.7.7 の samples/bpf]: https://elixir.bootlin.com/linux/v5.7.7/source/samples/bpf
[samples/bpf/bpf_load.c]: https://elixir.bootlin.com/linux/v5.7.7/source/samples/bpf/bpf_load.c
[socket作成]: https://elixir.bootlin.com/linux/v5.7.7/source/samples/bpf/sock_example.h#L13
[ロード]: https://elixir.bootlin.com/linux/v5.7.7/source/samples/bpf/bpf_load.c#L131
[アタッチ]: https://elixir.bootlin.com/linux/v5.7.7/source/samples/bpf/sockex1_user.c#L29
[bind(2)]: https://man7.org/linux/man-pages/man2/bind.2.html
[bpf(2)]: https://man7.org/linux/man-pages/man2/bpf.2.html
[packet(7)]: https://man7.org/linux/man-pages/man7/packet.7.html
[include/linux/filter.h]: https://elixir.bootlin.com/linux/v5.7.7/source/include/linux/filter.h
[tools/bpf]: https://elixir.bootlin.com/linux/v5.7.7/source/tools/bpf
[bpf_insn]: https://elixir.bootlin.com/linux/v5.7.7/source/include/uapi/linux/bpf.h#L65
[bpf_attr for BPF_PROG_LOAD]: https://elixir.bootlin.com/linux/v5.7.7/source/include/uapi/linux/bpf.h#L455
[bpf_prog_type]: https://elixir.bootlin.com/linux/v5.7.7/source/include/uapi/linux/bpf.h#L156
[bpf_cmd]: https://elixir.bootlin.com/linux/v5.7.7/source/include/uapi/linux/bpf.h#L85
