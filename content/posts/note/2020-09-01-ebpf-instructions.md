+++
title = "eBPF命令をデータ型で表す"
date = 2020-09-01
[taxonomies]
tags = ["linux","ebpf","network"]
+++

[前回]はeBPFのごく単純なプログラムを書いて実行可能なユーザ空間側のプログラムを作ることに専念した。
今回は色々な命令を試していくにあたっての準備として、命令をRust上で作りやすいようにする。

参考にするのは
* [Linux Socket Filtering aka Berkeley Packet Filter (BPF)]
    + ざっくり命令の分類を把握する
* [disasm.c]
    + オペレータの記号的な表現をつかむ

[JIT]の実装を読む、という手もあるがそもそもEMITされている命令が何なのか読み解けないので諦めた。

##　TLDR

[こんな感じの素朴なやつになった](https://github.com/utky/rbc/blob/ffe9ad1/src/asm/mod.rs)

## フォーマット

eBPFの命令フォーマットは下記のように定義されている。

```
op:8, dst_reg:4, src_reg:4, off:16, imm:32
```

合計64bitで一つの命令を表し、これが配列として並べられたものをeBPFプログラムと呼ぶ。

例えばC言語上で作った下記の命令があるとする。

```c
  struct bpf_insn insns[] = {
    { BPF_ALU64 | BPF_MOV | BPF_K, 0, 0, 0, -1 },
    { BPF_JMP | BPF_EXIT, 0, 0, 0, 0 }
  };
```
これらをメモリ上で表現すると下記のようになる。
```
0x7fffffffe800: 0xb7    0x00    0x00    0x00    0xff    0xff    0xff    0xff
0x7fffffffe808: 0x95    0x00    0x00    0x00    0x00    0x00    0x00    0x00
```

最初の `BPF_MOV` 命令をフィールドに分解すると下記のようになる。
```
0xb7    0x00    0x00    0x00    0xff    0xff    0xff    0xff
|opcode |reg    |off            |imm                        |
```

カーネルはメモリに配置されたこのバイト列を64bitごとに命令として解析していることになる。
このような命令をRustから作れるようにするべくデータ構造を考える。

## 命令の種類

eBPFの命令の種類をクラス(BPF_CLASS)と呼び、命令の大きな分類を表現する。

* BPF_LD
* BPF_LDX
* BPF_ST
* BPF_STX
* BPF_ALU
* BPF_JMP
* BPF_JMP32
* BPF_ALU64

これらには暗黙的にグループ分けが存在しており、そのグループによって opcode フィールドの形式がやや異なる。
現時点の実装では2種類のグループがある。

### arithmetic and jump
```
  +----------------+--------+--------------------+
  |   4 bits       |  1 bit |   3 bits           |
  | operation code | source | instruction class  |
  +----------------+--------+--------------------+
  (MSB)                                      (LSB)
```
算術演算とジャンプを行うための命令グループ。

### load and store
```
  +--------+--------+-------------------+
  | 3 bits | 2 bits |   3 bits          |
  |  mode  |  size  | instruction class |
  +--------+--------+-------------------+
  (MSB)                             (LSB)
```
メモリからレジスタへのデータの移動を行うための命令グループ。

Rust上ではこの命令クラス別のフィールドの違いを`enum`で下記のように表現する。
各フィールドが取りうる値の範囲を正しく制限できるように、`Mode`、`Size`、`Alu`、`Jmp`、`Src`もそれぞれ`enum`として定義しておく。
```rust
#[derive(Debug)]
pub enum Opcode {
  Ld(Mode, Size),
  Ldx(Mode, Size),
  St(Mode, Size),
  Stx(Mode, Size),
  Alu(Alu, Src),
  Jmp(Jmp, Src),
  Jmp32(Jmp, Src),
  Alu64(Alu, Src),
}
```
クラスを8ビットのデータに変換する方法は`From`トレイトを使う。
`Mode`や`Size`等の各フィールドのデータも`From`トレイトで`u8`にエンコードできるようにしておく。
```rust
impl From<Opcode> for u8 {
  fn from(o: Opcode) -> u8 {
    match o {
      Opcode::Ld(mode, size)  => u8::from(mode) | u8::from(size) | 0x00,
      Opcode::Ldx(mode, size) => u8::from(mode) | u8::from(size) | 0x01,
      Opcode::St(mode, size)  => u8::from(mode) | u8::from(size) | 0x02,
      Opcode::Stx(mode, size) => u8::from(mode) | u8::from(size) | 0x03,
      Opcode::Alu(alu, src)   => u8::from(alu)  | u8::from(src)  | 0x04,
      Opcode::Jmp(jmp, src)   => u8::from(jmp)  | u8::from(src)  | 0x05,
      Opcode::Jmp32(jmp, src) => u8::from(jmp)  | u8::from(src)  | 0x06,
      Opcode::Alu64(alu, src) => u8::from(alu)  | u8::from(src)  | 0x07,
    }
  }
}
```

ALU、JMPのクラスはさらに細分化された命令があるため、それぞれどういう操作が可能なのかを把握しておく必要がある。

あまりアセンブリ言語や抽象機械の命令をまじまじと眺めたことがないので、
結構よく解らない命令もちょくちょくあるので分からないところを拾っている。

どんな命令かを直感的につかむには [disasm.c] でのhuman readableなオペレータの記号を読むと大体わかる。

### 算術演算

算術演算オペレータ。

disasm上での表記は下記を参考にする。
[disasm.c#L66](https://elixir.bootlin.com/linux/v5.7.7/source/kernel/bpf/disasm.c#L66)

* BPF_ADD: `+=`
* BPF_SUB: `-=`
* BPF_MUL: `*=`
* BPF_DIV: `/=`
* BPF_OR: `|=`
* BPF_AND: `&=`
* BPF_LSH: `<<=`
* BPF_RSH: `>>=`
* BPF_NEG: `neg`
* BPF_MOD: `%=`
* BPF_XOR: `^=`
* BPF_MOV: `=`
* BPF_ARSH: `s>>=` 符号を維持したまま右シフト
* BPF_END: `endian` エンディアンの変換

[Linux Socket Filtering aka Berkeley Packet Filter (BPF)] にはとくに言及がないけど、
BPF_ENDはendinessを指定する必要がある。

[変換先endinessの定義](https://elixir.bootlin.com/linux/v5.7.7/source/include/uapi/linux/bpf.h#L32)

### ジャンプ

dst_reg と (src_reg または imm) を比較して true なら off の分だけプログラムカウンタに加算した場所へ飛ぶ。
false なら fall-through でそのままプログラムカウンタをインクリメントする。

disasm上での表記は下記を参考にする
[disasm.c#L90](https://elixir.bootlin.com/linux/v5.7.7/source/kernel/bpf/disasm.c#L90)

#### 同値性テスト

* BPF_JEQ: `==`
* BPF_JNE: `!=`

#### 符号なし整数の比較

* BPF_JGT: `>`
* BPF_JGE: `>=`
* BPF_JLT: `<`
* BPF_JLE: `<=`
* BPF_JSET: `&`

#### 符号つき整数の比較

* BPF_JSGT: `s>`
* BPF_JSGE: `s>=`
* BPF_JSLT: `s<`
* BPF_JSLE `s<=`:

#### その他

* BPF_JA: プログラムカウンタにimmを加算した場所へ飛ぶ (ここちょっと自信ないので要確認)
* BPF_CALL: 関数を呼び出す
* BPF_EXIT: 関数呼び出しから復帰する

## まとめ

カーネルのドキュメントでも網羅はされていないので、各所から情報を集めましょう。

[前回]: /posts/note/run-ebpf-socket-filter/
[Linux Socket Filtering aka Berkeley Packet Filter (BPF)]: https://www.kernel.org/doc/Documentation/networking/filter.txt
[JIT]: https://elixir.bootlin.com/linux/v5.7.7/source/arch/x86/net/bpf_jit_comp.c
[disasm.c]: https://elixir.bootlin.com/linux/v5.7.7/source/kernel/bpf/disasm.c
