ProveEverywhere - Android Version
=================================

(このアプリは、東京工業大学大学院の[ソフトウェア開発演習](https://github.com/itspsdl)という授業の最終課題のために作られました)

CoqIDE / ProofGeneral の Android アプリ版のような感じです。
coqtop サーバと通信します。


サーバ環境
----------

以下の環境で動作確認

サーバ: さくら VPS 1G プラン
OS: GNU/Linux Debian (wheezy)

- coq-8.4pl4
  - 8.3 以下だと prompt の形式が違うので動かない
  - 8.4pl1~3 は不明
- ghc-7.8.2
  - これより低くても多分動く
- cabal-install-1.20.0.3
  - これより低くても多分動く

### Coq のインストール

以下でインストール。さくら VPS の 1G のプランだと30分くらいかかった。

```sh
$ sudo apt-get install ocaml camlp5
$ wget http://coq.inria.fr/distrib/V8.4pl4/files/coq-8.4pl4.tar.gz
$ tar xf coq-8.4pl4.tar.gz
$ cd coq-8.4pl4
$ ./configure -prefix ~/coq
$ make world
$ sudo make install
$ echo 'export PATH=$HOME/coq/bin:$PATH' >> ~/.bashrc
```

以下でバージョンが出てくればOK。

```sh
$ coqtop -v
```

### GHC のインストール

```sh
$ wget http://www.haskell.org/ghc/dist/7.8.2/ghc-7.8.2-x86_64-unknown-linux-deb7.tar.xz
$ tar xf ghc-7.8.2-[TAB]
$ cd ghc-7.8.2
$ ./configure
$ sudo make install
```

### cabal-install のインストール

```sh
$ wget http://hackage.haskell.org/package/cabal-install-1.20.0.3/cabal-install-1.20.0.3.tar.gz
$ tar xf cabal-install-1.20.0.3.tar.gz
$ cabal-install-1.20.0.3
$ ./bootstrap.sh
$ echo 'export PATH=$HOME/.cabal/bin:$PATH' >> ~/.bashrc
```

### ProveEverywhere サーバのインストールと実行

```sh
$ git clone git@github.com:amutake/prove-everywhere.git
$ cd prove-everywhere/server
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal configure
$ cabal build
$ ./dist/build/prove-everywhere-server/prove-everywhere-server -p 3000
```
