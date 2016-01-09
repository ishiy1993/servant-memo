# Servant Example : Memo app
Servantで簡単なメモアプリを作ってみました。

サーバーとCLIクライアントがあります。データベースにはSqliteを使っています。

## ビルド

~~~
$ git clone https://github.com/ishiy1993/servant-memo.git
$ cd servant-memo
$ stack build
~~~

## 実行
サーバーの起動

~~~
$ stack exec memo-server
~~~

CLIクライアントの実行

~~~
$ stack exec -- memo-client (command)
~~~
