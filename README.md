# Servant Example : Memo app
Servantで簡単なメモアプリを作ってみました。

サーバーとCLIクライアントがあります。データベースにはSqliteを使っています。

## ビルド

~~~
$ git clone
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
