#!/bin/sh

go get github.com/BurntSushi/toml-test
$GOPATH/bin/toml-test $1
