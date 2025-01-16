#!/usr/bin/env sh

cd ~/350/plt

# print the current timestamp to the file
date +"20%y%m%d%H%M" >~/Library/Racket/8.11.1/pkgs/handin/handin-client/version

# copy the file for the server
cp ~/Library/Racket/8.11.1/pkgs/handin/handin-client/version ./

raco pack --collect --at-plt --replace ++setup cs350-handin-client cs350.plt cs350-handin-client flit todo-list websocket

git commit -a -m "Update PLT file"

git push
