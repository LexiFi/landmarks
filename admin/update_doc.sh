#!/bin/bash

pushd "$(dirname "$0")"

NOCOLOR='\e[0m'

ERR_COLOR='\e[0;31m\e[1m'
abort() {
  printf "$ERR_COLOR$1$NOCOLOR\n"
  exit 1
}

TRY_COLOR='\e[0;34m\e[1m'
try() {
  printf "$TRY_COLOR$*$NOCOLOR\n"
  "$@" || abort "Something wrong happened. Exiting."
}

MSG_COLOR='\e[0;32m\e[1m'
msg() {
  printf "$MSG_COLOR$*$NOCOLOR\n"
}

cd ..
try dune build @doc
cd admin
try rm -rf website
try git worktree prune
try git worktree add website origin/gh-pages
cd website
try git rm -rf api
try cp -rf ../../_build/default/_doc/_html api
try git add api
try git commit -m "Update docs"
msg "Do you want to push the changes ?"
read -n 1 -r
echo 
if [[ $REPLY =~ ^[Yy]$ ]]
then
  try git push origin HEAD:gh-pages
fi
popd
