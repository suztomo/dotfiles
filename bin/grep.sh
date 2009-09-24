#!/bin/zsh
if [ $# -lt 1 ]; then
  echo "specify word"
  exit
fi
grep -r $1 .
