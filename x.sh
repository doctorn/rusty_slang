#!/bin/bash
gcc -g -o crt0.o -c crt0.c
name=$(basename "$1" .slang)
cargo run -- $1 "$name.s" ${@:2}
gcc -g -o "$name.o" -c "$name.s"
gcc -g -o $name crt0.o "$name.o"
rm ./*.o
