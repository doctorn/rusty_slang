#!/bin/bash
gcc -o crt0.o -c crt0.c
name=$(basename "$1" .slang)
cargo run $1 "$name.s"
gcc -o "$name.o" -c "$name.s"
gcc -o $name crt0.o "$name.o"
rm ./*.o
