# slang
### Simple language

Compiler implementation to compliment compiler construction course.

Credit to Dr Timothy Griffin for slang examples and [OCaml reference implementation](https://github.com/Timothy-G-Griffin/cc_cl_cam_ac_uk).

This compiler is written in Rust, to use it you'll need to [install Rust](https://www.rust-lang.org/learn/get-started).

### Usage

To compile a program, you'll need to use `x.sh` (this will automatically build the compiler). 

Example

```
x.sh examples/expr.slang
```

This should give you two files: `expr.s` (the generated assembly) and `expr` the linked binary.
