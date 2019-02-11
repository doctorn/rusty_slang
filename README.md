# rusty\_slang
### s(imple)lang(uage)

Compiler implementation to compliment compiler construction course.

Credit to Dr Timothy Griffin for slang examples and [OCaml reference implementation](https://github.com/Timothy-G-Griffin/cc_cl_cam_ac_uk).

### Usage

This compiler is written in Rust, to use it you'll need to [install Rust](https://www.rust-lang.org/learn/get-started).

In particular, this compiler uses a nightly version of Rust. Once you've installed Rust, switch to a nightly version with:

```sh
rustup install nightly
rustup default nightly # this will set your default toolchain to nightly
```

You may need to add the following directories to your `PATH` if they aren't added automically:

```
~/.cargo/bin/
~/.rustup/nightly-{% your triple %}/bin/
```

To get the compiler simple clone this repository!

```sh
git clone https://github.com/doctorn/rusty_slang.git
```

To compile a program, you'll need to use `x.sh` (this will automatically build the compiler). 

Example:

```sh
./x.sh examples/expr.slang
```

This should give you two files: `expr.s` (the generated assembly) and `expr` the linked binary.

Running the linked binary is as simple as:

```sh
./expr
```
