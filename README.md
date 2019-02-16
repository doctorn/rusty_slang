# rusty\_slang
### s(imple)lang(uage)

Compiler implementation to compliment compiler construction course.

Credit to Dr Timothy Griffin for slang examples and [OCaml reference implementation](https://github.com/Timothy-G-Griffin/cc_cl_cam_ac_uk).

### Installation

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

To get the compiler clone this repository and install!

```sh
git clone https://github.com/doctorn/rusty_slang.git
cd rusty_slang
cargo install --path ./ # add --force to overwrite an existing installation
```

You should now have an executable called `slang` installed in `~/.cargo/bin/`. This can be used to compile `.slang` files.

### Usage

To compile a slang program to assembly, use the following:

```sh
slang my_program.slang
```

This should generate some assembly code in a file called `my_program.s`.

Assembly can be pretty cryptic, so I've added an option to generate comments for the assembly:

```sh
slang -C my_program.slang
```

If you want to assemble and link the compiled assembly, you can do so with the `-L` or `--link` options:

```sh
slang -L my_program.slang
```

This should give you two files: the first `my_program.s` (as before), and the second `my_program` (the linked executable). To execute your linked executable should be as simple as:

```sh
./my_program
```
