# My solutions for Advent of Code 2020

Good exercise for learning Haskell.

## How to run

With `nix`:
```bash
$ nix-shell
$ bazel run //app:main -- <day>
```

## How to test

Run all tests with:
```bash
$ nix-shell
$ bazel test "//test:*"
```
