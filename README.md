RuScript
------

## Build

Tool-chain dependencies: Rust (nightly), Haskell (8.01)

1. `git submodule update` to fetch `rust-gc` dev source
2. `runghc Test.hs` to run unit tests

To build the `rvm` and `rusc` separately:

1. `rvm`: `cd rvm; cargo build`
2. `rusc`: `cd rusc; stack build`

## Features

* Safe runtime written in Rust with garbage collection
* Lightweight stack-based bytecode
* High-level object-oriented language
* Static checking module

## Design
Please refer to `spec`

## TODOs
* `.rusi` Interface parser
    + Note #1: Whether or not to attach a `self` to each method signature
    + Note #2: Separate compilation and selective exports require more engineering, of which I have little experience in
* Concurrent primitives
    + Note #1: We need to construct runners inside the interpreters, and every runner is contained in a thread and responsible for a user-thread
    + Note #2: To share the data between threads, we need a concurrent GC system, which we don't really have now.
* Formal Specification Revision based on Coq

## Performance Issue
The current performance revealed by benchmarking the `sum.rus` is very poor, taking as much as 100 times longer than a naïve Python implementation.

Currently, the source of latency can be contributed to three aspects:

1. GC
2. Interpreter
3. Compiler

Actually, by increasing the instruction set, we can eliminate many "stupid" instruction sequence, such as `POP` then `PUSH`, which is equivalent to a write stack-top to local var without popping out.
