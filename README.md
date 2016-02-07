RuScript
------

## Features

* Safe runtime written in Rust with garbage collection
* Lightweight stack-based bytecode
* High-level object-oriented language
* Static checking module

## Design
Please refer to `spec`

## TODOs
* Module system and `prelude` library
* Concurrent primitives

## Performance Issue
The current performance revealed by benchmarking the `sum.rus` is very poor, taking as much as 100 times longer than a naïve Python implementation.

Currently, the source of latency can be contributed to three aspects:

1. GC
2. Interpreter
3. Compiler

Actaully, by increasing the instrucion set, we can eliminate many "stupid" instruction sequence, such as `POP` then `PUSH`, which is equivalent to a write stack-top to local var without popping out.


