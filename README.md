RuScript
------

## Features

* Safe runtime written in Rust with garbage collection
* Lightweight stack-based bytecode
* High-level object-oriented language
* Static checking module

## Design
Please refer to `docs/spec-new`.

## TODOs
* Write benchmarking examples, esp. the GC performance
* Concurrent primitives
* Module system

## Sugar
* Linked dot, e.g. `a.b.c` or `a.b.c.f(1,2,3).e(4.5.6)` (But this can be get around with `var` bindings)

