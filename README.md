RuScript
------

## Features

* Safe runtime written in Rust with garbage collection
* Lightweight stack-based bytecode
* High-level object-oriented language

## Design
Please refer to `docs/spec-new`.

## TODOs
* Debug as comment line arguments
* Write more test programs
* Visibility checking & inherited method/attribute checking
* Improve the compiler and document the compiler code in haddock style

## Sugar
* Linked dot, e.g. `a.b.c` or `a.b.c.f(1,2,3).e(4.5.6)` (But this can be get around with `var` bindings)

