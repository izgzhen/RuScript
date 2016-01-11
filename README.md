RuScript
------
Status: RFC

## Features

* Efficient runtime system with garbage collection
* Stack-based bytecode
* High-level object language

## Language Specification

Please refer to `docs/spec.md`.

## TODOs
* Inspect the source
* Write proposals for new features' implementation
* Control-flow opcode
    + `jump`: relative/abosolute, conditional/non-conditional which can be used to implement `if`
    + `for`: it can be compiled to manipulation over iterator object, although it can be more efficient if original-semantics is preserved.
* Need a library processing code-gen conveniently...

## In future
* High-level OO semantics: Inheritance & Encapsulation & Polymorphism
* Optimization
* REPL interface
* Rewrite GC
* Concurrency support
