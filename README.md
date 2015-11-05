RuScript
------
Status: Prototyping

## Features

* Object Runtime System with Garbage Collection
* Stack-based Bytecode Format
* High-level Language

## Language Specification

### Bytecode
Please refer to `src/stackcode.rs`.

### High-level

The compiler is written in Haskell and serves as a independent component in the toolchain.

## Spec

```
<source>        := [<statement>;]

<assignment>    := <identifier> = <expr>

<expr>          := <term> + <term>
                 | <term>

<term>          := <int>
                 | <ident>
                 | <string>
                 | new <ident>
                 | <ident>.<ident>([<expr>,])

<statement>     := <assignment>
                 | <classDecl>
                 | print <expr>
                 | return <expr>

<classDecl>     := class <ident> { [ <attrDecl> | <methodDecl> ] }

<attrDecl>      := <ident>

<methodDecl>    := fn <ident>([<ident>,]) {
                            [<globalDecl>]
                            [<statments>]
                        }

<globalDecl>    := global <ident>
```

## TODOs
* Simple test of local attrs
* Write unit tests
* Write behaviour spec

## In future
* Inheritance & Encapsulation & Polymorphism
* Optimization Loop
* REPL interface






