# Compiler Design

The compiler needs to compile the RuScript language into RuScript bytecode. First, it should enforce the correctness of advanced semantics; Second, it should try to eliminate the majority of runtime error through static checking; Third, it should try to optimize the bytecode to archive better efficiency and smaller binary.

## High-level Language Design
The [model](./Model.md) has already given some suggestion over how the language looks like. Here is a more formal description.

```
program := [ stmt | decl ]

decl    := fnDecl
         | clsDecl

stmt    := 'var' binding <'=' expr>
         | x '=' expr
         | 'if' expr { [stmt] } <'else' { [stmt] }>
         | 'while' expr { [stmt] }
         | 'return' expr

lhs     := x
         | x.attr

expr    := x
         | x.attr
         | x.f([ expr ])
         | cls([ expr ])
         | literal

literal := string
         | integer
         | boolean

fnDecl  := fnSig { [stmt] }

binding := x : type

type    := 'bool'
         | 'int'
         | 'str'
         | cls

clsDecl := 'class' cls <'inherits' cls> { [attr] [method] }

attr    := <'private'> binding

method  := <'private'> fnDecl
         | <'private'> 'virtual' fnSig

fnSig   := 'fn' f ([ binding ])

x       := 'self'
         | [alpha | digit]

```

Note: `<...>` means optional, `...|...` means selection, `[...]` means many, `'...'` means reserved name. However, this description is not very strict.

## Features
* Control-flow statements
* Object-Oriented
    + Inheritance
    + Polymorphism through virtual method
    + Encapsulation through `private` keyword
* Static checking
    + Built-in types
    + Contravariant/Covariant

The effect of encapsulation is check statically. The poly and inheritance will be checked, but implemented on a runtime basis.

## Change Log
Compared with the previous version, the new version changed in the following ways:

* **Simplified the variable scoping and runtime state**: The top-level and function/methods can only access its own locally declared variables.
* **Writable attribute**
* **Introduction of function declaration**
* **Introduction of type annotation**
* **Introduction of control-flow statements**
* **Introduction of OO advanced features**


