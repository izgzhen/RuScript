Language Specification
-------


Please refer to `../ruscript/src/stackcode.rs` for current supported instructions.

The compiler will try to transform high-level syntax and semantics into a low-level, stack based VM bytecode. The difficult thing is about how to express the complex declarations in a good style. Maybe some sectioning, meta info and blocking (might be as complex as LLVM) would be necessary.

The compiler is itself written in Haskell and serves as a dependent component in the toolchain.

## Spec

```
<source>        := [<statement>]
<assignment>    := <identifier> = <expr>
<expr>          := <term> + <term>
                 | <constructor>
<term>          := <int>
                 | <ident>
<constructor>   := <ident>()
<statement>     := <assignment>;
                 | <classDecl>
<classDecl>     := class <ident> { [ <attrDecl> | <methodDecl> ] }
<attrDecl>      := <ident>
<methodDecl>    := fn <ident>([<ident>,]) {
                            [<globalDecl>]
                            [<assignment>]
                            <expr>
                        }
<globalDecl>    := global <ident>
```

