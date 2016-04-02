# Ideas about verification 

I am thinking about abstracting over the RuScript and verify its correctness with Coq as a *practical project*.

Currently, we have two languages

+ a high-level OO language
+ a byte-code instruction set

The semantics model can be viewed as a description of RVM interpreter actions and states.

And there are two aspects worth verifying:

1. The OO compiler is correct
2. The Interpreter is correct

We will focus on the later one now.

## RVM

We will *translate* the Rust code into an abstract description. Things like mutability can be taken into consideration.

We want to show that the RVM interprets implementation will behave according to the spec over byte-code format.

But it is certainly difficult to even decide what the specification should look like.

*It would be boring if we don't even try to verify the rust-gc.*

