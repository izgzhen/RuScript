Rethink Design
-----


There are some mistakes made ... in the overall design. And a revamp is the only way out.

## Early Optimization
The use of integer-indexed, array-based local variables (or "registers") is a **BAD** decision since you are mimicking Python's dynamic programming model from the very beginning. Once control-flow is introduced, the current mixture of static and dynamic will collapse.

What is Python's dynamic programming model?

* Everything is an object
* Polymorphism through reflection
* Assignment as name binding plus heap-allocation
* Complex scoping mechanism

Actually, i doubt it is kind of dynamic lexical scoping -- history doesn't matter, position matters, and the category of the positions is limited and thus easy to resolve: local (module function, class method), global (module top-level), and enclosed.

The problem is that, *although scoping is static, referencing is dynamic*. The only reason why scoping rule has to exist is just to decide which dictionary to reference (local, global, or enclosed), but never promises anything about success of referencing.

My comment for such design is -- intuitive for user, anti-intuitive for compiler writer.

## Early Formalization
The 0.2 spec is good as a training for writing such materials ... but it is wasting itself. The model is not strong enough, and too verbose. For formalization, I believe some more concise and modular way of documenting do exist.

## ByteCode and execution model
### Bytecode instructions

* Frame
    + `call`
    + `return`
* Stack
    + `push`
    + `pop`
* PC
    + `jump`
* Built-in
    + `print`
    + `typeof`
    + `add`
    + `self`
* Meta-info and structure
    + `class`
    + `magic`
* Heap allocation
    + `new`

I think it would be better *bytecode and execution model can be stable before compiler and runtime is built*. Before introduction of module system, it should not be changed.

### Execution model
> Nearly same as the old one

#### Data
* Globally visible, static lifetime, random-access array
* Locally visible, frame lifetime, random-access array
* Globally visible, static lifetime, stack

The arrays should not be limited in length, although *maybe* the compiler can give a hint about the length it needs for function, top-level or method

### Text
One file is a module, composed of top-level instructions & bindings, function declaration and  class declarations. The order in which they mix doesn't matter. Declaration should be name-unique and unordered. Instructions is ordered.

* Module
    + Toplevel statements
    + Function declarations
        + local statements (codeblock)
    + Class declarations
        + Attribute declarations
        + Method declarations
            + local statements

The statements above is called *codeblock*. Inter-procedural transition (call/ret) happens at the entrance and exit of code block.

## Some new thinking about high-level language
OO is unavoidable. But the old purely dynamic property should be reconsidered. Static checking should be something *extra*. Currently, two checking is necessary as I consider:

* Scoping & Binding
    + Is variable allocated already?
    + Use more fine-grained scope -- not just codeblock level, but basic block level
* Typing
    + Primitive types + Classname-as-a-type
    + Function types (implicit reference-passing)

