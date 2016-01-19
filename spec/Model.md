# Runtime Model


## Informal Description

This model is mainly based on the previous version.

The runtime is composed of basically a loader, interpreter and a garbage collector.

The loader loads in the binary, read instructions from it, and structure the program text into top-level statements and declarations (functions and classes).

The interpreter executes a frame object and effect the global environment. The global environment is initialized from the declarations, plus a globally visible stack. The root frame is initialized from the top-level statements. And every function-call or method-invocation will create new frames from function body or method body.

Every frame contains its local registers in a way much like C. The arguments is passed in through stack. The return value is returned through stack.

Every user-space value is boxed and garbage-collected. Thus, the values in local/global registers and global data stack are pointers, and they are all the references that needs to be traced by collector. The user-value can be initialized from primitive literal, or class initializer.

The primitive types should include but not limited to:

* Boolean
* 32-bit Integer
* String

In statements, imperative style programming is also supported, including but not limited:

* Conditional Branching
* Conditional Looping

Thus, there are two ways of transferring control-flow:

* call/ret
* if..then..else... and while...do...

Currently, the language will be statically-typed, so dynamic-reflection is not required. However, the built-in operations should include at least:

* I/O
* Heap allocation
* Reference to `self`

## Epilogue
This is still an experimental project. When OO is fixed, the concurrency is next topic to explore. And also module system and FFI implementation. But here are two points worth attention:

* Reduce introduction of new bytecode instructions
* Designs should be as orthogonal as possible 

