# Runtime Model


## Description

The runtime is basically composed of

* Loader
* Interpreter
* Garbage collector (The actual implementation is in `rust-gc` library)

### Load

The loader loads in the binary, parse bytecode from it, and structure the program text into top-level statements and declarations (of functions and classes).

### Interpret

The interpreter executes frames. The global environment is initialized from the declarations, plus a global R/W stack. The root frame is initialized from the top-level statements. Every function-call or method-invocation will create new frame from function body.

### Frame

Every frame contains its local variables and access them by integer-indexing. The arguments are passed by stack. The return value is returned by stack.

### Value

Every value (including primitives and instance) is boxed and garbage-collected. Thus, the values in local variable array and global data stack are pointers, and they are all the references that need to be traced by collector.

### Primitive types

* Boolean
* 32-bit Integer
* String

### Imperative control-flow statements

* Conditional Branching
* Conditional Looping

### Built-in

* I/O
* Heap allocation
* Reference to `self` or `this`

## Epilogue
This is still an experimental project. When OO is fixed, the concurrency is the next topic to explore. And also module system and FFI implementation. But here are two points worth attention:

* Reduce introduction of new bytecode instructions
* Designs should be as orthogonal as possible 

