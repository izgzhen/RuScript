RuScript
------

### Design of Object system

Everything in RuScript will be object. But there are mainly four kinds of objects:

* Primitive Object, like `Int`, `Char`, `Bool` etc.
* Complex Object, user can define/construct them through class
* Function Object
* Module Object

The primitive object has built-in methods. Their type are also keywords. The complex object is the legacy design of attributes plus methods, where methods are actually function object. Function object represent code. They can't have static storage or non-local/dynamic scoping. But side-effect by passing are permitted.

The top-level environment will have a table of all names. The names could be prim objects, complex class, complex object, function object, or imported module object.

So there would be four hash tables. The first one will be mapping string to enum type, the second one will be string to `Complex` struct, the third one will be string to `Function` struct, the fourth one will be string to `Module` object.

However, they all share a similar standard message passing based interface.


### TODOs
* use nom to rewrite the parser
* write batch test (how to profile the GC usage history?), how to make sure there is no memory leackage?
* Add debug-only code to `rust-gc`'s `ruscript` branch

