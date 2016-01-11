OO Features
-----

> Proposing the implementation of more advanced OO features.

## Inheritance
Example:

```
class Father { }
class Child inherits Father { }
```

Only single-inheritance is supported. Without other features, `Father`'s all attributes and methods will be inherited directly. It is simply a namespace introduction with collision detection. Overriding will not be introduced now, although it is trivial to.

I am thinking about sharing of code. But if that's the case, then we need another primitive.

## Polymorphism
No compile-time polymorphism now. It should be implement in some dynamic, reflective way. A naive implementation is:

* Attaching type information to each primitive type, introduce `type` objects and `typeof()` built-in function
* Expose object member meta-information through interface like Python's `__dict__`, and also some built-in functions to manipulate it.

## Encapsulation
Encapsulation is about visibility ... and always interpolated with other features. For inheritance, the visibility is inherited as well. And for using object, private attribute & method will be simply hidden from accessing. This is definitely a static-checking.


---

Basically, after this effort, the product will be a highly dynamic system. So this makes me wonder how to make a type system for it in the future. Beyond that, what will a minimal set of bytecode IR look like?


