Bytecode Instructions
----

This design is based previous bytecode used and the [Python bytecode](https://docs.python.org/2/library/dis.html).

## Instructions

Please refer to `src/bytecode.rs` for the current version.

## Call Convention
For function `f(a, b, c)`, it will assume that the parameters are put on stack, TOS is `a`, NTOS is `b`, ...; For method, it will assume the TOS is `this` pointer, although it doesn't appear explicitly in the parameter list. When function/method returns, if it returns some value, this object should reside on the top of stack, or nothing special happens if it doesn't return any value.

## Other layout issues

### Class declarations
Example:

```haskell
class A : inherits X {
    a: Int
    b: Bool
    
    fn c(...) {
        ...
    }
    virtual fn d(...) {
        ...
    }
}
```

will be compiled to such layout:

```python
CLASS(2, 1, 0)      # n_attrs, n_methods, father_idx
PUSHSTR("a")
PUSHSTR("b")
### bytecode of function body
EBODY(2)            # n_locals
PUSHSTR("c")
### End. The virtual function will not be compiled at all
```

