# Runtime Design

With the revised execution model and byte code format, we can design a new runtime now.

## Data Structure
This is the headache part. The new spec is requiring full R/W of local, global and attribute. And also, although the new design will provide more static information, but still not sure how difficult it would be to implement the unlimited length random-access local array.

Thus, a simple demo of data model is:

```rust
fn loader(source: &String) ->
         (Vector<FunctionObj>, Vector<ClassObj>, CodeObj);
// loader: source code -> global functions, global classes, top-level codeblock

fn run(declarations: &Vector<Declaration>, topLevelCode: &CodeObj) {
    let mut stack = initStack(); // global stack
    runFrame(declarations, &mut stack, topLevelCode);
}

fn runFrame(declarations: &Vector<Declaration>,
            stack: &mut Stack<Object>,
            code: &CodeObj) {
    let mut locals = initLocals();
    for inst in code {
        interprete(declarations, inst, &mut stack, &mut locals);
    }
}

fn interprete(declarations: &Vector<Declaration>,
              inst:   &Instruction,
              stack:  &mut Stack<Object>,
              locals: &mut Vector<Object>) {
    match inst {
        CALL(...) -> {
            let codeObj = getCode(declarations, ...);
            match runFrame(declarations, &mut stack, &codeObj) {
                Some(ret) -> ...,
                None -> ...,
            }
        },
        // ....
    }
}

```

## Object System
The previous object system is designed well. But I need to reinforce some points:

### Indexing
To get an attribute or codeblock of method of any object, we must use name (String) to index. This is because they are object-scoped names. And we use name index a static structures (declarations)

For top-level names and local names, for example, declared function's name, declared class's name, variable name, we use compiled integer to index. They might reside in declarations or local/global array.

### Unified Interface
The idea of introducing such a object system is that,

1. Any such object is heap-allocatable, which can be a extensible design
2. All objects share a unified interface

The trait interface:


```rust
pub trait Object {
    fn invoke(&mut self,
              name: String,
              args: Vector<Object>) -> Option<Object>;

    fn get(&self, name: String) -> Object;
    
    fn set(&mut self, name: String, new_attr: Object);

    fn typeof(&self) -> String;
}
```

The `invoke` here is rather limited -- it is intended for simplest possible interface. As a result, it can't change local/global variables, or global stack et cetera. And user-defined methods won't be called in such a way.

It is mostly a fallback -- If user-defined methods can't be found, it must be some built-in ones. Or, it can be used conveniently to implement `PRINT` instruction. But don't forget, most of the methods invoked by this interfaces are intended for interpreter's use.

However, the `get` and `set` is exposed to user by `PUSHA` and `POPA`. However, we can certainly store anything we want as well.




