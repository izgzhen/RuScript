# Specification of RuScript

- Version: Draft v0.2
- Zhen Zhang
- Nov. 2015


## Abstract Computation Model
### Representation
The source code is an array of instructions coded in binary. The running program is a stack of frames and object pool, in which every frame is composed of:

1. Local Stack (abbr. *stack*)
2. Local registers (abbr. *locals*)
3. Global registers (abbr. *globals*)
5. Frame code block (abbr. *code*)

*Object pool* is a managed heap. All living objects will live in heap, and accessible by `Gc` type, or *pointer*; when some object becomes inaccessible, it will be garbaged collected.

The stack and registers are all container of pointers. The stack is where most computation operate on. The registers are meant for fast & indexed access of objects.

Stack can only be `push`ed or `pop`ped. It is assumed to be unlimited in length currently. And if underflowed, program will panic. Registers is an array of finite and fixed length. When index is invalid, program will panic.

### Code Structure
#### The grammar

```
<toplevel>      := [<statement>;]

<assignment>    := <identifier> = <expr>

<expr>          := <term> + <term>
                 | <term>

<term>          := <int>
                 | <ident>
                 | <string>
                 | new <ident>
                 | <ident>.<ident>([<expr>,])

<statement>     := <assignment>
                 | <classDecl>
                 | print <expr>
                 | return <expr>

<classDecl>     := class <ident> { [ <attrDecl> | <methodDecl> ] }

<attrDecl>      := <ident>

<methodDecl>    := fn <ident>([<ident>,]) {
                            [<globalDecl>]
                            [<statments>]
                        }

<globalDecl>    := global <ident>
```

Note #1: When we say "*A is defined in scope X*",

* If A is a variable, we means that there exist an assignment `A = <expr>` in X
* If A is a method, we means that A is defined in some class as a method
* If A is a class, we means that A is defined in some top-level statement


Note #2: By "*variable*", we means named primitive objects or instances

### Scope and Scoping
Kinds of scope includes:

* Global (or top-level) scope: contains defined variables, classes
* Local (or method) scope: contains arguments, declared globals, variable defined in method source
* Class scope: only and always `self`. `self` can be used to refer the class's attributes and methods as a normal accessor pattern.

Scoping rule of names:

* Globals: Visible to top-level code after it is defined on top-level; Visible to method code after it is defined on top-level and declared as `global` at the beginning of method.
* Locals: Only visible to methods code
* Classes: Visible after defined


Note: The principle is to give name collision alert to legal cases, and give errors when scoped nothing. If collision happens, the priorities are "local> class > global".

### Object System

Current object system is rather simple. It has the following features:

* Everything is an *object*, and every computation is *message passing*
* *Class* can be instantiated to *instance*
* *Accessor* pattern, which we use to read *attributes* and invoke *method*

The `_Object` contains all kinds of object defined:

* Primitive Objects
	* `Int_ty`: 32-bit signed integer
	* `Array_ty`: Integer-indexed growable array of objects
	* `String_ty`: Immutable string
* `Frame_ty`: Representing a frame
* `Instance_ty`: Represent a general object as a instance of class
* `Class_ty`: Represent class definition

Every object must fulfill `Object` trait:

```rust
trait Object {
    fn call(&self,
            name: &str,
            args: Vec<Gc<_Object>>,
            env: &Gc<Env>,
            globals: &mut Vec<Gc<_Object>>) -> Gc<_Object>;

    fn tyof(&self) -> &str;
}
```

## Stack System
Stack system is the implementation of the abstract computation model described above. It is an interpreter for variable-length instruction (i.e. *SCode*).

In the following text, all instructions and their operational semantics will be given in a formal way.

Note: Currently, the `SCode` is a mixture of classical data segment and text segment. All data will be carried in the instruction. But since actual binary layout has little to do with semantics, I will try to avoid referring to actual binary layout in formalization, but using $INST(D1, D2)$ to clarify the relationship between operator and operands.

### All instructions
* `PUSHG(Integer)`
* `PUSHL(Integer)`
* `PUSHA(Integer)`
* `POPG(Integer)`
* `POPL(Integer)`
* `POPA(Integer)`
* `ADD`
* `CALL(Integer, String, Integer)`
* `RET`
* `NEW(Integer)`
* `PUSH_INT(Integer)`
* `PUSH_STR(String)`
* `FRMEND`
* `CLASS(Integer, Integer)`
* `PRINT`

Naming conventions: `G` means global, `L` means local, `A` means attribute, `PUSHX(i)` means push `i`-indexed element of `X` on top of stack, `POPX(i)` means pop top of stack to `i`-indexed position of `X`.

Note: Although similar, This is not same as the Rust code.

### Formal Computation Model
* Environment: $E$, tuple $(L, G, S, C)$
* Locals: $L$, mutable finite array
* Globals: $G$, mutable finite array
* Stack: $S$, mutable infinite stack
* Classes: $C$, immutable array of class definitions
* Code block: $\Sigma$, immutable array of instructions
* Instruction Space: $I$, set of defined instructions
* Frame: $F$, tuple $(\Sigma, E)$
* Object: $\omega$, any entity satisfying `Object` trait
* Heap: $H$, set of managed objects
* Pointer: $\psi$, a managed reference to some $\omega \in H$
* Program: $P$, tuple of $(C, \Sigma)$
* Identifier Space: $\eta$
* Expression: $e$

Note: For commonly used collection type, set will be notated as $<>$, array will be notated as $[]$, tuple will be notated as $()$

A complete computation is defined as a function $f_{comp}: (P, World) \rightarrow World'$, in which the $World'$ is temporarily defined as $(B, H)$, in which $B$ is standard output buffer.

Note: To make formula cleaner, we might use Monad-like notation to describe side-effect, i.e. $f: A \rightarrow EnvM(B)$ is equivalent for $f: (A, Env) \rightarrow (B, Env')$

First, $P$ will be loaded into a root frame: $f_{load}: P \rightarrow F$.

Then, we will have an undecidable computing process $f_{run}: F -> WorldM(\omega)$

If $F$ is the root frame, then $\omega$ will be discarded and program terminates, if not, $\omega$ will be *returned* to caller and the current frame will be popped away.

*Method invoking* is a way to create a frame, run it and turn the returned value into a $term$. It is a way to abstract computation. It is actually a special case of object `call`. $f_{invoke}: (\omega, m, [e]) -> WorldM(F)$. So by invoking, we can get value of call expression $f_eval{(\omega.m([e])} = f_invoke(\omega, m, [e]) \gg = f_{run}$

Thus, we can abstract the computation as a recursive process of "invoking -> frame interpretation -> return value". Formally, we will present the "computation state" as a stack of frames: $F_0 \diamond F_1 \diamond F_2 ... \diamond F_i$, where $F_i$ is the current frame.s All $F_0 ... F_{n - 1}$ is "suspended computation".

In summary, by a single step interpretation, we will have

* Frame computation: $F_0 \diamond F_1 \diamond F_2 ... \diamond F_i \rightarrow F_0 \diamond F_1 \diamond F_2 ... \diamond F_i'$
* Frame suspension & creation: $F_0 \diamond F_1 \diamond F_2 ... \diamond F_i \rightarrow F_0 \diamond F_1 \diamond F_2 ... \diamond F_i \diamond F_{i + 1}$
* Frame continuation & completion: $F_0 \diamond F_1 \diamond F_2 ... \diamond F_i \rightarrow F_0 \diamond F_1 \diamond F_2 ... \diamond F_{i - 1}$

In the following formal semantics, if it is simply a frame computation, then we will conceal the frame structure; But if not, we will take everything into the formal presentation.

### Formal Semantics

#### `PUSHG(Integer)`
$$<(L, G, C, S), PUSHG(i)> \Rightarrow WorldM (L, G, C, G[i] \rhd S)\\ WorldM \leadsto H[G[i]] \uparrow$$

#### `PUSHL(Integer)`
$$<(L, G, C, S), PUSHL(i)> \Rightarrow WorldM (L, G, C, L[i] \rhd S)\\ WorldM \leadsto H[L[i]] \uparrow$$

#### `PUSHA(Integer)`
$$<(L, G, C, S@(\omega \rhd S')), PUSHA(i)> \Rightarrow WorldM (L, G, C, \omega.attrs[i] \rhd S)\\ WorldM \leadsto H[\omega] \uparrow$$

#### `POPG(Integer)`
$$<(L, G, C, \omega \rhd S'), POPG(i)> \Rightarrow WorldM (L, subst(G, i, \omega), C, S')\\ WorldM \leadsto H[G[i]] \downarrow$$


#### `POPL(Integer)`
$$<(L, G, C, \omega \rhd S'), POPL(i)> \Rightarrow WorldM (subst(L, i, \omega), G, C, S')\\ WorldM \leadsto H[L[i]] \downarrow$$

#### `POPA(Integer)`
$$<(L, G, C, \omega_1 \rhd \omega_2 \rhd S'), POPA(i)> \Rightarrow WorldM (L, G, C, subst(\omega_2, i, \omega_1) \rhd S')\\ WorldM \leadsto H[\omega_2.attrs[i]] \downarrow$$

#### `ADD`
$$<(L, G, C, \omega_1 \rhd \omega_2 \rhd S'), ADD>, \omega_3 \sim \omega_1 + \omega_2 \Rightarrow WorldM (L, G, C, \omega_3 \rhd S')\\ WorldM \leadsto H[\omega_1] \downarrow, H[\omega_2] \downarrow, H.new(\omega_3)$$

#### `CALL(Integer, String, Integer)`
$$<(L, G, C, \omega_1 \rhd \omega_2 \rhd ... \omega_n \rhd S'), CALL(i, m, n)>, \omega \leftarrow scope(L + G, i), \omega.m(\omega_1, ..., \omega_n) \sim \omega'  \Rightarrow WorldM (L, G, C, \omega' \rhd S')\\ WorldM \leadsto \forall i \in 1...n, H[\omega_i] \downarrow, H.new(\omega')$$

#### `RET`
$$F_0@<(L, G, C, \omega \rhd S'), RET> \Rightarrow terminate$$
or
$$F_0 \diamond ... \diamond F_{i - 1}@(Susp: \omega \rightarrow <E, i>)\diamond F_i@<(L, G, C, \omega \rhd S'), RET> \Rightarrow F_0 \diamond ... \diamond F_{i-1}@<E, i>$$

#### `NEW(Integer)`
$$<(L, G, C, S), NEW(i)>, \omega \leftarrow C[i].new() \Rightarrow WorldM (L, G, C, \omega \rhd S) \\ WorldM \leadsto H.new(\omega)$$

#### `PUSH_INT(Integer)`
$$<(L, G, C, S), PUSH\_INT(i)>, \omega \leftarrow Int.new(i) \Rightarrow WorldM (L, G, C, \omega \rhd S) \\ WorldM \leadsto H.new(\omega)$$

#### `PUSH_STR(String)`
$$<(L, G, C, S), PUSH\_INT(s)>, \omega \leftarrow String.new(s) \Rightarrow WorldM (L, G, C, \omega \rhd S) \\ WorldM \leadsto H.new(\omega)$$


#### `FRMEND`
This is a pseudo-instruction.

#### `CLASS(Integer, Integer)`
This is a pseudo-instruction.

#### `PRINT`
$$<(L, G, C, \omega \rhd S), PRINT>, \Rightarrow WorldM (L, G, C, \omega \rhd S) \\ WorldM \leadsto B.print(\omega)$$


## Exception handling and correctness protocol

### Exception Handling
Currently, RuScript doesn't have error handling capacity, if any undefined behaviour happens, the interpreter will simply panic and exit.

However, a protocol of correctness should exist. First, the language should have only high-level exceptions, like "divide by zero", "no such method", "out of index", but never low-level errors caused by virtual machine itself, like stack underflow, register indexing error, invalid instruction et cetera. By specifying the first category of exceptions as exception conforming to the formal spec, we should make sure only non-conforming computation can cause interpreter panic. Exceptions should only print information can exit normally.

Such a protocol can be guaranteed by compiler.

> Correctness Protocol: *As long as runtime system is enforcing the formal model, any compiled code should not cause panic to runtime system* 
### Correctness Protocol Implementation
Essentially, this implementation is a *correctness proof of an existing compiler*. So, here we will only give some examples of implementation.

The hand-written proofs can be lengthy, imprecise and unreadable, so maybe it is possible to construct a model in Coq. 

#### Example

The following excerpt can generate correct code, assuming that `pushTerm` can emit a segment of code, which when executed, can correctly leave the value on top of stack.

```
pushExpr (Plus tm1 tm2) = do
    pushTerm tm1
    pushTerm tm2
    emit SAdd
``` 

Proof:

According to assumption, let `pushTerm` be such a function $f_{term}: Term -> CompilerM(\Sigma)$, in which $Term$ is lexical representation of a term, and $CompilerM$ is the monad analogy of a compiler. And for any $t \in Term$, if $t$ is computable under environment $E$, then $<E, \ez> \Rightarrow E'$, in which top of stack of $E'$ is value-equivalent of $Term$.

So, let $\Sigma_1 \leftarrow f_{term}(t_1), \Sigma_2 \leftarrow f_{term}(t_2)$, then $ \Sigma_1 + \Sigma_2 + [SAdd] \leftarrow f_{expr}(Plus(t1, t2))$, Since $<(L, G, C, S), \Sigma_1>, \omega_1 \sim t_1 \Rightarrow (L, G, C, \omega_1 \rhd S)$, $<(L, G, C, S), \Sigma_2>, \omega_2 \sim t_2 \Rightarrow (L, G, C, \omega_2 \rhd S)$, so $<(L, G, C, S), \Sigma_1 + \Sigma_2>, \omega_1 \sim t_1, \omega_2 \sim t_2 \Rightarrow (L, G, C, \omega_2 \rhd \omega_1 \rhd S)$, then $<(L, G, C, S), \Sigma_1 + \Sigma_2 + SAdd>, \omega_3 \sim (t_1 + t_2) \Rightarrow (L, G, C, \omega_3 \rhd S)$, which conforms to the spec.




## High-Level Language Construct
Built on top of the foundation, we can compile the grammar mentioned at the very beginning to the stack code. Here, I will give a formal, detailed description of each construct we have. The semantics will be defined here, although implementation is left up to the compiler writer.

### Symbols
* Statement: $S$
* Expression: $E$
* Self: $self$
* Identifier (not a keyword): $Id$
* Assignment: $x \mapsto e$, $x \in V, e \in E$
* Term: $T$
* (real) parameter list: $\forall e_i \in E, (e_0, e_1, ...)$
* (formal) arguments list: $\forall x_i \in Id, (x_0, x_1, ...)$
* Class: $C$
* Method invoking: $\forall c \in C, e_i \in E, f \in Id, c.f(e_0, e_1, ...) \in T$
* Attribute accessing: $\forall c \in C, \alpha \in Id, c.\alpha \in T$
* Print: $\forall e \in E, print(e) \in S$
* Return: $\forall e \in E, return(e) \in S$
* Plus: $\forall t_0, t_1 \in T, t_0 + t_1 \in E$
* Declared `global` names: $x_g \in X_g$
* Locally defined names: $x_L \in X_L$
* Globally defined names: $x_G \in X_G$
* Integer $i \in \mathbb Z$
* String $s \in \mathbb S$
* Construction $\forall c \in C, new(c) \in T$


### Semantics
#### Assigment
$$\frac{x \in X_g, SC_G(x) = i, \omega \sim e}
		{F_{>0}@\langle (L, G, C, S), Compile (x \mapsto e) \rangle \Rightarrow (L, G[i \mapsto \omega]), C, S)}$$


$$\frac{x \in X_L, SC_L(x) = i, \omega \sim e}
		{F_{>0}@\langle (L, G, C, S), Compile (x \mapsto e) \rangle \Rightarrow (L[i \mapsto \omega], G, C, S)}$$

$$\frac{x \in X_G, SC_G(x) = i, \omega \sim e}
		{F_0@\langle (\emptyset, G, C, S), Compile (x \mapsto e) \rangle \Rightarrow (\emptyset, G[i \mapsto \omega], C, S)}$$


#### Expression & Term evaluation
$$\frac{t_0 \sim \omega_0, t_1 \sim \omega_1, e = t_0 + t_1}
		{e \sim \omega_0 + \omega_1}$$

$$\frac{\langle(L, G, C, S), Compile(push(t)) \rangle \Rightarrow (L, G, C, \omega \rhd S)}
	    {t \sim \omega}$$

$$\frac{x \in X_{g|L|G}, SC_{G|L}(x) = i, G|L[i] = \omega}{x \sim_{G|L} \omega}$$

$$\frac{prim \in \mathbb{Z|S}, prim \sim \omega}
	    {\langle S, Compile(push(prim)) \rangle \Rightarrow (\omega \rhd S)}$$

$$\frac{c \in C, \omega = New(c)}
	    {\langle S, Compile(push(new(c))) \rangle \Rightarrow (\omega \rhd S)}$$

$$\frac{x \in Id, x \sim \omega}
	    {\langle S, Compile(push(x)) \rangle \Rightarrow (\omega \rhd S)}$$

$$\frac{x \sim \omega_x, ClassOf(\omega_x) = c,  e_i \sim \omega_i, f_{run}(c.f, [\omega_i]) \sim \omega}
	    {\langle S, Compile(push(x.f([e_i]))) \rangle \Rightarrow (\omega \rhd S)}$$

$$\frac{x \sim \omega_x, ClassOf(\omega_x) = c, i = FindIndex(\alpha, c.attrs), \omega_x.attrs[i] = \omega }
	    {\langle (C, S), Compile(push(x.\alpha)) \rangle \Rightarrow (\omega \rhd S)}$$


#### Equivalence
$L \Rightarrow L'$ can be *equivalently* written as $L \Rightarrow L$ iff. $\forall x \in X_{L}, x \sim_L \omega, x \in X_{L'} \cap x \sim_{L'} \omega$. Same for $G$. $S$ must be completely equivalent.

And to make notation less cluttered, when $L, S, C, G$ is not referenced and equivalent under interpretation, we can neglect them in the expression.


#### Misc
$$\frac{cb_1, cb_2 \in [S], \langle E, Compile(cb_1) \rangle \Rightarrow E', \langle E', Compile(s_2) \rangle \Rightarrow E''}
	   {\langle E, Compile(cb_1 + cb_2) \rangle \Rightarrow E''}$$


$$\frac{ F_0 \diamond ... \diamond F_i @\langle(L_i, G, C, S_i) , Susp \rangle \diamond \langle ([\omega_0, \omega_1, ..., void, ...], G, C, empty\_stack) , f \rangle \Longrightarrow F_0 \diamond ... \diamond F_i \diamond \langle (L, G, C, \omega \rhd S) RET \rangle}{f_{run}(f, [\omega_i]) \sim \omega}$$



XXX: We should move "G" to a really global place ... coming up with good notation is hard for me.
