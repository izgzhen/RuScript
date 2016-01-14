Bytecode Instructions
----

This design is based previous bytecode used and the [Python bytecode](https://docs.python.org/2/library/dis.html).


## Frame Manipulation
* `CALL`: Call declared functions, the function is index by integer, arguments are passed by stack.
* `INVOKE`: Invoke methods on TOS, the method is indexed by name, arguments are passed by stack
* `RET`: Return from method/function; if any, the returned value is TOS

## Imperative Control
* `JUMP`: Jump relatively inside a codeblock unconditionally
* `JUMPF`: Jump relatively inside a codeblock if TOS is `False` boolean
* `JUMPT`: Jump relatively inside a codeblock if TOS is `True` boolean

## General Data Stack Manipulation
* `PUSHL`: Push integer-indexed local register value on stack
* `POPL`: Pop TOS to integer-indexed local register
* `PUSHG`: Push integer-indexed global register value on stack
* `POPG`: Pop TOS to integer-indexed global register

## OO-specific
* `NEW`: Instantiate a class and push new object on stack, the class is indexed by integer, and initializer arguments are passed-in by stack.
* `PUSHA`: Push certain attribute value of TOS on stack, the attribute is indexed by name.
* `POPA`: Pop TOS to certain attribute value of NTOS, the attribute is indexed by name.

## Built-in Specific
* `PUSH_STR`: Push a literal String object on stack
* `PUSH_INT`: Push a literal Integer object on stack
* `PUSH_BOOL`: Push a literal Boolean object on stack
* `ADD`: Pop two successive Integer from stack top, add them and push result on stack
* `NOT`: Find "not" of TOS Boolean object.
* `CMPG`: Compare TOS and NTOS (Next of TOS), if TOS > NTOS, push `True` boolean on stack, and `False` if not
* `CMPE`: Compare TOS and NTOS (Next of TOS), if TOS = NTOS, push `True` boolean on stack, and `False` if not


## I/O
* `PRINT`: Print TOS to stdout

## Pseudo Instruction
* `CLASS`: Mark the start of class declaration
* `EBODY`: Mark the end of function/method body

n
