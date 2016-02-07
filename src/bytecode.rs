//! 
//! Bytecode Instructions
//!

use super::*;

#[derive(Clone, Debug)]
pub enum ByteCode {

// --- Frame Manipulation ---

    /// Call declared function: arguments number, function index
    CALL(Integer),

    /// Invoke method of TOS: arguments number, method name
    INVOKE(String),

    /// Return without value
    RET,

// ---- Control -----

    /// Jump unconditionally: relative offset
    JUMP(Integer),

    /// Jump if TOS is `True` boolean: relative offset
    JUMPT(Integer),

    /// Jump if TOS is `False` boolean: relative offset
    JUMPF(Integer),

// --- General Data Stack Manipulation ---

    /// Push local variable on stack: variable index
    PUSH(Integer),

    /// Pop TOS to local variable: variable index
    POP(Integer),

// --- OO Specific ---

    /// Instantiate a class and push instance on stack: class index
    NEW(Integer),

    /// Push attribute of TOS on stack: attribute name
    PUSHA(String),

    /// Pop TOS to certain attribute of NTOS,
    /// the mutated NTOS will be pushed back: attribute name
    POPA(String),

// --- Built-in ----

    /// Push a literal String object on stack
    PUSHSTR(String),

    /// Push a literal Integer object on stack
    PUSHINT(Integer),

    /// Push a literal Boolean object on stack, zero is False, non-zero is True
    PUSHBOOL(Integer),

// ---- Pseudo Instruction ---

    /// Class declaration: Number of attributes, number of methods,
    /// father class index (negative for none)
    CLASS(Integer, Integer, Integer),

    /// Mark the start of function declaration
    SFUNC,

    /// Mark the end of function/method definition: Number of local vars
    EBODY(Integer),

}

