/* 
 * Bytecode Instructions
 *
 */

use super::*;

#[derive(Clone)]
pub enum ByteCode {

// --- Frame Manipulation ---

    CALL(Integer, Integer),
    // Call declared function: arguments number, function index

    INVOKE(Integer, String),
    // Invoke method of TOS: arguments number, method name

    RET,
    // Return without value

    RETTOS,
    // Return top of stack

// ---- Control -----

    JUMP(Integer),
    // Jump unconditionally: relative offset

    JUMPT(Integer),
    // Jump if TOS is `True` boolean: relative offset

    JUMPF(Integer),
    // Jump if TOS is `False` boolean: relative offset

// --- General Data Stack Manipulation ---

    PUSH(Integer),
    // Push local variable on stack: variable index

    POP(Integer),
    // Pop TOS to local variable: variable index

// --- OO Specific ---

    NEW(Integer, Integer),
    // Instantiate a class and push instance on stack: class index, arguments number

    PUSHA(String),
    // Push attribute of TOS on stack: attribute name

    POPA(String),
    // Pop TOS to certain attribute of NTOS: attribute name

    PUSHSELF,
    // Push reference to self on stack

// --- Built-in ----

    PUSHSTR(String),
    // Push a literal String object on stack

    PUSHINT(Integer),
    // Push a literal Integer object on stack

    PUSHBOOL(Integer),
    // Push a literal Boolean object on stack, zero is False, non-zero is True

    PRINT,
    // Print TOS to stdout

// ---- Pseudo Instruction ---
    
    CLASS(Integer, Integer, Integer),
    // Class declaration: Number of attributes, number of methods,
    // father class index (negative for none)

    EBODY,
    // Mark the end of function/method definition

    VIRT,
    // Mark `virtual` method
}

