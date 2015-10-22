use super::*;
use gc::*;

#[derive(Trace, Clone)]
pub enum SCode {
    PUSHL(ObjIdentTy),
    PUSHG(ObjIdentTy),
    ADD, // "Add" two objects together
    CALL(ObjIdentTy, Gc<String>, ArgIdentTy), // Receiver, Method name, and number of arguments
    RET, // return the stack top,

    // Extended
    NEW(ObjIdentTy), // Construct and push constructed object on stack
    PUSH_INT(int), // Push a literal on stack
    PUSH_STR(Gc<String>), // Push string of attribute on stack,

    FRMEND, // End code literal mode, indicate the number of globals, and push the frame object on stack
    CLASS(int, int),

    POPG(ObjIdentTy),

    PRINT,
}
