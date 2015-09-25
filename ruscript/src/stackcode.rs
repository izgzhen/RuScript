use super::*;


#[derive(Trace)]
pub enum SCode {
    PUSH(ObjIdentTy), // Use number as identifier, needs some translation to generate efficent opcode as well as symbal table
    ADD, // "Add" two objects together
    CALL(ObjIdentTy, String, ArgIdentTy), // Receiver, Method name, and number of arguments
    RET // return the stack top
} 
