use super::*;
use gc::*;
use std::fmt::*;
use std::io::Bytes;
use std::io;
use std::io::Take;
use self::SCode::*;

#[derive(Trace, Clone, Debug)]
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

fn deserialize(bytes: &[u8]) -> SCode {
    let size = bytes[0] as usize;
    let opcode = bytes[1];
    let mut operands: Vec<i32> = vec![];
    let mut i = 2;
    while i < size + 1 {
        let mut operand: i32 = 0;
        for j in 0..4 {
            operand = operand + ((bytes[i + 3 - j] as i32) << (j * 8));
        }
        i = i + 4;
        operands.push(operand)
    }

    match opcode {
        0 => PUSHL(operands[0]),
        1 => PUSHG(operands[0]),
        2 => ADD,
//         3 => // CALL(operands[0], ) {}
        4 => RET,
        5 => NEW(operands[0]),
        6 => PUSH_INT(operands[0] as int),
//        7 => 
        8 => FRMEND,
        9 => CLASS(operands[0], operands[1]),
        10 => POPG(operands[0]),
        11 => PRINT,
        _ => { unimplemented!(); }
    }
}

#[test]
fn test_deserialize() {
    let bytes = vec![5, 1, 0, 0, 0, 12];
    println!("{:?}", deserialize(&bytes));
}
