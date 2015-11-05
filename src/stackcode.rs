use super::*;
use gc::*;
use self::SCode::*;
use std::string::String;

#[allow(non_camel_case_types)]
#[derive(Trace, Clone, Debug)]
pub enum SCode {
    PUSHL(Integer),
    PUSHG(Integer),
    ADD, // "Add" two objects together
    CALL(Integer, Gc<String>, Integer), // Receiver, Method name, and number of arguments
    RET, // return the stack top,

    // Extended
    NEW(Integer), // Construct and push constructed object on stack
    PUSH_INT(Integer), // Push a literal on stack
    PUSH_STR(Gc<String>), // Push string of attribute on stack,

    FRMEND, // End code literal mode, indicate the number of globals, and push the frame object on stack
    CLASS(Integer, Integer),

    POPG(Integer),
    POPL(Integer),

    PRINT,

    POPA(Integer),
    PUSHA(Integer),
}

pub fn deserialize(bytes: &[u8], pos_mut: &mut usize) -> SCode {
    let pos = *pos_mut;
    let size = bytes[pos] as usize;
    let opcode = bytes[pos + 1];
    let mut operands: Vec<i32> = vec![];
    let mut i = 2;
    while i < size + 1 {
        let mut operand: i32 = 0;
        for j in 0..4 {
            operand = operand + ((bytes[pos + i + 3 - j] as i32) << (j * 8));
        }
        i = i + 4;
        operands.push(operand)
    }

    let scode = match opcode {
        0 => PUSHL(operands[0]),
        1 => PUSHG(operands[0]),
        2 => POPG(operands[0]),
        3 => ADD,
        4 => CALL(operands[0], read_string(bytes[pos + 10 .. pos + size + 1].to_vec()), operands[1]),
        5 => RET,
        6 => NEW(operands[0]),
        7 => PUSH_INT(operands[0] as Integer),
        8 => PUSH_STR(read_string(bytes[pos + 2 .. pos + size + 1].to_vec())),
        9 => FRMEND,
        10 => CLASS(operands[0], operands[1]),
        11 => PRINT,
        12 => POPL(operands[0]),
        13 => POPA(operands[0]),
        14 => PUSHA(operands[0]),
        15 => PUSHSELF,
        _ => { assert!(false, "Not implemented deserialization: {:?}", opcode); unimplemented!() }
    };

    *pos_mut = pos + size + 1;

    scode
}

#[test]
fn test_deserialize() {
    let bytes = vec![5, 1, 0, 0, 0, 12];
    println!("{:?}", deserialize(&bytes));
}

fn read_string(v: Vec<u8>) -> Gc<String> {
    let mut ret: String = "".to_string();
    if v.len() > 4 { // Deal with the completely coded segment
        match String::from_utf8(v[0..(v.len() - 4)].to_vec()) {
            Ok(s) => {
                ret = ret + &s;
            },
            Err(e) => { assert!(false, "error in reading string: {:?}", e); unimplemented!() }
        }

        let mut i = v.len() - 4;
        while i < v.len() && v[i] == 0 {
            i = i + 1;
        }

        if i < v.len() {
            match String::from_utf8(v[i..v.len()].to_vec()) {
                Ok(s) => {
                    ret = ret + &s;
                },
                Err(e) => { assert!(false, "error in reading string: {:?}", e); unimplemented!() }
            }
        }
    } else {
        let mut i = 0;
        while i < v.len() && v[i] == 0 {
            i = i + 1;
        }

        if i < v.len() {
            match String::from_utf8(v[i..v.len()].to_vec()) {
                Ok(s) => {
                    ret = ret + &s;
                },
                Err(e) => { assert!(false, "error in reading string: {:?}", e); unimplemented!() }
            }
        }
    }

    Gc::new(ret.clone())
}



