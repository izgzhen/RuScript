//! 
//! Deserialization of binary to bytecode
//!

use bytecode::ByteCode;
use bytecode::ByteCode::*;

/// Deserialize bytes array form pos_mut
pub fn deserialize(bytes: &[u8], pos_mut: &mut usize) -> ByteCode {
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

    let inst = match opcode {
        0 => CALL(operands[0]),
        1 => INVOKE(read_string(bytes[pos + 2 .. pos + size + 1].to_vec())),
        2 => RET,
        3 => JUMP(operands[0]),
        4 => JUMPT(operands[0]),
        5 => JUMPF(operands[0]),
        6 => PUSH(operands[0]),
        7 => POP(operands[0]),
        8 => NEW(operands[0]),
        9 => PUSHA(read_string(bytes[pos + 2 .. pos + size + 1].to_vec())),
        10 => POPA(read_string(bytes[pos + 2 .. pos + size + 1].to_vec())),
        11 => PUSHSTR(read_string(bytes[pos + 2 .. pos + size + 1].to_vec())),
        12 => PUSHINT(operands[0]),
        13 => PUSHBOOL(operands[0]),
        14 => CLASS(operands[0], operands[1], operands[2]),
        15 => SFUNC,
        16 => EBODY(operands[0]),
        17 => PUSHLIST,
        18 => PUSHNIL,
        _  => panic!("Not implemented deserialization: {:?}", opcode)
    };

    *pos_mut = pos + size + 1;

    inst
}

/// Read string embedded in bytes, the string encoding is UTF-8
fn read_string(v: Vec<u8>) -> String {
    let mut ret: String = "".to_string();

    if v.len() > 4 {
        // Deal with the completely coded segment
        match String::from_utf8(v[0..(v.len() - 4)].to_vec()) {
            Ok(s) => {
                ret = ret + &s;
            },
            Err(e) => panic!("error in reading string: {:?}", e)
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
                Err(e) => panic!("error in reading string: {:?}", e)
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
                Err(e) => panic!("error in reading string: {:?}", e)
            }
        }
    }

    ret
}