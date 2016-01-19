/* 
 * Runtime Environment
 *
 */

use class::*;
use bytecode::ByteCode;
use deserialize::*;
use std::fs::File;
use std::io::Read;
use function::*;

pub struct Env {
    pub classes   : Vec<Class>,
    pub functions : Vec<Function>,
}

pub fn load(f: &mut File) -> (Env, Vec<ByteCode>) {
    let mut classes   = vec![];
    let mut functions = vec![];
    let mut top_code  = vec![];
    let mut bytes: Vec<u8> = Vec::new();

    match Read::read_to_end(f, &mut bytes) {
        Ok(len) => {
            let mut start_pos = 0;
            let mut code = vec![];
            loop {
                code.push(deserialize(bytes.as_slice(), &mut start_pos));
                if start_pos >= len {
                    break;
                }
            }

            let mut pc = 0;
            while pc < code.len() {
                match code[pc] {
                    ByteCode::CLASS(ref nattrs, ref nmtds, ref father_idx) => {
                        pc = pc + 1;
                        let class = parse_class(&code, *nattrs as usize, *nmtds as usize, &mut pc);
                        classes.push(class);
                    },
                    ByteCode::SFUNC => {
                        pc = pc + 1;
                        functions.push(parse_function(&code, &mut pc));
                    },
                    ref inst => {
                        top_code.push(inst.clone());
                        pc = pc + 1;
                    }
                }
            }
        },
        Err(e) => {
            panic!("reading bytes error: {}", e);
        }
    }

    (Env { classes : classes, functions : functions }, top_code)
}
