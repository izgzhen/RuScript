/* 
 * Function Definition and related
 *
 */

use bytecode::ByteCode;

pub struct Function {
    pub code: Vec<ByteCode>,
    pub n_locals: usize,
}

pub fn parse_function(code: &Vec<ByteCode>, pc: &mut usize) -> Function {
    let mut cb = vec![];

    loop {
        match code[*pc] {
            ByteCode::EBODY(n) => {
                *pc = *pc + 1;
                return Function {
                    code: cb,
                    n_locals: n as usize,
                }
            },
            _ => {
                cb.push(code[*pc].clone());
                *pc = *pc + 1;
            }
        }
    }
}