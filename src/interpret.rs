/* 
 * Intrepreting bytecode instruction
 *
 */

use env::Env;
use gc::*;
use dispatch::DynObj;
use bytecode::ByteCode;
use object::*;

pub fn runFrame(env: &Env, stack: &mut Vec<Gc<DynObj>>, n_locals: usize, code: &Vec<ByteCode>) {
    let mut locals : Vec<Gc<DynObj>> = init_vec(n_locals, Gc::new(DynObj::Non));
    for inst in code {
        interpret(env, inst, stack, &mut locals);
    }
}

pub fn interpret(env: &Env, inst: &ByteCode, stack: &mut Vec<Gc<DynObj>>, locals: &mut Vec<Gc<DynObj>>) {

}


#[inline]
fn init_vec<T: Clone>(len: usize, init: T) -> Vec<T> {
    let mut v = Vec::with_capacity(len);
    for _ in 0..len {
        v.push(init.clone());
    }
    v
}