/* 
 * Intrepreting bytecode instruction
 *
 */

use env::Env;
use gc::*;
use dispatch::DynObj;
use bytecode::ByteCode;
use bytecode::ByteCode::*;
use object::*;
use function::*;
use class::*;

pub fn runFrame(env: &Env, stack: &mut Vec<Gc<DynObj>>,
                n_locals: usize, code: &Vec<ByteCode>) {
    let mut locals : Vec<Gc<DynObj>> = init_vec(n_locals, Gc::new(DynObj::Non));
    for inst in code {
        match inst {
            &CALL(fn_idx) => {
                let function: &Function = &env.functions[fn_idx as usize];
                runFrame(env, stack, function.n_locals, &function.code);
            },
            &INVOKE(ref mtd_name) => {
                let mut recv: Gc<DynObj> = stack.pop().unwrap(); // TOS
                match *recv {
                    DynObj::Ist(ref istobj) => {
                        let class: &Class = &env.classes[istobj.cls as usize];
                        // stack.push(recv.clone());
                        match class.methods.get(mtd_name) {
                            Some(ref function) => {
                                runFrame(env, stack, function.n_locals, &function.code);
                            },
                            None => { /* invoke built-in methods */
                                panic!("invoke built-in methods on instance")
                            }
                        }
                    },
                    _ => { panic!("Wrong type of receiver"); }
                }
            },
            &RET => {
                return;
            },
            other => interpret(env, other, stack, &mut locals),
        }
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