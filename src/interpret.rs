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
use instance::InstanceObj;
use super::*;
use primitives::*;

pub fn runFrame(env: &Env, stack: &mut Vec<Gc<DynObj>>,
                n_locals: usize, code: &Vec<ByteCode>) {
    let mut locals : Vec<Gc<DynObj>> = init_vec(n_locals, Gc::new(DynObj::Non));

    let mut pc: usize = 0;
    while pc < code.len() {
        match code[pc] {
            CALL(fn_idx) => {
                let function: &Function = &env.functions[fn_idx as usize];
                runFrame(env, stack, function.n_locals, &function.code);
                pc = pc + 1;
            },
            INVOKE(ref mtd_name) => {
                let mut recv: Gc<DynObj> = stack.pop().unwrap(); // TOS
                match *recv {
                    DynObj::Ist(ref istobj) => {
                        let class: &Class = &env.classes[istobj.cls as usize];
                        // stack.push(recv.clone());
                        match class.methods.get(mtd_name) {
                            Some(ref function) => {
                                runFrame(env, stack, function.n_locals, &function.code);
                                pc = pc + 1;
                            },
                            None => { 
                                panic!("FATAL: invoke built-in methods on instance")
                            }
                        }
                    },
                    _ => { panic!("Wrong type of receiver"); }
                }
            },
            RET => {
                return;
            },
            ref other => {
                interpret(env, other, stack, &mut locals, &mut pc);
                pc = pc + 1;
            }
        }
    }
}

pub fn interpret(env: &Env, inst: &ByteCode, stack: &mut Vec<Gc<DynObj>>,
                 locals: &mut Vec<Gc<DynObj>>, pc: &mut usize) {
    match inst {
        &JUMP(offset) => {
            *pc = *pc + offset as usize;
        },
        &JUMPT(offset) => {
            jump_if(stack, pc, true, offset);
        },
        &JUMPF(offset) => {
            jump_if(stack, pc, false, offset);
        },
        &PUSH(idx) => {
            stack.push(locals[idx as usize].clone());
        },
        &POP(idx) => {
            let obj = stack.pop().unwrap();
            locals[idx as usize] = obj.clone();
        },
        &NEW(cls_idx) => {
            let class: &Class = &env.classes[cls_idx as usize];
            let obj = InstanceObj::new(class.attrs.len(), cls_idx as usize, stack);
            stack.push(obj);
        },
        &PUSHA(ref attr_name) => {
            let tos = stack.pop().unwrap();
            stack.push(tos.get(attr_name, env));
        },
        &POPA(ref attr_name) => {
            let tos = stack.pop().unwrap();
            let mut ntos_copy: DynObj = (*stack.pop().unwrap()).clone();
            ntos_copy.set(attr_name, &tos, env);
            stack.push(Gc::new(ntos_copy));

            /*
                Although in fact we copied the object entirely, but only
                pointers are copied so the time overhead is not significant.

                Beyond that, the original NTOS is popped out and will be
                garbage-collected sometime, so the references to the unmodified
                parts are actually intact.
             */
        },
        &PUSHSTR(ref s) => {
            stack.push(StrObj::new(s.clone()));
        },
        &PUSHINT(i) => {
            stack.push(IntObj::new(i));
        },
        &PUSHBOOL(i) => {
            if i == 0 {
                stack.push(BoolObj::new(true));
            } else {
                stack.push(BoolObj::new(false));
            }
        },
        _ => { unimplemented!() }
    }
}

#[inline]
fn jump_if(stack: &mut Vec<Gc<DynObj>>, pc: &mut usize, cond: bool, offset: Integer) {
    let tos = stack.pop().unwrap();
    match *tos {
        DynObj::Bool(ref boolobj) => {
            if boolobj.val == cond {
                *pc = *pc + offset as usize;
            }
        }
        _ => panic!("JUMPT error: TOS is not bool")
    }
}


#[inline]
fn init_vec<T: Clone>(len: usize, init: T) -> Vec<T> {
    let mut v = Vec::with_capacity(len);
    for _ in 0..len {
        v.push(init.clone());
    }
    v
}