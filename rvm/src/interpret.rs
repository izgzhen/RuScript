//!
//! Intrepreting bytecode instruction
//!

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
use cmdopt::CmdOpt;

/// Interpreter State
pub struct Interpreter {
    cmd_opt : CmdOpt,
}

impl Interpreter {
    #[inline]
    pub fn new(opt: CmdOpt) -> Interpreter {
        Interpreter {
            cmd_opt : opt,
        }
    }

    /// Initialize and execute a frame
    pub fn run_frame(&self, env: &Env, stack: &mut Vec<Gc<DynObj>>,
                     n_locals: usize, code: &Vec<ByteCode>) {
        let mut locals : Vec<Gc<DynObj>> = init_vec(n_locals, Gc::new(DynObj::Non));

        let mut pc: usize = 0;
        while pc < code.len() {
            if self.cmd_opt.debug {
                println!("[DEBUG] Executing: {:?}", code[pc]);
            }

            // Dealing with frame control instructions
            match code[pc] {
                // Call global function
                CALL(fn_idx) => {
                    let function: &Function = &env.functions[fn_idx as usize];
                    self.run_frame(env, stack, function.n_locals, &function.code);
                    pc = pc + 1;
                },
                // Invoke method of instance (TOS)
                INVOKE(ref mtd_name) => {
                    let recv: Gc<DynObj> = stack.pop().unwrap();
                    match *recv {
                        // Check type at runtime
                        DynObj::Ist(ref istobj) => {
                            let class: &Class = &env.classes[istobj.cls as usize];
                            // `this` pointer
                            stack.push(recv.clone());
                            match class.get_method(mtd_name, env) {
                                Some(ref function) => {
                                    self.run_frame(env, stack, function.n_locals, &function.code);
                                    pc = pc + 1;
                                },
                                None => {
                                    panic!("FATAL: invoke built-in methods on instance")
                                }
                            }
                        },
                        _ => {
                            recv.invoke(mtd_name, stack, env);
                        },
                    };
                    pc = pc + 1
                },
                RET => {
                    return;
                },
                ref other => {
                    self.interpret(env, other, stack, &mut locals, &mut pc);
                    pc = pc + 1;
                }
            }
        }
    }

    /// Interpret in-frame instructions
    fn interpret(&self, env: &Env, inst: &ByteCode, stack: &mut Vec<Gc<DynObj>>,
                 locals: &mut Vec<Gc<DynObj>>, pc: &mut usize) {
        match inst {
            &JUMP(offset) => {
                // offset might be negative
                *pc = (*pc as Integer + offset) as usize;
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
                    stack.push(BoolObj::new(false));
                } else {
                    stack.push(BoolObj::new(true));
                }
            },
            &PUSHLIST => {
                stack.push(Gc::new(DynObj::List(ListObj::Empty)));
            },
            &PUSHNIL => {
                stack.push(Gc::new(DynObj::Non));
            }
            other => { panic!("{:?}'s interpretation is not implemented", other) }
        }
    }
}

/// Jump if TOS is bool object and its value is equal to `cond`
#[inline]
fn jump_if(stack: &mut Vec<Gc<DynObj>>, pc: &mut usize, cond: bool, offset: Integer) {
    let tos = stack.pop().unwrap();
    match *tos {
        DynObj::Bool(ref boolobj) => {
            if boolobj.val == cond {
                // offset might be negative
                *pc = (*pc as Integer + offset) as usize;
            }
        }
        _ => panic!("JUMPT error: TOS is not bool")
    }
}

/// Initialize a vector of length `len` and full of `init`
#[inline]
fn init_vec<T: Clone>(len: usize, init: T) -> Vec<T> {
    let mut v = Vec::with_capacity(len);
    for _ in 0..len {
        v.push(init.clone());
    }
    v
}
