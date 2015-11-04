use stackcode::*;
use stackcode::SCode::*;
use gc::*;
use super::*;
use super::object::*;
// use super::object::_Object::*;
use super::classty::*;
use super::primty::*;


pub fn interprete(inst: &SCode,
                  scratch: &mut Vec<Gc<_Object>>,
                  stack: &GcCell<Vec<Gc<_Object>>>,
                  env: &Gc<Env>,
                  globals: &mut Vec<Gc<_Object>>) {

    println!("interpreting: {:?}", inst);

    match inst {
        &PUSHL(x) => {
            stack.borrow_mut().push(scratch[x as usize].clone());
        },
        &POPL(x) => {
            let i = x as usize;
            if GLOBAL_MAXSIZE > i {
               scratch[i] = stack.borrow_mut().pop().unwrap(); 
            } else {
                assert!(false, "scratch is not as many as {}", i + 1);
            }
        },
        &ADD => {
            let a1 = stack.borrow_mut().pop().unwrap();
            let a2 = stack.borrow_mut().pop().unwrap();
            let ret = a1.call("add", vec![a2], env, globals);
            stack.borrow_mut().push(ret);
        },
        &NEW(x) => {
            let obj = env.__new__(x as usize);
            stack.borrow_mut().push(obj);
        },
        &PUSH_INT(i) => {
            let obj = Int_ty::new(i);
            stack.borrow_mut().push(obj);
        },
        &PUSH_STR(ref s) => {
            let obj = String_ty::new(s.clone());
            stack.borrow_mut().push(obj);
        },
        &PRINT => {
            let x = stack.borrow_mut().pop().unwrap();
            x.call("__print__", vec![], env, globals);
        },
        &PUSHG(x) => {
            stack.borrow_mut().push(globals[x as usize].clone());
        },                        
        &POPG(x) => {
            let i = x as usize;
            if GLOBAL_MAXSIZE > i {
               globals[i] = stack.borrow_mut().pop().unwrap(); 
            } else {
                assert!(false, "globals is not as many as {}", i + 1);
            }
        },
        _ => {
            assert!(false, "illegal instruction: {:?}", inst);
        }
    }
}