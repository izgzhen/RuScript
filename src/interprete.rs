use stackcode::*;
use stackcode::SCode::*;
use gc::*;
use super::*;
use super::object::*;
use super::object::_Object::*;
use super::classty::*;
use super::primty::*;


pub fn interprete(inst: &SCode,
                  locals: &Vec<Gc<_Object>>,
                  stack: &GcCell<Vec<Gc<_Object>>>,
                  env: &Gc<Env>,
                  globals: &mut Vec<Gc<_Object>>) -> Gc<_Object> {

    // println!("interpreting {:?}", inst);

    match inst {
        &PUSHL(x) => {
            stack.borrow_mut().push(locals[x as usize].clone());
        },
        &ADD => {        },
        &CALL(recv, ref method, narg) => {
            let ref obj = env.global_objs[recv as usize];

            let mut params = Vec::new();

            for _ in 0..narg {
                let x = stack.borrow_mut().pop().unwrap();
                params.push(x.clone());
            }
            return obj.call(method, params, env, globals);
        },
        &RET => {
            let x = stack.borrow_mut().pop().unwrap();
            return x;
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
            // assert!(false, "illegal instruction: {:?}", inst);
        }
    }

    Gc::new(Non)
}