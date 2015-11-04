use gc::*;
use self::_Object::*;
use classty::*;
use primty::*;
use framety::*;

// used for profiling
// use std::cell::Cell;
// thread_local!(static COUNT_DROPPED: Cell<u8> = Cell::new(0u8));

// impl Drop for _Object {
//     fn drop(&mut self) {
//         COUNT_DROPPED.with(|count| count.set(count.get() + 1)); 
//         println!("Dropping Object");
//     }
// }


#[derive(Trace)]
pub enum _Object {
    Int(Int_ty),
    Arr(Array_ty),
    Str(String_ty),
    Frm(Frame_ty),
    Its(Instance_ty),
    Cls(Class_ty),
    Non,
}

impl Object for _Object {
    fn call(&self, name : &str, args: Vec<Gc<_Object>>, env: &Gc<Env>, globals: &mut Vec<Gc<_Object>>) -> Gc<_Object> {
        match self {
            &Int(ref intty) => intty.call(name, args, env, globals),
            &Arr(ref arrty) => arrty.call(name, args, env, globals),
            &Str(ref strty) => strty.call(name, args, env, globals),
            &Frm(ref frmty) => frmty.call(name, args, env, globals),
            &Cls(ref clsty) => clsty.call(name, args, env, globals),
            &Its(ref itsty) => itsty.call(name, args, env, globals),
            &Non => {
                println!("None object is not cllable");
                Gc::new(Non)
            },
        }
    }

    fn tyof(&self) -> &str {
        match self {
            &Int(ref intty) => intty.tyof(),
            &Arr(ref arrty) => arrty.tyof(),
            &Str(ref strty) => strty.tyof(),
            &Frm(ref frmty) => frmty.tyof(),
            &Cls(ref clsty) => clsty.tyof(),
            &Its(ref itsty) => itsty.tyof(),
            &Non => { "<None>" },
        }
    }
}


