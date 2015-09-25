use gc::*;
use super::*;
use self::_Object::*;
use classty::*;
use primty::*;
use framety::*;

// used for profiling
use std::cell::Cell;
thread_local!(static COUNT_DROPPED: Cell<u8> = Cell::new(0u8));

#[derive(Trace)]
pub enum _Object {
    Int(Int_ty),
    Arr(Array_ty),
    Frm(Frame_ty),
    Its(Instance_ty),
    Cls(Class_ty),
    Non,
}

impl Object for _Object {
    fn call(&self, name : &str, args: Vec<Gc<_Object>>) -> Gc<_Object> {
        match self {
            &Int(ref intty) => intty.call(name, args),
            &Arr(ref arrty) => arrty.call(name, args),
            &Frm(ref frmty) => frmty.call(name, args),
            &Cls(ref clsty) => clsty.call(name, args),
            &Its(ref itsty) => itsty.call(name, args),
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
            &Frm(ref frmty) => frmty.tyof(),
            &Cls(ref clsty) => clsty.tyof(),
            &Its(ref itsty) => itsty.tyof(),
            &Non => { "<None>" },
        }
    }
}


impl Drop for _Object {
    fn drop(&mut self) {
        COUNT_DROPPED.with(|count| count.set(count.get() + 1)); 
        println!("Dropping Object");
    }
}


