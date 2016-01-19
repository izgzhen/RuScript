//!
//! Dynamic dispatch
//!
//! We need to explicitly deal with different types of
//! data dynamically. Although Object trait object
//! is also dynamic, but too dangerous to be casted
//! back
//!

use primitives::*;
use instance::InstanceObj;
use env::Env;
use object::*;
use gc::*;
use self::DynObj::*;

#[derive(Trace, Clone)]
pub enum DynObj {
    Int(IntObj),
    Bool(BoolObj),
    Str(StrObj),
    Ist(InstanceObj),
    /// None Type, mostly used to initialize empty local vars
    Non,
}

/// Dispatch Object trait calls explicitly at runtime
impl Object for DynObj {
    fn invoke(&mut self, name : &str, stack: &mut Vec<Gc<DynObj>>, env: &Env){
        match self {
            &mut Int(ref mut intobj) => intobj.invoke(name, stack, env),
            &mut Bool(ref mut arrobj) => arrobj.invoke(name, stack, env),
            &mut Str(ref mut strobj) => strobj.invoke(name, stack, env),
            &mut Ist(ref mut istobj) => istobj.invoke(name, stack, env),
            &mut Non => panic!("Non object is not usable"),
        }
    }

    fn get(&self, name: &str, env: &Env) -> Gc<DynObj> {
        match self {
            &Int(ref intobj) => intobj.get(name, env),
            &Bool(ref arrobj) => arrobj.get(name, env),
            &Str(ref strobj) => strobj.get(name, env),
            &Ist(ref istobj) => istobj.get(name, env),
            &Non => panic!("Non object is not usable"),
        }
    }

    fn set(&mut self, name: &str, new: &Gc<DynObj>, env: &Env) {
        match self {
            &mut Int(ref mut intobj) => intobj.set(name, new, env),
            &mut Bool(ref mut arrobj) => arrobj.set(name, new, env),
            &mut Str(ref mut strobj) => strobj.set(name, new, env),
            &mut Ist(ref mut istobj) => istobj.set(name, new, env),
            &mut Non => panic!("Non object is not usable"),
        }
    }

    fn tyof(&self) -> String {
        match self {
            &Int(ref intobj) => intobj.tyof(),
            &Bool(ref arrobj) => arrobj.tyof(),
            &Str(ref strobj) => strobj.tyof(),
            &Ist(ref istobj) => istobj.tyof(),
            &Non => panic!("Non object is not usable"),
        }
    }
}