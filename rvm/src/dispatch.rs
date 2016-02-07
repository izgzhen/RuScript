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
    List(ListObj),
    /// None Type, mostly used to initialize empty local vars
    Non,
}

/// Dispatch Object trait calls explicitly at runtime
impl Object for DynObj {
    fn invoke(&self, name : &str, stack: &mut Vec<Gc<DynObj>>, env: &Env){
        if name == "print" {
            // print!("{}", &self.to_string());
            return
        }

        match self {
            &Int(ref intobj) => intobj.invoke(name, stack, env),
            &Bool(ref boolobj) => boolobj.invoke(name, stack, env),
            &Str(ref strobj) => strobj.invoke(name, stack, env),
            &Ist(ref istobj) => istobj.invoke(name, stack, env),
            &List(ref listobj) => listobj.invoke(name, stack, env),
            &Non => invoke_fail("Nil", name),
        }
    }

    fn get(&self, name: &str, env: &Env) -> Gc<DynObj> {
        match self {
            &Int(ref intobj) => intobj.get(name, env),
            &Bool(ref boolobj) => boolobj.get(name, env),
            &Str(ref strobj) => strobj.get(name, env),
            &Ist(ref istobj) => istobj.get(name, env),
            &List(ref listobj) => listobj.get(name, env),
            &Non => panic!("Non object is not usable"),
        }
    }

    fn set(&mut self, name: &str, new: &Gc<DynObj>, env: &Env) {
        match self {
            &mut Int(ref mut intobj) => intobj.set(name, new, env),
            &mut Bool(ref mut boolobj) => boolobj.set(name, new, env),
            &mut Str(ref mut strobj) => strobj.set(name, new, env),
            &mut Ist(ref mut istobj) => istobj.set(name, new, env),
            &mut List(ref mut listobj) => listobj.set(name, new, env),
            &mut Non => panic!("Non object is not usable"),
        }
    }

    fn to_string(&self) -> String {
        match self {
            &Int(ref intobj) => intobj.to_string(),
            &Bool(ref listobj) => listobj.to_string(),
            &Str(ref strobj) => strobj.to_string(),
            &Ist(ref istobj) => istobj.to_string(),
            &List(ref listobj) => listobj.to_string(),
            &Non => "nil".to_string(),
        }
    }    

    fn tyof(&self) -> String {
        match self {
            &Int(ref intobj) => intobj.tyof(),
            &Bool(ref listobj) => listobj.tyof(),
            &Str(ref strobj) => strobj.tyof(),
            &Ist(ref istobj) => istobj.tyof(),
            &List(ref listobj) => listobj.tyof(),
            &Non => "<Nil>".to_string(),
        }
    }
}