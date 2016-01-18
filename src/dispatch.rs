/* 
 * Dynamic dispatch
 *
 * We need to explicitly deal with different types of
 * data dynamically. Although Object trait object
 * is also dynamic, but too dangerous to be casted
 * back
 */

use primitives::*;
use instance::InstanceObj;
use env::Env;
use object::*;
use gc::*;
use self::DynObj::*;

#[derive(Trace)]
pub enum DynObj {
    Int(IntObj),
    Bool(BoolObj),
    Str(StrObj),
    Its(InstanceObj),
}

impl Object for DynObj {
    fn invoke(&mut self, name : &str, args: Vec<Gc<DynObj>>, env: &Env) -> Option<Gc<DynObj>> {
        match self {
            &mut Int(ref mut intobj) => intobj.invoke(name, args, env),
            &mut Bool(ref mut arrobj) => arrobj.invoke(name, args, env),
            &mut Str(ref mut strobj) => strobj.invoke(name, args, env),
            &mut Its(ref mut itsobj) => itsobj.invoke(name, args, env),
        }
    }

    fn get(&self, name: &str, env: &Env) -> Gc<DynObj> {
        match self {
            &Int(ref intobj) => intobj.get(name, env),
            &Bool(ref arrobj) => arrobj.get(name, env),
            &Str(ref strobj) => strobj.get(name, env),
            &Its(ref itsobj) => itsobj.get(name, env),
        }
    }

    fn set(&mut self, name: &str, new: &Gc<DynObj>, env: &Env) {
        match self {
            &mut Int(ref mut intobj) => intobj.set(name, new, env),
            &mut Bool(ref mut arrobj) => arrobj.set(name, new, env),
            &mut Str(ref mut strobj) => strobj.set(name, new, env),
            &mut Its(ref mut itsobj) => itsobj.set(name, new, env),
        }
    }

    fn tyof(&self) -> String {
        match self {
            &Int(ref intobj) => intobj.tyof(),
            &Bool(ref arrobj) => arrobj.tyof(),
            &Str(ref strobj) => strobj.tyof(),
            &Its(ref itsobj) => itsobj.tyof(),
        }
    }
}