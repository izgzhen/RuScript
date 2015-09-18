use ast::*;
use ast::Statement::*;
use object::*;
use gc::*;


pub fn interprete(st: &Statement, env : &mut Environment) {
    // let lhs_ : String;
    // let rhs_ : Gc<Object>;
    let mut lhs_ = None;
    let mut rhs_ = None;
    match st {
        &Assign(ref name, ref expr) => {
            match eval(expr, &env) {
                Some(prim) => {
                    env.objects.insert(name.clone(), Gc::new(Object::PrimObj(prim)));
                }
                None => {}
            }
        },
        &Aliasing(ref lhs, ref rhs) => {
            let mut x = None;
            x = env.objects.get(rhs);
            match x {
                Some(gced) => {
                    lhs_ = Some(lhs.clone());
                    rhs_ = Some(gced.clone());
                },
                None => { }
            }
        }
    }
    match lhs_ {
        Some(lhs) => match rhs_ {
            Some(rhs) => { env.objects.insert(lhs, rhs); },
            None => {}
        },
        None => {}
    }
}

fn eval(expr : &Expr, env : &Environment) -> Option<PrimitiveType> {
    match expr {
        &Expr::Single(ref tm) => { evalTerm(tm, &env) },
        &Expr::Plus(ref tm1, ref tm2) => {
            let op1 = evalTerm(tm1, &env);
            let op2 = evalTerm(tm2, &env);
            if(op1.is_some() && op2.is_some()) {
                Some(op1.unwrap() + op2.unwrap())
            } else {
                None
            }
        },
    }
}

fn evalTerm(tm : &Term, env : &Environment) -> Option<PrimitiveType> {
    match tm {
        &Term::Single(ref f) => { evalFactor(f, &env) },
        &Term::Multi(ref f1, ref f2) => {
            let op1 = evalFactor(f1, &env);
            let op2 = evalFactor(f2, &env);
            if(op1.is_some() && op2.is_some()) {
                Some(op1.unwrap() + op2.unwrap())
            } else {
                None
            }
        },
    }
}

fn evalFactor(f : &Factor, env : &Environment) -> Option<PrimitiveType> {
    match f {
        &Factor::Int(i) => { Some(PrimitiveType::PrimInt(i)) },
        &Factor::Var(ref name) => {
            match env.objects.get(name) {
                Some(gced) => {
                    match *(gced.clone()) {
                        Object::PrimObj(ref prim) => Some(prim.clone()),
                        _ => None
                    }
                },
                None => { None }
            }
        },
        &Factor::Single(ref exprBox) => {
            eval(exprBox, &env)
        }
    }
}

pub fn assignToA() -> Statement {
    Assign("a".to_string(), Expr::Single(Term::Single(Factor::Int(1))))
}

pub fn aliasingToB() -> Statement {
    Aliasing("b".to_string(), "a".to_string())
}

