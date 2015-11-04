//  #![feature(custom_derive)]
//  #![feature(box_syntax)]

// extern crate ruscript;
// extern crate gc;

// use ruscript::object::*;
// use gc::*;
// use ruscript::stackcode::SCode::*;


// #[test]
// fn test_class_instance() {
//     {
//         let i = Int_ty::new(10);
//         let j = Int_ty::new(20);

//         let cb = Box::new(vec![PUSH(0), PUSH(1), ADD, RET]);
//         let globals = Box::new(vec![]);

//         let mtd = Method_ty {
//             name  : "adder".to_string(),
//             frame : Frame_ty::new(cb, globals),
//         };

//         let cls = Class_ty::new("example", vec![mtd], vec![]);

//         let child_obj = __new__(&cls);

//         let ret = child_obj.call("adder", vec![i, j]);

//         match *ret {
//             _Object::Int(ref intty) => { assert_eq!(30, intty.unbox()); }, // 30 = 10 + 20
//             _ => {}
//         }
//     }

//     gc::force_collect();   
// }
