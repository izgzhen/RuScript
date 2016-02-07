extern crate rvm;
extern crate gc;

use std::fs::File;
use std::env;
use rvm::cmdopt::*;
use rvm::interpret::Interpreter;

fn main() {
    if let Some(ref src) = env::args().nth(1) {

        let opt = parse_opt();

        let mut f_opt = File::open(src);
        match f_opt {
            Ok(ref mut f) => {
                let (e, top, top_n) = rvm::env::load(f);
                let interpreter = Interpreter::new(opt.clone());

                let times: usize = match opt.bench {
                    None => 1,
                    Some(ref n) => n.clone(),
                };

                println!("times {:?}", times);

                for _ in 0..times {
                    let mut stack = vec![];
                    interpreter.run_frame(&e, &mut stack, top_n, &top);
                }
            },
            Err(err) => panic!("Read binary error: {:?}", err),
        }
    } else {
        println!("Usage: {} <binary>", env::args().nth(0).unwrap());
    }

    gc::force_collect();
}