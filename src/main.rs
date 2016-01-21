extern crate ruscript;
extern crate gc;

use std::fs::File;
use std::env;

fn main() {
    if let Some(ref src) = env::args().nth(1) {
        let mut f_opt = File::open(src);
        match f_opt {
            Ok(ref mut f) => {
                let (e, top, top_n) = ruscript::env::load(f);
                let mut stack = vec![];
                ruscript::interpret::runFrame(&e, &mut stack, top_n, &top);
            },
            Err(err) => panic!("Read binary error: {:?}", err),
        }
    } else {
        println!("Usage: {} <binary>", env::args().nth(0).unwrap());
    }

    gc::force_collect();
}