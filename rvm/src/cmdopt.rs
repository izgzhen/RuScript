//! 
//! Command line Option
//!

use std::env;

#[derive(Clone)]
pub struct CmdOpt {
    /// Dump debug information
    pub debug   : bool,
    /// Dump GC profile information
    pub gc_dump : bool,
    /// Benchmark the program by repeating
    pub bench   : Option<usize>,
}

/// Default command line options
pub fn default_cmdopt() -> CmdOpt {
    CmdOpt {
        debug   : false,
        gc_dump : false,
        bench   : None,
    }
}

pub fn parse_opt() -> CmdOpt {
    let mut i = 2;
    let mut opt = default_cmdopt();
    let nargs = env::args().count();

    while i < nargs {
        match env::args().nth(i) {
            Some(ref opt_str) => {
                if opt_str == "-debug" {
                    opt.debug = true; 
                } else if opt_str == "-bench" && i + 1 < nargs {
                    let s = env::args().nth(i + 1).unwrap();
                    opt.bench = Some(s.parse::<usize>().unwrap());
                    i = i + 1;
                } else if opt_str == "-gc_dump" {
                    opt.gc_dump = true;
                }
            },
            None => {},
        };
        i = i + 1;
    }

    opt
}
