#![feature(box_syntax)]
use std::io::{self, Read};
use std::fs::File;
use std::env;
use std::path::Path;

extern crate getopts;
use getopts::Options;

extern crate parser;
use parser::*;

extern crate llvm_gen;
use llvm_gen::*;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        return;
    }
    let name = args[0].clone();

    let mut opts = Options::new();
    opts.optflag("h", "help", "print this help menu");
    opts.optflag("l", "llvm", "generate LLVM IR");
    opts.optflag("p", "parse", "parse source code and show AST");
    let matches = match opts.parse(&args[1..]) {
        Ok(m) => m,
        Err(f) => panic!(f.to_string()),
    };

    if matches.opt_present("h") {
        print_usage(&name, &opts);
        return;
    }

    if matches.free.len() == 0 {
        print_usage(&name, &opts);
        return;
    }
    let mut buffer = String::new();
    let path = Path::new(&matches.free[0]);
    File::open(path).unwrap().read_to_string(&mut buffer);
    let (source_name, program) = (path.file_stem().unwrap().to_str().unwrap(), buffer);

    match module(program.as_str()) {
        Ok(e) => {
            if matches.opt_present("p") {
                println!("{:?}", e);
                return;
            } else if matches.opt_present("l") {
                codegen(&e, &source_name.to_string());
            }
        }
        Err(err) => {
            println!("{}", err);
        }
    }
}

fn print_usage(program: &str, opts: &Options) {
    let brief = format!("Usage: {} FILE [options]", program);
    print!("{}", opts.usage(&brief));
}