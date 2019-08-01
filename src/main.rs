mod ast;
mod codegen;
mod context;
mod typecheck;

use clap::{App, Arg};
use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub grammar); // synthesized by LALRPOP

fn main() {
    let matches = App::new("minirust")
        .version("0.1")
        .author("Doug Woos <dwoos@cs.washington.edu>")
        .about("Oddity driver")
        .arg(
            Arg::with_name("INPUT")
                .help("input filename")
                .required(true)
                .index(1),
        )
        .arg(
            Arg::with_name("OUTPUT")
                .help("output filename")
                .required(true)
                .index(2),
        )
        .get_matches();

    let input =
        std::fs::read_to_string(matches.value_of("INPUT").unwrap()).expect("error reading input");

    let output_file = matches.value_of("OUTPUT").unwrap();
    let mut parsed = grammar::ProgramParser::new()
        .parse(&input)
        .expect("parse error");
    typecheck::check_program(&mut parsed).expect("type error");
    println!("{:?}", parsed);
    codegen::compile_program(parsed, output_file).expect("compile error");
}
