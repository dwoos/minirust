mod context;

mod ast;

mod parse;

mod codegen;

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
    let mut parsed = parse::parse_program(&input).expect("parse error");
    let checked = typecheck::check_program(&mut parsed).expect("type error");
    codegen::compile_program(checked, std::path::Path::new(output_file)).expect("compile error");
}
