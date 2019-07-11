use aline::Config;
use std::fs::File;
use std::io::{stdin, stdout};
use structopt::StructOpt;

fn main() {
    let config: Config = Config::from_args();
    if config.verbose {
        println!("{:#?}", config);
    }

    if config.inputs.is_empty() {
        config.parse_and_output(stdin(), stdout());
    }

    for fname in &config.inputs {
        if config.print_filename {
            println!("# {}", fname.to_str().unwrap());
        }
        let f = File::open(fname).unwrap();
        config.parse_and_output(f, stdout());
    }
}
