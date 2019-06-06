use aline::{output, Config};
use std::fs::File;
use std::io::stdin;
use structopt::StructOpt;

fn main() {
    let config: Config = Config::from_args();
    if config.verbose {
        println!("{:#?}", config);
    }

    if config.inputs.is_empty() {
        for r in config.parser_iter(stdin()) {
            println!("{}", output(r, &config));
        }
    }

    for fname in &config.inputs {
        if config.print_filename {
            println!("# {}", fname.to_str().unwrap());
        }
        let f = File::open(fname).unwrap();
        for r in config.parser_iter(f) {
            println!("{}", output(r, &config));
        }
    }
}
