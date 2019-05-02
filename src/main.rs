use aline::{process, Config};
use std::io::{stdin, BufRead, Error};
use structopt::StructOpt;

fn main() {
    let config = Config::from_args();
    if config.verbose {
        println!("{:#?}", config);
    }

    if config.inputs.len() == 0 {
        let r = stdin();
        process(r.lock());
    }
}
