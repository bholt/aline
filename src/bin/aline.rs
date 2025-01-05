use aline::{Config, InputFormat};
use std::fs::File;
use std::io::{stdin, stdout};
use structopt::StructOpt;

fn main() {
    let mut config: Config = Config::from_args();
    if config.verbose {
        println!("{:#?}", config);
    }

    if config.inputs.is_empty() {
        config.parse_and_output(stdin(), stdout());
        return;
    }

    if config.input_format == None
        && config
            .inputs
            .iter()
            .all(|i| i.extension().map_or(false, |e| e == "csv"))
    {
        config.input_format = Some(InputFormat::CSV);
    }

    for fname in &config.inputs {
        if config.print_filename {
            println!("# {}", fname.to_str().unwrap());
        }
        let f = File::open(fname).unwrap();
        config.parse_and_output(f, stdout());
    }
}
