use aline::{output, Config, FieldSelector, Fields, InputFormat};
use csv;
use std::fs::File;
use std::io::{stdin, BufRead, BufReader, Read};
use structopt::StructOpt;

fn main() {
    let config: Config = Config::from_args();
    if config.verbose {
        println!("{:#?}", config);
    }

    let parse = config.parser();

    if config.inputs.len() == 0 {
        let r = stdin();
        let rl = r.lock();
        for line in rl.lines() {
            let l = line.expect("unable to read line");
            let pl = parse(&l);
            let out = output(pl.as_ref(), &config.output, &config.fields);
            println!("{}", out);
        }
    }

    for fname in &config.inputs {
        if config.print_filename {
            println!("# {}", fname.to_str().unwrap());
        }
        let f = File::open(fname).unwrap();

        if config.input_format == Some(InputFormat::CSV) {
            let b: Box<dyn Iterator<Item = Box<dyn Fields + 'static>>> = config.parser_iter(f);
            for r in b {
                println!("{}", output(r.as_ref(), &config.output, &config.fields));
            }
        } else {
            let fbuf = BufReader::new(f);
            for line in fbuf.lines() {
                let l = line.expect("unable to read line");
                let pl = parse(&l);
                let out = output(pl.as_ref(), &config.output, &config.fields);
                println!("{}", out);
            }
        }
    }
}
