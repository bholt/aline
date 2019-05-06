use aline::Config;
use regex::Regex;
use std::fs::File;
use std::io::{stdin, BufRead, BufReader, Error};
use std::process::exit;
use structopt::StructOpt;

fn process_lines(config: &Config, reader: impl BufRead) -> Result<(), Error> {
    let delim = Regex::new(&config.delimiter).unwrap();
    for line in reader.lines() {
        let l = line?;
        if let Some(f) = config.field {
            let mut parts = delim.split(&l);
            if let Some(part) = parts.nth(f) {
                println!("{}", part);
            }
        }
    }
    Ok(())
}

fn extract_field(
    config: &Config,
    lines: impl Iterator<Item = String>,
) -> impl Iterator<Item = String> {
    let delim = Regex::new(&config.delimiter).unwrap();
    let field = config.field.clone();
    return lines.flat_map(move |line| {
        if let Some(n) = field {
            let mut parts = delim.split(&line);
            return parts.nth(n).map(String::from);
        } else {
            return Some(line.clone());
        }
    });
}

fn main() {
    let config: Config = Config::from_args();
    if config.verbose {
        println!("{:#?}", config);
    }

    if config.inputs.len() == 0 {
        let r = stdin();
        let rl = r.lock();
        process_lines(&config, rl).unwrap_or_else(|e| {
            eprintln!("error processing stdin: {:?}", e);
            exit(1)
        });
    }

    for fname in &config.inputs {
        if config.print_filename {
            println!("# {}", fname.to_str().unwrap());
        }
        let f = File::open(fname).unwrap();
        let fbuf = BufReader::new(f);
        // process_lines(&config, fbuf);
        let lines_cleaned = fbuf.lines().flat_map(|r| match r {
            Ok(l) => Some(l),
            Err(e) => {
                eprintln!("error reading line: {:?}", e);
                None
            }
        });
        let fields = extract_field(&config, lines_cleaned);
        for f in fields {
            println!("{}", f);
        }
    }
}
