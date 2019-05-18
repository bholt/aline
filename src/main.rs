use aline::{output, Config, FieldSelector, Fields, InputFormat};
use csv;
use std::fs::File;
use std::io::{stdin, BufRead, BufReader, Read};
use structopt::StructOpt;

struct CSVRecord {
    r: csv::StringRecord,
}

impl Fields for CSVRecord {
    fn field(&self, f: &FieldSelector) -> Option<String> {
        match f {
            FieldSelector::Index(i) => self.r.get(*i).map(|s| s.to_owned()),
            FieldSelector::Key(_) => None,
        }
    }
}

struct CSVParser<R: Read> {
    r: csv::StringRecordsIntoIter<R>,
}

impl<R: Read> CSVParser<R> {
    fn new(r: R) -> Self {
        CSVParser {
            r: csv::Reader::from_reader(r).into_records(),
        }
    }
}

impl<R: Read> Iterator for CSVParser<R> {
    type Item = Box<dyn Fields>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.r.next() {
            Some(r) => match r {
                Ok(v) => Some(Box::new(CSVRecord { r: v })),
                Err(e) => {
                    eprintln!("record parsing error: {:?}", e);
                    None
                }
            },
            None => None,
        }
    }
}

fn csv_parser<R: Read>(reader: R) -> impl Iterator<Item = Box<dyn Fields + 'static>> {
    csv::Reader::from_reader(reader)
        .into_records()
        .flat_map(|r| match r {
            Ok(v) => {
                let b: Box<dyn Fields> = Box::new(CSVRecord { r: v });
                Some(b)
            }
            Err(e) => {
                eprintln!("record parsing error: {:?}", e);
                None
            }
        })
}

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
            for r in csv_parser(f) {
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
