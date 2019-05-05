use std::io::BufRead;
use std::io::Result;
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "aline",
    author = "Brandon Holt",
    version = "0.1.0",
    about = "Replacement for awk/cut/sed. Never again struggle to remember which one does what you want or splits fields how you want, just switch to aline for all your line-parsing needs"
)]
pub struct Config {
    /// Delimiter to use for separating
    #[structopt(short = "d", long = "delimiter", default_value = "\\s+")]
    pub delimiter: String,

    #[structopt(short = "f", long = "field")]
    pub field: Option<usize>,

    #[structopt(short = "v", long = "verbose")]
    pub verbose: bool,

    #[structopt(parse(from_os_str))]
    pub inputs: Vec<PathBuf>,

    #[structopt(short = "p", long = "print-filenames")]
    /// Print each filename prefixed with '#' before its results
    pub print_filename: bool,
}

impl Config {
    pub fn handle_line<'a>(&self, l: &'a str) -> Option<&'a str> {
        if let Some(f) = self.field {
            let mut parts = l.split(&self.delimiter);
            return parts.nth(f);
        }
        return Some(l);
    }
}

//pub fn process_reader<'a>(
//    config: &'a Config,
//    r: &'a mut impl BufRead,
//) -> impl Iterator<Item = Option<String>> + 'a {
//    return r.lines().map(move |rl| {
//        let l = rl.unwrap();
//        println!(">>> {}", &l);
//        if let Some(f) = config.field {
//            let mut parts = l.split(&config.delimiter);
//            return parts.nth(f).map(String::from);
//        }
//        return Some(l);
//    });
//}

#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::io::{BufRead, BufReader};

    #[test]
    fn scratch() {
        //        let f = File::open("Cargo.toml").unwrap();
        //        let fbuf = BufReader::new(f);
        //        let lines = fbuf.lines();
        let xs = vec![1, 7, 8, 2, 6, 3, 4];
        println!("{:?}", xs);
        let ys = xs
            .iter()
            .flat_map(|x| if x > &5 { None } else { Some(x) })
            .collect::<Vec<&i32>>();
        println!("{:?}", ys);
        assert!(false);
    }
}
