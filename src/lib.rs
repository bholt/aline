use regex::Regex;
use std::collections::HashMap;
use std::fmt;
use std::path::PathBuf;
use std::str::FromStr;
use structopt::StructOpt;

#[derive(Debug, StructOpt, Default, Eq, PartialEq)]
#[structopt(
    name = "aline",
    author = "Brandon Holt",
    version = "0.1.0",
    about = "Replacement for awk/cut/sed. Never again struggle to remember which one does what you want or splits fields how you want, just switch to aline for all your line-parsing needs"
)]
#[structopt(raw(setting = "structopt::clap::AppSettings::ColoredHelp"))]
pub struct Config {
    #[structopt(parse(from_os_str))]
    // Inputs passed as arguments
    pub inputs: Vec<PathBuf>,

    #[structopt(short = "d", long = "delimiter", default_value = "\\s+")]
    /// Delimiter to use for parsing fields, cannot be set along with --input.
    pub delimiter: String,

    #[structopt(short = "i", long = "input")]
    /// Input format (csv, json), cannot be set along with --delimiter.
    pub input_format: Option<String>,

    #[structopt(short = "f", long = "field")]
    // Field(s) to print, specified by number or name (depending on the input format).
    pub field: Option<usize>,

    #[structopt(short = "k", long = "keys")]
    // Field(s) to print, specified by number or name (depending on the input format).
    pub fields: Vec<FieldSelector>,

    #[structopt(short = "v", long = "verbose")]
    pub verbose: bool,

    #[structopt(short = "p", long = "print-filenames")]
    /// Print each filename prefixed with '#' before its results
    pub print_filename: bool,
}

impl Config {
    fn default() -> Config {
        Config {
            inputs: vec![],
            delimiter: "".to_string(),
            input_format: None,
            field: None,
            fields: vec![],
            verbose: false,
            print_filename: false,
        }
    }

    fn parser(&self) -> impl Fn(&str) -> ParsedLine {
        let delim = Regex::new(&self.delimiter).unwrap();
        // if Some(input) = &self.input_format {
        return move |l: &str| {
            let parts = delim.split(&l).map(String::from).collect::<Vec<String>>();
            ParsedLine {
                fields: parts,
                keys: HashMap::new(),
            }
        };
    }
}

/// Selects a field to output
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum FieldSelector {
    Index(usize),
    Key(String),
}

impl FromStr for FieldSelector {
    type Err = std::io::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.parse::<usize>() {
            Ok(i) => Ok(FieldSelector::Index(i)),
            Err(_) => Ok(FieldSelector::Key(String::from(s))),
        }
    }
}

// ParsedLine contains the parsed fields of a line
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ParsedLine {
    fields: Vec<String>,
    keys: HashMap<String, String>,
}

// Output format to use when printing.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum OutputFormat {
    Space,
    CSV,
    JSON,
    Custom(String),
}

impl ParsedLine {
    fn new(fields: &[&str]) -> Self {
        ParsedLine {
            fields: fields.iter().map(|s| String::from(*s)).collect(),
            keys: HashMap::new(),
        }
    }

    fn field(&self, f: &FieldSelector) -> Option<String> {
        match f {
            FieldSelector::Index(i) => {
                if i < &self.fields.len() {
                    Some(self.fields[*i].clone())
                } else {
                    None
                }
            }
            FieldSelector::Key(k) => self.keys.get(k).cloned(),
        }
    }

    fn output(&self, format: OutputFormat, sel: Vec<FieldSelector>) -> String {
        match format {
            OutputFormat::Space => sel
                .iter()
                .map(|f| self.field(f))
                .flatten()
                .collect::<Vec<String>>()
                .join(" "),
            OutputFormat::CSV => sel
                .iter()
                .map(|f| self.field(f))
                .flatten()
                .map(|f| {
                    if f.contains(",") {
                        format!("\"{}\"", f)
                    } else {
                        f
                    }
                })
                .collect::<Vec<String>>()
                .join(","),
            _ => unimplemented!(),
        }
    }
}

//fn run(config: &Config, lines: impl Iterator<Item = String>) -> impl Iterator<Item = String> {
//    unimplemented!()
//}

#[cfg(test)]
mod tests {
    use crate::{Config, FieldSelector, OutputFormat, ParsedLine};
    use std::collections::HashMap;
    use structopt::StructOpt;
    use FieldSelector::*;
    use OutputFormat::*;

    /// Helper to create a Config from flags for testing
    fn cfg(args: &[&str]) -> Config {
        Config::from_iter_safe([&["aline"], args].concat()).expect("invalid config flags")
    }

    #[test]
    fn test_cfg_helper() {
        assert_eq!(
            Config {
                delimiter: ",".to_string(),
                ..Config::default()
            },
            cfg(&["-d,"]),
        )
    }

    /// Helper to create parsed line for testing
    fn pline(fields: &[&str]) -> ParsedLine {
        ParsedLine {
            fields: fields.iter().map(|s| String::from(*s)).collect(),
            keys: HashMap::new(),
        }
    }

    #[test]
    fn parsed_line_output() {
        for (ps, fmt, fs, out) in &[
            (&["a", "b"], Space, vec![Index(0)], "a"),
            (&["a", "b"], Space, vec![Index(1)], "b"),
            (&["a", "b"], Space, vec![Index(0), Index(1)], "a b"),
            (&["a", "b"], CSV, vec![Index(1)], "b"),
            (&["a", "b"], CSV, vec![Index(0), Index(1)], "a,b"),
            (&["a", "b,c"], CSV, vec![Index(0), Index(1)], "a,\"b,c\""),
        ] {
            assert_eq!(*out, pline(*ps).output(fmt.clone(), fs.to_vec()));
        }
    }

    #[test]
    fn parser() {
        let p = cfg(&["-d,"]).parser();
        assert_eq!(p("a,b"), pline(&["a", "b"]));
    }

    #[test]
    fn test_fmt() {
        assert_eq!("b a", format!("{1} {0}", "a", "b"));
    }
}
