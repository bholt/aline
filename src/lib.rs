use regex::Regex;
use std::collections::HashMap;
use std::path::PathBuf;
use std::str::FromStr;
use structopt::{StructOpt, clap};

mod re;

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

    #[structopt(short = "d", long = "delimiter")]
    /// Delimiter to use for parsing fields, cannot be set along with --input.
    pub delimiter: Option<String>,

    #[structopt(short = "i", long = "input")]
    /// Input format (csv, json), cannot be set along with --delimiter.
    pub input_format: Option<InputFormat>,

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
    fn parser(&self) -> impl Fn(&str) -> ParsedLine {
        if let Some(fmt) = &self.input_format {
            match fmt {
                "" =>
            }
        } else {
            self.parser_delim()
        }
    }

    fn parser_delim(&self) -> impl Fn(&str) -> ParsedLine {
        let delim = if let Some(d) = &self.delimiter {
            Regex::new(d).unwrap()
        } else {
            Regex::new(r"\s+").unwrap()
        };
        let whitespace = self.delimiter.is_none();

        return move |line: &str| {
            // if this is splitting by whitespace then skip the first "field"
            let ltrim = if whitespace { line.trim() } else { line };
            println!("@> {:?}", ltrim);
            let parts = delim
                .split(&ltrim)
                .map(String::from)
                .collect::<Vec<String>>();
            ParsedLine {
                fields: parts,
                keys: HashMap::new(),
            }
        };
    }
}

/// How to parse the input
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum InputFormat {
    JSON,
    CSV,
    Custom(String),
}

impl FromStr for InputFormat {
    type Err = clap::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        unimplemented!()
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

    /// Helper to create parsed line for testing
    fn pline(fields: &[&str]) -> ParsedLine {
        ParsedLine {
            fields: fields.iter().map(|s| String::from(*s)).collect(),
            keys: HashMap::new(),
        }
    }

    #[test]
    fn test_cfg_helper() {
        assert_eq!(
            Config {
                delimiter: Some(",".to_string()),
                ..Config::default()
            },
            cfg(&["-d,"]),
        )
    }

    #[test]
    fn parsed_line_output() {
        for (ps, fmt, fs, out) in &[
            (pline(&["a", "b"]), Space, vec![Index(0)], "a"),
            (pline(&["a", "b"]), Space, vec![Index(1)], "b"),
            (pline(&["a", "b"]), Space, vec![Index(0), Index(1)], "a b"),
            (pline(&["a", "b"]), CSV, vec![Index(1)], "b"),
            (pline(&["a", "b"]), CSV, vec![Index(0), Index(1)], "a,b"),
            (
                pline(&["a", "b,c"]),
                CSV,
                vec![Index(0), Index(1)],
                "a,\"b,c\"",
            ),
            (
                pline(&["a", "b", "c"]),
                CSV,
                vec![Index(0), Index(1), Index(2)],
                "a,b,c",
            ),
        ] {
            assert_eq!(
                *out,
                ps.output(fmt.clone(), fs.to_vec()),
                "\n### TEST CASE: ps={:?} fmt={:?} fs={:?} ###\n",
                *ps,
                fmt,
                fs
            );
        }
    }

    #[test]
    fn parser() {
        for (line, config, expect) in &[
            ("a b", cfg(&[]), pline(&["a", "b"])),
            ("a  b", cfg(&[]), pline(&["a", "b"])),
            (" a  b  ", cfg(&[]), pline(&["a", "b"])), // skip leading whitespace
            ("a,b", cfg(&["-d,"]), pline(&["a", "b"])),
            ("a,,b", cfg(&["-d,"]), pline(&["a", "", "b"])), // include empty comma fields
        ] {
            assert_eq!(
                config.parser()(line),
                *expect,
                "\n### TEST CASE: line={:?} config={:?} ###\n",
                *line,
                config
            );
        }
    }

    #[test]
    fn test_fmt() {
        assert_eq!("b a", format!("{1} {0}", "a", "b"));
    }
}
