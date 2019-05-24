use regex::{Captures, Regex};
use serde_json;
use std::collections::HashMap;
use std::io::{BufRead, BufReader, Read};
use std::path::PathBuf;
use std::str::FromStr;
use structopt::{clap, StructOpt};

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
    /// Field(s) to print, specified by number or name (depending on the input format).
    pub fields: Vec<FieldSelector>,

    #[structopt(short = "o", long = "output")]
    // Output format
    pub output: Option<OutputFormat>,

    #[structopt(short = "v", long = "verbose")]
    pub verbose: bool,

    #[structopt(short = "p", long = "print-filenames")]
    /// Print each filename prefixed with '#' before its results
    pub print_filename: bool,
}

impl Config {
    pub fn parser_iter<R: Read + 'static>(
        &self,
        reader: R,
    ) -> Box<dyn Iterator<Item = Box<dyn Fields + 'static>>> {
        match &self.input_format {
            Some(InputFormat::CSV) => Box::new(csv_parser(reader)),
            Some(InputFormat::JSON) => Box::new(self.json_parser_iter(reader)),
            Some(InputFormat::Custom(s)) => Box::new(custom_parser_iter(s, reader)),
            None => Box::new(self.delim_parser_iter(reader)),
        }
    }

    pub fn parser(&self) -> Box<dyn Fn(&str) -> Box<dyn Fields>> {
        match &self.input_format {
            Some(InputFormat::JSON) => Box::new(self.parser_json()),
            Some(_) => unimplemented!(),
            None => Box::new(self.parser_delim()),
        }
    }

    fn parser_json(&self) -> impl Fn(&str) -> Box<dyn Fields> {
        move |line: &str| {
            let v: serde_json::Value = serde_json::from_str(line).unwrap();
            Box::new(v)
        }
    }

    fn json_parser_iter<R: Read>(
        &self,
        reader: R,
    ) -> impl Iterator<Item = Box<dyn Fields + 'static>> {
        BufReader::new(reader).lines().flat_map(move |r| match r {
            Ok(line) => {
                let v: serde_json::Value = serde_json::from_str(line.as_str()).unwrap();
                let b: Box<dyn Fields> = Box::new(v);
                Some(b)
            }
            Err(e) => {
                eprintln!("line parsing error: {:?}", e);
                None
            }
        })
    }

    fn parser_delim(&self) -> impl Fn(&str) -> Box<dyn Fields> {
        let delim = if let Some(d) = &self.delimiter {
            Regex::new(d).unwrap()
        } else {
            Regex::new(r"\s+").unwrap()
        };
        let whitespace = self.delimiter.is_none();

        move |line: &str| {
            // if this is splitting by whitespace then skip the first "field"
            let ltrim = if whitespace { line.trim() } else { line };
            let fields = delim
                .split(&ltrim)
                .map(String::from)
                .collect::<Vec<String>>();
            Box::new(DelimitedLine { fields })
        }
    }

    fn delim_parser_iter<R: Read>(
        &self,
        reader: R,
    ) -> impl Iterator<Item = Box<dyn Fields + 'static>> {
        let delim = if let Some(d) = &self.delimiter {
            Regex::new(d).unwrap()
        } else {
            Regex::new(r"\s+").unwrap()
        };
        let whitespace = self.delimiter.is_none();
        BufReader::new(reader).lines().flat_map(move |r| {
            match r {
                Ok(line) => {
                    // if this is splitting by whitespace then skip the first "field"
                    let ltrim = if whitespace {
                        line.trim()
                    } else {
                        line.as_ref()
                    };
                    let fields = delim
                        .split(&ltrim)
                        .map(String::from)
                        .collect::<Vec<String>>();
                    let b: Box<dyn Fields> = Box::new(DelimitedLine { fields });
                    Some(b)
                }
                Err(e) => {
                    eprintln!("line parsing error: {:?}", e);
                    None
                }
            }
        })
    }
}

fn csv_parser<R: Read>(reader: R) -> impl Iterator<Item = Box<dyn Fields + 'static>> {
    csv::Reader::from_reader(reader)
        .into_records()
        .flat_map(|r| match r {
            Ok(v) => {
                let b: Box<dyn Fields> = Box::new(v);
                Some(b)
            }
            Err(e) => {
                eprintln!("record parsing error: {:?}", e);
                None
            }
        })
}

fn custom_parser_iter<R: Read>(
    input: &str,
    reader: R,
) -> impl Iterator<Item = Box<dyn Fields + 'static>> {
    let pattern = Regex::new(input).unwrap();
    BufReader::new(reader).lines().flat_map(move |r| match r {
        Ok(line) => match pattern.captures(line.as_str()) {
            Some(c) => {
                let b: Box<dyn Fields> = Box::new(RegexLine::new(&pattern, &c));
                Some(b)
            }
            None => None,
        },
        Err(e) => {
            eprintln!("line parsing error: {:?}", e);
            None
        }
    })
}

struct RegexLine {
    groups: Vec<Option<String>>,
    named_groups: HashMap<String, String>,
}

impl RegexLine {
    fn new<'t>(pattern: &Regex, captures: &Captures<'t>) -> Self {
        let mut r = RegexLine {
            groups: captures
                .iter()
                .map(|m| m.map(|m| m.as_str().to_string()))
                .collect(),
            named_groups: HashMap::new(),
        };
        for name in pattern.capture_names().flatten() {
            if let Some(m) = captures.name(name) {
                r.named_groups
                    .insert(name.to_string(), m.as_str().to_string());
            }
        }
        r
    }
}

impl Fields for RegexLine {
    fn field(&self, f: &FieldSelector) -> Option<String> {
        match f {
            FieldSelector::Index(i) => {
                if *i >= self.groups.len() {
                    None
                } else {
                    self.groups[*i].clone()
                }
            }
            FieldSelector::Key(k) => self.named_groups.get(k).map(ToOwned::to_owned),
        }
    }
}

impl Fields for csv::StringRecord {
    fn field(&self, f: &FieldSelector) -> Option<String> {
        match f {
            FieldSelector::Index(i) => self.get(*i).map(|s| s.to_owned()),
            FieldSelector::Key(_) => None,
        }
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
        match s.to_ascii_lowercase().as_ref() {
            "json" => Ok(InputFormat::JSON),
            "csv" => Ok(InputFormat::CSV),
            _ => Ok(InputFormat::Custom(s.to_string())),
        }
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

pub trait Fields {
    fn field(&self, f: &FieldSelector) -> Option<String>;
}

pub struct DelimitedLine {
    fields: Vec<String>,
}

impl Fields for DelimitedLine {
    fn field(&self, f: &FieldSelector) -> Option<String> {
        match f {
            FieldSelector::Index(i) => {
                if i < &self.fields.len() {
                    Some(self.fields[*i].clone())
                } else {
                    None
                }
            }
            FieldSelector::Key(_) => None,
        }
    }
}

impl Fields for serde_json::Value {
    fn field(&self, f: &FieldSelector) -> Option<String> {
        let v = match f {
            FieldSelector::Index(i) => self.get(i),
            FieldSelector::Key(k) => self.get(k),
        };

        return v.map(|v| format!("{}", v));
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

impl FromStr for OutputFormat {
    type Err = clap::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_ascii_lowercase().as_ref() {
            " " => Ok(OutputFormat::Space),
            "json" => Ok(OutputFormat::JSON),
            "csv" => Ok(OutputFormat::CSV),
            _ => Ok(OutputFormat::Custom(s.to_string())),
        }
    }
}

impl Fields for ParsedLine {
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
}

pub fn output(fields: &dyn Fields, format: &Option<OutputFormat>, sel: &[FieldSelector]) -> String {
    let fmt = format.clone().unwrap_or(OutputFormat::Space);
    match fmt {
        OutputFormat::Space => sel
            .iter()
            .map(|f| fields.field(f))
            .flatten()
            .collect::<Vec<String>>()
            .join(" "),
        OutputFormat::CSV => sel
            .iter()
            .map(|f| fields.field(f))
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

//fn run(config: &Config, lines: impl Iterator<Item = String>) -> impl Iterator<Item = String> {
//    unimplemented!()
//}

#[cfg(test)]
mod tests {
    use crate::FieldSelector::*;
    use crate::OutputFormat::*;
    use crate::{output, Config, FieldSelector, OutputFormat, ParsedLine};
    use std::collections::HashMap;
    use structopt::StructOpt;

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
                output(ps, &Some(fmt.clone()), fs),
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
    fn end_to_end() {
        let c = cfg(&["-d,", "-f1"]);
        let p = c.parser();
        let pl = p("a,b,c");
        let out = output(pl.as_ref(), &c.output, &c.fields);
        assert_eq!("b", out);
    }
}
