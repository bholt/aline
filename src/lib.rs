use regex::{Captures, Regex};
use serde::{Deserialize, Serialize};
use serde_json;
use std::collections::HashMap;
use std::io::{BufRead, BufReader, Read, Write};
use std::iter::FromIterator;
use std::path::PathBuf;
use std::str::FromStr;
use structopt::{clap, StructOpt};

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
    pub fields: FieldSelectors,

    #[structopt(short = "o", long = "output")]
    // Output format
    pub output: Option<OutputFormat>,

    #[structopt(short = "v", long = "verbose")]
    pub verbose: bool,

    #[structopt(short = "p", long = "print-filenames")]
    /// Print each filename prefixed with '#' before its results
    pub print_filename: bool,
}

#[derive(Debug, Default, Eq, PartialEq)]
pub struct FieldSelectors {
    r: Vec<FieldSelector>,
}

impl FieldSelectors {
    fn has_named_fields(&self) -> bool {
        for f in &self.r {
            if let FieldSelector::Key(_) = f {
                return true;
            }
        }
        false
    }
}

// Implement FromStr for our custom type so we can parse comma-delimited fields like `-f 1,2`
impl FromStr for FieldSelectors {
    type Err = clap::Error;

    fn from_str(arg: &str) -> Result<Self, Self::Err> {
        Ok(FieldSelectors {
            r: arg
                .split(',')
                .map(|s| FieldSelector::from_str(s))
                .collect::<Result<Vec<_>, _>>()?,
        })
    }
}

impl AsRef<[FieldSelector]> for FieldSelectors {
    fn as_ref(&self) -> &[FieldSelector] {
        &self.r
    }
}

impl Config {
    pub fn parse_and_output(&self, reader: impl Read, writer: impl Write) {
        match &self.input_format {
            Some(InputFormat::CSV) => self.output(self.csv_parser(reader), writer),
            Some(InputFormat::JSON) => self.output(self.json_parser_iter(reader), writer),
            Some(InputFormat::Custom(s)) => self.output(custom_parser_iter(s, reader), writer),
            None => self.output(self.delim_parser_iter(reader), writer),
        }
    }

    pub fn parser_iter<R: Read + 'static>(
        &self,
        reader: R,
    ) -> Box<dyn Iterator<Item = Box<dyn Fields + 'static>>> {
        match &self.input_format {
            Some(InputFormat::CSV) => Box::new(self.csv_parser(reader)),
            Some(InputFormat::JSON) => Box::new(self.json_parser_iter(reader)),
            Some(InputFormat::Custom(s)) => Box::new(custom_parser_iter(s, reader)),
            None => Box::new(self.delim_parser_iter(reader)),
        }
    }

    fn output(&self, iter: impl Iterator<Item = Box<dyn Fields>>, writer: impl Write) {
        let mut w = writer;
        let output = self.output.clone().unwrap_or(OutputFormat::Space);
        match output {
            OutputFormat::Space => {
                for fields in iter {
                    writeln!(
                        w,
                        "{}",
                        self.fields
                            .as_ref()
                            .iter()
                            .map(|f| fields.field(f))
                            .flatten()
                            .collect::<Vec<String>>()
                            .join(" ")
                    )
                    .unwrap();
                }
            }
            OutputFormat::CSV => {
                let has_header = self.fields.has_named_fields();
                let mut csv_writer = csv::WriterBuilder::new()
                    .has_headers(has_header)
                    .from_writer(w);

                if has_header {
                    // write header
                    csv_writer
                        .write_record(self.fields.as_ref().iter().map(|f| f.to_string()))
                        .unwrap();
                }

                for fields in iter {
                    let v = self
                        .fields
                        .as_ref()
                        .iter()
                        .map(|f| fields.field(f).unwrap_or_default());
                    csv_writer.write_record(v).unwrap();
                }
            }
            OutputFormat::JSON => {
                for fields in iter {
                    let iter = self.fields.as_ref().iter().map(|f| {
                        (
                            f.to_string(),
                            fields
                                .field(f)
                                .map_or_else(|| serde_json::Value::Null, serde_json::Value::String),
                        )
                    });
                    let fmap = serde_json::Map::from_iter(iter);
                    let out = serde_json::to_string(&fmap).unwrap();
                    writeln!(w, "{}", out).unwrap();
                }
            }
            _ => unimplemented!(),
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
                    let fields: Vec<_> = delim.split(&ltrim).map(String::from).collect();
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

    fn csv_parser<R: Read>(&self, reader: R) -> impl Iterator<Item = Box<dyn Fields + 'static>> {
        let has_header = self.fields.has_named_fields();

        let mut rb = csv::ReaderBuilder::new();
        rb.flexible(true);
        rb.has_headers(has_header);

        let mut r = rb.from_reader(reader);

        let h = Box::new(csv_header_map(if has_header {
            r.headers().ok()
        } else {
            None
        }));

        r.into_records().flat_map(move |r| match r {
            Ok(v) => {
                let b: Box<dyn Fields> = Box::new(CSVRecord {
                    row: v,
                    header: h.clone(),
                });
                Some(b)
            }
            Err(e) => {
                eprintln!("record parsing error: {:?}", e);
                None
            }
        })
    }
}

fn custom_parser_iter<R: Read>(
    input: &str,
    reader: R,
) -> impl Iterator<Item = Box<dyn Fields + 'static>> {
    let pattern = Regex::new(input).unwrap();
    BufReader::new(reader).lines().flat_map(move |r| match r {
        // TODO: try to replace with some native regex match struct rather than converting to vec/map
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

/// Extracted regex match groups
#[derive(Debug)]
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

fn csv_header_map(h: Option<&csv::StringRecord>) -> HashMap<String, usize> {
    let mut m = HashMap::new();
    if let Some(h) = h {
        for (i, f) in h.iter().enumerate() {
            m.insert(f.to_owned(), i);
        }
    }
    m
}

#[derive(Debug)]
struct CSVRecord {
    row: csv::StringRecord,
    header: Box<HashMap<String, usize>>,
}

impl Fields for CSVRecord {
    fn field(&self, f: &FieldSelector) -> Option<String> {
        match f {
            FieldSelector::Index(i) => self.row.get(*i).map(ToOwned::to_owned),
            FieldSelector::Key(k) => {
                if let Some(i) = &self.header.get(k) {
                    self.row.get(**i).map(ToOwned::to_owned)
                } else {
                    None
                }
            }
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

/// Selects a field to output
#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub enum FieldSelector {
    Index(usize),
    Key(String),
}

impl ToString for FieldSelector {
    fn to_string(&self) -> String {
        match self {
            FieldSelector::Index(i) => format!("{}", i),
            FieldSelector::Key(k) => k.to_owned(),
        }
    }
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

pub trait Fields: std::fmt::Debug {
    fn field(&self, f: &FieldSelector) -> Option<String>;
}

#[derive(Debug)]
pub struct DelimitedLine {
    fields: Vec<String>,
}

impl Fields for DelimitedLine {
    fn field(&self, f: &FieldSelector) -> Option<String> {
        match f {
            FieldSelector::Index(i) => {
                if *i < self.fields.len() {
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
        v.map(|v| match v {
            serde_json::Value::String(s) => s.to_owned(),
            _ => format!("{}", v),
        })
    }
}

// ParsedLine contains the parsed fields of a line
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ParsedLine {
    fields: Vec<String>,
    keys: HashMap<String, String>,
}

impl Fields for ParsedLine {
    fn field(&self, f: &FieldSelector) -> Option<String> {
        match f {
            FieldSelector::Index(i) => {
                if *i < self.fields.len() {
                    Some(self.fields[*i].clone())
                } else {
                    None
                }
            }
            FieldSelector::Key(k) => self.keys.get(k).cloned(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{Config, FieldSelector, FieldSelectors};
    use shlex;
    use structopt::StructOpt;

    /// Helper to create a Config from flags for testing
    fn cfg(args: &str) -> Config {
        let mut args = shlex::split(args).expect("invalid args string");
        args.insert(0, "aline".to_owned());
        Config::from_iter_safe(args).expect("invalid config flags")
    }

    #[test]
    fn test_cfg_helper() {
        assert_eq!(
            Config {
                delimiter: Some(",".to_string()),
                fields: FieldSelectors {
                    r: vec![FieldSelector::Index(0)]
                },
                ..Config::default()
            },
            cfg("-d, -f0"),
        )
    }

    #[test]
    fn end_to_end() {
        fn e2e(line: &'static str, args: &str) -> String {
            let config = cfg(args);
            let mut s = Vec::new();
            config.parse_and_output(line.as_bytes(), &mut s);
            String::from_utf8(s).unwrap()
        }
        let l = "a,b,c";
        assert_eq!(e2e(l, "-d, -f1"), "b\n");
        assert_eq!(e2e(l, "-d, -f2"), "c\n");
    }

    macro_rules! e2e_assert {
        ($line:expr, $args:expr, $expect:expr) => {
            let config = cfg($args);
            let mut buf = Vec::new();
            config.parse_and_output($line.as_bytes(), &mut buf);
            let got = String::from_utf8(buf).unwrap();
            let mut want = String::from($expect);
            if !want.ends_with('\n') {
                want.push('\n');
            }
            assert_eq!(got, want, "\nconfig: {:#?}", config);
        };
    }

    #[test]
    fn csv() {
        let l = "a,b,c";
        e2e_assert!(l, "-d, -f1", "b");
        e2e_assert!(l, "-d, -f2", "c");
        e2e_assert!(l, "-d, -f1,2", "b c");
        e2e_assert!(l, "-i csv -f1", "b");
        e2e_assert!(l, "-i csv -f0,2", "a c");
        e2e_assert!(l, "-i csv -o csv -f0,2", "a,c");
        e2e_assert!(l, "-d, -o json -f0,2", r#"{"0":"a","2":"c"}"#);
    }

    #[test]
    fn csv_with_header() {
        let line_with_header = "a,b,c\n0,1,2";
        e2e_assert!(line_with_header, "-i csv -o csv -f a", "a\n0");
        e2e_assert!(line_with_header, "-i csv -o csv -f c,b", "c,b\n2,1");
        e2e_assert!(
            line_with_header,
            "-i csv -o json -f c,b",
            r#"{"b":"1","c":"2"}"#
        );
    }

    #[test]
    fn json() {
        let json_map = r#"{"a":0, "b": "bb", "c": 2}"#;
        e2e_assert!(json_map, "-i json -f a", "0");
        e2e_assert!(json_map, "-i json -f a,b", "0 bb");
        e2e_assert!(json_map, "-i json -o csv -f a,b", "a,b\n0,bb");
        let json_list = r#"[0, "bb", 2]"#;
        e2e_assert!(json_list, "-i json -o csv -f 0,1", "0,bb");
    }
}
