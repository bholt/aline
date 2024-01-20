use aline::{Config, FieldSelector, FieldSelectors};
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
            fields: Some(FieldSelectors {
                r: vec![FieldSelector::Index(0)]
            }),
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
        if want.len() > 0 && !want.ends_with('\n') {
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
    e2e_assert!(l, "-i comma -f1", "b");
    e2e_assert!(l, "-i comma -f0,2", "a c");
    e2e_assert!(l, "-i comma -o comma -f0,2", "a,c");
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
    e2e_assert!(json_list, "-i json -o comma -f 0,1", "0,bb");
    e2e_assert!(json_list, "-i json -o csv -f 0,1", "0,1\n0,bb");
}

#[test]
fn custom_input() {
    let text = "[2019-04-10 03:01:20.001] Found x in y";
    e2e_assert!(text, r#"-i 'Found (?P<a>\w+) in (?P<b>\w+)' -f a,b"#, "x y");
    e2e_assert!(text, r#"-i 'Found (.*) in (.*)' -f 0"#, "Found x in y");
    e2e_assert!(text, r#"-i 'Found (.*) in (.*)' -f 1,2"#, "x y");
}

#[test]
fn custom_output() {
    let text = "a,b,c\n0,1,2";
    e2e_assert!(text, r#"-i csv -o '_{0}_{2}_'"#, "_0_2_");

    e2e_assert!(text, r#"-i csv -o '_{a}_{c}_'"#, "_0_2_");
}

#[test]
fn count_output() {
    let text = "a\nb\na";
    // space output
    e2e_assert!(text, r#"-f 0 -c"#, " 2  a\n 1  b");
    // custom output
    e2e_assert!(text, r#"-o '_{0}_' -c"#, " 2  _a_\n 1  _b_");
    // json output
    e2e_assert!(
        text,
        r#"-f 0 -o json -c"#,
        "{\"count\":2,\"0\":\"a\"}\n{\"count\":1,\"0\":\"b\"}"
    );
}

#[test]
fn count_csv_output() {
    let text = "a,b\n0,foo\n1,bar\n2,foo";
    e2e_assert!(text, "-i csv -f b -o csv -c", "count,b\n2,foo\n1,bar");
}

#[test]
fn grep() {
    let text = "a,b\n0,foo\n1,bar\n2,foo";
    e2e_assert!(text, "-i csv -f a -e b=foo", "0\n2");
    e2e_assert!(text, "-i csv -f a -e b=bar", "1");
    e2e_assert!(text, "-i csv -f a -e b=baz", "");
}

#[test]
fn assets_csv() {
    let text = include_str!("testdata/assets.csv");
    e2e_assert!(
        text,
        "-i csv -f kanzi",
        "000BE6\n720190\n720191\n720192\n720193"
    );
    e2e_assert!(
        text,
        "-i csv -e kanzi=720190 -f unique_chip_id_little_endian_hex",
        "2e00e30eac641c00"
    );
    e2e_assert!(
        text,
        "-i csv -e kanzi=720190 -f serial_number",
        "C039366011112305Z"
    );
    e2e_assert!(
        text,
        "-i csv -e kanzi=720190 -f mlb_serial_num",
        "C0303430007E1258J"
    );
}
