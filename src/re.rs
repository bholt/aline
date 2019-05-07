#[cfg(test)]
mod tests {
    use regex::Regex;

    /// Test to see if we can make Regex do the parsing into a Captures struct (which has numbered and named groups) and then use the captures group to generate our output.
    #[test]
    fn re() {
        let line = "field0 field1 field2";

        let re = Regex::new(r"\s*(?:(\S+)\s+)+(\S+)?").unwrap();
        println!("{:?}", re.captures(line));

        println!("---");

        let resplit = Regex::new(r"\s+").unwrap();
        println!("{:?}", resplit.split(line).collect::<Vec<&str>>());

        assert!(false)
    }
}
