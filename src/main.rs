use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "aline",
    author = "Brandon Holt",
    version = "0.1.0",
    about = "Replacement for awk/cut/sed. Never again struggle to remember which one does what you want or splits fields how you want, just switch to aline for all your line-parsing needs"
)]
struct Config {
    /// Delimiter to use for separating
    #[structopt(short = "d", long = "delimiter", default_value = "\\s+")]
    delimiter: String,

    #[structopt(short = "v", long = "verbose")]
    verbose: bool,
}

fn main() {
    let config = Config::from_args();
    println!("{:#?}", config);
}
