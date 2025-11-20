use clap::Parser;

use tornapi::add;

/// Simple tool to calculate recent chain stats via Torn API.
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Flags {
    /// Date/time from which to process.
    #[arg(short, long)]    
    since: String,
}

fn main() {
    let flags = Flags::parse();
    println!("Working from {}", flags.since);
    println!("Answer is {}", add(17, 25));
}