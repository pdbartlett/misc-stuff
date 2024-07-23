use std::env;
use std::io;
use std::fs::File;
use std::io::Read;

use serde_json::Value;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
      return Err(Box::from("Usage: jsontpl json-data-file upon-template-file"));
    }

    // Load JSON data
    let mut json_str = String::new();
    let mut json_file = File::open(&args[1])?;
    json_file.read_to_string(&mut json_str)?;
    let data: Value = serde_json::from_str(&json_str)?;

    // Load template
    let mut tpl_str = String::new();
    let mut tpl_file = File::open(&args[2])?;
    tpl_file.read_to_string(&mut tpl_str)?;

    // Render template with JSON data
    let mut engine = upon::Engine::new();
    engine.add_template("template", tpl_str)?;
    let mut stdout = io::BufWriter::new(io::stdout());
    engine.template("template").render(&data).to_writer(&mut stdout)?;

    Ok(())
}

