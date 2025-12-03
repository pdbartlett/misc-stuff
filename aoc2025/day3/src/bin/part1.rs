use anyhow::{anyhow, bail, Context, Result};
use std::env;
use std::fs;

fn main() -> Result<()> {
  let args: Vec<String> = env::args().collect();
  if args.len() != 2 {
    bail!("Usage: {} <path>", args[0]);
  }
  let data = fs::read_to_string(args[1].clone())
      .context(format!("Failed to read from file '{}'", args[1]))?;
  let mut password: u64 = 0;
  for line in data.trim().lines() {
    let mut first: char = '0';
    let n = line.len();
    for c in line.chars().take(n-1) {
      if c > first { first = c; }
      if c == '9' { break; }
    }
    let mut second: char = '0';
    let (_, rest) = line.split_once(first)
        .ok_or(anyhow!("This shouldn't happen!"))?;
    for c in rest.chars() {
      if c > second { second = c; }
      if c == '9' { break; }
    }
    let jolts = 10u8 * (first as u8 - '0' as u8) + (second as u8 - '0' as u8);
    password += jolts as u64;
  }
  println!("Password is {}", password);
  Ok(())
}
