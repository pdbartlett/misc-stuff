use anyhow::{bail, Context, Result};
use std::env;
use std::fs;

fn main() -> Result<()> {
  let args = env::args().collect::<Vec<_>>();
  if args.len() != 2 {
    bail!("Usage: {} <path>", args[0]);
  }
  let data = fs::read_to_string(&args[1])
      .context(format!("Failed to read from file '{}'", args[1]))?;
  let mut password: u64 = 0;
  for line in data.trim().lines() {
    let mut first = b'0';
    let n = line.len();
    for c in line.bytes().take(n-1) {
      if c > first { first = c; }
      if c == b'9' { break; }
    }
    let rest = line.bytes().skip_while(|&c| c != first);
    let mut second = b'0';
    for c in rest {
      if c > second { second = c; }
      if c == b'9' { break; }
    } 
    let jolts = 10u8 * (first - b'0') + second - b'0';
    password += jolts as u64;
  }
  println!("Password is {}", password);
  Ok(())
}
