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
    let mut s = line.to_string();
    let to_drop = line.len() - 12;
    for _ in 1..=to_drop {
      let n = s.len();
      let mut hi = s[1..].to_string();
      for i in 1..n {
        let dropi = format!("{}{}", &s[..i], &s[i+1..]);
        if dropi > hi { hi = dropi; }
      }
      s = hi;
    }
    password += s.parse::<u64>()?;
  }
  println!("Password is {}", password);
  Ok(())
}
