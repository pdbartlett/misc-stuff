use anyhow::{bail, Context, Result};
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
    let mut s = line.to_string();
    let to_drop = line.len() - 12;
    for _ in 1..=to_drop {
      let n = s.len();
      let mut hi = s[1..].to_string();
      for i in 1..n {
        let dropi= format!("{}{}", s[..i].to_string(), s[i+1..].to_string());
        if dropi > hi { hi = dropi; }
      }
      s = hi;
    }
    let n: u64 = s.parse().context(format!("Failed to parse {} as number", s))?;
    password += n;
  }
  println!("Password is {}", password);
  Ok(())
}
