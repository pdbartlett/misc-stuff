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
  let mut password1: u64 = 0;
  let mut password2: u64 = 0;
  for range in data.trim().split_terminator(',') {
    if let Some((lower, upper)) = range.split_once('-') {
      let start: u64 = lower.parse().context(format!("Failed to parse {} as number", lower))?;
      let end: u64 = upper.parse().context(format!("Failed to parse {} as number", upper))?;
      for n in start..=end {
        let s = n.to_string();
        let c = s.len();
        if c % 2 == 0 {
          let (lhs, rhs) = s.split_at(c/2);
          if lhs == rhs { password1 += n; }
        }
        for i in 1..=c/2 {
          if c % i == 0 {
            let part = &s[..i];
            if s == part.repeat(c/i) { password2 += n; break; }
          }
        }
      }
    }
  }
  println!("Passwords are {} and {}", password1, password2);
  Ok(())
}
