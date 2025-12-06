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
  let mut wheel: i32 = 50;
  let mut password1: u32 = 0;
  let mut password2: u32 = 0;
  for line in data.lines() {
    let (direction, amount) = line.split_at(1);
    let num: i32 = amount.parse()
        .context(format!("Failed to parse '{}' as numeric", amount))?;
    let mult: i32 = if direction == "L" { -1 } else { 1 };
    for _ in 0..num {
      wheel = (wheel + mult) % 100;
      if wheel == 0 { password2 += 1; }
    }
    if wheel == 0 { password1 += 1; }
  }
  println!("Passwords are {} and {}", password1, password2);
  Ok(())
}
