use anyhow::{anyhow, bail, Context, Result};
use std::{env, fs};

fn main() -> Result<()> {
  let args = env::args().collect::<Vec<_>>();
  if args.len() != 2 {
    bail!("Usage: {} <path>", args[0]);
  }
  let data = fs::read_to_string(&args[1])
      .context(format!("Failed to read from file '{}'", args[1]))?;
  let mut lines = data.lines().map(|s| s.to_string()).collect::<Vec<String>>();
  let n = lines.iter().map(|s| s.len()).max().unwrap_or_default();
  for line in lines.iter_mut() {
    let n1 = line.len();
    if n1 < n { *line = format!("{}{}", *line, " ".repeat(n - n1)); }
  }
  let m = lines.len() - 1;
  let mut nums: Vec<Vec<u128>> = vec!();
  let mut v: Vec<u128> = vec!();
  for i in (0..n).rev() {
    let bytes = (0..m).map(|j| lines[j].as_bytes()[i]).filter(|b| *b != b' ').collect::<Vec<_>>();
    match bytes.len() {
      0 => { nums.push(v); v = vec!(); },
      _ => { v.push(String::from_utf8(bytes)?.parse()?); },
    }
  }
  nums.push(v);
  let mut results: Vec<u128> = vec!();
  for (i, cell) in lines[lines.len()-1].split_ascii_whitespace().rev().enumerate() {
    results.push(match cell {
        "+" => nums[i].iter().sum(),
        "*" => nums[i].iter().product(),
        op => return Err(anyhow!("Unexpected operator: {}", op)),
    })
  }
  println!("Password is {}", results.iter().sum::<u128>());
  Ok(())
}
