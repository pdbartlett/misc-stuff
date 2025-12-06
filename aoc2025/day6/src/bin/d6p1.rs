use anyhow::{anyhow, bail, Context, Result};
use std::{env, fs};

fn main() -> Result<()> {
  let args: Vec<String> = env::args().collect();
  if args.len() != 2 {
    bail!("Usage: {} <path>", args[0]);
  }
  let data = fs::read_to_string(args[1].clone())
      .context(format!("Failed to read from file '{}'", args[1]))?;
  let lines = data.lines().collect::<Vec<_>>();
  let mut nums: Vec<Vec<u128>> = vec!();
  for cell in lines[0].split_ascii_whitespace() {
    nums.push(vec!(cell.parse()?));
  }
  for i in 1..lines.len()-1 {
    for (j, cell) in lines[i].split_ascii_whitespace().enumerate() {
      nums[j].push(cell.parse()?);
    }
  }
  let mut results: Vec<u128> = vec!();
  for (i, cell) in lines[lines.len()-1].split_ascii_whitespace().enumerate() {
    results.push(match cell {
        "+" => nums[i].iter().sum(),
        "*" => nums[i].iter().product(),
        op@_ => return Err(anyhow!("Unexpected operator: {}", op)),
    })
  }
  println!("Password is {}", results.iter().sum::<u128>());
  Ok(())
}
