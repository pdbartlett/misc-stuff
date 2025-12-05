use anyhow::{anyhow, bail, Context, Result};
use predicates::prelude::*;
use std::{cmp, env, fmt, fs};

#[derive(Clone, Copy, Debug)]
struct Fresh {
  lo: u64,
  hi: u64,
}
impl Predicate<u64> for Fresh {
  fn eval(&self, x: &u64) -> bool {
    *x >= self.lo && *x <= self.hi
  }
}
impl predicates::reflection::PredicateReflection for Fresh {}
impl fmt::Display for Fresh {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "(lo={}, hi={})", self.lo, self.hi)
  }
}

fn main() -> Result<()> {
  let args: Vec<String> = env::args().collect();
  if args.len() != 2 {
    bail!("Usage: {} <path>", args[0]);
  }
  let data = fs::read_to_string(args[1].clone())
      .context(format!("Failed to read from file '{}'", args[1]))?;
  let mut lines = data.lines();
  let mut checks: Vec<Fresh> = vec!();
  loop {
    let line = lines.next().ok_or(anyhow!("Unexpected end of data"))?;
    if line.trim() == "" { break; }
    let (lo, hi) = line.split_once('-').ok_or(anyhow!("Invalid range: {}", line))?;
    let mut check = Fresh{lo: lo.parse()?, hi: hi.parse()?};
    checks.retain(|other| {
      if other.hi < check.lo-1 || other.lo > check.hi+1 {
        return true
      }
      check = Fresh{lo: cmp::min(check.lo, other.lo), hi: cmp::max(check.hi, other.hi)};
      false
    });
    checks.push(check)
  }
  let mut password1 = 0u64;
  for line in lines {
    let x = line.parse::<u64>()?;
    if checks.iter().any(|&f| f.eval(&x)) { password1 += 1; }
  }
  let mut password2 = 0u64;
  for check in checks {
    password2 += check.hi - check.lo + 1;
  }
  println!("Passwords are {} and {}", password1, password2);
  Ok(())
}
