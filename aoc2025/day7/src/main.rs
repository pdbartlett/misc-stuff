use anyhow::{anyhow, bail, Context, Result};
use std::{env, fs, collections::VecDeque};

fn main() -> Result<()> {
  let args = env::args().collect::<Vec<_>>();
  if args.len() != 2 {
    bail!("Usage: {} <path>", args[0]);
  }
  let data = fs::read_to_string(&args[1])
      .context(format!("Failed to read from file '{}'", args[1]))?;
  let mut grid = data.lines().map(|x| x.bytes().collect::<Vec<_>>()).collect::<Vec<_>>();
  let mut beams = VecDeque::new();
  let src = grid[0].iter().position(|x| *x == b'S').ok_or(anyhow!("Could not find source!"))?;
  beams.push_back((0usize, src));
  let mut password1 = 0u32;
  loop {
    if beams.is_empty() { break; }
    let (mut r, c) = beams.pop_front().unwrap();
    loop {
      r += 1;
      if r == grid.len() { break; }
      match grid[r][c] {
        b'.' => grid[r][c] = b'|',
        b'|' => { break; },
        b'^' => {
          password1 += 1;
          if grid[r][c-1] == b'.' {
            grid[r][c-1] = b'|';
            beams.push_back((r, c-1));
          }
          if grid[r][c+1] == b'.' {
            grid[r][c+1] = b'|';
            beams.push_back((r, c+1));
          }
          break;
        }
        b => bail!("Unexpected element in grid: {}", b),
      }
    }
  }
  println!("Password is {}", password1);
  Ok(())
}