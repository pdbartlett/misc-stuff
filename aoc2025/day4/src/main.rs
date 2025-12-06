use anyhow::{bail, Context, Result};
use num_integer::div_rem;
use std::env;
use std::fs;

fn main() -> Result<()> {
  let args = env::args().collect::<Vec<_>>();
  if args.len() != 2 {
    bail!("Usage: {} <path>", args[0]);
  }
  let data = fs::read_to_string(&args[1])
      .context(format!("Failed to read from file '{}'", args[1]))?;
  let mut grid = data.lines().map(|x| x.bytes().collect::<Vec<_>>()).collect::<Vec<_>>();
  let rows: i32 = grid.len().try_into()?;
  let cols: i32 = grid[0].len().try_into()?;
  let neighbours = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)];
  let mut password1 = 0u64;
  let mut password2 = 0u64;
  loop {
    let mut remove = 0u64;
    for i in 0..rows*cols {
      let (r, c) = div_rem(i, cols);
      if grid[r as usize][c as usize] != b'@' { continue; }
      let n: u8 = neighbours.map(|(roff, coff)| {
          let rn = r + roff;
          let cn = c + coff;
          if rn >= 0 && rn < rows && cn >= 0 && cn < cols && grid[rn as usize][cn as usize] != b'.' { 1 } else { 0 }
      }).iter().sum();
      if n < 4 {
        grid[r as usize][c as usize] = b'x';
        remove += 1;
      }
    }
    if remove == 0 { break; }
    if password1 == 0 { password1 += remove; }
    password2 += remove;
    for row in grid.iter_mut() {
      for cell in row.iter_mut() {
        if *cell == b'x' { *cell = b'.'; }
      }
    }
  }
  println!("Passwords are {} and {}", password1, password2);
  Ok(())
}
