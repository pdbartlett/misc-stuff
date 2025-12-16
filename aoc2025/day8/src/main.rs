use anyhow::{anyhow, bail, Context, Result};
use binary_heap_plus::BinaryHeap;
use std::{cmp::Ordering, collections::{HashMap, HashSet}, env, fs, rc::Rc};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
struct Location(i32, i32, i32);
impl Location {
  fn distance_to(&self, other: &Self) -> f64 {
    f64::sqrt(
      ((self.0 - other.0) as f64).powi(2) +
      ((self.1 - other.1) as f64).powi(2) +
      ((self.2 - other.2) as f64).powi(2))
  }
}

#[derive(Debug)]
struct LocPair(Location, Location);
impl LocPair {
  fn length(&self) -> f64 {
    self.0.distance_to(&self.1)
  }
}

struct Circuit(HashSet<Location>);

fn main() -> Result<()> {
  let args = env::args().collect::<Vec<_>>();
  if args.len() != 3 {
    bail!("Usage: {} <path> <num-connections>", args[0]);
  }
  let num_conns = args[2].parse::<usize>()?;
  let data = fs::read_to_string(&args[1])
      .context(format!("Failed to read from file '{}'", args[1]))?;
  let mut locations = Vec::new();
  for line in data.lines() {
    let parts = line.split_terminator(',').collect::<Vec<_>>();
    if parts.len() != 3 { bail!("Expected 3 values per line; got {} in {}", parts.len(), line)}
    locations.push(Location(parts[0].parse::<i32>()?, parts[1].parse::<i32>()?, parts[2].parse::<i32>()?));
  }
  let mut nearest = BinaryHeap::new_by(|a: &LocPair, b: &LocPair| {
    match (a.length() - b.length()).signum() {
      0.0 => Ordering::Equal,
      1.0 => Ordering::Greater,
      _ => Ordering::Less,
    }
  });
  let n = locations.len();
  for i in 0..n {
    for j in i+1..n {
      nearest.push(LocPair(locations[i], locations[j]));
      while nearest.len() > num_conns {
        let _ = nearest.pop().ok_or(anyhow!("Heap should not be empty"));
      }
    }
  }
  let mut circuit_map: HashMap<Location, Rc<Circuit>> = HashMap::new();
  for pair in nearest.into_sorted_vec().iter().rev() {
    let mut c = Circuit(HashSet::new());
    if let Some(c0) = circuit_map.get(&pair.0) {
      for loc in c0.0.iter() {
        c.0.insert(*loc);
      }
    } else {
      c.0.insert(pair.0);
    }
    if let Some(c1) = circuit_map.get(&pair.1) {
      for loc in c1.0.iter() {
        c.0.insert(*loc);
      }
    } else {
      c.0.insert(pair.1);
    }
    let crc = Rc::new(c);
    for loc in crc.0.iter() {
      circuit_map.insert(*loc, crc.clone());
    }
  }
  let mut locations = HashSet::new();
  let mut longest = BinaryHeap::new_min();
  for v in circuit_map.values() {
    let mut is_new = true;
    for loc in v.0.iter() {
      if !locations.insert(loc) {
        is_new = false;
        break;
      }
    }
    if is_new {
      longest.push(v.0.len() as u32);
      if longest.len() > 3 {
        let _ = longest.pop().ok_or(anyhow!("Heap should not be empty"));
      }
    }
  }
  let mut password = 1u128;
  for n in longest {
    password *= n as u128;
  }
  println!("Password is {}", password);
  Ok(())
}
