use std::collections::HashMap;
use std::collections::HashSet;

fn main() {
    let mut facts = HashMap::new();
    for i in 0..=9 {
        facts.insert(i.to_string().chars().nth(0).unwrap(), fact(i));
    }
    let mut c = 0;
    for i in 0..1_000_000 {
        let mut seen = HashSet::new();
        let mut n = i;
        loop {
            seen.insert(n);
            let mut sum = 0;
            for j in n.to_string().chars() {
                sum += facts[&j];
            }
            if seen.contains(&sum) { break; }
            n = sum;
        }
        if seen.len() == 60 { c += 1; }
    }
    println!("{}", c);
}

fn fact(n: u32) -> u32 {
    if n < 2 { return 1; }
    n * fact(n - 1)
}
