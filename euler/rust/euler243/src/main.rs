const N: usize = 100;
const T: f64 = 15499.0/94744.0;

fn sieve(n: usize) -> Vec<usize> {
    let mut is_prime = Vec::with_capacity(n+1);
    is_prime.push(false); // 0
    is_prime.push(false); // 1
    for _i in 1..n { is_prime.push(true); /* for now */ }
    for i in 2..=(n/2) {
        if is_prime[i] {
            let mut j = i * 2;
            while j <= n {
              is_prime[j] = false;
              j += i;
            }
        }
    }
    is_prime.iter().enumerate().filter(|(_, b)| **b).map(|(c, _)| c).collect::<Vec<usize>>()
}

fn main() {
  let primes = sieve(N);
  eprintln!("Calculated {} prime(s) up to {}", primes.len(), N);
  let mut n = 1;
  'denom: for i in &primes {
      n *= i;
      eprintln!("Trying n={}...", n);
      let mut c = 0;
      'frac: for j in 1..n {
          for p in &primes {
              if *p > i/2 { break; }
              if j % p == 0 && n % p == 0 { continue 'frac; }
          }
          c += 1;
          let ratio = (c as f64) / ((n - 1) as f64);
          if ratio > T { continue 'denom; }
      }
      let ratio = (c as f64) / ((i - 1) as f64);
      if ratio < T {
          eprintln!("{}: {} / {} == {}", i, c, i - 1, ratio);
          break;
      }
  }
}
