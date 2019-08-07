extern crate regex;
use regex::Regex;

fn main() {
    let re = Regex::new(r"^1\d2\d3\d4\d5\d6\d7\d8\d9\d0$").unwrap();
    let min_square = 1020304050607080900_i64;
    let mut i = (((min_square as f64).sqrt() / 10.0).floor() as i64) * 10;
    loop {
        let s = (i * i).to_string();
        if re.is_match(&s) {
            println!("{} => {}", i, s);
            break;
        }
        i += 10;
    }
}
