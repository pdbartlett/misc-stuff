extern crate itertools;
use itertools::Itertools;

use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::path::Path;

use Hand::*;

#[derive(PartialEq, PartialOrd, Eq, Ord, Clone, Copy, Debug)]
struct Rank(u8);

fn make_rank(ch: char) -> Rank {
    Rank(match ch {
        '2'...'9' => (ch as u8) - ('0' as u8),
        'T' => 10,
        'J' => 11,
        'Q' => 12,
        'K' => 13,
        'A' => 14,
        _   => panic!(format!("Unexpected rank: {:?}", ch)),
    })
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Clone, Copy, Debug)]
struct Card(Rank, char);

fn make_card(s: &str) -> Card {
    Card(make_rank(s.chars().nth(0).unwrap()), s.chars().nth(1).unwrap())
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Debug)]
enum Hand {
    High{x: Rank, o1: Rank, o2: Rank, o3: Rank, o4: Rank},
    Pair{x: Rank, o1: Rank, o2: Rank, o3: Rank},
    TwoPair{x1: Rank, x2: Rank, o: Rank},
    Three{x: Rank, o1: Rank, o2: Rank},
    Straight{x: Rank},
    Flush{x1: Rank, x2: Rank, x3: Rank, x4: Rank, x5: Rank},
    FullHouse{x1: Rank, x2: Rank},
    Four{x: Rank, o: Rank},
    StraightFlush{x: Rank},
}

fn assess_hand(cards: &mut std::str::SplitWhitespace) -> Hand {
    let sorted = cards.take(5).map(make_card).sorted().rev().collect::<Vec<_>>();
    match maybe_straight(&sorted) {
        Some(rank) => if is_flush(&sorted) {
            return StraightFlush{x: rank};
        } else {
            return Straight{x: rank};
        },
        _ => (),
    }
    if is_flush(&sorted) {
        return Flush{x1: sorted[0].0, x2: sorted[1].0, x3: sorted[2].0, x4: sorted[3].0, x5: sorted[4].0};
    }
    if find_multi(&sorted, 4, 0) == 0 {
        return Four{x: sorted[0].0, o: sorted[4].0};
    }
    if find_multi(&sorted, 4, 0) == 1 {
        return Four{x: sorted[1].0, o: sorted[0].0};
    }
    match find_multi(&sorted, 3, 0) {
        1 => return Three{x: sorted[1].0, o1: sorted[0].0, o2: sorted[4].0},
        0 => if sorted[3].0 == sorted[4].0 {
                return FullHouse{x1: sorted[0].0, x2: sorted[3].0};
            } else {
                return Three{x: sorted[0].0, o1: sorted[3].0, o2: sorted[4].0};
            },
        2 => if sorted[0].0 == sorted[1].0 {
                return FullHouse{x1: sorted[2].0, x2: sorted[0].0};
            } else {
                return Three{x: sorted[2].0, o1: sorted[0].0, o2: sorted[1].0};
            },
        _ => (),
    };
    match find_multi(&sorted, 2, 0) {
        0 => match find_multi(&sorted, 2, 2) {
            2 => TwoPair{x1: sorted[0].0, x2: sorted[2].0, o: sorted[4].0},
            3 => TwoPair{x1: sorted[0].0, x2: sorted[3].0, o: sorted[2].0},
            _ => Pair{x: sorted[0].0, o1: sorted[2].0, o2: sorted[3].0, o3: sorted[4].0},
        },
        1 => match find_multi(&sorted, 2, 3) {
            3 => TwoPair{x1: sorted[1].0, x2: sorted[3].0, o: sorted[2].0},
            _ => Pair{x: sorted[1].0, o1: sorted[0].0, o2: sorted[3].0, o3: sorted[4].0},
        },
        2 => Pair{x: sorted[2].0, o1: sorted[0].0, o2: sorted[1].0, o3: sorted[4].0},
        3 => Pair{x: sorted[3].0, o1: sorted[0].0, o2: sorted[1].0, o3: sorted[2].0},
        _ => High{x: sorted[0].0, o1: sorted[1].0, o2: sorted[2].0, o3: sorted[3].0, o4: sorted[4].0},
    }
}

fn maybe_straight(cards: &Vec<Card>) -> Option<Rank> {
    if (cards[0].0).0 == 14
    && (cards[1].0).0 == 5
    && (cards[2].0).0 == 4
    && (cards[3].0).0 == 3
    && (cards[4].0).0 == 2 {
        return Some(Rank(5));
    }
    for i in 1..5 {
        if (cards[i-1].0).0 - (cards[i].0).0 != 1 { return None; }
    }
    Some(cards[0].0)
}

fn is_flush(cards: &Vec<Card>) -> bool {
    for i in 1..5 {
        if cards[i].1 != cards[0].1 { return false; }
    }
    true
}

fn find_multi(cards: &Vec<Card>, n: usize, s: usize) -> usize {
    for i in s..=(5-n) {
        if cards[i].0 == cards[i+n-1].0 { return i; }
    }
    999
}

fn main() {
    let path = Path::new("p054_poker.txt");
    let file = File::open(&path).unwrap();
    let lines = BufReader::new(file).lines();
    let mut c = 0;
    for line in lines {
        if let Ok(data) = line {
            let mut cards = data.split_whitespace();
            let hand1 = assess_hand(&mut cards);
            let hand2 = assess_hand(&mut cards);
            let p1 = hand1 > hand2;
            println!("{:?}: {:?} => {:?} and {:?}", if p1 {"P1"} else {"P2"}, data, hand1, hand2);
            if p1 { c += 1; }
        }
    }
    eprintln!("Count: {}", c);
}
