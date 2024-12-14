#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use itertools::Itertools;

use crate::utils;

fn parse_input(input: &str) -> Vec<((i32,i32), (i32,i32), (i32,i32))> {
    let c = input
        .lines()
        .filter(|l| l.trim() != "")
        .map(|l| l.split_once(": ").unwrap().1)
        .map(|l| l.split_once(", ").unwrap())
        .map(|(l,r)| (l[2..].parse::<i32>().unwrap(), r[2..].parse::<i32>().unwrap()))
        .collect_vec()
        .chunks(3)
        .map(|c| (c[0], c[1], c[2]))
        .collect_vec();
    c
}

pub fn part_one(input: &Vec<((i32,i32), (i32,i32), (i32,i32))>) -> i32 {
    let mut price = 0;
    'outer: for (a,b,t) in input {
        let (x, y) = t;
        let x_reachable = x % utils::gcd(a.0, b.0) == 0;
        let y_reachable = y % utils::gcd(a.1, b.1) == 0;

        if !x_reachable || !y_reachable {
            continue 'outer;
        }

        for b_counter in (0 ..=100).rev() {
            let x_calc = x - b_counter * b.0;
            let y_calc = y - b_counter * b.1;
            let a_counter = x_calc / a.0;
            if x_calc < 0 || y_calc < 0 || x_calc % a.0 != 0 || y_calc % a.1 != 0 || a_counter > 100 || a_counter != y_calc / a.1 {
                continue;
            }

            // println!("{}x{:?} + {}x{:?} = {:?} ({} & {})", a_counter, a, b_counter, b, t, a_counter * a.0 + b_counter * b.0, a_counter * a.1 + b_counter * b.1);

            price += a_counter * 3 + b_counter;
            break;
        }
    }

    price
}

pub fn part_two(input: &Vec<((i32,i32), (i32,i32), (i32,i32))>) -> i128 {
    let mut price = 0;
    let input = input.iter().map(|(a,b,t)| {
        ((a.0 as i128, a.1 as i128), (b.0 as i128, b.1 as i128), (t.0 as i128 + 10000000000000, t.1 as i128 + 10000000000000))
    }).collect_vec();
    'outer: for (a,b,t) in input {

        let (x, y) = t;
        let x_reachable = x % utils::gcd(a.0, b.0) == 0;
        let y_reachable = y % utils::gcd(a.1, b.1) == 0;

        if !x_reachable || !y_reachable {
            continue 'outer;
        }

        // Cramers rule
        let det = a.0 * b.1 - a.1 * b.0;

        if det == 0 {
            panic!("Oh no!");
        }

        let a_ = (x * b.1 - y * b.0) / det;
        let b_ = (a.0 * y - a.1 * x) / det;

        if (a.0 * a_ + b.0 * b_, a.1 * a_ + b.1 * b_) == (x, y) {
            price += a_ * 3 + b_
        }
    }

    price
}

pub fn execute(input: &str) -> (String, String) {
    let parsed = parse_input(input);

    let part1 = part_one(&parsed);
    let part2 = part_two(&parsed);

    (part1.to_string(), part2.to_string())
}

#[tokio::test]
async fn test_day() {
    use crate::input_provider::get_puzzle_input;
    let input = get_puzzle_input(2024, 13).await.unwrap();
    let (part1, part2) = execute (&input);

    assert_eq!("29201", part1);
    assert_eq!("104140871044942", part2);
}
