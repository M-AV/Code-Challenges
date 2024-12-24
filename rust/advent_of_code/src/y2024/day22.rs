#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use itertools::{all, Itertools};

fn parse_input(input: &str) -> Vec<i64> {
    input.lines().map(|l| l.parse().unwrap()).collect_vec()
}

fn mix(first: i64, second: i64) -> i64 {
    first ^ second
}

fn calculate_secret_number(initial: i64) -> i64 {
    let mut result = initial * 64;
    result = initial ^ result;
    result = result % 16777216;

    let mut temp = result / 32;
    result = result ^ temp;
    result = result % 16777216;

    temp = result * 2048;
    result = result ^ temp;
    result = result % 16777216;

    result
}

pub fn part_one(input: &Vec<i64>) -> i64 {
    input.iter()
        .map(|l| {
            let mut number = *l;
            for i in 0 .. 2000 {
                number = calculate_secret_number(number);
            }
            number
        })
        .sum()
}

pub fn part_two(input: &Vec<i64>) -> i64 {

    let mut all_prices_diffs = vec![];
    for buyer in input {
        let mut prices = vec![];
        let mut diffs = vec![];
        let mut number = *buyer;
        
        let mut previous = buyer % 10;

        for i in 0 .. 2000 {
            number = calculate_secret_number(number);
            diffs.push(number % 10 - previous);
            prices.push(number % 10);

            previous = number % 10;
        }

        all_prices_diffs.push((prices, diffs));
    }


    let mut max_bananas = 0;
    for sequence in all_prices_diffs[0].1.windows(4) {
        let mut bananas = 0;

        for all_prices_idx in 0 .. all_prices_diffs.len() {
            let (prices, diffs) = &all_prices_diffs[all_prices_idx];
            let idx = diffs.windows(4).position(|window| window == sequence);
            if idx.is_some() {
                bananas += prices[idx.unwrap() + 3];
            }
        }

        if max_bananas < bananas {
            max_bananas = bananas;
        }
    }

    max_bananas
}

pub fn execute(input: &str) -> (String, String) {
    let parsed= parse_input(input);

    let part1 = part_one(&parsed);
    let part2 = part_two(&parsed);

    (part1.to_string(), part2.to_string())
}

// Takes ~177s (part 2)
//#[tokio::test]
async fn test_day() {
    use crate::input_provider::get_puzzle_input;
    let input = get_puzzle_input(2024, 22).await.unwrap();
    let (part1, part2) = execute (&input);

    assert_eq!("13022553808", part1);
    assert_eq!("1555", part2);
}
