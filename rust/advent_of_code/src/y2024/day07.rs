#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use std::slice::Iter;

use itertools::Itertools;
use recursive::recursive;

fn parse_input(input: &str) -> Vec<(u64, Vec<u64>)> {
    let parsed = input
        .lines()
        .map(|l| l.split_once(": ").unwrap())
        .map(|(left, right)| (
            left.parse::<u64>().unwrap(), 
            right.split_whitespace().map(|i| i.parse::<u64>().unwrap()).collect_vec()))
        .collect_vec();

    parsed
}

#[recursive]
fn can_be_true(expected_result: &u64, numbers: &[u64], result: u64, with_concat: bool) -> bool {
    // Since we're always increasing the number, we can reject any value when it gets too high
    if result > *expected_result {
        return false;
    }

    if result == *expected_result && numbers.len() == 0 {
        return true;
    } else if numbers.len() == 0 { // If we're not at the right value in the end, reject
        return false;
    }

    if let Some((head, tail)) = numbers.split_first() {
        let plus_result = can_be_true(expected_result, tail, result + head, with_concat);
        if plus_result {
            return true;
        }

        let multiply_result = can_be_true(expected_result, tail, result * head, with_concat);

        if multiply_result {
            return true;
        }

        if !with_concat {
            return false;
        }

        // Probably quite slow, but easiest way I could think of to concat two numbers
        let concat_result = format!("{}{}", result, head).parse().unwrap();

        let multiply_result = can_be_true(expected_result, tail, concat_result, with_concat);

        return multiply_result;
    }
    
    return false;
}

pub fn part_one(input: &Vec<(u64, Vec<u64>)>) -> u64 {
    let filtered = input.iter()
        .filter(|(n, l)| can_be_true(n, l.split_first().unwrap().1, l[0], false))
        .collect_vec();

    filtered.iter().map(|(n, _)| n)
        .sum()
}

pub fn part_two(input: &Vec<(u64, Vec<u64>)>) -> u64 {
    let filtered = input.iter()
        .filter(|(n, l)| can_be_true(n, l.split_first().unwrap().1, l[0], true))
        .collect_vec();        

    filtered.iter().map(|(n, _)| n)
        .sum()
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
    let input = get_puzzle_input(2024, 7).await.unwrap();
    let (part1, part2) = execute (&input);

    assert_eq!("2941973819040", part1);
    assert_eq!("249943041417600", part2);
}
