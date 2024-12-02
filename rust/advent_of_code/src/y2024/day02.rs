#![allow(dead_code)]
#![allow(unused_variables)]

use itertools::Itertools;

fn parse_input(input: &str) -> Vec<Vec<i32>> {
    let result = input
        .lines()
        .map(|l| l.split(' ').collect::<Vec<&str>>())
        .map(|l| {
            l.iter().map(|n| n.parse::<i32>().unwrap()).collect::<Vec<i32>>()
        })
        .collect::<Vec<Vec<i32>>>();
    result
}

fn is_safe_line(line: &Vec<i32>) -> bool {
    let is_increasing = line.iter().is_sorted();
    let is_decreasing = line.iter().is_sorted_by(|a,b| a >= b);

    if !is_increasing && !is_decreasing {
        return false;
    }

    let differ_by_one_to_three = line.iter().tuple_windows().all(|(a, b)| {
        let diff = (a - b).abs();
        return 1 <= diff && diff <= 3;
    });

    differ_by_one_to_three
}

fn is_safe_line_part2(line: &Vec<i32>) -> bool {
    if is_safe_line(line) {
        return true;
    }

    let length = line.len();

    for i in 0..length {
        let first = &line[0..i];
        let last = &line[i+1..];

        let mut idx_removed = first.to_vec();
        idx_removed.extend(last.to_vec());

        if is_safe_line(&idx_removed) {
            return true;
        }
    }

    false
}

/// Part 1 rules: 
/// - The levels are either all increasing or all decreasing.
/// - Any two adjacent levels differ by at least one and at most three.
pub fn part_one(input: &Vec<Vec<i32>>) -> i32 {
    input.iter().filter(|l| is_safe_line(l)).count() as i32
}

pub fn part_two(input: &Vec<Vec<i32>>) -> i32 {
    input.iter().filter(|l| is_safe_line_part2(l)).count() as i32
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
    let input = get_puzzle_input(2024, 02).await.unwrap();
    let (part1, part2) = execute (&input);

    assert_eq!("246", part1);
    assert_eq!("318", part2);
}
