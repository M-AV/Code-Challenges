#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use std::collections::{HashMap, HashSet};

use itertools::Itertools;
use recursive::recursive;

fn parse_input(input: &str) -> (Vec<&str>, Vec<&str>) {
    let (first, second) = input.split_once("\n\n").unwrap();

    let towel_patterns = first.split(", ").collect_vec();
    let designs = second.lines().collect_vec();

    (towel_patterns, designs)
}

/// It wasn't feasable to just brute force our way through. So as a quick improvement, we keep track
/// of known solutions and check that every time. A lot of smaller solutions are recurring.
#[recursive]
fn is_possible(design: &str, patterns: &Vec<&str>, known_solutions: &mut HashMap<String, bool>) -> bool {
    if design.len() == 0 {
        return true;
    }

    let s = known_solutions.get(design);
    if s.is_some() {
        return *s.unwrap();
    }

    for pattern in patterns {
        if design.starts_with(pattern) {
            let remaining = &design[pattern.len()..];
            if remaining.len() == 0 {
                known_solutions.insert(design.to_string(), true);
                return true;
            }
            if is_possible(remaining, patterns, known_solutions) {
                known_solutions.insert(design.to_string(), true);
                return true;
            }
        }
    }

    known_solutions.insert(design.to_string(), false);
    false
}



pub fn part_one((patterns, designs): &(Vec<&str>, Vec<&str>)) -> i32 {
    let mut combinations = 0;
    let mut known_solutoins = HashMap::<String, bool>::new();

    for design in designs {
        if is_possible(design, patterns, &mut known_solutoins) {
            combinations += 1;
        }
    }
    
    combinations
}

/// Same idea as before: Recursively solve "sub parts" of the string and count how many combinations.
/// Here we have to check every possible start value and add them up, to get all possibilities.
#[recursive]
fn count_combinations(design: &str, patterns: &Vec<&str>, known_solutions: &mut HashMap<String, i64>) -> i64 {
    let mut combinations: i64 = 0;

    if design.len() == 0 {
        return combinations;
    }

    // If we know the solution already, just return that
    let s = known_solutions.get(design);
    if s.is_some() {
        return *s.unwrap();
    }

    for pattern in patterns {
        // If the design start with this pattern, this is a pattern we can use now.
        if design.starts_with(pattern) {
            let remaining = &design[pattern.len()..];
            // If there is no remaining, we found 1 way to arrange the design.
            if remaining.len() == 0 {
                combinations += 1;
                continue;
            }

            let sub_combinations = count_combinations(remaining, patterns, known_solutions);
            if sub_combinations > 0 {
                combinations += sub_combinations;
            }
        }
    }

    // Before returning, we persist the solution for this design.
    known_solutions.insert(design.to_string(), combinations);
    combinations
}

pub fn part_two((patterns, designs): &(Vec<&str>, Vec<&str>)) -> i64 {
    let mut combinations = 0;
    let mut known_solutions = HashMap::<String, i64>::new();

    let mut is_possible_solutions = HashMap::<String, bool>::new();

    for design in designs {
        if !is_possible(design, patterns, &mut is_possible_solutions) {
            continue;
        }

        combinations += count_combinations(design, patterns, &mut known_solutions);
    }
    
    combinations
}

pub fn execute(input: &str) -> (String, String) {
    let parsed = parse_input(input);

    // println!("{:?}", &parsed);

    let part1 = part_one(&parsed);
    let part2 = part_two(&parsed);

    (part1.to_string(), part2.to_string())
}

#[tokio::test]
async fn test_day() {
    use crate::input_provider::get_puzzle_input;
    let input = get_puzzle_input(2024, 19).await.unwrap();
    let (part1, part2) = execute (&input);

    assert_eq!("290", part1);
    assert_eq!("712058625427487", part2);
}
