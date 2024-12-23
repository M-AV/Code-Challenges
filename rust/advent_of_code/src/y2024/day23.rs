#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use std::collections::{HashMap, HashSet};

use itertools::Itertools;

fn parse_input(input: &str) -> HashMap<&str, Vec<&str>> {
    let mut map = HashMap::new();

    for (left ,right) in input.lines()
        .map(|l| l.split_once('-').unwrap()) {
            let left_entry = map.entry(left).or_insert(vec![]);
            left_entry.push(right);

            let right_entry = map.entry(right).or_insert(vec![]);
            right_entry.push(left);
    }
        
    map
}

pub fn part_one(input: &HashMap<&str, Vec<&str>>) -> usize {
    let mut used = HashSet::new();

    let mut cycles = vec![];

    input.keys().for_each(|k| {
        if used.contains(k) {
            return;
        }

        let mut used_for_this_cycle = HashSet::new();

        for first_level in input[k].iter() {
            if used.contains(&first_level) { continue; }
            for second_level in input[first_level].iter() {
                if used.contains(&second_level) { continue; }
                if second_level == k { continue; }
                if used_for_this_cycle.contains(second_level) { continue; }
                for third_level in input[second_level].iter() {
                    if third_level == k {
                        if k.starts_with('t') || first_level.starts_with('t') || second_level.starts_with('t') {
                            cycles.push((k, first_level, second_level));
                        }

                        used_for_this_cycle.insert(first_level);
                    }
                }
            }
        }

        used.insert(k);

    });

    cycles.len()
}

pub fn part_two(input: &HashMap<&str, Vec<&str>>) -> i32 {
    0
}

pub fn execute(input: &str) -> (String, String) {
    let parsed = parse_input(input);

    println!("{:?}", parsed);

    let part1 = part_one(&parsed);
    let part2 = part_two(&parsed);

    (part1.to_string(), part2.to_string())
}

//#[tokio::test]
async fn test_day() {
    use crate::input_provider::get_puzzle_input;
    let input = get_puzzle_input(2024, 23).await.unwrap();
    let (part1, part2) = execute (&input);

    assert_eq!("1304", part1);
    assert_eq!("?", part2);
}
