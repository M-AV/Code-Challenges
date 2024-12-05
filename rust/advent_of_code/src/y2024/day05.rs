#![allow(dead_code)]
#![allow(unused_variables)]

use std::cmp::Ordering;

use itertools::Itertools;

#[derive(Debug)]
struct Parsed {
    rules: Vec<(i32, i32)>,
    updates: Vec<Vec<i32>>
}

fn parse_input(input: &str) -> Parsed {
    let rules = input.lines()
        .take_while(|l| !l.is_empty())
        .map(|l|
            l
                .split('|').collect_vec().iter()
                .map(|i| i.parse::<i32>().unwrap())
                .collect_tuple::<(i32, i32)>()
                .unwrap())
        .collect_vec();

    let updates: Vec<Vec<i32>> = input.lines()
        .skip_while(|l| !l.is_empty())
        .skip(1)
        .map(|l| 
            l
                .split(',').collect_vec().iter()
                .map(|i| i.parse::<i32>().unwrap())
                .collect_vec()
            )
        .collect_vec();

    Parsed {
        rules: rules,
        updates: updates
    }
}

/// Idea is to check if every item in an update is correct compared to the items that came before.
/// If the last number satisfies this, then all the previous numbers must be correct as well.
fn is_ordered_correctly(rules: &Vec<(i32, i32)>, update: &Vec<i32>) -> bool {
    // Iterate through each char from the 2nd and up (the 1st is implicitly correct as there are no previous numbers)
    for i in 1 .. update.len() {
        let item = update[i];

        // Find all rules where the current item has to be first
        let rules = rules.iter().filter(|(l,r)| item == *l).collect_vec();

        for j in 0 .. i {
            // From the rules where 'item' should be first, is there any rule that says 'update[j]' should be after?
            // If so, the line breaks the rule and we continue to the next one
            let breaking_rules = rules.iter().filter(|(l, r)| update[j] == *r).collect_vec().len() > 0;
            if breaking_rules {
                return false;
            }
        }
    }

    true
}

fn part_one(input: &Parsed) -> i32 {
    let mut result = 0;

    for update in input.updates.iter() {
        if is_ordered_correctly(&input.rules, update) {
            let middle_item = update[update.len() / 2];
            result = result + middle_item;
        }
    }

    result
}

fn part_two(input: &Parsed) -> i32 {
    let mut result = 0;

    for update in input.updates.iter() {
        if is_ordered_correctly(&input.rules, update) {
            continue;
        }

        let mut clone = update.clone();
        // I just sort it and check higher/lower based on the rules
        clone.sort_by(|a, b| {

            // I'm sure there is a better way to lookup a single rule..
            let mut rule = input.rules.iter()
                .filter(|(l,r)| l == a && r == b)
                .collect_vec();

            if rule.len() > 0 {
                return Ordering::Less;
            }

            rule = input.rules.iter()
                .filter(|(l,r)| r == a && l == b)
                .collect_vec();

            if rule.len() > 0 {
                return Ordering::Greater;
            }

            Ordering::Equal
        });

        let middle_item = clone[clone.len() / 2];
        result = result + middle_item;
    }

    result
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
    let input = get_puzzle_input(2024, 5).await.unwrap();
    let (part1, part2) = execute (&input);

    assert_eq!("4662", part1);
    assert_eq!("5900", part2);
}
