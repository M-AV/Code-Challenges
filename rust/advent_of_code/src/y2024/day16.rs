#![allow(dead_code)]
#![allow(unused_variables)]

use itertools::Itertools;

pub fn part_one(input: &str) -> i32 {
    0
}

pub fn part_two(input: &str) -> i32 {
    0
}

pub fn execute(input: &str) -> (String, String) {
    println!("{:?}", input);

    // let parsed: (Vec<i32>, Vec<i32>) = parse_input(input);

    let part1 = part_one(input);
    let part2 = part_two(input);

    (part1.to_string(), part2.to_string())
}

#[tokio::test]
async fn test_day() {
    use crate::input_provider::get_puzzle_input;
    let input = get_puzzle_input(2024, 16).await.unwrap();
    let (part1, part2) = execute (&input);

    assert_eq!("?", part1);
    assert_eq!("?", part2);
}
