#![allow(dead_code)]
#![allow(unused_variables)]

use itertools::Itertools;

use crate::input_provider::get_puzzle_input;

// We parse the input into two vectors that we can work with later
fn parse_input(input: &str) -> (Vec<i32>, Vec<i32>) {
    let lines: Vec<(i32, i32)> = input
        .lines()
        .map(|f| f.split("   ").collect())
        .map(|f:Vec<&str>| {
            (
                f[0].parse::<i32>().unwrap(), 
                f[1].parse::<i32>().unwrap()
            )
        })
        .collect();

    let left:Vec<i32> = lines
        .clone()
        .into_iter()
        .map(|f| f.0)
        .collect();

    let right:Vec<i32> = lines
        .into_iter()
        .map(|f| f.1)
        .collect();
    

    (left, right)

}

pub fn part_one((left, right): &(Vec<i32>, Vec<i32>)) -> i32 {
    let left_sorted = left.iter().sorted();
    let right_sorted = right.iter().sorted();

    let zipped = left_sorted.zip(right_sorted);

    let mut result = 0; 
    for (l, r) in zipped {
        result += (l - r).abs()
    }

    result
}

pub fn part_two((left, right): &(Vec<i32>, Vec<i32>)) -> i32 {
    let mut result = 0;

    for x in left {
        let mut counter = 0;
        for y in right {
            if x == y {
                counter = counter + 1;
            }
        }
        result = result + (x * counter);
    }

    result
}

pub fn execute(input: &str) -> (String, String) {
    let parsed: (Vec<i32>, Vec<i32>) = parse_input(input);

    let part1 = part_one(&parsed);
    let part2 = part_two(&parsed);

    (part1.to_string(), part2.to_string())
}

#[tokio::test]
async fn test_day() {
    let input = get_puzzle_input(2024, 01).await.unwrap();
    let (part1, part2) = execute (&input);

    assert_eq!("1938424", part1);
    assert_eq!("22014209", part2);
}
