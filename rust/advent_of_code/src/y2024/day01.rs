#![allow(dead_code)]
#![allow(unused_variables)]

use itertools::Itertools;

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

// mod tests {
//     use super::*;

//     #[test]
//     fn test_execute() {
//         let (part1, part2) = execute (get_puzzle_input(2020, 02));
//         let result = part_one(&advent_of_code::template::read_file("examples", DAY));
//         assert_eq!(result, None);
//     }
// }