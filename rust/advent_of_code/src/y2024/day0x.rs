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
// mod tests {
//     use super::*;

//     #[test]
//     fn test_execute() {
//         let (part1, part2) = execute (get_puzzle_input(2020, 02));
//         let result = part_one(&advent_of_code::template::read_file("examples", DAY));
//         assert_eq!(result, None);
//     }
// }