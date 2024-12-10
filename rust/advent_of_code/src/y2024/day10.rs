#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use itertools::Itertools;
use recursive::recursive;

use crate::utils::{self, parse_2d_vec};

fn parse_input(input: &str) -> Vec<Vec<i32>> {
    let result: Vec<Vec<i32>> = parse_2d_vec(input)
        .into_iter()
        .map(|l| l.into_iter().map(|c| c.to_digit(10).unwrap() as i32).collect_vec())
        .collect_vec();

    let result = utils::add_padding_to_2d_vec(&result, 99);
    result
}

/// Finds all reachable destinations by recursively moving through the grid.
/// Duplicate destinations are returned for the sake of Part 2 as each duplicate
/// represents a unique path to get there
#[recursive]
fn find_reachable_destinations((x,y): (usize, usize), grid: &Vec<Vec<i32>>, prev_value: i32) -> Vec<(usize, usize)> {
    let val = grid[x][y];
    if val != prev_value + 1 { return vec![]; }
    if val == 9 { 
        return vec![(x,y)]; 
    } 

    let res = vec![(x-1, y) , (x+1, y), (x,y-1), (x, y+1)].into_iter()
        .map(|loc| find_reachable_destinations(loc, grid, val))
        .flatten()
        .collect_vec();
    res
}

pub fn part_one(input: &Vec<Vec<i32>>) -> i32 {
    let mut result = 0;

    for x in 0 .. input.len() {
        for y in 0 .. input[0].len() {
            if input[x][y] != 0 { continue; }

            let destinations = find_reachable_destinations((x,y), input, -1)
                    .into_iter()
                    .unique()
                    .collect_vec();

            result += destinations.len() as i32;
        }
    }

    result
}

pub fn part_two(input: &Vec<Vec<i32>>) -> i32 {
    let mut result = 0;

    for x in 0 .. input.len() {
        for y in 0 .. input[0].len() {
            if input[x][y] != 0 { continue; }

            let destinations = find_reachable_destinations((x,y), input, -1);
            result += destinations.len() as i32;
        }
    }

    result
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
    let input = get_puzzle_input(2024, 10).await.unwrap();
    let (part1, part2) = execute (&input);

    assert_eq!("760", part1);
    assert_eq!("1764", part2);
}
