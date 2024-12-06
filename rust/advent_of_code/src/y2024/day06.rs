#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use std::{collections::HashSet, ops::Index};

use itertools::Itertools;

use crate::utils;

fn parse_input(input: &str) -> Vec<Vec<char>> {
    let parsed = utils::parse_2d_vec(input);
    let with_padding = utils::add_padding_to_2d_vec(&parsed, '!');

    with_padding
}

fn next_loc(direction: char, (x,y): (usize, usize)) -> (usize, usize) {
    match direction {
        '^' => (x-1, y),
        '>' => (x, y+1),
        'v' => (x+1, y),
        '<' => (x, y-1),
        _ => panic!("Value not supported")
    }
}

fn next_direction(dir: char) -> char {
    match dir {
        '>' => 'v',
        'v' => '<',
        '<' => '^',
        '^' => '>',
        _ => panic!("Value not supported")
    }
}

/// Idea is simple, just simulate the steps of the guard until he exits the map
pub fn part_one(input: &Vec<Vec<char>>) -> usize {
    let mut visited_locs:HashSet<(usize, usize)> = HashSet::new();

    let mut current_direction = '^';
    let mut current_idx = utils::find_first_index(&input, current_direction);

    loop {
        let (x_next, y_next) = next_loc(current_direction, current_idx);
        visited_locs.insert(current_idx);

        // Here I assume we will eventually reach an edge and the guy is not walking in circles!
        match input[x_next][y_next] {
            '!' => break,
            '#' => current_direction = next_direction(current_direction),
            '.' | '^' => current_idx = next_loc(current_direction, current_idx),
            _ => panic!("Time to debug")
        }
    }

    visited_locs.len()
}

// Now we have to consider loops, so instead 
pub fn part_two(input: &Vec<Vec<char>>) -> i32 {
    0
}

pub fn execute(input: &str) -> (String, String) {
    
    let parsed = parse_input(input);

    utils::print_2d_vec(&parsed);

    let part1 = part_one(&parsed);
    let part2 = part_two(&parsed);

    (part1.to_string(), part2.to_string())
}

#[tokio::test]
async fn test_day() {
    use crate::input_provider::get_puzzle_input;
    let input = get_puzzle_input(2024, 6).await.unwrap();
    let (part1, part2) = execute (&input);

    assert_eq!("4964", part1);
    assert_eq!("?", part2);
}
