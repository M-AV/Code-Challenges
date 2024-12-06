#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use std::{collections::{HashMap, HashSet}, ops::Index};

use itertools::Itertools;

use crate::utils::{self, print_2d_vec};

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

fn simulate_steps(input: &Vec<Vec<char>>) -> (HashSet<(usize, usize)>, bool) {
    let mut grid: Vec<Vec<char>> = input.clone();
    let mut visited_locs:HashSet<(usize, usize)> = HashSet::new();

    let mut current_direction = '^';
    let mut current_idx = utils::find_first_index(&grid, current_direction);
    let first_index = current_idx;
    let mut in_loop = false;

    loop {
        let (x_next, y_next) = next_loc(current_direction, current_idx);
        visited_locs.insert(current_idx);

        // To speed things up, only set direction the first time we cross a path
        if grid[current_idx.0][current_idx.1] == '.' {
            grid[current_idx.0][current_idx.1] = current_direction;
        }

        match grid[x_next][y_next] {
            '!' => break,
            '#' => current_direction = next_direction(current_direction),
            '^' | 'v' | '<' | '>' if grid[x_next][y_next] == current_direction => {
                in_loop = true;
                break;
            },
            '.' | '^' | 'v' | '<' | '>' => current_idx = (x_next, y_next),
            _ => panic!("Time to debug")
        }
    }

    (visited_locs, in_loop)

}

/// Idea is simple, just simulate the steps of the guard until he exits the map
pub fn part_one(input: &Vec<Vec<char>>) -> usize {
    let (steps, is_in_loop) = simulate_steps(input);

    steps.len()
}

/// Now we have to consider loops.. To make the guard turn, the obstacle should be placed somewhere
/// on the initial path. Anywhere else, shouldn't affect him
pub fn part_two(input: &Vec<Vec<char>>) -> i32 {
    let starting_pos = utils::find_first_index(input, '^');

    let (mut steps, _) = simulate_steps(input);
    steps.remove(&starting_pos);


    let mut loop_counter = 0;

    for (x,y) in steps {
        let mut cloned_grid = input.clone();
        cloned_grid[x][y] = '#';

        let (_, in_loop) = simulate_steps(&cloned_grid);
        if in_loop {
            loop_counter = loop_counter + 1;
        }
    }


    loop_counter
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
    let input = get_puzzle_input(2024, 6).await.unwrap();
    let (part1, part2) = execute (&input);

    assert_eq!("4964", part1);
    assert_eq!("1740", part2);
}
