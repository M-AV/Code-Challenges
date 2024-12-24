#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use std::collections::{HashMap, HashSet, VecDeque};

use itertools::Itertools;
use tokio::runtime::TryCurrentError;

use crate::utils::{add_padding_to_2d_vec, find_first_index, parse_2d_vec, print_2d_vec, print_2d_vec_with_callback, print_2d_vec_with_highlight};

fn get_next_locs(grid: &Vec<Vec<char>>, (x,y): (usize, usize), prev_loc: (usize, usize)) -> Vec<(usize, usize)> {
    let s = vec![(x-1, y), (x+1, y), (x, y-1), (x,y+1)].into_iter()
        .filter(|(x_, y_)| {
            if prev_loc == (*x_, *y_) {
                return false;
            }
            if grid[*x_][*y_] == '@' {
                return true;
            }
            return grid[*x_][*y_] == '.' || grid[*x_][*y_] == 'E';
        }).collect_vec();

    return s
}

fn count_steps(input: &Vec<Vec<char>>, start: (usize, usize), end: (usize, usize)) -> i64 {
    let mut costs = HashMap::new();
    costs.insert(start, 0);

    let mut next_locs = VecDeque::new();
    next_locs.push_back(((0,0), start));

    while let Some((prev, current)) = next_locs.pop_front() {
        let possible_next = get_next_locs(input, current, prev);

        let current_cost = *costs.entry(current).or_insert(i64::MAX);

        possible_next.iter().for_each(|n| {
            let cost_entry = costs.entry(*n).or_insert(i64::MAX);
            if current_cost + 1 < *cost_entry {
                *cost_entry = current_cost + 1;
                next_locs.push_back((current, *n));
            }
        });
    }

    *costs.entry(end).or_insert(i64::MAX)
}

fn is_suitable_cheat(input: &Vec<Vec<char>>, (x,y): (usize, usize)) -> bool {
    let is_wall = x == 0 || y == 0 || x == input.len() - 1 || y == input[0].len() - 1;
    if is_wall {
        return false;
    }

    let is_surrounded_by_walls = vec![(x-1, y), (x+1, y), (x, y-1), (x,y+1)].iter()
        .all(|(x_,y_)| input[*x_][*y_] == '#');

    !is_surrounded_by_walls
}

pub fn part_one(input: &Vec<Vec<char>>) -> i64 {
    let start_loc = find_first_index(input, 'S');
    let end_loc = find_first_index(input, 'E');

    let initial_steps = count_steps(input, start_loc, end_loc);
    let mut counter = 0;

    for x in 0 .. input.len() {
        for y in 0 .. input[0].len() {
            if input[x][y] == '#' && is_suitable_cheat(input, (x,y)) {
                let mut grid_clone = input.clone();
                grid_clone[x][y] = '@';
                let new_steps = count_steps(&grid_clone, start_loc, end_loc);

                if initial_steps - new_steps >= 100 {
                    counter += 1;
                }
            }
        }
    }

    counter
}

pub fn part_two(input: &Vec<Vec<char>>) -> i32 {
    0
}

pub fn execute(input: &str) -> (String, String) {
    let parsed = &parse_2d_vec(input);

    print_2d_vec(&parsed, false);

    let part1 = part_one(&parsed);
    let part2 = part_two(&parsed);

    (part1.to_string(), part2.to_string())
}

//#[tokio::test]
async fn test_day() {
    use crate::input_provider::get_puzzle_input;
    let input = get_puzzle_input(2024, 20).await.unwrap();
    let (part1, part2) = execute (&input);

    assert_eq!("1338", part1);
    assert_eq!("?", part2);
}
