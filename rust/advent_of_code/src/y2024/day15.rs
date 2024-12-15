#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]
#![allow(unused_assignments)]

use itertools::Itertools;
use recursive::recursive;
use tokio::task::LocalEnterGuard;

use crate::utils::{parse_2d_vec, print_2d_vec};

fn parse_input(input: &str) -> (Vec<Vec<char>>, Vec<char>) {
    let (grid, moves) = input.split_once("\n\n").unwrap();

    let grid = parse_2d_vec(grid);
    let moves = moves.chars().filter(|c| '\n' != *c).collect_vec();
    (grid, moves)
}

#[recursive]
fn move_item(grid: &mut Vec<Vec<char>>, location: (usize, usize), direction: char) -> (bool, (usize, usize)) {
    let mut next_loc = location;

    match direction {
        '^' => next_loc = (location.0 - 1, location.1),
        '>' => next_loc = (location.0, location.1+1),
        'v' => next_loc = (location.0 + 1, location.1),
        '<' => next_loc = (location.0, location.1-1),
        _ => panic!("Do the robot"),
    }

    match grid[next_loc.0][next_loc.1] {
        '#' => return (false, location),
        'O' => {
            let (moved, _) = move_item(grid, next_loc, direction);
            if moved {
                grid[next_loc.0][next_loc.1] = grid[location.0][location.1];
                grid[location.0][location.1] = '.';
                return (true, next_loc);
            }
            return (false, location);
        },
        '.' => {
            grid[next_loc.0][next_loc.1] = grid[location.0][location.1];
            grid[location.0][location.1] = '.';
            return (true, next_loc);
        }
        _ => panic!("Invalid char")
    }
}


pub fn part_one(input: &(Vec<Vec<char>>, Vec<char>)) -> usize {
    let mut grid = input.0.clone();

    let mut robot_loc = (0, 0);

    'outer: for x in 0 .. grid.len() {
        for y in 0 .. grid[0].len() {
            if grid[x][y] == '@' {
                robot_loc = (x,y);
                break 'outer;
            }
        }
    }

    input.1.iter().for_each(|c| {
        robot_loc = move_item(&mut grid, robot_loc, *c).1;

        // print_2d_vec(&grid);
    });


    let mut result = 0;
    
    for x in 0 .. grid.len() {
        for y in 0 .. grid[0].len() {
            if grid[x][y] == 'O' {
                result += x * 100 + y;
            }
        }
    }
    result
}

pub fn part_two(input: &(Vec<Vec<char>>, Vec<char>)) -> i32 {
    // Make widened grid
    // Adjust move logic to consider box width
    // Adjust GPS calculation
    0
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
    let input = get_puzzle_input(2024, 15).await.unwrap();
    let (part1, part2) = execute (&input);

    assert_eq!("1511865", part1);
    // assert_eq!("?", part2);
}
