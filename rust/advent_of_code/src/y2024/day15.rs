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

fn find_robot(grid: &Vec<Vec<char>>) -> (usize, usize) {
    let mut robot_loc = (0, 0);
    'outer: for x in 0 .. grid.len() {
        for y in 0 .. grid[0].len() {
            if grid[x][y] == '@' {
                robot_loc = (x,y);
                break 'outer;
            }
        }
    }
    robot_loc
}

fn get_destination(location: (usize, usize), direction: char) -> (usize, usize) {
    match direction {
        '^' => (location.0 - 1, location.1),
        '>' => (location.0, location.1+1),
        'v' => (location.0 + 1, location.1),
        '<' => (location.0, location.1-1),
        _ => panic!("Do the robot"),
    }
}

/// Idea is to recursively go through each block that we need to move and move it if we can.
/// If we can't move one, we return false and none of the other items will be moved.
#[recursive]
fn move_item(grid: &mut Vec<Vec<char>>, location: (usize, usize), direction: char) -> (bool, (usize, usize)) {
    let next_loc = get_destination(location, direction);

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

    let mut robot_loc = find_robot(&grid);

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

// -- Part 2

fn create_widened_grid(org_grid: &Vec<Vec<char>>) -> Vec<Vec<char>> {
    let mut grid = vec![vec!['.'; org_grid[0].len() * 2]; org_grid.len()];

    let mut new_y = 0;
    for x in 0 .. org_grid.len() {
        new_y = 0;
        for y in 0 .. org_grid[0].len() {
            if org_grid[x][y] == 'O' {
                grid[x][new_y] = '[';
                grid[x][new_y + 1] = ']';
            } else if org_grid[x][y] == '@' {
                grid[x][new_y] = '@';
                grid[x][new_y + 1] = '.';
            } else {
                grid[x][new_y] = org_grid[x][y];
                grid[x][new_y + 1] = org_grid[x][y];
            }
        
        
            new_y += 2;
        }
    }
    grid
}

/// Recursively check if we can move all required pieces - Part 2 logic
#[recursive]
fn can_move(grid: &mut Vec<Vec<char>>, location: (usize, usize), direction: char) -> bool {
    if grid[location.0][location.1] == '#' {
        return false;
    }

    let next_loc = get_destination(location, direction);

    match grid[next_loc.0][next_loc.1] {
        '#' => return false,
        '[' => {
            if direction == '>' {
                return can_move(grid, (next_loc.0, next_loc.1 +1), direction);
            }
            if direction == '<' {
                return can_move(grid, next_loc, direction);
            }

            let can_move_left = can_move(grid, next_loc, direction);
            let can_move_right = can_move(grid, (next_loc.0, next_loc.1+1), direction);

            return can_move_left && can_move_right;
        },
        ']' => {
            if direction == '>' {
                return can_move(grid, next_loc, direction);
            }
            if direction == '<' {
                return can_move(grid, (next_loc.0, next_loc.1-1), direction);
            }

            let can_move_left = can_move(grid, next_loc, direction);
            let can_move_right = can_move(grid, (next_loc.0, next_loc.1-1), direction);

            return can_move_left && can_move_right;
        },
        '.' => {
            return true;
        }
        _ => panic!("Invalid char")
    }
}

#[recursive]
fn move_item_part2(grid: &mut Vec<Vec<char>>, location: (usize, usize), direction: char) -> (bool, (usize, usize)) {
    let next_loc = get_destination(location, direction);

    match grid[next_loc.0][next_loc.1] {
        '#' => return (false, location),
        '[' => {

            if direction == '>' || direction == '<' {
                move_item_part2(grid, (next_loc.0, next_loc.1), direction);
                grid[next_loc.0][next_loc.1] = grid[location.0][location.1];
                grid[location.0][location.1] = '.';
                return (true, next_loc);
            }
    
            move_item_part2(grid, next_loc, direction);
            move_item_part2(grid, (next_loc.0, next_loc.1 + 1), direction);

            grid[next_loc.0][next_loc.1] = grid[location.0][location.1];
            grid[location.0][location.1] = '.';

            return (true, next_loc);
        },
        ']' => {
            if direction == '>' || direction == '<' {
                move_item_part2(grid, (next_loc.0, next_loc.1), direction);
                grid[next_loc.0][next_loc.1] = grid[location.0][location.1];
                grid[location.0][location.1] = '.';
                return (true, next_loc);
            }
    
            move_item_part2(grid, next_loc, direction);
            move_item_part2(grid, (next_loc.0, next_loc.1 - 1), direction);

            grid[next_loc.0][next_loc.1] = grid[location.0][location.1];
            grid[location.0][location.1] = '.';

            return (true, next_loc);
        },
        '.' => {
            grid[next_loc.0][next_loc.1] = grid[location.0][location.1];
            grid[location.0][location.1] = '.';
            return (true, next_loc);
        }
        _ => panic!("Invalid char")
    }
}

/// We can't use the simple combined check if we can move + move logic we used for Part 1. 
/// Instead we need to check initially if we can move the blocks - without actually moving any - 
/// and if we can, then move all the blocks
pub fn part_two((org_grid, directions): &(Vec<Vec<char>>, Vec<char>)) -> usize {
    let mut grid = create_widened_grid(org_grid);
    let mut robot_loc = find_robot(&grid);

    directions.iter().for_each(|c| {
        if can_move(&mut grid, robot_loc, *c) {
            robot_loc = move_item_part2(&mut grid, robot_loc, *c).1;
            // print_2d_vec(&grid);
        }
    });

    let mut result = 0;
    
    for x in 0 .. grid.len() {
        for y in 0 .. grid[0].len() {
            if grid[x][y] == '[' {
                result += x * 100 + y;
            }
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
    let input = get_puzzle_input(2024, 15).await.unwrap();
    let (part1, part2) = execute (&input);

    assert_eq!("1511865", part1);
    assert_eq!("1519991", part2);
}
