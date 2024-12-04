#![allow(dead_code)]
#![allow(unused_variables)]

use crate::utils;

/// Parses the input into a 2D vector and adds a padding to avoid having to deal with the (literal) edge-cases
fn parse_input(input: &str) -> Vec<Vec<char>> {
    let parsed = utils::parse_2d_vec(&input);
    let with_padding = utils::add_padding_to_2d_vec(&parsed, '.');
    with_padding
}

fn get_xmas_count(grid: &Vec<Vec<char>>, (i,j): (usize, usize)) -> i32 {
    let mut result = 0;
    // Horizontal
    if grid[i][j+1] == 'M' && grid[i][j+2]  == 'A' && grid[i][j+3] == 'S' {
        result = result + 1;
    }
    if grid[i][j-1] == 'M' && grid[i][j-2]  == 'A' && grid[i][j-3] == 'S' {
        result = result + 1;
    }
    // Vertical
    if grid[i+1][j] == 'M' && grid[i+2][j]  == 'A' && grid[i+3][j] == 'S' {
        result = result + 1;
    }
    if grid[i-1][j] == 'M' && grid[i-2][j]  == 'A' && grid[i-3][j] == 'S' {
        result = result + 1;
    }

    // Diagonal
    if grid[i+1][j+1] == 'M' && grid[i+2][j+2]  == 'A' && grid[i+3][j+3] == 'S' {
        result = result + 1;
    }
    if grid[i-1][j-1] == 'M' && grid[i-2][j-2]  == 'A' && grid[i-3][j-3] == 'S' {
        result = result + 1;
    }
    if grid[i+1][j-1] == 'M' && grid[i+2][j-2]  == 'A' && grid[i+3][j-3] == 'S' {
        result = result + 1;
    }
    if grid[i-1][j+1] == 'M' && grid[i-2][j+2]  == 'A' && grid[i-3][j+3] == 'S' {
        result = result + 1;
    }

    result
}

fn is_mas_in_x_shape(grid: &Vec<Vec<char>>, (i,j): (usize, usize)) -> bool {
    if  (grid[i-1][j-1] == 'M' && grid[i+1][j+1] == 'S') || 
        (grid[i-1][j-1] == 'S' && grid[i+1][j+1] == 'M') {

        if  (grid[i+1][j-1] == 'M' && grid[i-1][j+1] == 'S') ||
            (grid[i+1][j-1] == 'S' && grid[i-1][j+1] == 'M') {
                return true;
            }
    }

    false
}

pub fn part_one(input: &Vec<Vec<char>>) -> i32 {
    let mut result = 0;
    
    for i in 0 .. input.len() {
        for j in 0 .. input[0].len() {
            let val = input[i][j];
            if input[i][j] != 'X' { 
                continue;
            }

            result = result + get_xmas_count(&input, (i,j))
        }
    }

    result
}

pub fn part_two(input: &Vec<Vec<char>>) -> i32 {
    let mut result = 0;
    
    for i in 0 .. input.len() {
        for j in 0 .. input[0].len() {
            let val = input[i][j];
            if input[i][j] != 'A' { 
                continue;
            }

            if is_mas_in_x_shape(input, (i,j)) {
                result = result + 1;
            }
        }
    }

    result
}

pub fn execute(input: &str) -> (String, String) {
    let parsed: Vec<Vec<char>> = parse_input(input);

    let part1 = part_one(&parsed);
    let part2 = part_two(&parsed);

    (part1.to_string(), part2.to_string())
}

#[tokio::test]
async fn test_day() {
    use crate::input_provider::get_puzzle_input;
    let input = get_puzzle_input(2024, 4).await.unwrap();
    let (part1, part2) = execute (&input);

    assert_eq!("2654", part1);
    assert_eq!("1990", part2);
}
