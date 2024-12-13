#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use std::{collections::HashSet, default, fs::Permissions};

use itertools::Itertools;

use crate::utils::{add_padding_to_2d_vec, parse_2d_vec};

fn parse_input(input: &str) -> Vec<Vec<char>> {
    let grid = parse_2d_vec(input);
    add_padding_to_2d_vec(&grid, '.')
}

fn calc_region(input: &Vec<Vec<char>>, position: (usize, usize)) -> (HashSet<(usize, usize)>, i32, i32) {
    let mut coordinates = HashSet::new();
    let mut plots = 0;
    let mut perimeter = 0;

    let value = input[position.0][position.1];

    if value == '.' {
        coordinates.insert(position);
        return (coordinates, plots, perimeter)
    }

    let mut queue = vec![position];

    while let Some((x,y)) = queue.pop() {
        if coordinates.contains(&(x,y)) {
            continue;
        }
        let item = input[x][y];
        if item == value {
            plots += 1;
            coordinates.insert((x,y));

            queue.push((x-1, y));
            queue.push((x+1, y));
            queue.push((x, y-1));
            queue.push((x, y+1));
        } else {
            perimeter += 1;
        }
    }

    (coordinates, plots, perimeter)
}

pub fn part_one(input: &Vec<Vec<char>>) -> i32 {
    let mut markings = input.iter()
        .map(|s| s.iter().map(|i| 0).collect_vec())
        .collect_vec();

    let mut result = 0;

    for x in 0 .. markings.len() {
        for y in 0 .. markings[0].len() {
            if markings[x][y] == 1 {
                continue;
            }

            let (locs, plots, perimeter) = calc_region(input, (x,y));

            locs.iter().for_each(|(x_,y_)| markings[*x_][*y_] = 1);

            result += plots * perimeter;
        }
    }

    result
}

fn get_next_loc(input: &Vec<Vec<char>>, (x,y): &(usize, usize), direction: char) -> ((usize, usize), char){
    let val = input[*x][*y];

    match direction {
        '^' => {
            if input[x-1][*y] == val {
                return ((x-1, *y), '^')
            } else {
                return ((*x, *y), '>');
            }
        },
        '>' => {
            if input[*x][y+1] == val {
                return ((*x, y+1), '>')
            } else {
                return ((*x, *y), 'v');
            }
        },
        'v' => {
            if input[x+1][*y] == val {
                return ((x+1, *y), 'v')
            } else {
                return ((*x, *y), '<');
            }
        },
        '<' => {
            if input[*x][y-1] == val {
                return ((*x, y-1), '<')
            } else {
                return ((*x, *y), '^');
            }
        },
        _ => panic!("Shouldn't happen")
    }
}

/// It's ugly, but it works.. Basic idea:
/// - Find all regions by doing the exact same thing we did in Part 1
/// - Find all edges/lines by collecting (outside, inside) coordinate pairs
/// - Iteratively find all connected lines and remove them, while counting
pub fn part_two(input: &Vec<Vec<char>>) -> i32 {
    let mut markings = input.iter()
        .map(|s| s.iter().map(|i| 0).collect_vec())
        .collect_vec();

        let mut result = 0;

        for x in 0 .. markings.len() {
            for y in 0 .. markings[0].len() {
                if markings[x][y] == 1 {
                    continue;
                }
    
                let (locs, plots, perimeter) = calc_region(input, (x,y));
    
                locs.iter().for_each(|(x_,y_)| markings[*x_][*y_] = 1);

                // Since I have padded the grid, I will get some empty regions
                if plots == 0 {
                    continue;
                }

                // Iterate through all locations in a region and collect anyone with a non-region neighbor
                let value = input[x][y];
                let mut edge_lines = HashSet::new();

                locs.iter().for_each(|(x,y)| {
                    if input[x-1][*y] != value {
                        edge_lines.insert(((x-1, *y), (*x,*y), '-'));
                    }
                    if input[x+1][*y] != value {
                        edge_lines.insert(((x+1, *y), (*x,*y), '-'));
                    }
                    if input[*x][y+1] != value {
                        edge_lines.insert(((*x, y+1), (*x,*y), '|'));
                    }
                    if input[*x][y-1] != value {
                        edge_lines.insert(((*x, y-1), (*x,*y), '|'));
                    }
                });

                // Iterate through all edge pairs, while removing them and reomve all they are attached to
                let mut edges = 0;
                while let Some((outside, inside, dir)) = edge_lines.iter().next().cloned() {
                    edge_lines.remove(&(outside, inside, dir));

                    let (mut i_outside, mut i_inside, mut i_dir) = (outside, inside, dir);
                    match dir {
                        '|' => {
                            while let Some(x) 
                                = edge_lines.take(&((i_outside.0-1, i_outside.1),(i_inside.0-1, i_inside.1),'|')) {
                                    (i_outside, i_inside, i_dir) = x;
                                }

                            (i_outside, i_inside, _) = (outside, inside, dir);
                            
                            while let Some(x) 
                                = edge_lines.take(&((i_outside.0+1, i_outside.1),(i_inside.0+1, i_inside.1),'|')) {
                                    (i_outside, i_inside, _) = x;
                                }
                        },
                        '-' => {
                            while let Some(x) 
                                = edge_lines.take(&((i_outside.0, i_outside.1-1),(i_inside.0, i_inside.1-1),'-')) {
                                    (i_outside, i_inside, _) = x;
                                }

                            (i_outside, i_inside, _) = (outside, inside, dir);
                            
                            while let Some(x) 
                                = edge_lines.take(&((i_outside.0, i_outside.1+1),(i_inside.0, i_inside.1+1),'-')) {
                                    (i_outside, i_inside, _) = x;
                                }
                        },
                        _ => panic!("at the disco")
                    }

                    edges += 1;
                }

                result += plots * edges;
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
    let input = get_puzzle_input(2024, 12).await.unwrap();
    let (part1, part2) = execute (&input);

    assert_eq!("1424006", part1);
    assert_eq!("858684", part2);
}
