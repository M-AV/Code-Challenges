#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use std::collections::{HashMap, HashSet};

use itertools::Itertools;

use crate::utils::{self, parse_2d_vec, print_2d_vec};

fn parse_input(input: &str) -> Vec<Vec<char>> {
    parse_2d_vec(input)
}

/// Find antinode on "each side" of an antenna pair
fn find_antinodes_part1(first: &(usize, usize), second: &(usize, usize)) -> ((i32, i32), (i32, i32)) {
    let first_i = (first.0 as i32, first.1 as i32);
    let second_i = (second.0 as i32, second.1 as i32);

    (
        (first_i.0 - (second_i.0 - first_i.0), first_i.1 - (second_i.1 - first_i.1)),
        (second_i.0 - (first_i.0 - second_i.0), second_i.1 - (first_i.1 - second_i.1)),
    )
}

// Find antinodes when repeated for each antenna pair (incl. the pair)
fn find_antinodes_part2(
    grid_size: &(usize, usize), 
    first: &(usize, usize), 
    second: &(usize, usize)) 
    -> Vec<(i32, i32)> {
    // Better way to do this? 
    let grid_size_i = (grid_size.0 as i32, grid_size.1 as i32);
    let first_i = (first.0 as i32, first.1 as i32);
    let second_i = (second.0 as i32, second.1 as i32);

    let mut points = vec![first_i, second_i];

    let x_diff = second_i.0 - first_i.0;
    let y_diff = second_i.1 - first_i.1;
    let mut prev = first_i;

    // For simplicity, I just duplicate the code: one time per direction

    loop {
        let next = (prev.0 - x_diff, prev.1 - y_diff);
        if next.0 < 0 || grid_size_i.0 <= next.0 || next.1 < 0 || grid_size_i.1 <= next.1 {
            break;
        }

        points.push(next);
        prev = next;
    }

    let x_diff = first_i.0 - second_i.0;
    let y_diff = first_i.1 - second_i.1;

    loop {
        let next = (prev.0 - x_diff, prev.1 - y_diff);
        if next.0 < 0 || grid_size_i.0 <= next.0 || next.1 < 0 || grid_size_i.1 <= next.1 {
            break;
        }

        points.push(next);
        prev = next;
    }

    points
}

fn find_antenna_locations(input: &Vec<Vec<char>>) -> HashMap<char, Vec<(usize, usize)>> {
    let mut signal_locs = HashMap::<char, Vec<(usize,usize)>>::new();

    // Find locations of all antennas
    for x in 0 .. input.len() {
        for y in 0 .. input[0].len() {
            if input[x][y] == '.' {
                continue;
            }

            let entry = signal_locs.entry(input[x][y]).or_insert_with(|| vec![]);
            entry.push((x,y));
        }
    }
    signal_locs
}

pub fn part_one(input: &Vec<Vec<char>>, signal_locs: &HashMap<char, Vec<(usize, usize)>>) -> usize {
    let mut locations = HashSet::new();

    for (signal_type, locs) in signal_locs {
        locs.iter()
            .tuple_combinations()
            .map(|(x,y)| find_antinodes_part1(x, y))
            .map(|(x,y)| vec![x, y])
            .flatten()
            .filter(|(x,y)| 
                    *x >= 0 && *y >= 0 && *x < input.len() as i32 && *y < input[0].len() as i32
                )
            .for_each(|loc| { locations.insert((loc.0 as usize, loc.1 as usize)); });
    }

    let s = locations.len();

    // utils::print_2d_vec_with_highlight(input, &locations.into_iter().collect_vec());

    s
}

pub fn part_two(input: &Vec<Vec<char>>, signal_locs: &HashMap<char, Vec<(usize, usize)>>) -> usize {
    let mut locations = HashSet::new();

    for (signal_type, locs) in signal_locs {
        locs.iter()
            .tuple_combinations()
            .map(|(x,y)| find_antinodes_part2(&(input.len(), input[0].len()), x, y))
            .flatten()
            .for_each(|loc| { locations.insert((loc.0 as usize, loc.1 as usize)); });
    }

    let s = locations.len();

    s
}

pub fn execute(input: &str) -> (String, String) {
    let parsed = parse_input(input);
    let signal_locs = find_antenna_locations(&parsed);

    let part1 = part_one(&parsed, &signal_locs);
    let part2 = part_two(&parsed, &signal_locs);

    (part1.to_string(), part2.to_string())
}

#[tokio::test]
async fn test_day() {
    use crate::input_provider::get_puzzle_input;
    let input = get_puzzle_input(2024, 8).await.unwrap();
    let (part1, part2) = execute (&input);

    assert_eq!("394", part1);
    assert_eq!("1277", part2);
}
