#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use std::collections::{HashMap, HashSet};

use itertools::Itertools;

use crate::utils::{self, parse_2d_vec, print_2d_vec};

fn parse_input(input: &str) -> Vec<Vec<char>> {
    parse_2d_vec(input)
}

fn find_antinodes(first: &(usize, usize), second: &(usize, usize)) -> ((i32, i32), (i32, i32)) {
    let first_i = (first.0 as i32, first.1 as i32);
    let second_i = (second.0 as i32, second.1 as i32);

    (
        (first_i.0 - (second_i.0 - first_i.0), first_i.1 - (second_i.1 - first_i.1)),
        (second_i.0 - (first_i.0 - second_i.0), second_i.1 - (first_i.1 - second_i.1)),
    )
}

pub fn part_one(input: &Vec<Vec<char>>) -> usize {
    let mut signal_locs = HashMap::<char, Vec<(usize,usize)>>::new();

    for x in 0 .. input.len() {
        for y in 0 .. input[0].len() {
            if input[x][y] == '.' {
                continue;
            }

            let entry = signal_locs.entry(input[x][y]).or_insert_with(|| vec![]);
            entry.push((x,y));
        }
    }

    let mut locations = HashSet::new();

    for (signal_type, locs) in signal_locs {
        locs.iter()
            .tuple_combinations()
            .map(|(x,y)| find_antinodes(x, y))
            .map(|(x,y)| vec![x, y])
            .flatten()
            .unique()
            .filter(|(x,y)| 
                    *x >= 0 && *y >= 0 && *x < input.len() as i32 && *y < input[0].len() as i32
                )
            .for_each(|loc| { locations.insert((loc.0 as usize, loc.1 as usize)); });
    }

    let s = locations.len();

    // utils::print_2d_vec_with_highlight(input, &locations.into_iter().collect_vec());

    s
}

pub fn part_two(input: &Vec<Vec<char>>) -> i32 {
    0
}

pub fn execute(input: &str) -> (String, String) {
    // println!("{:?}", input);

    let parsed = parse_input(input);

    // print_2d_vec(&parsed);

    let part1 = part_one(&parsed);
    let part2 = part_two(&parsed);

    (part1.to_string(), part2.to_string())
}

//#[tokio::test]
async fn test_day() {
    use crate::input_provider::get_puzzle_input;
    let input = get_puzzle_input(2024, 8).await.unwrap();
    let (part1, part2) = execute (&input);

    assert_eq!("394", part1);
    assert_eq!("?", part2);
}
