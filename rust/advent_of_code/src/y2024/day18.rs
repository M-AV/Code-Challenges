#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use std::{collections::HashMap, i32};

use itertools::Itertools;

use crate::utils::add_padding_to_2d_vec;

fn parse_input(input: &str) -> Vec<(usize,usize)> {
    input.lines()
        .map(|l| l.split_once(',').unwrap())
        .map(|(l,r)| (l.parse::<usize>().unwrap(), r.parse::<usize>().unwrap()))
        .collect_vec()
}



pub fn part_one(input: &Vec<(usize,usize)>, locs_to_simulate: usize, find_any: bool) -> i32 {
    let mut grid = vec![vec!['.'; 71]; 71];

    for i in 0 .. locs_to_simulate {
        let (x,y) = input[i];

        grid[y][x] = '#';
    }

    let bordered_grid = add_padding_to_2d_vec(&grid, '#');
    
    let mut costs = HashMap::new();
    let mut queue = vec![];
    let start_loc: (usize, usize) = (1,1);
    let dest_loc: (usize, usize) = (71,71);

    costs.insert(start_loc, 0);
    queue.push(start_loc);

    while let Some((x,y)) = queue.pop() {
        if (x,y) == dest_loc {
            if find_any {
                break;
            }
            continue;
        }

        let next_locs = vec![(x, y-1), (x, y+1), (x-1, y), (x+1, y)].into_iter()
            .filter(|(x_, y_)| bordered_grid[*y_][*x_] != '#')
            .collect_vec();

        for loc in next_locs {
            let next_cost = costs.get(&(x,y)).unwrap() + 1;
            let cost_entry = costs.entry(loc).or_insert(i32::MAX);

            if find_any && *cost_entry != i32::MAX {
                continue;
            }

            if *cost_entry > next_cost {
                *cost_entry = next_cost;
                queue.push(loc);
            }
        }
    }

    *costs.entry(dest_loc).or_insert(i32::MAX)


}

/// Modified Part 1 to take as input how many locs to simulate as well as a flag to indicate
/// if we're just looking for any path or the fastest path. When looking for any, we only visit 
/// loc once
pub fn part_two(input: &Vec<(usize,usize)>) -> String {
    for i in 1025 .. input.len() {
        let res = part_one(input, i, true);

        if res == i32::MAX {
            let (x,y) = input[i - 1];

            return format!("{},{}", x, y);
        }
    }

    panic!("There is no spoon");
}

pub fn execute(input: &str) -> (String, String) {
    let parsed = parse_input(input);

    let part1 = part_one(&parsed, 1024, false);
    let part2 = part_two(&parsed);

    (part1.to_string(), part2.to_string())
}

#[tokio::test]
async fn test_day() {
    use crate::input_provider::get_puzzle_input;
    let input = get_puzzle_input(2024, 18).await.unwrap();
    let (part1, part2) = execute (&input);

    assert_eq!("250", part1);
    assert_eq!("56,8", part2);
}
