#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use std::{collections::{HashMap, HashSet, VecDeque}, i64, ops::Index};

use itertools::Itertools;

use crate::utils::{parse_2d_vec, print_2d_vec, print_2d_vec_with_callback, print_2d_vec_with_highlight};

fn parse_input(input: &str) -> Vec<Vec<char>> {
    parse_2d_vec(input)
}

fn get_possible_moves(input: &Vec<Vec<char>>, (x,y): (usize, usize), direction: char) -> Vec<((usize, usize), char)> {
    let items = vec![((x-1, y), '^'), ((x+1, y), 'v'), ((x, y-1), '<'), ((x, y+1), '>')];

    items.into_iter().filter(|((x_,y_), _)| {
        match input[*x_][*y_] {
            '#' => false,
            _ => true
        }
    }).collect_vec()
}

fn get_previous_loc((x,y): (usize, usize), dir: char) -> (usize, usize) {
    match dir {
        '>' => (x, y-1),
        'v' => (x-1, y),
        '<' => (x, y+1),
        '^' => (x+1, y),
        _ => panic!("No diagonals brosef")
    }
}

fn calc_turn_cost(old_dir: char, new_dir: char) -> u128 {
    if old_dir == new_dir {
        return 0;
    }

    let dirs = vec!['>', 'v', '<', '^'];

    match (old_dir, new_dir) {
        ('<', '>') | ('>', '<') | ('^', 'v') | ('v', '^')=> 2000,
        _ => 1000
    }
}

/// Count locations by using the "previous" HashMap to backtrack all fastests paths 
/// through the grid
fn count_locations(
    costs: HashMap<((usize, usize), char), u128>, 
    previous: HashMap<((usize, usize), char), HashSet<((usize, usize), char)>>, 
    end_pos: (usize, usize), 
    min_cost: u128) -> u128 {
    let mut shortest_path_states = HashSet::new();
    let mut stack = Vec::new();

    // Add all directions where we reach the end state with minimum cost
    for d in ['>', 'v', '<', '^'] {
        if costs.get(&(end_pos, d)) == Some(&min_cost) {
            stack.push((end_pos, d));
        }
    }

    // Backtrack through all paths
    while let Some(state) = stack.pop() {
        if !shortest_path_states.insert(state) {
            continue;
        }

        if let Some(preds) = previous.get(&state) {
            for &pred_state in preds {
                stack.push(pred_state);
            }
        }
    }

    shortest_path_states
        .into_iter()
        .map(|(l, _)| l)
        .unique()
        .collect_vec()
        .len() as u128
}

/// Idea for part 2: 
/// Keep track of every "previous" location, when we find a new "shortest path".
/// In the end, we can then backtrack from the end location, through all the visited locs.
fn shortest_path_go_go_go(input: &Vec<Vec<char>>, part1: bool) -> u128 {
    let mut next_locs = VecDeque::new();
    // Every time we update costs, we should also update previous
    let mut costs = HashMap::new();
    let mut previous: HashMap<((usize, usize), char), HashSet<((usize, usize), char)>> = HashMap::new();
    let start_pos = (input.len() - 2, 1 as usize);
    let end_pos  = (1 as usize, input[0].len() - 2);
    next_locs.push_back((start_pos, '>', 0 as u128));
    costs.insert((start_pos, '>'), 0 as u128);


    while let Some((loc, dir, cost)) = next_locs.pop_front() {
        let possible_steps = get_possible_moves(input, loc, dir);
        let previous_loc = get_previous_loc(loc, dir);
   
        possible_steps.iter().for_each(|(next_loc, next_direction)| {
            // Update cost for all (relevant) directions of current position
            let cost_entry = costs.entry((loc, *next_direction)).or_insert(u128::MAX);
            let turn_cost = calc_turn_cost(dir, *next_direction);
            let current_cost = cost + turn_cost;
        
            if current_cost < *cost_entry {
                *cost_entry = current_cost;

                let preds = previous.entry((loc, *next_direction)).or_insert_with(HashSet::new);
                preds.clear();
                preds.insert((loc, dir));
            } else if current_cost == *cost_entry {
                let preds = previous.entry((loc, *next_direction)).or_insert_with(HashSet::new);
                preds.insert((loc, dir));
            }

            if next_loc == &previous_loc || loc == end_pos {
                return;
            }

            // If next position can be reached with a lower cost than any previous visit,
            // we visit again
            let entry = costs.entry((*next_loc, *next_direction)).or_insert(u128::MAX);
            let next_step_cost = current_cost + 1;

            if next_step_cost < *entry {
                *entry = current_cost + 1;
                next_locs.push_back((*next_loc, *next_direction, current_cost + 1));

                let preds = previous.entry((*next_loc, *next_direction)).or_insert_with(HashSet::new);
                preds.clear();
                preds.insert((loc, *next_direction));
            } else if next_step_cost == *entry {
                let preds = previous.entry((*next_loc, *next_direction)).or_insert_with(HashSet::new);
                preds.insert((loc, *next_direction));
            }
        });
    }

    let min_cost = *vec!['>', 'v', '<', '^'].iter().map(|d| costs.get(&(end_pos, *d)))
        .filter(|s: &Option<&u128>| s.is_some())
        .map(|s| s.unwrap())
        .min()
        .unwrap();

    if part1 {
        return min_cost;
    } else {
        count_locations(costs, previous, end_pos, min_cost)
    }

    
}

pub fn part_one(input: &Vec<Vec<char>>) -> u128 {
    shortest_path_go_go_go(input, true)
}

pub fn part_two(input: &Vec<Vec<char>>) -> u128 {
    shortest_path_go_go_go(input, false)
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
    let input = get_puzzle_input(2024, 16).await.unwrap();
    let (part1, part2) = execute (&input);

    assert_eq!("83432", part1);
    assert_eq!("467", part2);
}
