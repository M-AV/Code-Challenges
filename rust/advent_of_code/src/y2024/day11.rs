#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use std::collections::HashMap;

use itertools::Itertools;
use recursive::recursive;

use crate::utils::int_length;

#[derive(Debug)]
#[derive(Clone)]
pub struct Node {
    value: i64,
    children: Vec<Node>
}

impl Node {
    fn new(value: i64) -> Self {
        Node {
            value: value,
            children: Vec::new(),
        }
    }

    fn add_child(&mut self, key: i64) {
        let new_node= Node::new(key);
        self.children.push(new_node);
    }
}


fn parse_input(input: &str) -> Vec<Node>{
    input.split(" ").map(|s| s.parse().unwrap())
        .map(|v| Node::new(v))
        .collect_vec()
}

#[recursive]
fn iterate_tree(tree: &mut [Node], callback: &mut impl FnMut(&mut Node)) {
    for node in tree {
        if node.children.len() == 0 {
            callback(node);
        } else {
            iterate_tree(&mut node.children, callback);
        }
    }
}

fn process_node_part1(node: &mut Node) {
    if node.value == 0 {
        node.value = 1;
        return;
    } 
    let val_len = int_length(node.value) as usize;
    if val_len % 2 == 0 {
        let str = node.value.to_string();

        node.children.push(Node::new(str[..(val_len / 2)].parse::<i64>().unwrap()));
        node.children.push(Node::new(str[(val_len / 2)..].parse::<i64>().unwrap()));
        node.value = -1;
    } else {
        node.value *= 2024;
    }
}

fn count_nodes(input: &mut Vec<Node>) -> i64 {
    let mut counter = 0;

    let mut count_nodes = |node: &mut Node| {
        counter += 1;
    };

    iterate_tree(input, &mut count_nodes);
    counter
}

pub fn part_one(input: &mut Vec<Node>, iterations: i32) -> i64 {
    for i in 0 .. iterations {
        iterate_tree(input, &mut process_node_part1);
    }

    count_nodes(input)
}

// -- Part 2 --

struct Stone {
    value: i64,
    count: i64,
    additions: i64
}
impl Stone {
    fn new(value: i64, count: i64) -> Self {
        Stone {
            value: value,
            count: count,
            additions: 0
        }
    }
}

/// As expected, it's not feasable to do it the naive way in part 2. 
/// Didn't change part 1, to keep it as a reference
pub fn part_two(input: &str, iterations: i32) -> i64 {
    let mut stones = HashMap::<i64, Stone>::new();

    input
        .split_whitespace()
        .map(|s| s.parse::<i64>().unwrap())
        .for_each(|f| {

            let binding = Stone::new(f, 0);
            let entry = stones.entry(f).or_insert(binding);
            entry.count += 1;
        });

    for _ in 0 .. iterations {
        let indexes = stones.keys().cloned().collect_vec();
        for &val in &indexes {
            
            let entry = stones.get_mut(&val).unwrap();
            if entry.count == 0 {
                continue;
            }
            let count = entry.count;
            entry.count = 0;

            if val == 0 {
                let new_value = 1;

                let new_entry = stones.entry(new_value).or_insert(Stone::new(new_value, 0));
                new_entry.additions += count;
     
                continue;
            } 
            
            let val_len = int_length(val) as usize;
            if val_len % 2 == 0 {
                let str = val.to_string();
                let left = str[(val_len / 2)..].parse::<i64>().unwrap();
                let new_entry = stones.entry(left).or_insert(Stone::new(left, 0));
                new_entry.additions += count;

                let right = str[..(val_len / 2)].parse::<i64>().unwrap();
                let new_entry = stones.entry(right).or_insert(Stone::new(right, 0));
                new_entry.additions += count;
            } else {
                let new_value = val * 2024;
                let new_entry = stones.entry(new_value).or_insert(Stone::new(new_value, 0));
                new_entry.additions += count;
            }
        }

        let indexes = stones.keys().cloned().collect_vec();
        indexes.iter().for_each(|&val| {
            if let Some(entry) = stones.get_mut(&val) {
                entry.count = entry.count + entry.additions;
                entry.additions = 0; // Reset after updating
            }
        });
    }

    stones.iter().map(|s| s.1.count).sum()
}

pub fn execute(input: &str) -> (String, String) {
    let mut parsed = parse_input(input);

    let part1 = part_one(&mut parsed, 25);
    let part2 = part_two(&input, 75);

    (part1.to_string(), part2.to_string())
}

#[tokio::test]
async fn test_day() {
    use crate::input_provider::get_puzzle_input;
    let input = get_puzzle_input(2024, 11).await.unwrap();
    let (part1, part2) = execute (&input);

    assert_eq!("189167", part1);
    assert_eq!("225253278506288", part2);
}
