#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use itertools::Itertools;
use recursive::recursive;

use crate::utils::int_length;

#[derive(Debug)]
#[derive(Clone)]
struct Node {
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

fn process_node(node: &mut Node) {
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

pub fn part_one(input: &mut Vec<Node>) -> i64 {
    for i in 0 .. 25 {
        iterate_tree(input, &mut process_node);
    }

    count_nodes(input)
}

pub fn part_two(input: &mut Vec<Node>) -> i64 {
    for i in 0 .. 75 {
        iterate_tree(input, &mut process_node);
    }

    count_nodes(input)
}

pub fn execute(input: &str) -> (String, String) {
    let mut parsed = parse_input(input);

    // println!("{:?}", parsed);

    let part1 = part_one(&mut parsed.clone());
    let part2 = part_two(&mut parsed);

    (part1.to_string(), part2.to_string())
}

//#[tokio::test]
async fn test_day() {
    use crate::input_provider::get_puzzle_input;
    let input = get_puzzle_input(2024, 11).await.unwrap();
    let (part1, part2) = execute (&input);

    assert_eq!("189167", part1);
    assert_eq!("?", part2);
}
