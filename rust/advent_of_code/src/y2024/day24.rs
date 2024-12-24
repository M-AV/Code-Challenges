#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use std::collections::HashMap;

use itertools::Itertools;

#[derive(Debug)]
pub struct Instruction<'a> {
    register1: &'a str,
    register2: &'a str,
    op: &'a str,
    dest_register: &'a str
}

fn parse_input(input: &str) -> (Vec<(&str, i32)>, Vec<Instruction>) {
    let (values, instructions) = input.split_once("\n\n").unwrap();
    let initial_values = values.lines()
        .map(|l| l.split_once(": ").unwrap())
        .map(|(name, val)| (name, val.parse::<i32>().unwrap()))
        .collect_vec();

    let instructions = 
        instructions.lines()
            .map(|l| l.split_once(" -> ").unwrap())
            .map(|(l,r)| {
                let op = l.split(' ').collect_vec();
                Instruction {
                    dest_register: r,
                    op: op[1],
                    register1: op[0],
                    register2: op[2]
                }
            })
            .collect_vec();
         

    (initial_values, instructions)
}

pub fn part_one((values, instructions): &(Vec<(&str, i32)>, Vec<Instruction>)) -> i64 {
    let mut value_map = HashMap::<&str, i32>::new();

    for (key, val) in values {
        value_map.insert(key, *val);
    }

    let mut still_missing = true;

    while still_missing {
        still_missing = false;
        for ins in instructions {
            let first = value_map.get(&ins.register1);
            if first.is_none() {
                still_missing = true;
                continue;
            }
            let second = value_map.get(&ins.register2);
            if second.is_none() {
                still_missing = true;
                continue;
            }

            let res = match ins.op {
                "OR" => *first.unwrap() | *second.unwrap(),
                "XOR" => *first.unwrap() ^ *second.unwrap(),
                "AND" => *first.unwrap() & *second.unwrap(),
                _ => panic!("Segmentation fault")
            };

            let dest = value_map.entry(&ins.dest_register).or_insert(0);
            *dest = res;
        }
    }

    let z_registers = value_map.iter()
        .filter(|(key, val)| key.starts_with('z'))
        .sorted_by(|f,s| s.0.cmp(f.0))
        .map(|(_, v)| v)
        .fold(0 as i64, |agg,item| {
            (agg * 2) + (*item as i64)
        });

    z_registers
}

pub fn part_two(input: &(Vec<(&str, i32)>, Vec<Instruction>)) -> i32 {
    0
}

pub fn execute(input: &str) -> (String, String) {
    let parsed = parse_input(input);

    println!("{:?}", parsed.1.len());

    let part1 = part_one(&parsed);
    let part2 = part_two(&parsed);

    (part1.to_string(), part2.to_string())
}

//#[tokio::test]
async fn test_day() {
    use crate::input_provider::get_puzzle_input;
    let input = get_puzzle_input(2024, 24).await.unwrap();
    let (part1, part2) = execute (&input);

    assert_eq!("57344080719736", part1);
    assert_eq!("?", part2);
}
