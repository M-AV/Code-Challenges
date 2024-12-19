#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use std::process::Output;

use itertools::Itertools;

fn parse_input(input: &str) -> (Vec<i64>, Vec<u8>) {
    let s = input.lines()
        .filter(|l| l.len() > 0)
        .map(|l| l.split_once(": ").unwrap().1)
        .collect_vec();


    let registers = vec![
        s[0].parse::<i64>().unwrap(),
        s[1].parse::<i64>().unwrap(),
        s[2].parse::<i64>().unwrap()
    ];

    let program = s[3].split(',').map(|v| v.parse::<u8>().unwrap()).collect_vec();

    (registers, program)
}

fn map_combo_operand(register: &Vec<i64>, val: u8) -> i64 {
    match val {
        0 | 1 | 2 | 3 => val as i64,
        4 => register[0],
        5 => register[1],
        6 => register[2],
        7 => panic!("ILLEGAL PROGRAMOS!"),
        _ => panic!("Invalid combo")
    }
}

fn simulate_program(registers_: &Vec<i64>, program: &Vec<u8>, break_for_part2: bool) -> Vec<i64> {
    let mut registers = registers_.clone();

    let mut pointer = 0;
    let mut output = vec![];

    loop {
        if pointer > program.len() - 2 {
            break;
        }

        let opcode = program[pointer];
        let operand = program[pointer + 1];

        match opcode {
            0 => {
                let combo = map_combo_operand(&registers, operand);
                let denominator = 2_i64.pow(combo as u32);
                registers[0] = registers[0] / denominator;
            },
            1 => {
                registers[1] = registers[1] ^ operand as i64;
            },
            2 => {
                let combo = map_combo_operand(&registers, operand);
                registers[1] = combo % 8;
            },
            3 => {
                if registers[0] != 0 {
                    pointer = operand as usize;
                    continue;
                }
            },
            4 => {
                registers[1] = registers[1] ^ registers[2]
            },
            5 => {
                let combo = map_combo_operand(&registers, operand);
                let result = combo % 8;

                output.push(result);
            },
            6 => {
                let combo = map_combo_operand(&registers, operand);
                let denominator = 2_i64.pow(combo as u32);
                registers[1] = registers[0] / denominator;
            },
            7 => {
                let combo = map_combo_operand(&registers, operand);
                let denominator = 2_i64.pow(combo as u32);
                registers[2] = registers[0] / denominator;
            }
            _ => panic!("So much chaos")
        }

        pointer += 2;
    }

    output
}

pub fn part_one((registers_, program): &(Vec<i64>, Vec<u8>)) -> String {
    let output = simulate_program(registers_, program, false);

    output.into_iter().join(",")
}

pub fn part_two((registers_, program): &(Vec<i64>, Vec<u8>)) -> i64 {
    2
}

pub fn execute(input: &str) -> (String, String) {
    let parsed = parse_input(input);

    // println!("{:?}", parsed);

    let part1 = part_one(&parsed);
    let part2 = part_two(&parsed);

    (part1.to_string(), part2.to_string())
}

#[tokio::test]
async fn test_day() {
    use crate::input_provider::get_puzzle_input;
    let input = get_puzzle_input(2024, 17).await.unwrap();
    let (part1, part2) = execute (&input);

    assert_eq!("2,0,4,2,7,0,1,0,3", part1);
    // assert_eq!("?", part2);
}
