#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use std::process::Output;

use itertools::Itertools;

/// I don't think i128 is necessary, but I had an overflow at some point (due to a different error)
/// so I adjusted it never adjusted it back
fn parse_input(input: &str) -> (Vec<i128>, Vec<u8>) {
    let s = input.lines()
        .filter(|l| l.len() > 0)
        .map(|l| l.split_once(": ").unwrap().1)
        .collect_vec();


    let registers = vec![
        s[0].parse::<i128>().unwrap(),
        s[1].parse::<i128>().unwrap(),
        s[2].parse::<i128>().unwrap()
    ];

    let program = s[3].split(',').map(|v| v.parse::<u8>().unwrap()).collect_vec();

    (registers, program)
}

fn map_combo_operand(register: &Vec<i128>, val: u8) -> i128 {
    match val {
        0 | 1 | 2 | 3 => val as i128,
        4 => register[0],
        5 => register[1],
        6 => register[2],
        7 => panic!("ILLEGAL PROGRAMOS!"),
        _ => panic!("Invalid combo")
    }
}

fn simulate_program(registers_: &Vec<i128>, program: &Vec<u8>) -> Vec<i128> {
    let mut registers = registers_.clone();

    let mut pointer = 0;
    let mut output = vec![];

    loop {
        if pointer > program.len() - 2 {
            break;
        }

        // println!("{:?}", registers);

        let opcode = program[pointer];
        let operand = program[pointer + 1];

        match opcode {
            0 => {
                let combo = map_combo_operand(&registers, operand);
                let denominator = 2_i128.pow(combo as u32);
                registers[0] = registers[0] / denominator;
            },
            1 => {
                registers[1] = registers[1] ^ operand as i128;
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
                let denominator = 2_i128.pow(combo as u32);
                registers[1] = registers[0] / denominator;
            },
            7 => {
                let combo = map_combo_operand(&registers, operand);
                let denominator = 2_i128.pow(combo as u32);
                registers[2] = registers[0] / denominator;
            }
            _ => panic!("So much chaos")
        }

        pointer += 2;
    }

    output
}

pub fn part_one((registers_, program): &(Vec<i128>, Vec<u8>)) -> String {
    let output = simulate_program(registers_, program);

    output.into_iter().join(",")
}


/// 2,4 : A % 8 -> B       - Cut value down to 0-7 (3 bits)
/// 1,7 : B ^ 7 -> B       - Flip all bits
/// 7,5 : A / 2^B -> C     - Takes a drastically reduced A value into C
/// 1,7 : B ^ 7 -> B       - Flip all bits back
/// 0,3 : A / 8 -> A       - Reduce A
/// 4,1 : B ^ C -> B       - 
/// 5,5 : Print register B - XOR between B and C gets printed
/// 3,0 : Jump to start (unless A = 0)
/// 
/// Take aways:
///  - A is always reduced by 7/8 every iteration
///  - No negative numbers
///  - We have to end at 0
///  - I don't know what the 7,5 value can become, but since the output is always between 0 and 7,
///    I assume it will never be higher than that.
///    Hence, we are always working on the "smallest" bits and removing them for the next iteration
/// 
///  It should be possible to find a way to generate the last value. Multiply that by 8 and do the 
///  same for the other values
pub fn part_two((registers_, program): &(Vec<i128>, Vec<u8>)) -> i128 {
    let mut possible_values = vec![];
    let mut possible_values_2 = vec![];

    // Here I just brute-force possible initial values (i.e. which values gives the 2 last entries)
    for i in 0 .. 64 {
        let mut registers = registers_.clone();
        registers[0] = i;

        let output = simulate_program(&registers, program);

        if output.len() == 2 && output[0] == 3 && output[1] == 0 {
            println!("Init: {} {:o} {:?}", i, i, output);
            possible_values.push(i);
        }
    }
    
    // For each possible value so far, find possible new values.
    // Keep going until we have all values
    let mut output_idx = program.len() - 3;
    loop {
        while let Some(value) = possible_values.pop() {
            let new_value = value * 8;

            for i in 0 .. 8 {
                let mut registers = registers_.clone();
                registers[0] = new_value + i;
                let program_result = simulate_program(&registers, program);
                if program_result[0] == program[output_idx] as i128 {
                    possible_values_2.push(new_value + i)
                }
            }
        }

        possible_values = possible_values_2.clone();
        possible_values_2.clear();

        if output_idx == 0 {
            break;
        }
        output_idx -= 1;
    }


    *possible_values.iter().min().unwrap()
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
    let input = get_puzzle_input(2024, 17).await.unwrap();
    let (part1, part2) = execute (&input);

    assert_eq!("2,0,4,2,7,0,1,0,3", part1);
    assert_eq!("265601188299675", part2);
}
