#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use itertools::Itertools;

fn calculate_checksum(disc: Vec<i32>) -> u64 {
    let result = disc.into_iter()
        .enumerate()
        .map(|(pos, v)| (pos as i64) * (v as i64))
        .filter(|v| *v > 0)
        .sum::<i64>() as u64;
    result
}

fn parse_input(input: &Vec<i32>) -> Vec<i32> {
    let mut disc = vec![-1; input.into_iter().sum::<i32>() as usize];

    // Organize vector, just like in the examples
    let mut block_id_counter = 0;
    let mut disc_idx = 0;
    for i in 0 .. input.len() {
        let value = input[i];
        if i % 2 == 0 {
            for val in (1 ..=value).rev() {
                disc[disc_idx] = block_id_counter;
                disc_idx += 1;
            }
            block_id_counter += 1;
        } else {
            for val in (1 ..=value).rev() {
                disc[disc_idx] = -1;
                disc_idx += 1;
            }
        }
    }
    disc
}

pub fn part_one(input: &Vec<i32>) -> u64 {
    let mut disc = parse_input(input);

    // Move items around
    let mut fill_idx = 0;
    for i in (0 .. disc.len()).rev() {
        if disc[i] == -1 || i <= fill_idx {
            continue;
        }

        for j in fill_idx ..=i {
            if disc[j] != -1 {
                continue;
            }

            disc[j] = disc[i];
            disc[i] = -1;
            fill_idx = j + 1;
            break;
        }
    }

    let result = calculate_checksum(disc);

    result
}


pub fn part_two(input: &Vec<i32>) -> u64 {
    let mut disc = parse_input(input);

    let mut visited_idx = disc.len();

    // Move items around
    for i in (0 .. disc.len()).rev() {
        if disc[i] == -1 || i >= visited_idx {
            continue;
        }

        // Find block size
        let block_end_idx = i;
        let mut block_start_idx = i;
        for j in (0 .. i).rev() {
            if disc[j] != disc[i] {
                block_start_idx = j + 1;
                break;
            }
        }

        visited_idx = block_start_idx;

        let block_size = (block_end_idx - block_start_idx) + 1;
    
        // Find insert place
        'outer: for j in 0 ..=i {
            if disc[j] != -1 {
                continue;
            }
            if j >= block_start_idx {
                break;
            }

            let dest_start_idx = j;
            
            for k in j .. (j + block_size) {
                if disc[k] != -1 {
                    continue 'outer; // Technically we will check some of the same spots multiple times, but it shouldn't matter
                }
            }

            // Move block
            for k in 0 .. block_size {
                disc[dest_start_idx + k] = disc[block_start_idx + k];
                disc[block_start_idx + k] = -1;
            }

            break;
        }
    }
    
    let result = calculate_checksum(disc);
    result
}

pub fn execute(input: &str) -> (String, String) {
    let parsed = input.chars().map(|c| c.to_digit(10).unwrap() as i32).collect_vec();

    let part1 = part_one(&parsed);
    let part2 = part_two(&parsed);

    (part1.to_string(), part2.to_string())
}

#[tokio::test]
async fn test_day() {
    use crate::input_provider::get_puzzle_input;
    let input = get_puzzle_input(2024, 9).await.unwrap();
    let (part1, part2) = execute (&input);

    assert_eq!("6360094256423", part1);
    assert_eq!("6379677752410", part2);
}
