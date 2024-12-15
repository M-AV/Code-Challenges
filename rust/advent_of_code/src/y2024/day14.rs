#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use std::collections::{HashMap, VecDeque};

use itertools::Itertools;

use crate::utils::print_2d_vec_with_callback;


const GRID_WIDTH: i32 = 101;
const GRID_HEIGHT: i32 = 103;


fn parse_input(input: &str) -> Vec<((i32,i32), (i32, i32))> {
    let s = input.lines()
        .map(|l| l.split_once(" v=").unwrap())
        .map(|(l,r)| (&l[2..], r))
        .map(|(l,r)| (l.split_once(',').unwrap(), r.split_once(',').unwrap()))
        .map(|(l,r)| (
            (l.0.parse::<i32>().unwrap(), l.1.parse::<i32>().unwrap()), 
            (r.0.parse::<i32>().unwrap(), r.1.parse::<i32>().unwrap())
        ))
        .collect_vec();
    s
}

fn calc_next_loc(p: (i32,i32), v: (i32,i32)) -> (i32, i32) {
    let mut new_x = p.0 + v.0;
    let mut new_y =  p.1 + v.1;

    while new_x < 0 {
        new_x = GRID_WIDTH + new_x;
    }
    while new_y < 0 {
        new_y = GRID_HEIGHT + new_y;
    }

    new_x = new_x % GRID_WIDTH;
    new_y = new_y % GRID_HEIGHT;

    (new_x, new_y)
}

pub fn part_one(input: &Vec<((i32,i32), (i32, i32))>) -> i64 {
    let mut queue = VecDeque::with_capacity(input.len());
    let mut temp_queue = VecDeque::with_capacity(input.len());

    input.iter().for_each(|l| queue.push_back(l.clone()));

    for i in 0 .. 100 {
        while let Some((p, v)) = queue.pop_front() {
            let next_loc = calc_next_loc(p, v); 
            temp_queue.push_back((next_loc, v));
        }

        (queue, temp_queue) = (temp_queue, queue);
    }

    let mut quadrants = vec![0, 0, 0, 0];


    let x_center = GRID_WIDTH / 2;
    let y_center = GRID_HEIGHT / 2;

    queue.iter().for_each(|((x,y),v)| {
        if *x < x_center && *y < y_center {
            quadrants[0] += 1;
        } else if *x > x_center && *y < y_center {
            quadrants[1] += 1;
        } else if *x < x_center && *y > y_center {
            quadrants[2] += 1;
        } else if *x > x_center && *y > y_center {
            quadrants[3] += 1;
        }
    });


    println!("{:?}", quadrants);

    quadrants[0] * quadrants[1] * quadrants[2] * quadrants[3]
}

pub fn part_two(input: &Vec<((i32,i32), (i32, i32))>) -> i32 {
    let grid: Vec<Vec<char>> = vec![vec!['.'; GRID_WIDTH as usize]; GRID_HEIGHT as usize];

    let mut queue = VecDeque::with_capacity(input.len());
    let mut temp_queue = VecDeque::with_capacity(input.len());

    input.iter().for_each(|l| queue.push_back(l.clone()));

    let mut iteration_counter = 0;

    // I assumed the christmas tree must be in the middle, so added a few locs I thought should be filled.
    // These 4 locs works for my input, but is not guaranteed to work for all.
    let locs_needed_for_christmas_tree = vec![(50, 51), (51,51), (52,51), (53,51)];

    loop {
        while let Some((p, v)) = queue.pop_front() {
            let next_loc = calc_next_loc(p, v); 
            temp_queue.push_back((next_loc, v));
        }

        (queue, temp_queue) = (temp_queue, queue);
        iteration_counter += 1;


        if locs_needed_for_christmas_tree.iter().filter(|(x,y)| {
            let count = queue.iter()
                .filter(|(p,_)| p.0 == *x && p.1 == *y)
                .collect_vec()
                .len();
            count == 0
        }).collect_vec().len() == 0 {
            // Uncomment to see the Christmas tree
            // print_2d_vec_with_callback(&grid, |v, (x,y)| {
            //     let count = queue.iter()
            //         .filter(|(p,_)| p.0 == x as i32 && p.1 == y as i32)
            //         .collect_vec()
            //         .len();
                
            //     if count > 0 {
            //         return Some('#');
            //     }

            //     None
            // });
            return iteration_counter;
        }
    }

    
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
    let input = get_puzzle_input(2024, 14).await.unwrap();
    let (part1, part2) = execute (&input);

    assert_eq!("228410028", part1);
    assert_eq!("8258", part2);
}
