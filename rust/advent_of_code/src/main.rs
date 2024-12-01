#![allow(dead_code)]
use std::{str::FromStr, time::Duration};

mod input_provider;
mod y2020;
mod y2024;

pub fn get_solver(year: i32, day: i32) -> Option<fn(&str) -> (String, String)> {
    match (year, day) {
        (2020, 02) => Some(y2020::day02::execute),
        (2024, 01) => Some(y2024::day01::execute),
        _ => None
    }
}

#[tokio::main]
async fn main() {
    let year = 2024;
    let day =  01;

    println!("## Puzzle {}/12-{}", year, day);

    match solve_day(year, day).await {
        Ok((p1, p2, time)) => {
            println!("Solution:");
            println!("   Part 1: {}", p1);
            println!("   Part 2: {}", p2);
            println!("   Time spent: {:.2?}", time);
        },
        Err(err) => println!("Something went wrong.. {}", err)
    }
}

async fn solve_day(year: i32, day: i32) -> Result<(String, String, Duration), String> {
    use std::time::Instant;
    let now = Instant::now();

    let input_result = input_provider::get_puzzle_input(year, day).await;

    match input_result {
        Ok(input) => {

            let solver = get_solver(year, day);

            let (first, second) = match solver {
                Some(x) => x(&input),
                None => ("No solver avail.".to_string(), "No solver avail".to_string())
            };

            let elapsed = now.elapsed();
            Ok((String::from(first), String::from(second), elapsed))
        },
        Err(err) => Err(format!("Unable to get input: {}", err))
    }    
}