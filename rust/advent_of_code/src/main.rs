#![allow(dead_code)]
use std::time::Duration;

mod input_provider;
mod y2020;
mod y2024;
mod setup;
mod utils;

pub fn get_solver(year: i32, day: i32) -> Option<fn(&str) -> (String, String)> {
    match (year, day) {
        (2020, 02) => Some(y2020::day02::execute),
        (2024, 01) => Some(y2024::day01::execute),
        (2024, 02) => Some(y2024::day02::execute),
        (2024, 03) => Some(y2024::day03::execute),
        (2024, 04) => Some(y2024::day04::execute),
        (2024, 05) => Some(y2024::day05::execute),
        (2024, 06) => Some(y2024::day06::execute),
        (2024, 07) => Some(y2024::day07::execute),
        (2024, 08) => Some(y2024::day08::execute),
        (2024, 09) => Some(y2024::day09::execute),
        (2024, 10) => Some(y2024::day10::execute),
        (2024, 11) => Some(y2024::day11::execute),
        (2024, 12) => Some(y2024::day12::execute),
        (2024, 13) => Some(y2024::day13::execute),
        (2024, 14) => Some(y2024::day14::execute),
        (2024, 15) => Some(y2024::day15::execute),
        (2024, 16) => Some(y2024::day16::execute),
        (2024, 17) => Some(y2024::day17::execute),
        (2024, 18) => Some(y2024::day18::execute),
        (2024, 19) => Some(y2024::day19::execute),
        (2024, 20) => Some(y2024::day20::execute),
        (2024, 21) => Some(y2024::day21::execute),
        (2024, 22) => Some(y2024::day22::execute),
        (2024, 23) => Some(y2024::day23::execute),
        (2024, 24) => Some(y2024::day24::execute),
        (2024, 25) => Some(y2024::day25::execute),
        _ => None
    }
}

#[tokio::main]

async fn main() {
    // setup::setup_year();
    // return;

    let year = 2024;
    let day =  05;

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