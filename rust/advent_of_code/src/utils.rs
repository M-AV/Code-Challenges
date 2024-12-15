use std::{fmt::Display, ops::{Div, Mul, Rem}};
use colored::Colorize;
use recursive::recursive;


pub fn print_2d_vec<T>(grid: &Vec<Vec<T>>) where T : Display {
    for i in 0 .. grid.len() {
        for j in 0 .. grid[0].len() {
            print!("{} ", grid[i][j])
        }
        println!("");
    }
}

pub fn print_2d_vec_with_highlight<T>(grid: &Vec<Vec<T>>, highlight: &Vec<(usize, usize)>) where T : Display {
    for i in 0 .. grid.len() {
        for j in 0 .. grid[0].len() {
            if highlight.contains(&(i, j)) {
                print!("{} ", grid[i][j].to_string().red());
            } else {
                print!("{} ", grid[i][j]);
            }
        }
        println!("");
    }
}

pub fn print_2d_vec_with_callback<T, F>(grid: &Vec<Vec<T>>, mut callback: F) 
    where T : Display, F: FnMut(&T, (usize, usize)) -> Option<T>
{
    for i in 0 .. grid.len() {
        for j in 0 .. grid[0].len() {
            let item = &grid[i][j];
            match callback(&item, (j, i)) {
                Some(value) => print!("{} ", value),
                None => print!("{} ", item),
            }
        }
        println!("");
    }
}

pub fn parse_2d_vec(input: &str) -> Vec<Vec<char>> {
    input.lines().map(|l| l.chars().collect::<Vec<char>>()).collect()
}

pub fn add_padding_to_2d_vec<T>(grid: &Vec<Vec<T>>, value: T) -> Vec<Vec<T>> where T : Clone {
    let rows_count = grid.len();
    let colum_count = grid[0].len();

    let mut new_grid = Vec::with_capacity(rows_count + 2);
    new_grid.push(vec![value.clone(); colum_count + 2]);

    for i in 0 .. rows_count {
        let mut row = vec![value.clone(); colum_count + 2];

        for j in 0 .. colum_count {
            row[j+1] = grid[i][j].clone();
        }

        new_grid.push(row);
    }

    new_grid.push(vec![value.clone(); colum_count + 2]);

    new_grid
}

pub fn find_first_index<T>(grid: &Vec<Vec<T>>, value: T) -> (usize, usize) where T : PartialEq, T : Clone {
    let mut current_idx = (0,0);
    'outer: for x in 0 .. grid.len() {
        for y in 0..grid[0].len() {
            let item = grid[x][y].clone();
            if item == value {
                current_idx = (x,y);
                break 'outer;
            }

        }
    }
    current_idx
}

/// Concat two numbers as if they were strings. E.g. 12 & 21 becomes 1221.
pub fn concat_numbers(left: u64, right: u64) -> u64 {
    left * 10u64.pow(right.ilog10() + 1) + right
}

pub fn int_length(value: i64) -> u32 {
    value.checked_ilog10().unwrap_or(0)+1
}

/// Greatest common divisor
#[recursive]
pub fn gcd<T>(a: T, b: T) -> T
where T: Copy + PartialEq + PartialOrd + From<u8> + Rem<Output = T> {
    if a < b {
        return gcd(b, a);
    }
    if b == T::from(0u8) {
        return a;
    }
    gcd(b, a % b)
}

/// Least Common Multiple
pub fn lcm<T>(a: T, b: T) -> T 
where T : Copy + PartialEq + PartialOrd + Mul<Output = T> + Div<Output = T> + From<u8> + Rem<Output = T> {
    a * (b / gcd(a, b))
}
