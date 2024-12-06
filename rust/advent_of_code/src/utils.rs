use std::fmt::Display;


pub fn print_2d_vec<T>(grid: &Vec<Vec<T>>) where T : Display {
    for i in 0 .. grid.len() {
        for j in 0 .. grid[0].len() {
            print!("{} ", grid[i][j]);
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