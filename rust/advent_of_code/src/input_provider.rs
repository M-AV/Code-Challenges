use std::{fs::{self, File}, io::Write};

use reqwest::{header::{HeaderMap, HeaderValue, COOKIE}, Client};

pub async fn get_puzzle_input(year: i32, day: i32) -> Result<String, String> {
    let file_loc = get_file_location(year, day);
    let input = fs::read_to_string(file_loc);

    match input {
        Ok(s) => Ok(s),
        Err(_) => {
            let input = download_puzzle_input(year, day).await;
            
            match input {
                Ok(s) => {
                    if !s.starts_with("Puzzle inputs differ by user.  Please log in") {
                        save_input(year, day, &s);  
                        Result::Ok(s)
                    } else {
                        Result::Err(s)
                    }
                },
                Err(err) => Err(err)
            }
        }
    }
}

async fn download_puzzle_input(year: i32, day:i32) -> Result<String, String> {
    let url = get_url(year, day);
    let token = get_auth_token();

    let cookie_format = format!("session={}", token);
    let header_value = HeaderValue::from_str(&cookie_format).unwrap();

    let mut headers = HeaderMap::new();
    headers.insert(COOKIE, header_value);

    let cl = Client::new();
    let response = cl.get(url).headers(headers).send().await;

    match response {
        Ok(s) => {
            let mut input = s.text().await.unwrap();
            if input.ends_with('\n') {
                input.pop(); // Remove last newline if there is one
            }
            Ok(input)
        },
        Err(err) => Err(format!("Request failed: {}", err))
    }
}

fn save_input(year: i32, day: i32, input: &String) {
    let path = get_file_location(year, day);

    let mut file = File::create(path).unwrap();
    file.write_all(input.as_bytes()).unwrap();
}

fn get_auth_token() -> String {
    let s = fs::read_to_string("authtoken.txt");
    // Remove any extra chars that might have been introduced
    s.unwrap().trim_matches(|c: char| c.is_whitespace() || c == '\u{FEFF}').to_string()
}

fn get_url(year: i32, day: i32) -> String {
    format!("https://adventofcode.com/{}/day/{}/input", year, day)
}

fn get_file_location(year: i32, day: i32) -> String {
    format!("inputs/{}-{:02}.txt", year, day)
}