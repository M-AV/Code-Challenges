use regex::Regex;

/// Initially I wanted to capture the integers with the regex, but couldn't figure out how to do it nicely,
/// so ended up just matching the whole 'mul(..,..)' and manually extracting the integers
fn extract_mul_instructions(input: &str) -> Vec<(i32, i32)> {
    let regex = Regex::new(r"mul\((\d+),(\d+)\)").unwrap();

    let matches: Vec<_> = regex
        .find_iter(input)
        .map(|m| m.as_str())
        .map(|m: &str| {
            let comma_idx = m.chars().position(|c| c == ',').unwrap();
            (&m[4..comma_idx], &m[comma_idx+1..&m.len() - 1])
        })
        .map(|(first, second)| (
            first.parse::<i32>().unwrap(), 
            second.parse::<i32>().unwrap()
        ))
        .collect();

    matches
}

pub fn part_one(input: &str) -> i32 {
    let result:i32 = extract_mul_instructions(input)
        .iter()
        .map(|(left, right)| left * right)
        .sum();

    result
}


/// Since I used a Regex for the first part, it was easier just to re-parse it for the 2nd part.
pub fn part_two(input: &str) -> i32 {
    let regex = Regex::new(r"mul\(\d+,\d+\)|do\(\)|don't\(\)").unwrap();

    let matches: Vec<_> = regex
        .find_iter(input)
        .map(|m| m.as_str()).collect();

    let mut is_enabled = true;
    let mut sum = 0;
    for m in matches {
        if m == "don't()" {
            is_enabled = false;
        } else if m == "do()" {
            is_enabled = true;
        } else if is_enabled {
            let comma_idx = m.chars().position(|c| c == ',').unwrap();
            let first = &m[4..comma_idx].parse::<i32>().unwrap();
            let second = &m[comma_idx+1..&m.len() - 1].parse::<i32>().unwrap();
            sum = sum + first * second;
        }
    }

    sum
}

pub fn execute(input: &str) -> (String, String) {
    let part1 = part_one(input);
    let part2 = part_two(input);

    (part1.to_string(), part2.to_string())
}

#[tokio::test]
async fn test_day() {
    use crate::input_provider::get_puzzle_input;
    let input = get_puzzle_input(2024, 3).await.unwrap();
    let (part1, part2) = execute (&input);

    assert_eq!("159892596", part1);
    assert_eq!("92626942", part2);
}
