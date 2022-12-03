use std::fs::File;
use std::io;
use std::io::BufRead;
use std::io::BufReader;

fn main() -> Result<(), io::Error> {
    let file_name = "input.txt";
    let file = File::open(file_name)?;
    let reader = BufReader::new(file);
    let mut sums = reader.lines()
        .filter_map(|line| line.ok())
        .fold(Vec::new(), |mut state, line| {
            if line.is_empty() {
                state.push(0)
            }
            else {
                let calories = line.parse::<i32>().unwrap();
                if state.is_empty() {
                    state.push(calories)
                }
                else {
                    let len = state.len();
                    state[len - 1] += calories;
                }
            }
            state
        });

    let part1 = sums.iter().max().unwrap();
    println!("Part 1: {part1}");

    sums.sort();
    sums.reverse();
    let part2 = sums.iter().take(3).sum::<i32>();
    println!("Part 2: {part2}");

    Ok(())
}
