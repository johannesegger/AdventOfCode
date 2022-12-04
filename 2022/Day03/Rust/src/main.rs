use std::{fs::File, io::{BufReader, Error, BufRead}, collections::HashSet};

fn get_prio (c: char) -> i32 {
    if c >= 'a' && c <= 'z' { c as i32 - 'a' as i32 + 1 }
    else if c >= 'A' && c <= 'Z' { c as i32 - 'A' as i32 + 27 }
    else { panic!("Can't get priority of '{c}'") }
}

fn main() -> Result<(), Error> {
    let file = File::open("input.txt")?;
    let reader = BufReader::new(file);
    let lines = reader.lines().map(|l| l.unwrap()).collect::<Vec<String>>();
    let part1 : i32 = lines.iter()
        .map(|l| {
            let (left, right) = l.split_at(l.len() / 2);
            let left_set = HashSet::<char>::from_iter(left.chars());
            let right_set = HashSet::<char>::from_iter(right.chars());
            let intersection = left_set.intersection(&right_set).next().unwrap();
            return get_prio(intersection.clone());
        })
        .sum();
    println!("Part 1: {part1}");

    let part2 : i32 = lines
        .chunks(3)
        .map(|chunk| {
            let intersection = chunk.iter()
                .map(|line| line.chars().collect::<HashSet<_>>())
                .reduce(|a, b| a.intersection(&b).cloned().collect::<HashSet<_>>())
                .unwrap();
            return get_prio(intersection.iter().next().unwrap().clone());
        })
        .sum();
    println!("Part 2: {part2}");

    return Ok(());
}
