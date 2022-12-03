use std::fs::File;
use std::io;
use std::io::BufRead;
use std::io::BufReader;

fn get_score(line: &str) -> i32 {
    match line {
        "A X" => 1 + 3,
        "A Y" => 2 + 6,
        "A Z" => 3 + 0,
        "B X" => 1 + 0,
        "B Y" => 2 + 3,
        "B Z" => 3 + 6,
        "C X" => 1 + 6,
        "C Y" => 2 + 0,
        "C Z" => 3 + 3,
        x => panic!("Invalid line \"{x}\"")
    }
}

fn get_game(line: &str) -> String {
    match line {
        "A X" => "A Z".into(),
        "A Y" => "A X".into(),
        "A Z" => "A Y".into(),
        "B X" => "B X".into(),
        "B Y" => "B Y".into(),
        "B Z" => "B Z".into(),
        "C X" => "C Y".into(),
        "C Y" => "C Z".into(),
        "C Z" => "C X".into(),
        x => panic!("Invalid line \"{x}\"")
    }
}

fn main() -> Result<(), io::Error> {
    let file = File::open("input.txt")?;
    let reader = BufReader::new(file);
    let lines = reader.lines().map(|l| l.unwrap()).collect::<Vec<String>>();

    let part1 : i32 = lines.iter()
        .map(|l| get_score(&l))
        .sum();
    println!("Part1: {part1}");

    let part2 : i32 = lines.iter()
        .map(|l| get_game(&l))
        .map(|l| get_score(&l))
        .sum();
    println!("Part2: {part2}");

    return Ok(());
}
