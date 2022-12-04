use std::{fs::File, io::{BufReader, BufRead}};

fn parse(line: &str) -> ((i32, i32), (i32, i32)) {
    let parts = line
        .split(',')
        .map(|section| section
            .split('-')
            .map(|v| v.parse::<i32>().unwrap())
            .collect::<Vec<i32>>()
        )
        .collect::<Vec<Vec<i32>>>();
    ((parts[0][0], parts[0][1]), (parts[1][0], parts[1][1]))
}

fn main() {
    let file = File::open("input.txt").expect("Can't open input.txt");
    let reader = BufReader::new(file);
    let input = reader.lines()
        .map(|l| parse(&(l.unwrap())))
        .collect::<Vec<((i32, i32), (i32, i32))>>();
    let part1 = input
        .iter()
        .filter(|((x1, x2), (y1, y2))| (x1 <= y1 && x2 >= y2) || (y1 <= x1 && y2 >= x2))
        .count();
    println!("Part 1: {part1}");

    let part2 = input
        .iter()
        .filter(|((x1, x2), (y1, y2))| (x1 <= y2 && x2 >= y1) || (y1 <= x2 && y2 >= x1))
        .count();
    println!("Part 2: {part2}");
}
