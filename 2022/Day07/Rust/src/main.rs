use std::{rc::Rc, cell::Cell};

fn main() {
    use std::time::Instant;
    let now = Instant::now();

    let input = include_str!("..\\input.txt").lines();
    let mut path = String::new();
    let mut dir = Rc::new(Dir { parent: None, size: Cell::new(0) });
    let mut dirs: Vec<Rc<Dir>> = Vec::new();
    for line in input {
        if line.starts_with("$ cd ") {
            let dir_name = &line[5..];
            if dir_name == ".." {
                path.drain(path.rfind('/').unwrap() ..);
                let parent = dir.parent.as_ref().unwrap();
                dir = Rc::clone(parent);
            }
            else {
                let d: Dir;
                if dir_name == "/" {
                    path.clear();
                    d = Dir { parent: None, size: Cell::new(0) };
                }
                else {
                    path.push('/');
                    path.push_str(dir_name);
                    d = Dir { parent: Some(dir), size: Cell::new(0) };
                }
                dirs.push(Rc::new(d));
                dir = Rc::clone(dirs.last().unwrap());
            }
        }
        else if line.chars().next().unwrap().is_digit(10) {
            let size = line[0 .. line.find(' ').unwrap()].parse::<i32>().unwrap();
            let mut parent = Some(&dir);
            loop {
                match parent {
                    Some(p) => {
                        let dir_size = p.size.get();
                        p.size.set(dir_size + size);
                        let grand_parent = p.parent.as_ref();
                        parent = grand_parent;
                    },
                    None => break
                };
            }
        }
    }

    let part1: i32 = dirs.iter().map(|dir| dir.size.get())
        .filter(|v| *v < 100_000)
        .sum();
    println!("Part 1: {part1}");

    let total_used_space = dirs.first().unwrap().size.get();
    let free_space = 70_000_000 - total_used_space;
    let space_to_free = 30_000_000 - free_space;

    let part2 = dirs.iter()
        .map(|v| v.size.get())
        .filter(|v| *v >= space_to_free)
        .min()
        .unwrap();
    println!("Part 2: {part2}");

    let elapsed = now.elapsed();
    println!("Elapsed: {:.2?}", elapsed);
}

struct Dir {
    parent: Option<Rc<Dir>>,
    size: Cell<i32>
}
