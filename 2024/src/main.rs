use std::fs;
use std::path::PathBuf;

use day01::day01;
use day02::day02;

mod day01;
mod day02;

fn main() {
    println!("--- Day 1 ---");
    let mut contents = fs::read_to_string(PathBuf::from_iter(["inputs", "day01"].iter()))
        .expect("Cannot read input");
    let (d01_p1, d01_p2) = day01(&contents);
    println!("Part 1: {}", d01_p1);
    println!("Part 2: {}", d01_p2);

    println!("\n--- Day 2 ---");
    contents = fs::read_to_string(PathBuf::from_iter(["inputs", "day02"].iter()))
        .expect("Cannot read input");
    let (d02_p1, d02_p2) = day02(&contents);
    println!("Part 1: {}", d02_p1);
    println!("Part 2: {}", d02_p2);
}
