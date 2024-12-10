use std::fs;
use std::path::PathBuf;

use day01::day01;

mod day01;

fn main() {
    println!("--- Day 1 ---");
    let contents = fs::read_to_string(PathBuf::from_iter(["inputs", "day01"].iter()))
        .expect("Cannot read input");
    let (d01_p1, d01_p2) = day01(&contents);
    println!("Part 1: {}", d01_p1);
    println!("Part 2: {}", d01_p2);
}
