use std::fs;

use day01::day01;

mod day01;

fn main() {
    println!("--- Day 1 ---");
    let contents = fs::read_to_string("inputs/day01").expect("Cannot read input");
    let (d01_p1, d01_p2) = day01(&contents);
    println!("Part 1: {}", d01_p1);
    println!("Part 2: {}", d01_p2);
}
