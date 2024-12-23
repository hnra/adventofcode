use std::fs;
use std::path::PathBuf;

use day01::Day1Solver;
use day02::Day2Solver;
use day03::Day3Solver;
use day04::Day4Solver;
use solver::DaySolver;

mod day01;
mod day02;
mod day03;
mod day04;
mod solver;

fn main() {
    // Why not just a vector of functions?!
    let solvers: Vec<Box<dyn DaySolver>> = vec![
        Box::new(Day1Solver {}),
        Box::new(Day2Solver {}),
        Box::new(Day3Solver {}),
        Box::new(Day4Solver {}),
    ];

    if let Some(day_str) = std::env::args().nth(1) {
        let day: usize = day_str.parse().unwrap();
        let solver = &solvers[day - 1];
        use_solver(&**solver, day);
    } else {
        for (i, solver) in solvers.iter().enumerate() {
            let foo = solver;
            let day = i + 1;
            use_solver(&**foo, day);
        }
    }
}

fn use_solver(solver: &dyn DaySolver, day: usize) {
    println!("--- Day {} ---", day);

    let file_name = format!("day{:0width$}", day, width = 2);
    let input = fs::read_to_string(PathBuf::from_iter(["inputs", &file_name].iter()))
        .expect("Cannot read input");

    let (p1, p2) = solver.solve(&input);
    println!("Part 1: {}", p1);
    println!("Part 2: {}\n", p2);
}
