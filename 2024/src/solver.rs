use std::fmt::Display;

pub trait DaySolver {
    fn solve(self: &Self, input: &str) -> (Box<dyn Display>, Box<dyn Display>);
}
