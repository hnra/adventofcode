use crate::solver::DaySolver;
use std::fmt::Display;

pub struct Day2Solver {}
impl DaySolver for Day2Solver {
    fn solve(
        self: &Self,
        input: &str,
    ) -> (Box<(dyn Display + 'static)>, Box<(dyn Display + 'static)>) {
        let (p1, p2) = day02(input);
        (Box::new(p1), Box::new(p2))
    }
}

fn day02(input_str: &str) -> (usize, usize) {
    let reports: Vec<Vec<i32>> = input_str
        .lines()
        .map(|l| {
            l.split_whitespace()
                .map(|word| word.parse().unwrap())
                .collect()
        })
        .collect();

    let p1 = reports.iter().filter(|r| p1_is_safe(r)).count();
    let p2 = reports.iter().filter(|r| p2_is_safe(r)).count();
    (p1, p2)
}

fn p1_is_safe(report: &Vec<i32>) -> bool {
    return is_safe(report).is_none();
}

fn is_safe(report: &Vec<i32>) -> Option<usize> {
    let mut ix = 0;
    let is_decreasing = report[0] - report[1] > 0;
    for (curr, next) in report.iter().zip(report.iter().skip(1)) {
        let diff = curr - next;
        if diff > 0 && !is_decreasing {
            return Some(ix);
        } else if diff < 0 && is_decreasing {
            return Some(ix);
        }

        let abs_diff = diff.abs();
        if abs_diff < 1 || abs_diff > 3 {
            return Some(ix);
        }

        ix = ix + 1;
    }

    return None;
}

fn p2_is_safe(report: &Vec<i32>) -> bool {
    if let Some(ix) = is_safe(report) {
        let mut report_clone = report.clone();
        let mut removed_ix = report_clone.remove(ix);
        if is_safe(&report_clone).is_none() {
            return true;
        }
        report_clone.insert(ix, removed_ix);

        if ix + 1 <= report_clone.len() {
            removed_ix = report_clone.remove(ix + 1);
            if is_safe(&report_clone).is_none() {
                return true;
            }
            report_clone.insert(ix + 1, removed_ix);
        }

        if ix > 0 {
            report_clone.remove(ix - 1);
            if is_safe(&report_clone).is_none() {
                return true;
            }
        }

        return false;
    } else {
        return true;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn example() {
        let reports = "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9";
        assert_eq!((2, 4), day02(reports));
    }

    #[test]
    fn p1_is_safe_rules() {
        assert!(!p1_is_safe(&vec![3, 2, 3]));
        assert!(p1_is_safe(&vec![1, 2, 3]));
        assert!(p1_is_safe(&vec![3, 1, -2]));
        assert!(!p1_is_safe(&vec![3, 3, 3]));
        assert!(!p1_is_safe(&vec![1, 5, 10]));
    }
}
