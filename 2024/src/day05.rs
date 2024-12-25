use crate::solver::DaySolver;
use std::collections::HashSet;
use std::fmt::Display;

pub struct Day5Solver {}
impl DaySolver for Day5Solver {
    fn solve(
        self: &Self,
        input: &str,
    ) -> (Box<(dyn Display + 'static)>, Box<(dyn Display + 'static)>) {
        let (p1, p2) = day05(input);
        (Box::new(p1), Box::new(p2))
    }
}

fn day05(input: &str) -> (u32, u32) {
    let mut rules = HashSet::new();
    let mut updates = Vec::new();
    let mut is_rules = true;
    for line in input.lines() {
        if line == "" {
            is_rules = false;
            continue;
        }

        if is_rules {
            rules.insert(line.to_string());
        } else {
            updates.push(line.split(",").map(|s| s).collect::<Vec<&str>>());
        }
    }

    let mut p1 = 0;
    let mut p2 = 0;
    for update in updates {
        if gen_rules(&update).all(|u| rules.contains(&u)) {
            let middle = update[update.len() / 2];
            p1 += middle.parse::<u32>().unwrap();
        } else {
            let fixed_update = fix_update(&rules, &update);
            let middle = &fixed_update[fixed_update.len() / 2];
            p2 += middle.parse::<u32>().unwrap();
        }
    }

    (p1, p2)
}

fn gen_rules<'a>(update: &'a [&str]) -> impl Iterator<Item = String> + 'a {
    update.iter().enumerate().flat_map(|(i, page)| {
        update
            .iter()
            .skip(i + 1)
            .map(move |p| format!("{page}|{p}"))
    })
}

// Shouldn't rules be HashSet<&str>?
fn fix_update(rules: &HashSet<String>, update: &[&str]) -> Vec<String> {
    let mut new_update = update
        .iter()
        .map(|u| u.to_string())
        .collect::<Vec<String>>();

    let mut i = 0;
    while i < new_update.len() - 1 {
        let mut is_correct = true;
        let page = &new_update[i];
        for (j, p) in new_update[i + 1..].iter().enumerate() {
            let rule = format!("{page}|{p}");
            if !rules.contains(&rule) {
                is_correct = false;
                new_update.swap(i, i + 1 + j);
                break;
            }
        }

        if is_correct {
            i += 1;
        }
    }

    new_update
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn example() {
        let example_text = "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47";

        let (p1, _) = day05(&example_text);
        assert_eq!(p1, 143);
    }

    #[test]
    fn it_gen_rules() {
        let rules = gen_rules(&vec!["75", "47", "61", "53"]).collect::<Vec<String>>();
        assert_eq!("75|47", rules[0]);
        assert_eq!("75|61", rules[1]);
        assert_eq!("75|53", rules[2]);
        assert_eq!("47|61", rules[3]);
        assert_eq!("47|53", rules[4]);
        assert_eq!("61|53", rules[5]);
    }

    #[test]
    fn it_fixes_update() {
        let update = vec!["75", "97", "47", "61", "53"];
        let rules = HashSet::from(
            [
                "47|53", "97|13", "97|61", "97|47", "75|29", "61|13", "75|53", "29|13", "97|29",
                "53|29", "61|53", "97|53", "61|29", "47|13", "75|47", "97|75", "47|61", "75|61",
                "47|29", "75|13", "53|13",
            ]
            .map(|s| s.to_string()),
        );
        let fixed_update = fix_update(&rules, &update);
        assert_eq!(fixed_update, vec!["97", "75", "47", "61", "53"]);
    }
}
