use regex::Regex;

pub fn day03(puzzle_input: &str) -> (i32, i32) {
    let mul_re = r"mul\((\d+),(\d+)\)";
    let dont_re = r"don't\(\)";
    let do_re = r"do\(\)";

    let p1_re = Regex::new(mul_re).unwrap();
    let mut p1: i32 = 0;
    for m in p1_re.captures_iter(puzzle_input) {
        let a: i32 = m[1].parse().unwrap();
        let b: i32 = m[2].parse().unwrap();
        p1 = p1.checked_add(a.checked_mul(b).unwrap()).unwrap();
    }

    let mut p2: i32 = 0;
    let mut should_do = true;
    let p2_re =
        Regex::new(format!("(?:{})|(?:{})|(?:{})", mul_re, dont_re, do_re).as_str()).unwrap();
    for m in p2_re.captures_iter(puzzle_input) {
        if m.get(1) == None {
            if m[0] == *"do()" {
                should_do = true;
            } else {
                should_do = false;
            }
        } else if should_do {
            let a: i32 = m[1].parse().unwrap();
            let b: i32 = m[2].parse().unwrap();
            p2 = p2.checked_add(a.checked_mul(b).unwrap()).unwrap();
        }
    }

    (p1, p2)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn example() {
        let example_text =
            "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))";
        let (p1, _) = day03(example_text);
        assert_eq!(161, p1);

        let example_text_p2 =
            "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))";
        let (_, p2) = day03(example_text_p2);
        assert_eq!(48, p2);
    }
}
