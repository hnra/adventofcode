pub fn day01(input_str: &str) -> (u32, u32) {
    let (mut left, mut right): (Vec<u32>, Vec<u32>) = input_str
        .lines()
        .map(|l| {
            let mut split = l.split_whitespace();
            let left: u32 = split.next().unwrap().parse().unwrap();
            let right: u32 = split.next().unwrap().parse().unwrap();
            (left, right)
        })
        .unzip();

    left.sort();
    right.sort();

    let part1 = left
        .iter()
        .zip(right.iter())
        .map(|(l, r)| l.abs_diff(*r))
        .sum();

    let mut part2: u32 = 0;
    for l in left.iter() {
        let i = right.partition_point(|r| *r < *l);
        let cnt = right[i..].partition_point(|r| *r < *l + 1);
        let (sum, _) = (*l).overflowing_mul(cnt.try_into().unwrap());
        part2 += sum;
    }

    (part1, part2)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn example() {
        let example_lists = "3   4
4   3
2   5
1   3
3   9
3   3";
        let (p1, p2) = day01(&example_lists);
        assert_eq!(p1, 11);
        assert_eq!(p2, 31);
    }
}
