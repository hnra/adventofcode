use crate::solver::DaySolver;
use std::fmt::Display;

pub struct Day4Solver {}
impl DaySolver for Day4Solver {
    fn solve(
        self: &Self,
        input: &str,
    ) -> (Box<(dyn Display + 'static)>, Box<(dyn Display + 'static)>) {
        let (p1, p2) = day04(input);
        (Box::new(p1), Box::new(p2))
    }
}

fn day04(input: &str) -> (usize, usize) {
    let width = input.find("\n").unwrap();
    let words = input.replace("\n", "");
    let height = words.len() / width;

    let p1 = cnt_horizontal_and_diag(&words, width)
        + cnt_horizontal_and_diag(&rotate(&words, width), height);
    let p2 = cnt_xes(&words, width);

    (p1, p2)
}

fn cnt_horizontal_and_diag(input: &str, width: usize) -> usize {
    let height = input.len() / width;
    let mut cnt = 0;
    let chars = input.as_bytes();
    for (i, c) in chars.iter().enumerate() {
        if *c != b'X' && *c != b'S' {
            continue;
        }

        let (x, y) = to_xy(i, width);
        if x + 4 <= width {
            if is_xmas(&chars[i..i + 4]) {
                cnt += 1;
            }

            if y + 4 <= height {
                let diag = vec![
                    chars[i],
                    chars[i + width + 1],
                    chars[i + 2 * (width + 1)],
                    chars[i + 3 * (width + 1)],
                ];
                if is_xmas(&diag) {
                    cnt += 1;
                }
            }
        }
    }

    cnt
}

fn is_xmas(s: &[u8]) -> bool {
    match s {
        b"XMAS" => true,
        b"SAMX" => true,
        _ => false,
    }
}

fn rotate(s: &str, width: usize) -> String {
    let height = s.len() / width;
    let mut new_str_vec = vec![' '; s.len()];

    for (i, c) in s.char_indices() {
        let row = i / width;
        let col = i % width;

        let new_row = col;
        let new_col = row.abs_diff(height - 1);

        let new_i = new_row * height + new_col;
        new_str_vec[new_i] = c;
    }

    new_str_vec.into_iter().collect()
}

fn to_xy(i: usize, width: usize) -> (usize, usize) {
    (i % width, i / width)
}

fn to_i((x, y): &(usize, usize), width: usize) -> usize {
    y * width + x
}

fn nth(xy: &(usize, usize), width: usize, chars: &[u8]) -> Option<u8> {
    let i = to_i(xy, width);
    if i < chars.len() {
        Some(chars[i])
    } else {
        None
    }
}

fn nths(coords: &[(usize, usize)], width: usize, chars: &[u8]) -> Option<Vec<u8>> {
    coords
        .iter()
        .map(|xy| nth(xy, width, &chars))
        .collect::<Option<Vec<u8>>>()
}

fn cnt_xes(input: &str, width: usize) -> usize {
    let height = input.len() / width;
    let mut cnt = 0;
    let chars = input.as_bytes();
    for (i, c) in chars.iter().enumerate() {
        if *c != b'A' {
            continue;
        }

        let (x, y) = to_xy(i, width);
        if x == 0 || y == 0 || x == width - 1 || y == height - 1 {
            continue;
        }

        if nths(&vec![(x - 1, y - 1), (x, y), (x + 1, y + 1)], width, chars)
            .is_some_and(|s| is_mas(&s))
            && nths(&vec![(x + 1, y - 1), (x, y), (x - 1, y + 1)], width, chars)
                .is_some_and(|s| is_mas(&s))
        {
            cnt += 1;
        }
    }

    cnt
}

fn is_mas(s: &[u8]) -> bool {
    match s {
        b"MAS" => true,
        b"SAM" => true,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn example() {
        let example_text = "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX";
        let (p1, p2) = day04(example_text);
        assert_eq!(18, p1);
        assert_eq!(9, p2);
    }

    #[test]
    fn test_is_xmas() {
        assert!(is_xmas(b"XMAS"));
        assert!(is_xmas(b"SAMX"));
        assert!(!is_xmas(b"SMX"));
    }

    #[test]
    fn horizontal() {
        let example_text = "XMAS
SAMX";
        let (p1, _) = day04(example_text);
        assert_eq!(2, p1);
    }

    #[test]
    fn vertical() {
        let example_text = "X00S
M00A
A00M
S00X";
        let (p1, _) = day04(example_text);
        assert_eq!(2, p1);
    }

    #[test]
    fn diagonal() {
        let example_text = "X00S
0MA0
0MA0
X00S";
        let (p1, _) = day04(example_text);
        assert_eq!(2, p1);
    }

    #[test]
    fn diagonal_rev() {
        let example_text = "S00X
0AM0
0AM0
S00X";
        let (p1, _) = day04(example_text);
        assert_eq!(2, p1);
    }

    #[test]
    fn can_find_mas() {
        let example_text = "M.S
.A.
M.S";
        let (_, p2) = day04(&example_text);
        assert_eq!(1, p2);
    }
}
