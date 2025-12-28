use std::{
    fs,
    cmp,
};

#[derive(Debug, Eq, PartialEq, Clone)]
enum Coord {
    Empty,
    Paper,
}

pub fn solve() {
    let input_file = fs::read_to_string("inputs/4.txt")
        .expect("unable to read file");

    let map: Vec<Vec<Coord>> = input_file
        .trim_end()
        .split('\n')
        .collect::<Vec<&str>>()
        .iter()
        .map(|s| {
            s.chars()
                .map(|c| {
                    match c {
                        '.' => Coord::Empty,
                        '@' => Coord::Paper,
                        _ => panic!("invalid character"),
                    }
                })
                .collect()
        })
        .collect();

    let part1 = solve_part1(&map);
    let part2 = solve_part2(&map);
    println!("{}", part1);
    println!("{}", part2);
}

fn solve_part1(map: &Vec<Vec<Coord>>) -> usize {
    let mut res = 0;
    for row in map.iter().enumerate() {
        for col in row.1.iter().enumerate() {
            let accessible = match col.1 {
                Coord::Paper => check_paper(&map, row.0, col.0),
                _ => false,
            };
            if accessible {
                res += 1;
            }
        }
    }
    res
}

fn solve_part2(map: &Vec<Vec<Coord>>) -> usize {
    let mut res = 0;
    let mut map_copy = map.clone();
    for row in map.iter().enumerate() {
        for col in row.1.iter().enumerate() {
            let (x, y) = (row.0, col.0);
            let accessible = match col.1 {
                Coord::Paper => check_paper(&map, x, y),
                _ => false,
            };
            if accessible {
                res += 1;
                map_copy[x][y] = Coord::Empty;
            }
        }
    }
    if res == 0 {
        res
    } else {
        res + solve_part2(&map_copy)
    }
}

fn check_paper(map: &Vec<Vec<Coord>>, x: usize, y: usize) -> bool {
    let (start_r, end_r) = (
        x.saturating_sub(1),
        cmp::min(x + 1, map.len() - 1)
    );
    let (start_c, end_c) = (
        y.saturating_sub(1),
        cmp::min(y + 1, map[0].len() - 1)
    );

    let rolls: usize = map
        .iter()
        .skip(start_r)
        .take(end_r - start_r + 1)
        .collect::<Vec<&Vec<Coord>>>()
        .iter()
        .map(|r| {
            r.iter().skip(start_c).take(end_c - start_c + 1)
                .filter(|c| **c == Coord::Paper).count()
        })
        .sum();

    // -1 to not count the current roll we're checking
    if rolls - 1 < 4 { true } else { false }
}
