use std::{fs, ops};

#[derive(Debug, Eq, PartialEq, Clone)]
enum Ingredient {
    Range(ops::RangeInclusive<usize>),
    ID(usize),
}

pub fn solve() {
    let input_file =
        fs::read_to_string("inputs/5.txt").expect("unable to read file");

    let ingredients = input_file
        .trim_end()
        .split('\n')
        .collect::<Vec<&str>>()
        .iter()
        .filter_map(|i| {
            if i.is_empty() {
                return None;
            }
            let ingredient = match i.split_once("-") {
                Some((a, b)) => {
                    let (s, e) = (
                        a.parse::<usize>()
                            .unwrap_or_else(|_| panic!("invalid range")),
                        b.parse::<usize>()
                            .unwrap_or_else(|_| panic!("invalid range")),
                    );
                    Ingredient::Range(ops::RangeInclusive::new(s, e))
                }
                None => {
                    let id = i
                        .parse::<usize>()
                        .unwrap_or_else(|_| panic!("invalid range"));
                    Ingredient::ID(id)
                }
            };
            Some(ingredient)
        })
        .collect::<Vec<Ingredient>>();

    let part1 = solve_part1(&ingredients);
    let part2 = solve_part2(&ingredients);

    println!("{}", part1);
    println!("{}", part2);
}

fn solve_part1(list: &Vec<Ingredient>) -> usize {
    list.iter()
        .filter_map(|i| match i {
            Ingredient::Range(_) => None,
            Ingredient::ID(id) => {
                if check_id(list, id) {
                    Some(1)
                } else {
                    None
                }
            }
        })
        .sum()
}

fn solve_part2(list: &[Ingredient]) -> usize {
    let mut ranges = list
        .iter()
        .filter_map(|i| match i {
            Ingredient::Range(r) => Some(r),
            Ingredient::ID(_) => None,
        })
        .collect::<Vec<&ops::RangeInclusive<usize>>>();
    ranges.sort_by_key(|a| a.start());
    // Forces our for loop below to push the current range.
    ranges.push(&ops::RangeInclusive::new(usize::MAX, usize::MAX));

    let mut merged_ranges = Vec::new();
    let mut cur = ranges[0].clone();
    for r in ranges[1..].iter() {
        if cur.end() < r.start() {
            merged_ranges.push(cur);
            cur = ops::RangeInclusive::new(*r.start(), *r.end());
        } else if cur.end() < r.end() {
            cur = ops::RangeInclusive::new(*cur.start(), *r.end());
        }
    }
    merged_ranges.iter().map(|r| r.end() - r.start() + 1).sum()
}

fn check_id(list: &Vec<Ingredient>, id: &usize) -> bool {
    for i in list {
        let contains = match i {
            Ingredient::Range(r) => r.contains(id),
            _ => false,
        };
        if contains {
            return true;
        }
    }
    false
}
