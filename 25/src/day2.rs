use std::{
    fs,
};

pub fn solve() {
    let input_file = fs::read_to_string("inputs/2.txt")
        .expect("unable to read file");

    let part1: u64 = input_file
        .trim_end()
        .split(',')
        .collect::<Vec<&str>>()
        .iter()
        .map(|s| {
            let (a, b) = s.split_once("-").expect("invalid range");
            (
                a.parse::<u64>().unwrap_or_else(|_| panic!("invalid range")),
                b.parse::<u64>().unwrap_or_else(|_| panic!("invalid range"))
            )
        })
        .flat_map(|(a,b)| a..=b)
        .filter(|n| {
            let num = n.to_string();
            num[..(num.len() / 2)] == num[(num.len() / 2) ..]
        })
        .sum();

        let part2: u64 = input_file
        .trim_end()
        .split(',')
        .collect::<Vec<&str>>()
        .iter()
        .map(|s| {
            let (a, b) = s.split_once("-").expect("invalid range");
            (
                a.parse::<u64>().unwrap_or_else(|_| panic!("invalid range")),
                b.parse::<u64>().unwrap_or_else(|_| panic!("invalid range"))
            )
        })
        .flat_map(|(a,b)| a..=b)
        .filter(|n| {
            let num = n.to_string();
            for i in 1..=num.len()/2 {
                if check_repeated_digits(&num, i) {
                    return true;
                }
            }
            return false;
        })
        .sum();

    println!("{:?}", part1);
    println!("{:?}", part2);
}

fn check_repeated_digits(num: &str, len: usize) -> bool {
    if num.len() % len != 0 {
        return false;
    }

    let chunk = &num[0..len];
    let num_chunks = num.len() / len;
    for y in 1..num_chunks {
        if &num[y * len..(y+1) * len] != chunk {
            return false
        }
    }
    return true
}
