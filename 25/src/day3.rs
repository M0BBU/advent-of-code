use std::{
    fs,
    mem,
};

pub fn solve() {
    let input_file = fs::read_to_string("inputs/3.txt")
        .expect("unable to read file");

    let part1: usize = input_file
        .trim_end()
        .split('\n')
        .collect::<Vec<&str>>()
        .iter()
        .map(|s| {
            s.chars().map(|c| c as u8 - b'0').collect()
        })
        .collect::<Vec<Vec<u8>>>()
        .iter()
        .map(|nums| {
            let mut max = 0;
            for i in 0..nums.len() {
                for j in (i+1)..nums.len() {
                    let check = ((nums[i] * 10) + nums[j]) as usize;
                    if max < check {
                        max = check;
                    }
                }
            }
            max
        })
        .sum();

    let part2: usize = input_file
        .trim_end()
        .split('\n')
        .collect::<Vec<&str>>()
        .iter()
        .map(|s| {
            s.chars().map(|c| c as u8 - b'0').collect()
        })
        .collect::<Vec<Vec<u8>>>()
        .iter()
        .map(|batteries| {
            get_max_12_digits(batteries)
        })
        .sum();

    println!("{:?}", part1);
    println!("{:?}", part2);
}

fn get_max_12_digits(nums: &Vec<u8>) -> usize {
    let end = nums.len() - 12;
    let mut batteries = nums[end..].to_vec();

    for mut n in nums[..end].iter().copied().rev() {
        for b in batteries.iter_mut() {
            if n < *b {
                break
            }
            mem::swap(&mut n, b);
        }
    }

    batteries
        .iter()
        .map(|&b| b as usize)
        .reduce(|acc, b| (acc * 10) + b)
        .unwrap()
}
