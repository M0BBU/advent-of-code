// This is pretty gross code :(

use std::fs;

pub fn solve() {
    let part1 = solve_part1();
    let part2 = solve_part2();
    println!("{:?}", part1);
    println!("{:?}", part2);
}

fn solve_part1() -> usize {
    let input_file = fs::read_to_string("inputs/6.txt").expect("invalid file");
    let input = input_file
        .trim_end()
        .split('\n')
        .collect::<Vec<&str>>()
        .iter()
        .map(|e| e.split_whitespace().collect())
        .collect::<Vec<Vec<&str>>>();

    let mut res = 0;
    for c in 0..input[0].len() {
        let mut exp = Vec::new();

        for r in input.iter() {
            let s = r[c];
            let n = match s.parse::<usize>() {
                Ok(n) => {
                    exp.push(n);
                    0
                }
                Err(_) => match s {
                    "+" => exp.iter().sum(),
                    "*" => exp.iter().product(),
                    _ => panic!("invalid equation"),
                },
            };
            res += n;
        }
    }
    res
}

fn solve_part2() -> usize {
    let input_file = fs::read_to_string("inputs/6.txt").expect("invalid file");
    let input = input_file.trim_end().split('\n').collect::<Vec<&str>>();

    let mut nums = Vec::new();
    let mut ops = Vec::new();
    let mut result = 0;
    for c in 0..input[0].len() {
        let mut num = String::new();

        for r in input.iter() {
            let s = r.chars().nth(c).unwrap_or(' ');
            match s {
                '0'..='9' => num.push(s),
                '+' | '*' => ops.push(s),
                _ => (),
            }
        }

        if num.is_empty() {
            result += calculate(&nums, &mut ops);

            nums.clear();
            num.clear();
            continue;
        }
        nums.push(num.parse::<usize>().expect("unable to parse as num"));
    }

    result += calculate(&nums, &mut ops);
    result
}

fn calculate(nums: &[usize], ops: &mut Vec<char>) -> usize {
    let op = ops.pop().expect("expected character but none!");
    match op {
        '+' => nums.iter().sum(),
        '*' => nums.iter().product(),
        _ => panic!("invalid operation"),
    }
}
