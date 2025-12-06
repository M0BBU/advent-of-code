use std::{
    fmt,
    fs::File,
    io::{BufRead, BufReader},
};

enum Rotation{
    Left(i32),
    Right(i32),
}

struct Dial {
    pos: i32,
    num: i32,
}

impl fmt::Display for Rotation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Rotation::Left(dist) => write!(f, "Left {dist}"),
            Rotation::Right(dist) => write!(f, "Right {dist}"),
        }
    }
}

pub fn solve() {
    let input_file = File::open("inputs/1.txt")
        .expect("unable to read file");
    let file_reader = BufReader::new(input_file);

    let rotations = file_reader
        .lines()
        .filter_map(|line| {
            let line_res = line.unwrap();
            match parse_rotation(&line_res) {
                Ok(r) => Some(r),
                Err(e) => {
                    println!("got: {e}");
                    None
                },
            }
        })
        .collect::<Vec<Rotation>>();

    let part1 = rotations
        .iter()
        .fold(Dial{
            pos: 50,
            num: 0,
        }, |acc, rot| {
            solve_part1(acc, rot)
        });

    let part2 = rotations
        .iter()
        .fold(Dial{
            pos: 50,
            num: 0,
        }, |acc, rot| {
            solve_part2(acc, rot)
        });

    println!("part1: {}", part1.num);
    println!("part2: {}", part2.num);
}

fn solve_part1(dial: Dial, rot: &Rotation) -> Dial {
    let dist = match rot {
        Rotation::Left(x) => -x,
        Rotation::Right(x) => *x,
    };

    let num = dial.pos + dist;
    let pos = (num).rem_euclid(100);
    Dial{
        pos: pos,
        num: dial.num + if pos == 0 { 1 } else { 0 },
    }
}

fn solve_part2(dial: Dial, rot: &Rotation) -> Dial {
    let dist = match rot {
        Rotation::Left(x) => -x,
        Rotation::Right(x) => *x,
    };

    let num = dial.pos + dist;
    let clicks = if num == 0 {
        1
    } else if num < 0 && dial.pos != 0 {
        (num / 100).abs() + 1
    } else {
        (num / 100).abs()
    };
    let pos = (num).rem_euclid(100);

    Dial{
        pos: pos,
        num: dial.num + clicks,
    }
}

fn parse_rotation(line: &str) -> Result<Rotation, String> {
    let len = line.len();
    if len == 0 {
        return Err(String::from("input not correct length"));
    }

    let direction = line.get(0..1).ok_or("invalid dir input")?;
    let parsed_dist = line.get(1..len).ok_or("invalid parse dir input")?;
    let distance = parsed_dist.parse::<i32>()
        .map_err(|e| String::from(format!("{}", e)))?;

    return match(direction, distance) {
        ("L", d) => Ok(Rotation::Left(d)),
        ("R", d) => Ok(Rotation::Right(d)),
        _ => Err(String::from("invalid direction")),
    };
}
