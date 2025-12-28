use std::{collections, fs};

pub fn solve() {
    let input = fs::read_to_string("inputs/7.txt").expect("invalid file");

    let part1 = solve_part1(&input);
    let part2 = solve_part2(&input);
    println!("{:?}", part1);
    println!("{:?}", part2);
}

fn solve_part1(input: &str) -> usize {
    let diagram = input.trim_end().split('\n').collect::<Vec<&str>>();

    let mut new_diagram = Vec::new();
    let mut skip_c = false;
    new_diagram.push(diagram[0].to_string());

    let mut res = 0;
    for r in diagram[1..].iter().enumerate() {
        let mut new_r = String::new();
        for c in r.1.chars().enumerate() {
            if skip_c {
                skip_c = false;
                continue;
            }
            match c.1 {
                '.' => {
                    let new_c = get_prev_char(&new_diagram, r.0 + 1, c.0);
                    new_r.push(new_c);
                }
                '^' => {
                    let new_c = get_prev_char(&new_diagram, r.0 + 1, c.0);
                    if new_c == '|' {
                        res += 1;
                        new_r.pop();
                        new_r.push(new_c);
                        new_r.push('^');
                        new_r.push(new_c);
                        skip_c = true;
                    } else {
                        new_r.push('^');
                    }
                }
                _ => (),
            };
        }
        new_diagram.push(new_r);
    }

    res
}

fn solve_part2(input: &str) -> usize {
    let diagram = input.trim_end().split('\n').collect::<Vec<&str>>();

    let new_diagram = vec![diagram[0].to_string()];
    let mut seen: collections::HashMap<(usize, usize), usize> =
        collections::HashMap::new();

    count_timelines(&mut seen, &diagram, &new_diagram, 1)
}

fn count_timelines(
    seen: &mut collections::HashMap<(usize, usize), usize>,
    old_diagram: &[&str],
    diagram: &[String],
    r: usize,
) -> usize {
    if r == old_diagram.len() - 1 {
        return 1;
    }

    let row = old_diagram[r];
    let mut new_diagram = diagram.to_vec();
    let mut new_r = old_diagram[r].to_string();
    let mut res = 0;

    for c in row.chars().enumerate() {
        match c.1 {
            '.' => {
                let new_c = get_prev_char(diagram, r, c.0);
                new_r = replace_char(&new_r, c.0, new_c);
                if new_c == '|' {
                    if let Some(&num) = seen.get(&(r, c.0)) {
                        res += num;
                    } else {
                        new_diagram.push(new_r.clone());
                        let result = count_timelines(
                            seen,
                            old_diagram,
                            &new_diagram,
                            r + 1,
                        );
                        res += result;
                        seen.insert((r, c.0), result);
                    }
                }
            }
            '^' => {
                let new_c = get_prev_char(diagram, r, c.0);
                if new_c == '|' {
                    if let Some(&num) = seen.get(&(r, c.0)) {
                        res += num;
                    } else {
                        let left_new_r = replace_char(&new_r, c.0 - 1, new_c);
                        new_diagram.push(left_new_r.clone());
                        let left_result = count_timelines(
                            seen,
                            old_diagram,
                            &new_diagram,
                            r + 1,
                        );
                        new_diagram.pop();

                        let right_new_r = replace_char(&new_r, c.0 + 1, new_c);
                        new_diagram.push(right_new_r.clone());
                        let right_result = count_timelines(
                            seen,
                            old_diagram,
                            &new_diagram,
                            r + 1,
                        );
                        new_diagram.pop();

                        seen.insert((r, c.0), left_result + right_result);
                        res += left_result + right_result;
                    }
                }
            }
            _ => (),
        };
    }

    res
}

fn replace_char(row: &str, idx: usize, ch: char) -> String {
    row.chars()
        .enumerate()
        .map(|(i, c)| if i == idx { ch } else { c })
        .collect()
}

fn get_prev_char(diagram: &[String], x: usize, y: usize) -> char {
    match diagram[x - 1].chars().nth(y) {
        Some('S') => '|',
        Some('^') => '.',
        Some(c) => c,
        None => panic!("no char found"),
    }
}
