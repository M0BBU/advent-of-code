use std::fs;

#[derive(Debug, Eq, PartialEq, Clone)]
struct Coord {
    x: isize,
    y: isize,
    z: isize,
}

pub fn solve() {
    let input = fs::read_to_string("inputs/8.txt").expect("invalid file");
    let coords = input
        .trim_end()
        .split('\n')
        .collect::<Vec<&str>>()
        .iter()
        .map(|n| {
            let nums = n.split(',').collect::<Vec<&str>>();
            Coord {
                x: nums
                    .first()
                    .expect("0 idx not found")
                    .parse::<isize>()
                    .unwrap_or_else(|_| panic!("invalid num")),
                y: nums
                    .get(1)
                    .expect("1 idx not found")
                    .parse::<isize>()
                    .unwrap_or_else(|_| panic!("invalid num")),
                z: nums
                    .get(2)
                    .expect("2 idx not found")
                    .parse::<isize>()
                    .unwrap_or_else(|_| panic!("invalid num")),
            }
        })
        .collect::<Vec<Coord>>();

    let len = coords.len();
    let mut graph = vec![vec![0.0; len]; len];
    let mut pairs = Vec::new();
    for (i, a) in coords.iter().enumerate() {
        let offset = i + 1;
        for (j, b) in (offset..).zip(coords.iter().skip(offset)) {
            pairs.push((i, j));
            graph[i][j] = distance(a, b);
        }
    }
    pairs.sort_by(|(x, y), (i, j)| {
        graph[*x][*y].partial_cmp(&graph[*i][*j]).unwrap()
    });

    let part1 = solve_part1(&pairs, len);
    let part2 = solve_part2(&coords, &pairs, len);
    println!("{}", part1);
    println!("{}", part2);
}

fn solve_part1(pairs: &[(usize, usize)], len: usize) -> usize {
    let mut parents = vec![0; len]
        .iter()
        .enumerate()
        .map(|(a, _)| a)
        .collect::<Vec<usize>>();
    let len = parents.len();
    let mut size = vec![1; len];

    for (x, y) in pairs.iter().take(1000) {
        merge_sets(&mut parents, &mut size, *x, *y);
    }
    size.sort();
    size.iter().rev().take(3).product()
}

fn solve_part2(
    coords: &[Coord],
    pairs: &[(usize, usize)],
    len: usize,
) -> isize {
    let mut parents = vec![0; len]
        .iter()
        .enumerate()
        .map(|(a, _)| a)
        .collect::<Vec<usize>>();
    let len = coords.len();
    let mut size = vec![1; len];

    for (x, y) in pairs.iter() {
        let sz = merge_sets(&mut parents, &mut size, *x, *y);
        if sz == len {
            return coords[*x].x * coords[*y].x;
        }
    }
    panic!("should not have gotten here")
}

fn find_parent(parents: &mut [usize], x: usize) -> usize {
    if parents[x] == x {
        x
    } else {
        let val = find_parent(parents, parents[x]);
        parents[x] = val;
        val
    }
}

fn merge_sets(
    parents: &mut [usize],
    size: &mut [usize],
    x: usize,
    y: usize,
) -> usize {
    let a = find_parent(parents, x);
    let b = find_parent(parents, y);
    if a != b {
        size[a] += size[b];
        // Don't want stale values since we already merged these two sets!
        size[b] = 1;
        parents[b] = a;
        size[a]
    } else {
        size[a]
    }
}

fn distance(c: &Coord, c1: &Coord) -> f64 {
    let x = (c.x - c1.x) * (c.x - c1.x);
    let y = (c.y - c1.y) * (c.y - c1.y);
    let z = (c.z - c1.z) * (c.z - c1.z);

    ((x + y + z) as f64).sqrt()
}
