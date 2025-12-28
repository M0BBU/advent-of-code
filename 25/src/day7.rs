use std::fs;

pub fn solve() {
    let input = fs::read_to_string("inputs/7.txt").expect("invalid file");
    let diagram = input.trim_end().split('\n').collect::<Vec<&str>>();
    let width = diagram[0].len();

    let mut splits = 0;
    let mut timelines = vec![0; width];
    timelines[width / 2] = 1;

    for r in diagram.iter().skip(2) {
        for c in r.chars().enumerate() {
            // The key idea here is that we only need the *total number* of
            // timelines. Not the actual timelines themselves! This means we
            // can just keep track of the number of timelines that reach each
            // column with a dynamic programming approach :}
            // This conveniently lets us calculate part1 at the same time as
            // well.
            if timelines[c.0] >= 1 && c.1 == '^' {
                splits += 1;

                timelines[c.0 - 1] += timelines[c.0];
                timelines[c.0 + 1] += timelines[c.0];
                timelines[c.0] = 0;
            }
        }
    }

    let num_timelines: usize = timelines.iter().sum();
    println!("{}", splits);
    println!("{}", num_timelines);
}
