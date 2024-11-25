use std::fs;

fn main() {
    println!("Let's go!");

    let filename = "input.txt";
    let input = fs::read_to_string(filename)
        .expect("Error reading input file");
    let a_iter = str::lines(&input).skip(0).map(|l:&str| l.parse::<i32>().expect("Not a number"));
    let b_iter = str::lines(&input).skip(1).map(|l:&str| l.parse::<i32>().expect("Not a number"));
    let c_iter = str::lines(&input).skip(2).map(|l:&str| l.parse::<i32>().expect("Not a number"));

    let mut previous_depth = 9999999;
    let mut increase_count = 0;

    let sliding_window = a_iter.zip(b_iter.zip(c_iter));

    for(_, (a, (b, c))) in sliding_window.enumerate() {
        let depth = a + b + c;

        if depth > previous_depth  {
            increase_count += 1;
        }

        previous_depth = depth;
    }

    println!("Increase count: {}", increase_count);
}
