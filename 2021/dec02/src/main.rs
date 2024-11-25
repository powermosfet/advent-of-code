use std::fs;

enum Move {
    Forward(i32),
    Up(i32),
    Down(i32),
}

#[derive(Debug)]
struct Position {
    depth: i32,
    horizontal: i32,
    aim: i32,
}

fn step(p: Position, m: Move) -> Position {
    match m {
        Move::Forward(amount) => Position {
            horizontal: p.horizontal + amount,
            ..p
        },

        Move::Up(amount) => Position {
            depth: p.depth - amount,
            ..p
        },

        Move::Down(amount) => Position {
            depth: p.depth + amount,
            ..p
        },
    }
}

fn step_2(p: Position, m: Move) -> Position {
    match m {
        Move::Forward(amount) => Position {
            horizontal: p.horizontal + amount,
            depth: p.depth + (p.aim * amount),
            ..p
        },

        Move::Up(amount) => Position {
            aim: p.aim - amount,
            ..p
        },

        Move::Down(amount) => Position {
            aim: p.aim + amount,
            ..p
        },
    }
}

fn parse_move(input: &str) -> Result<Move, &str> {    
    let (command_str, amount_str) = input.split_once(" ")
        .expect("Unable to parse move");
    let amount = amount_str.parse::<i32>()
        .expect("Unable to parse amount");

    match command_str {
        "forward" => Ok(Move::Forward(amount)),
        "up" => Ok(Move::Up(amount)),
        "down" => Ok(Move::Down(amount)),
        _ => Err("Unknown command"),
    }
}

fn main() {
    println!("Let's go!");

    let filename = "input.txt";
    let input = fs::read_to_string(filename)
        .expect("Error reading input file");

    println!("Puzzle #1:");
    let starting_pos = Position 
        { depth: 0
        , horizontal: 0
        , aim: 0 
        };
    let final_pos = str::lines(&input)
        .map(parse_move)
        .map(|move_result| move_result.expect("could not parse the moves"))
        .fold(starting_pos, step);
    println!("Answer: {:?}", final_pos.depth * final_pos.horizontal);

    println!("Puzzle #2:");
    let starting_pos_2 = Position 
        { depth: 0
        , horizontal: 0
        , aim: 0 
        };
    let final_pos = str::lines(&input)
        .map(parse_move)
        .map(|move_result| move_result.expect("could not parse the moves"))
        .fold(starting_pos_2, step_2);
    println!("Answer: {:?}", final_pos.depth * final_pos.horizontal);
}
