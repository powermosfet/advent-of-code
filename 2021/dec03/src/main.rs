use std::fs;
use std::iter::FromIterator;


#[derive(Debug)]
enum BitError {
    ParseError(String),
}

#[derive(Debug)]
#[derive(PartialEq)]
enum Bit {
    Zero,
    One,
}

fn not(x: &Bit) -> Bit {
    match x {
        Bit::Zero => Bit::One,
        Bit::One => Bit::Zero,
    }
}

fn parse_bit(input: &str) -> Result<Bit, BitError> {
    match input {
        "0" => Ok(Bit::Zero),
        "1" => Ok(Bit::One),
        _ => Err(BitError::ParseError(input.to_string())),
    }
}

fn most_common_bit(index: usize, line_vector: &Vec<&str>) -> Bit {
    let mut zero_count = 0;
    let mut one_count = 0;


    for line in line_vector.iter() {
        let bit = parse_bit(&line[index .. index + 1]);
        match bit {
            Ok(Bit::Zero) => {
                zero_count += 1;
            }

            Ok(Bit::One) => {
                one_count += 1;
            }

            Err(error) => {
                println!("{:?}", error);
            }
        }
    }

    if zero_count > one_count {
        Bit::Zero
    } else {
        Bit::One
    }
}

fn bit_at(index: usize, line: &str) -> Result<Bit, BitError> {
    parse_bit(&line[index .. index + 1])
}

fn gamma(diagnostics: &Vec<&str>) -> Vec<Bit> {
    let diagnostic_length = diagnostics[0].len();
    let mut gamma_vec = Vec::new();

    for i in 0..diagnostic_length {
        gamma_vec.push(most_common_bit(i,  &diagnostics));
    }

    gamma_vec
}

fn epsilon(gamma_rate: &Vec<Bit>) -> Vec<Bit> {
    gamma_rate.iter().map(not).collect()
}

fn to_decimal(bits: &Vec<Bit>) -> i32 {
    let mut result = 0;
    let mut position = 1;

    for b in bits.iter().rev() {
        result = match b {
            Bit::Zero => result,
            Bit::One => result + position,
        };
        position *= 2;
    }

    result
}

fn puzzle_1(diagnostics: &Vec<&str>) -> i32 {
    let g = gamma(diagnostics);
    let e = epsilon(&g);

    to_decimal(&g) * to_decimal(&e)
}

fn oxygen(index: usize, diagnostics: &Vec<&str>) -> Vec<Bit> {
    let b = most_common_bit(index, &diagnostics);
    let diagnostics_filtered = diagnostics.iter()
            .map(|x| *x)
            .filter(|d| bit_at(index, d).unwrap() == b)
            .collect::<Vec<&str>>();
    if diagnostics_filtered.len() > 1 {
        oxygen(index + 1, &diagnostics_filtered)
    } else {
        diagnostics_filtered
            .first()
            .unwrap()
            .chars()
            .map(|c| parse_bit(&c.to_string()[..]).unwrap())
            .collect()
    }
}

fn co2(index: usize, diagnostics: &Vec<&str>) -> Vec<Bit> {
    let b = most_common_bit(index, &diagnostics);
    let diagnostics_filtered = diagnostics.iter()
            .map(|x| *x)
            .filter(|d| bit_at(index, d).unwrap() != b)
            .collect::<Vec<&str>>();
    if diagnostics_filtered.len() > 1 {
        co2(index + 1, &diagnostics_filtered)
    } else {
        diagnostics_filtered
            .first()
            .unwrap()
            .chars()
            .map(|c| parse_bit(&c.to_string()[..]).unwrap())
            .collect()
    }
}

fn puzzle_2(diagnostics: &Vec<&str>) -> i32 {
    let o = oxygen(0, diagnostics);
    let c = co2(0, diagnostics);

    to_decimal(&o) * to_decimal(&c)
}

fn main() {
    println!("Let's go!");

    let filename = "input.txt";
    let input = fs::read_to_string(filename)
        .expect("Error reading input file");

    let all_lines = Vec::from_iter(str::lines(&input));

    println!("Puzzle #1:");
    let answer1 = puzzle_1(&all_lines);
    println!("Answer: {}", answer1);

    println!("Puzzle #2:");
    let answer2 = puzzle_2(&all_lines);
    println!("Answer: {}", answer2);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_most_common_bit() {
        let i = vec!("00100",
                     "11110",
                     "10110",
                     "10111",
                     "10101",
                     "01111",
                     "00111",
                     "11100",
                     "10000",
                     "11001",
                     "00010",
                     "01010");
        let a = gamma(&i);
        let e = [ Bit::One, Bit::Zero, Bit::One, Bit::One, Bit::Zero ];
        assert_eq!(a, e);

        let n = to_decimal(&a);
        assert_eq!(n, 22);

        assert_eq!(puzzle_1(&i), 198)
    }

    #[test]
    fn test_oxygen() {
        let i = vec!("00100",
                     "11110",
                     "10110",
                     "10111",
                     "10101",
                     "01111",
                     "00111",
                     "11100",
                     "10000",
                     "11001",
                     "00010",
                     "01010");
        let o = oxygen(0, &i);
        assert_eq!(to_decimal(&o), 23);
    }
}
