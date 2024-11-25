enum Error {
    Unimplemented,
    ParseError,
}

struct Point {
    x: i32,
    y: i32,
}

struct Line {
    start: Point,
    end: Point,
}

impl Line {
    fn parse(input: &str) -> Result<Line, Error> {
        let parts = input.split(" -> ");
        Err(Error::Unimplemented)
    }

    fn parse_many(lines: impl Iterator<Item = &str>) -> Result<Vec<Line>, Error> {
        let r = Vec::new();

        for line in lines {
            let result = Line::parse(line);

            match result {
                Ok(parsed_line) => {
                    r.push(parsed_line);
                },

                Err(_) => {
                    return Err(Error::ParseError);
                },
            }
        }

        r
    }
}

fn main() {
    println!("Hello, world!");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_example() {
        let i = [ 
            "0,9 -> 5,9",
            "8,0 -> 0,8",
            "9,4 -> 3,4",
            "2,2 -> 2,1",
            "7,0 -> 7,4",
            "6,4 -> 2,0",
            "0,9 -> 2,9",
            "3,4 -> 1,4",
            "0,0 -> 8,8",
            "5,5 -> 8,2",
        ];
        let lines : Vec<Line> = Line::parse_many(i)
            .expect("Could not parse input")
            .iter()
            .filter(|l| l.is_horizontal() || l.is_vertical())
            .collect();

        assert_eq!(lines.len(), 5);
    }
}
