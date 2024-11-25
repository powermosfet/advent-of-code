use std::fs;

struct Input {
    draw_numbers: Vec<i32>,
    boards: Vec<Board>,
}

impl Input {
    fn parse(input: &String) -> Input {
        let mut paragraphs = input.split("\n\n");
        let draw_numbers = paragraphs
            .next()
            .expect("No paragraphs in input")
            .split(",")
            .map(|s| s.parse().expect("Could not parse draw number"))
            .collect();
        let boards = paragraphs
            .map(|p| Board::from_int(
                    p.split_whitespace()
                      .map(|s| s.parse().expect("Could not parse board number"))
                      )
                )
            .collect();

        Input {
            draw_numbers,
            boards,
        }
    }

    fn find_last_winner(self) -> Option<i32> {
        let mut new_boards : Vec<_>;

        new_boards = self.boards;

        for n in self.draw_numbers.into_iter() {
            new_boards = new_boards.into_iter()
                .map(|b| b.mark(n))
                .collect();

            if new_boards.len() == 1 && new_boards[0].bingo() {
                return Some(n * new_boards[0].sum_unmarked());
            }

            new_boards = new_boards.into_iter()
                .filter(|b| !b.bingo())
                .collect();
        }

        None
    }

    fn find_first_winner(self) -> Option<i32> {
        let mut new_boards : Vec<_>;

        new_boards = self.boards;

        for n in self.draw_numbers.into_iter() {
            new_boards = new_boards.into_iter()
                .map(|b| b.mark(n))
                .collect();
            for b in new_boards.iter() {
                if b.bingo() {
                    return Some(n * b.sum_unmarked());
                }
            }
        }

        None
    }
}

struct Board {
    cells: Vec<Cell>,
}

impl Board {
    fn from_int(values: impl Iterator<Item = i32>) -> Board {
        Board {
            cells: values
                .map(|n| Cell { value: n, marked: false })
                .take(25)
                .collect()
        }
    }

    fn from_cells(cells: Vec<Cell>) -> Board {
        Board { cells }
    }

    fn mark(self, n: i32) -> Board {
        Board::from_cells(self.cells.into_iter()
                   .map(|c| {
                       if c.value == n {
                           Cell { value: n, marked: true }
                       } else {
                           c
                       }
                   })
                   .collect::<Vec<_>>()
                   )
    }

    fn get_row(&self, n: usize) -> [&Cell; 5] {
        let index = n * 5;

        [ &self.cells[index]
        , &self.cells[index + 1]
        , &self.cells[index + 2]
        , &self.cells[index + 3]
        , &self.cells[index + 4]
        ]
    }

    fn get_column(&self, n: usize) -> [&Cell; 5] {
        [ &self.cells[n]
        , &self.cells[n + (5 * 1)]
        , &self.cells[n + (5 * 2)]
        , &self.cells[n + (5 * 3)]
        , &self.cells[n + (5 * 4)]
        ]
    }

    fn bingo(&self) -> bool {
        let sections = [
            self.get_row(0),
            self.get_row(1),
            self.get_row(2),
            self.get_row(3),
            self.get_row(4),
            self.get_column(0),
            self.get_column(1),
            self.get_column(2),
            self.get_column(3),
            self.get_column(4)
        ];

        sections.iter()
            .any(|section| section.iter().all(|cell| cell.marked))
    }

    fn sum_unmarked(&self) -> i32 {
        self.cells.iter()
            .map(|c| if c.marked { 0 } else { c.value })
            .sum()
    }
}

struct Cell {
    value: i32,
    marked: bool,
}

fn main() {
    println!("Let's go!");

    let filename = "input.txt";
    let input = fs::read_to_string(filename)
        .expect("Error reading input file");

    println!("Puzzle #1:");
    let input1 = Input::parse(&input);
    let answer1 = input1.find_first_winner();
    println!("Answer: {:?}", answer1);

    println!("Puzzle #2:");
    let input2 = Input::parse(&input);
    let answer2 = input2.find_last_winner();
    println!("Answer: {:?}", answer2);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_row() {
        let b = Board::from_int(1..26);
        let r = b.get_row(3);

        for (c1, c2value) in r.iter().zip(16..20) {
            assert_eq!(c1.value, c2value); 
        }
    }

    #[test]
    fn test_get_column() {
        let b = Board::from_int(1..26);
        let c = b.get_column(2);

        for (c1, c2value) in c.iter().zip([3, 8, 13, 18, 23].iter()) {
            assert_eq!(c1.value, *c2value); 
        }
    }

    #[test]
    fn test_mark_bingo() {
        let b1 = Board::from_int(1..26);
        let b2 = b1.mark(3);
        let b3 = b2.mark(8);
        let b4 = b3.mark(13);
        let b5 = b4.mark(18);
        assert_eq!(b5.bingo(), false); 

        let b6 = b5.mark(23);
        assert_eq!(b6.bingo(), true); 
    }

    #[test]
    fn test_example() {
        let i = Input {
            draw_numbers: vec![7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1],
            boards: vec![
                Board::from_int(vec![22, 13, 17, 11,  0,
                                      8,  2, 23,  4, 24,
                                     21,  9, 14, 16,  7,
                                      6, 10,  3, 18,  5,
                                      1, 12, 20, 15, 19].into_iter()),
                Board::from_int(vec![ 3, 15,  0,  2, 22,
                                      9, 18, 13, 17,  5,
                                     19,  8,  7, 25, 23,
                                     20, 11, 10, 24,  4,
                                     14, 21, 16, 12,  6].into_iter()),
                Board::from_int(vec![14, 21, 17, 24,  4,
                                     10, 16, 15,  9, 19,
                                     18,  8, 23, 26, 20,
                                     22, 11, 13,  6,  5,
                                      2,  0, 12,  3,  7].into_iter()),
            ],
        };

        let winner = i.find_first_winner().expect("No winner!");
        assert_eq!(winner, 4512);
    }
}
