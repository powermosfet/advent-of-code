import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import simplifile.{read}

pub fn main() {
  io.println("\nPart 1:")

  let example_input =
    "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"

  io.println("example:")
  part_one(example_input)
  io.println("")

  io.println("Input file:")
  let filepath = "./src/day02/input.txt"
  case read(from: filepath) {
    Ok(input) -> part_one(input)
    Error(_) -> io.println("Error reading input file")
  }
}

type Status {
  Unknown
  Increasing
  Decreasing
}

pub fn part_one(input) {
  input
  |> parse_reports
  |> list.map(classify_report)
  |> list.filter(is_safe)
  |> list.length
  |> int.to_string
  |> io.println
}

fn classify_report(report) {
  list.zip(report, list.drop(report, 1))
  |> list.try_fold(Unknown, fn(status, pair) {
    let #(a, b) = pair
    let diff = b - a
    case status, diff {
      _, 0 -> Error(Nil)
      _, diff if diff > 3 -> Error(Nil)
      _, diff if diff < -3 -> Error(Nil)
      Unknown, diff if diff > 0 -> Ok(Increasing)
      Unknown, diff if diff < 0 -> Ok(Decreasing)
      Increasing, diff if diff < 0 -> Error(Nil)
      Increasing, diff if diff > 0 -> Ok(Increasing)
      Decreasing, diff if diff < 0 -> Ok(Decreasing)
      Decreasing, diff if diff > 0 -> Error(Nil)
      // _, _ -> Error(Nil)
    }
  })
}

fn is_safe(status) {
  status == Ok(Increasing) || status == Ok(Decreasing)
}

fn parse_reports(input) {
  input
  |> string.split("\n")
  |> list.try_map(fn(line) {
    line
    |> string.split(" ")
    |> list.try_map(int.parse)
  })
  |> result.unwrap([])
}

pub fn part_two(input) {
  todo
}
