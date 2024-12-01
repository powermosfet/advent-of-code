import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import simplifile.{read}

pub fn main() {
  io.println("\nPart 1:")

  let example_input =
    "3   4
4   3
2   5
1   3
3   9
3   3"

  io.println("example:")
  part_one(example_input)
  io.println("")

  io.println("Input file:")
  let filepath = "./src/day01/input.txt"
  case read(from: filepath) {
    Ok(input) -> part_one(input)
    Error(_) -> io.println("Error reading input file")
  }

  io.println("\nPart 2:")
  io.println("example:")
  part_two(example_input)
  io.println("")
  io.println("Input file:")
  case read(from: filepath) {
    Ok(input) -> part_two(input)
    Error(_) -> io.println("Error reading input file")
  }
}

pub fn part_one(input) {
  input
  |> split_lists
  |> sort_both
  |> get_differences
  |> int.sum
  |> int.to_string
  |> io.println
}

pub fn split_lists(input) {
  string.split(input, on: "\n")
  |> list.map(fn(line) {
    use #(first, second) <- result.try(string.split_once(line, on: "   "))
    use first_int <- result.try(int.parse(first))
    use second_int <- result.map(int.parse(second))
    #(first_int, second_int)
  })
  |> list.map(result.unwrap(_, #(0, 0)))
  |> list.unzip
}

pub fn sort_both(lists) {
  let #(list_a, list_b) = lists
  #(list.sort(list_a, by: int.compare), list.sort(list_b, by: int.compare))
}

pub fn get_differences(lists) {
  let #(list_a, list_b) = lists
  list.zip(list_a, list_b)
  |> list.map(fn(pair) {
    let #(a, b) = pair
    int.absolute_value(a - b)
  })
}

pub fn part_two(input) {
  input
  |> split_lists
  |> calculate_score
  |> int.to_string
  |> io.println
}

pub fn calculate_score(lists) {
  let #(list_a, list_b) = lists
  list_a
  |> list.map(fn(id) { id * list.count(list_b, fn(x) { x == id }) })
  |> int.sum
}
