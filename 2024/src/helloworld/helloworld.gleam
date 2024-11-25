import gleam/int
import gleam/io
import gleam/list
import gleam/string
import simplifile.{read}

pub fn main() {
  io.println("Let's count the lines")
  let filepath = "./src/helloworld/input.txt"
  let assert Ok(inlines) = read(from: filepath)
  let lines = string.split(inlines, on: "\n")
  io.println("Line count: " <> int.to_string(list.length(lines)))
}
