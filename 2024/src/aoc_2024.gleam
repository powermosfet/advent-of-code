import gleam/io
import helloworld/helloworld
import gleam/dict
import argv
import gleam/list

pub fn main() {
  io.println("Hello from aoc_2024!")

  let parts = dict.from_list([
    #("helloworld", helloworld.main)
    ])

  case argv.load().arguments {
    [ part ] ->
      case dict.get(parts, part) {
        Ok(part_fn) -> part_fn()
        Error(_) -> io.println("Could not find part '" <> part <> "'.")
      }
    _ -> {
      parts
        |> dict.to_list
        |> list.map(_, fn(entry) {
          let #(name, part_fn) = entry
          io.println("")
          io.println("Part '" <> name <> "':")
          part_fn()
        })
      Nil
    }
  }
}
