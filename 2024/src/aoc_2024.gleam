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
    [ part ] -> exec_part(part, dict.get(parts, part))
    _ -> {
      parts
        |> dict.to_list
        |> list.map(_, fn(entry) {
          let #(name, part_fn) = entry
          exec_part(name, Ok(part_fn))
        })
      Nil
    }
  }
}

pub fn exec_part(name, fn_result) {
  io.println("")
  case fn_result {
    Ok(part_fn) -> {
      io.println("Part '" <> name <> "':")
      part_fn()
    }
    Error(_) -> io.println("Could not find part '" <> name <> "'.")
  }
}
