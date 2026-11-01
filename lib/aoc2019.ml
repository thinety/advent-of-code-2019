module Intcode = Intcode

let rec lines () =
  try
    let line = read_line () in
    line :: lines ()
  with End_of_file -> []
