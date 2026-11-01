let fuel1 mass = (mass / 3) - 2
let solve1 input = List.map fuel1 input |> List.fold_left ( + ) 0

let rec fuel2 mass =
  let f = fuel1 mass in
  if f > 0 then f + fuel2 f else 0

let solve2 input = List.map fuel2 input |> List.fold_left ( + ) 0

let () =
  let lines = Aoc2019.lines () in
  let input = List.map int_of_string lines in
  let solution1 = solve1 input in
  let solution2 = solve2 input in
  Printf.printf "%d\n%d\n" solution1 solution2
