let solve input noun verb =
  let memory = Array.of_list input in
  memory.(1) <- noun;
  memory.(2) <- verb;
  Aoc2019.Intcode.execute memory;
  memory.(0)

let solve1 input = solve input 12 2

let solve2 input =
  let rec loop noun verb =
    if solve input noun verb == 19690720 then (100 * noun) + verb
    else if noun == 99 then failwith "not found"
    else if verb == 99 then loop (noun + 1) 0
    else loop noun (verb + 1)
  in
  loop 0 0

let () =
  let program = Aoc2019.Intcode.program () in
  let solution1 = solve1 program in
  let solution2 = solve2 program in
  Printf.printf "%d\n%d\n" solution1 solution2
