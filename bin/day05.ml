let solve1 input =
  let memory = Array.of_list input in
  Aoc2019.Intcode.execute memory [ 1 ]

let solve2 input =
  let memory = Array.of_list input in
  Aoc2019.Intcode.execute memory [ 5 ]

let () =
  let program = Aoc2019.Intcode.program () in
  let solution1 = solve1 program in
  let solution2 = solve2 program in
  List.iter (fun x -> Printf.printf "%d\n" x) solution1;
  Printf.printf "\n";
  List.iter (fun x -> Printf.printf "%d\n" x) solution2
