type direction = Vertical of int | Horizontal of int

let parse_wire s =
  let parse_direction s =
    let d, n = String.cut_first 1 s in
    let n = int_of_string n in
    match d with
    | "R" -> Horizontal n
    | "L" -> Horizontal (-n)
    | "D" -> Vertical (-n)
    | "U" -> Vertical n
    | _ -> failwith "invalid input"
  in
  String.split_on_char ',' s |> List.map parse_direction

let solve (wire1, wire2) score =
  let f1 (sol, x1, y1, n1) dir1 =
    let f2 (sol, x2, y2, n2) dir2 =
      let intersection u1 u2 v n u v1 v2 m =
        if u >= min u1 u2 && u <= max u1 u2 && v >= min v1 v2 && v <= max v1 v2
        then
          let s = score (abs u + abs v) (n + abs (u - u1) + m + abs (v - v1)) in
          min s sol
        else sol
      in
      let sol =
        match (dir1, dir2) with
        | Horizontal k1, Vertical k2 ->
            intersection x1 (x1 + k1) y1 n1 x2 y2 (y2 + k2) n2
        | Vertical k1, Horizontal k2 ->
            intersection x2 (x2 + k2) y2 n2 x1 y1 (y1 + k1) n1
        | _ -> sol
      in
      match dir2 with
      | Horizontal k2 -> (sol, x2 + k2, y2, n2 + abs k2)
      | Vertical k2 -> (sol, x2, y2 + k2, n2 + abs k2)
    in
    let sol, _, _, _ = List.fold_left f2 (sol, 0, 0, 0) wire2 in
    match dir1 with
    | Horizontal k1 -> (sol, x1 + k1, y1, n1 + abs k1)
    | Vertical k1 -> (sol, x1, y1 + k1, n1 + abs k1)
  in
  let sol, _, _, _ = List.fold_left f1 (max_int, 0, 0, 0) wire1 in
  sol

let solve1 input =
  let score manhattan steps = manhattan in
  solve input score

let solve2 input =
  let score manhattan steps = steps in
  solve input score

let () =
  let wire1 = read_line () |> parse_wire in
  let wire2 = read_line () |> parse_wire in
  let input = (wire1, wire2) in
  let solution1 = solve1 input in
  let solution2 = solve2 input in
  Printf.printf "%d\n%d\n" solution1 solution2
