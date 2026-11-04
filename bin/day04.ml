let rec range a b =
 fun () -> if a <= b then Seq.Cons (a, range (a + 1) b) else Seq.Nil

let digits n x =
  let rec go i x l =
    if i = 0 then l else go (i - 1) (x / 10) ((x mod 10) :: l)
  in
  go n x []

let solve1 (a, b) =
  let valid password =
    let ds = digits 6 password in
    let rec go had_adjacent last ds =
      match ds with
      | [] -> had_adjacent
      | x :: ds -> last <= x && go (x = last || had_adjacent) x ds
    in
    go false (-1) ds
  in
  Seq.fold_left (fun ans x -> if valid x then ans + 1 else ans) 0 (range a b)

let solve2 (a, b) =
  let valid password =
    let ds = digits 6 password in
    let rec go had_adjacent adjacent last ds =
      match ds with
      | [] -> adjacent = 2 || had_adjacent
      | x :: ds ->
          last <= x
          &&
          if x = last then go had_adjacent (adjacent + 1) x ds
          else go (adjacent = 2 || had_adjacent) 1 x ds
    in
    go false 1 (-1) ds
  in
  Seq.fold_left (fun ans x -> if valid x then ans + 1 else ans) 0 (range a b)

let () =
  let a = read_line () |> int_of_string in
  let b = read_line () |> int_of_string in
  let input = (a, b) in
  let solution1 = solve1 input in
  let solution2 = solve2 input in
  Printf.printf "%d\n%d\n" solution1 solution2
