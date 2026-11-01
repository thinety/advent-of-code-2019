let program () =
  let line = read_line () in
  let nums = String.split_on_char ',' line in
  List.map int_of_string nums

let execute memory =
  let rec loop ip =
    match memory.(ip) with
    | 99 -> ()
    | 1 ->
        let x = memory.(memory.(ip + 1)) in
        let y = memory.(memory.(ip + 2)) in
        memory.(memory.(ip + 3)) <- x + y;
        loop (ip + 4)
    | 2 ->
        let x = memory.(memory.(ip + 1)) in
        let y = memory.(memory.(ip + 2)) in
        memory.(memory.(ip + 3)) <- x * y;
        loop (ip + 4)
    | _ -> failwith "invalid opcode"
  in
  loop 0
