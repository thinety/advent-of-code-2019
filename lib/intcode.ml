let program () =
  let line = read_line () in
  let nums = String.split_on_char ',' line in
  List.map int_of_string nums

let execute memory input =
  let rec loop ip input =
    let instruction = memory.(ip) in
    let mode i =
      let rec div10 x n = if n <= 0 then x else div10 (x / 10) (n - 1) in
      div10 instruction (i + 1) mod 10
    in
    let parameter i = memory.(ip + i) in
    let get i =
      match mode i with
      (* position mode *)
      | 0 -> memory.(parameter i)
      (* immediate mode *)
      | 1 -> parameter i
      | _ -> failwith "invalid parameter mode"
    in
    let set i value =
      match mode i with
      (* position mode *)
      | 0 -> memory.(parameter i) <- value
      | _ -> failwith "invalid parameter mode"
    in
    let opcode = instruction mod 100 in
    match opcode with
    (* halt *)
    | 99 -> []
    (* add *)
    | 1 ->
        let x = get 1 in
        let y = get 2 in
        set 3 (x + y);
        loop (ip + 4) input
    (* multiply *)
    | 2 ->
        let x = get 1 in
        let y = get 2 in
        set 3 (x * y);
        loop (ip + 4) input
    (* input *)
    | 3 -> (
        match input with
        | [] -> failwith "no input"
        | x :: input ->
            set 1 x;
            loop (ip + 2) input)
    (* output *)
    | 4 ->
        let x = get 1 in
        x :: loop (ip + 2) input
    (* jump if true *)
    | 5 ->
        let x = get 1 in
        let jump = get 2 in
        loop (if x <> 0 then jump else ip + 3) input
    (* jump if false *)
    | 6 ->
        let x = get 1 in
        let jump = get 2 in
        loop (if x = 0 then jump else ip + 3) input
    (* less than *)
    | 7 ->
        let x = get 1 in
        let y = get 2 in
        set 3 (if x < y then 1 else 0);
        loop (ip + 4) input
    (* equals *)
    | 8 ->
        let x = get 1 in
        let y = get 2 in
        set 3 (if x = y then 1 else 0);
        loop (ip + 4) input
    | _ -> failwith "invalid opcode"
  in
  loop 0 input
