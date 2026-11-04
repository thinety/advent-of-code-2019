let program () =
  let line = read_line () in
  let nums = String.split_on_char ',' line in
  List.map int_of_string nums

let execute memory input =
  let get mode parameter =
    match mode with
    (* position mode *)
    | 0 -> memory.(parameter)
    (* immediate mode *)
    | 1 -> parameter
    | _ -> failwith "invalid parameter mode"
  in
  let set mode parameter value =
    match mode with
    (* position mode *)
    | 0 -> memory.(parameter) <- value
    | _ -> failwith "invalid parameter mode"
  in
  let rec loop ip input =
    let instruction = memory.(ip) in
    let opcode = instruction mod 100 in
    let r = instruction / 100 in
    let mode1 = r mod 10 in
    let r = r / 10 in
    let mode2 = r mod 10 in
    let r = r / 10 in
    let mode3 = r mod 10 in
    match opcode with
    (* halt *)
    | 99 -> []
    (* add *)
    | 1 ->
        let x = get mode1 memory.(ip + 1) in
        let y = get mode2 memory.(ip + 2) in
        set mode3 memory.(ip + 3) (x + y);
        loop (ip + 4) input
    (* multiply *)
    | 2 ->
        let x = get mode1 memory.(ip + 1) in
        let y = get mode2 memory.(ip + 2) in
        set mode3 memory.(ip + 3) (x * y);
        loop (ip + 4) input
    (* input *)
    | 3 -> (
        match input with
        | [] -> failwith "no input"
        | x :: input ->
            set mode1 memory.(ip + 1) x;
            loop (ip + 2) input)
    (* output *)
    | 4 ->
        let x = get mode1 memory.(ip + 1) in
        x :: loop (ip + 2) input
    (* jump if true *)
    | 5 ->
        let x = get mode1 memory.(ip + 1) in
        let jump = get mode2 memory.(ip + 2) in
        loop (if x <> 0 then jump else ip + 3) input
    (* jump if false *)
    | 6 ->
        let x = get mode1 memory.(ip + 1) in
        let jump = get mode2 memory.(ip + 2) in
        loop (if x = 0 then jump else ip + 3) input
    (* less than *)
    | 7 ->
        let x = get mode1 memory.(ip + 1) in
        let y = get mode2 memory.(ip + 2) in
        set mode3 memory.(ip + 3) (if x < y then 1 else 0);
        loop (ip + 4) input
    (* equals *)
    | 8 ->
        let x = get mode1 memory.(ip + 1) in
        let y = get mode2 memory.(ip + 2) in
        set mode3 memory.(ip + 3) (if x = y then 1 else 0);
        loop (ip + 4) input
    | _ -> failwith "invalid opcode"
  in
  loop 0 input
