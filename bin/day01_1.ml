let change (instr : string) : int =
    let dir = match instr.[0] with
    | 'L' -> -1
    | 'R' ->  1
    | _ -> raise Exit
    in dir * int_of_string (Str.string_after instr 1) ;;

let step1 (prev, ans) delta : int * int =
    (* Prone to integer overflow, but fast *)
    let next = prev + delta in
    let is_zero = next mod 100 == 0 in
    (next, ans + Bool.to_int is_zero) ;;

let step2 (prev, ans) delta : int * int =
    (* Overflow behavior *)
    let next = (prev + delta mod 100 + 100) mod 100 in

    let full_spins = abs (delta / 100) in

    (* Multiplication by `delta` to set the right sign *)
    let passed_zero = delta * prev > delta * next in
    let end_at_zero = next == 0 in
    let partial_spin = prev != 0 && (end_at_zero || passed_zero) in

    let clicks = full_spins + Bool.to_int partial_spin in
    (next, ans + clicks) ;;

let calc step lines =
    let deltas = List.map change lines in
    List.fold_left step (50, 0) deltas
    |> fun (_, x) -> x ;;

Aoc_25.Gen_exec.exec @@ fun ex lines ->
    let step = match ex with
    | Aoc_25.Gen_exec.One -> step1
    | Aoc_25.Gen_exec.Two -> step2
    in
    calc step lines |> Int.to_string ;;
