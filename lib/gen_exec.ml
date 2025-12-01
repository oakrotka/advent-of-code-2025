(* Generic execution of programs *)

type ex_num = One | Two ;;

let exec (fn : ex_num -> string Seq.t -> string) =
    let usage = "args: exnum infile" in
    if Array.length Sys.argv != 3 then
        raise @@ invalid_arg usage;

    let lines =
        In_channel.with_open_text Sys.argv.(2) In_channel.input_lines
        |> List.to_seq
        |> Seq.filter (fun s -> String.length s > 0)
    in

    let ex =
        match int_of_string_opt Sys.argv.(1) with
        | Some 1 -> One
        | Some 2 -> Two
        | _ -> raise @@ invalid_arg usage
    in

    print_endline @@ fn ex lines ;;
