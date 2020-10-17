open Printf

module IntSet = Set.Make (struct
  type t = int

  let compare = compare
end)

type solution =
  | Unsolvable
  | Solvable of int option list

let range = [ 0; 1; 2; 3; 4; 5; 6; 7; 8 ]

let chop fn grid n =
  let enumerated = List.mapi (fun i cell -> (i, cell)) grid in
  let included =
    List.filter
      (fun (i, cell) ->
        let row = i / 9 in
        let column = i mod 9 in
        fn row column == n)
      enumerated
  in
  List.map (fun (_, cell) -> cell) included


let row grid n = chop (fun row _ -> row) grid n

let column grid n = chop (fun _ column -> column) grid n

let square grid n = chop (fun row column -> (row / 3 * 3) + (column / 3)) grid n

let valid9 list =
  let not_empty = List.filter_map (fun i -> i) list in
  let unique = not_empty |> IntSet.of_list |> IntSet.to_seq in
  let seq_length = Seq.fold_left (fun n _ -> n + 1) 0 in
  seq_length unique == List.length not_empty


let valid grid =
  let fns = [ row; column; square ] in
  List.for_all (fun fn -> List.for_all (fun n -> valid9 (fn grid n)) range) fns


let print_grid grid =
  List.iteri
    (fun i cell ->
      let s = match cell with None -> "." | Some v -> string_of_int v in
      printf " %s " s ;
      if i mod 9 == 8 then print_endline "" else ())
    grid


let rec solve grid =
  if valid grid
  then
    if List.filter Option.is_none grid == []
    then Solvable grid
    else
      let enumerated = List.mapi (fun i cell -> (i, cell)) grid in
      let to_try, _ =
        List.find (fun (i, cell) -> Option.is_none cell) enumerated
      in
      let put_in list n v =
        List.mapi (fun i cell -> if i == n then Some v else cell) list
      in
      List.fold_left
        (fun acc value_to_try ->
          match acc with
          | Unsolvable ->
              solve (put_in grid to_try value_to_try)
          | Solvable s ->
              Solvable s)
        Unsolvable
        (List.map (( + ) 1) range)
  else Unsolvable


let () =
  (*
  154938276
  679142835
  823756491
  485279613
  731684529
  962315784
  516823947
  297461358
  348597162
  *)
  let grid =
    [
    None;   None;   None;   None;   None;   None;   None;   None;   None;
    Some 6; Some 7; Some 9; Some 1; Some 4; Some 2; Some 8; Some 3; Some 5;
    Some 8; Some 2; Some 3; Some 7; Some 5; Some 6; Some 4; Some 9; Some 1;
    Some 4; Some 8; Some 5; Some 2; Some 7; Some 9; Some 6; Some 1; Some 3;
    None;   None;   None;   None;   None;   None  ; None;   None;   None;
    Some 9; Some 6; Some 2; Some 3; Some 1; Some 5; Some 7; Some 8; Some 4;
    Some 5; Some 1; Some 6; Some 8; Some 2; Some 3; Some 9; Some 4; Some 7;
    Some 2; Some 9; Some 7; Some 4; Some 6; Some 1; Some 3; Some 5; Some 8;
    Some 3; Some 4; Some 8; Some 5; Some 9; Some 7; Some 1; Some 6; Some 2;
    ] [@ocamlformat "disable"]
  in
  let s = solve grid in
  match s with
  | Unsolvable ->
      print_endline "Unsolvable"
  | Solvable s ->
      print_endline "Solvable" ;
      print_grid s
