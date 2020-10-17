open Printf

module IntSet = Set.Make (struct
  type t = int

  let compare = compare
end)

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
  Seq.fold_left (fun n _ -> n + 1) 0 unique == List.length not_empty


let valid grid =
  let fns = [ row; column; square ] in
  List.for_all (fun fn -> List.for_all (fun n -> valid9 (fn grid n)) range) fns


let () =
  let grid =
    [
    Some 1; Some 5; Some 4; Some 9; Some 3; Some 8; Some 2; Some 7; Some 6;
    Some 6; Some 7; Some 9; Some 1; Some 4; Some 2; Some 8; Some 3; Some 5;
    Some 8; Some 2; Some 3; Some 7; Some 5; Some 6; Some 4; Some 9; Some 1;
    Some 4; Some 8; Some 5; Some 2; Some 7; Some 9; Some 6; Some 1; Some 3;
    Some 7; Some 3; Some 1; Some 6; Some 8; Some 4; Some 5; Some 2; Some 9;
    Some 9; Some 6; Some 2; Some 3; Some 1; Some 5; Some 7; Some 8; Some 4;
    Some 5; Some 1; Some 6; Some 8; Some 2; Some 3; Some 9; Some 4; Some 7;
    Some 2; Some 9; Some 7; Some 4; Some 6; Some 1; Some 3; Some 5; Some 8;
    Some 3; Some 4; Some 8; Some 5; Some 9; Some 7; Some 1; Some 6; Some 2;
    ] [@ocamlformat "disable"]
  in
  let v = valid grid in
  printf "%b" v


let () = print_endline ""
