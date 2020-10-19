open Printf

(* Helper functions that the standard library doesn't provide *)
let _seq_length = Seq.fold_left (fun n _ -> n + 1) 0

let _filteri fn list =
  list
  |> List.mapi (fun i elem -> (i, elem))
  |> List.filter (fun (i, elem) -> fn i elem)
  |> List.map (fun (i, elem) -> elem)


let _findi fn list =
  let index, _ =
    list
    |> List.mapi (fun i elem -> (i, elem))
    |> List.find (fun (i, cell) -> fn cell)
  in
  index


let _put_in list n v = List.mapi (fun i elem -> if i == n then v else elem) list

let _inchar_opt ic = try Some (input_char ic) with End_of_file -> None

(* Types and constants *)
module IntSet = Set.Make (struct
  type t = int

  let compare = compare
end)

type solution =
  | Unsolvable
  | Solvable of int option list

let range = [ 0; 1; 2; 3; 4; 5; 6; 7; 8 ]

(* Main functions *)
let chop fn grid n =
  _filteri
    (fun i cell ->
      let row = i / 9 in
      let column = i mod 9 in
      fn row column == n)
    grid


let row = chop (fun row _ -> row)

let column = chop (fun _ column -> column)

let square = chop (fun row column -> (row / 3 * 3) + (column / 3))

let valid9 list =
  let not_empty = List.filter_map (fun i -> i) list in
  let unique = not_empty |> IntSet.of_list |> IntSet.to_seq in
  _seq_length unique == List.length not_empty


let valid grid =
  let fns = [ row; column; square ] in
  List.for_all (fun fn -> List.for_all (fun n -> valid9 (fn grid n)) range) fns


let is_complete = List.for_all Option.is_some

let rec solve grid =
  if not (valid grid)
  then Unsolvable
  else if is_complete grid
  then Solvable grid
  else
    let next_index = _findi Option.is_none grid in
    List.fold_left
      (fun acc value_to_try ->
        match acc with
        | Unsolvable ->
            solve (_put_in grid next_index (Some value_to_try))
        | Solvable s ->
            Solvable s)
      Unsolvable
      (List.map (( + ) 1) range)


let parse file =
  let ic = open_in file in
  let rec read_char acc =
    match _inchar_opt ic with
    | None ->
        acc
    | Some c ->
        let parsed =
          match c with
          | '.' ->
              Some None
          | _ ->
              let i = int_of_char c in
              if i >= 49 && i <= 57 then Some (Some (i - 48)) else None
        in

        read_char (parsed :: acc)
  in
  let parsed = read_char [] |> List.filter_map (fun x -> x) |> List.rev in
  let length = List.length parsed in
  if length == 81
  then Ok parsed
  else
    Error (sprintf "Invalid Sudoku. Got Length %d, expected length 82" length)


let print_grid grid =
  List.iteri
    (fun i cell ->
      let s = match cell with None -> "." | Some v -> string_of_int v in
      printf " %s " s ;
      if i mod 9 == 8 then print_endline "" else ())
    grid


let () =
  match Array.length Sys.argv with
  | 2 ->
      let filename = Sys.argv.(1) in
      let parsed = parse filename in
      ( match parsed with
      | Error e ->
          print_endline e
      | Ok parsed ->
          print_endline "Loaded Sudoku:" ;
          print_grid parsed ;
          let solution = solve parsed in
          print_endline "" ;
          ( match solution with
          | Unsolvable ->
              print_endline "Unsolvable"
          | Solvable s ->
              print_endline "Solution found" ;
              print_grid s ) )
  | _ ->
      print_endline "Usage: ocaml sudoku.ml <sudoku_file>.sdk"
