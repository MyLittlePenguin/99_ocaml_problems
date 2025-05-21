let id x = x

let l_to_string fn list =
  let rec aux acc = function
    | [] -> acc ^ "]"
    | a :: [] -> acc ^ fn a ^ "]"
    | a :: b -> aux (acc ^ fn a ^ "; ") b
  in
  aux "[" list

(* list of string to string*)
let los_to_string list = l_to_string id list

let loi_to_string list = l_to_string Int.to_string list

(* list of list of strings *)
let lolos_to_string list = list |> List.map los_to_string |> los_to_string
let o_to_string fn = function Some x -> "Some " ^ fn x | None -> "None"

(* option of string *)
let oos_to_string = o_to_string id

(* list of options of string *)
let looos_to_string list = list |> List.map oos_to_string |> los_to_string
