exception AssertionFailure of string

let fail msg = raise @@ AssertionFailure msg

let eq a b = a = b

let assert_fn (fn: 'a -> 'b -> bool ) a b msg =
  if fn a b
  then ()
  else fail msg

let assert_equals (fn: 'a -> string) a b txt =
  let a = fn a in
  let b = fn b in
  assert_fn eq a b (txt ^ ": " ^ a ^ " <> " ^ b)

let id x = x

let l_to_string fn list =
  let rec aux acc = function
    | [] -> acc ^ "]"
    | a :: [] -> acc ^ fn a ^ "]"
    | a :: b -> aux (acc ^ fn a ^ "; ") b
  in aux "[" list

(* list of string to string*)
let los_to_string list = l_to_string id list

(* list of list of strings *)
let lolos_to_string list =
  list |> (List.map los_to_string) |> los_to_string

let o_to_string fn = function
  | Some x -> "Some " ^ fn x
  | None -> "None"

(* option of string *)
let oos_to_string = o_to_string id

(* list of options of string *)
let looos_to_string list =
  list |> (List.map oos_to_string) |> los_to_string
