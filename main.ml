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

(* 1. *)
let rec last = function
  | [] -> None
  | [head] -> Some head
  | head :: tail -> (last [@tailcall]) tail

(* 2. *)
let rec last_two = function
  | [] | [_] -> None
  | [a; b] -> Some (a, b)
  | _ :: b -> (last_two [@tailcall]) b

(* 3. *)
let rec at i list =
  match i, list with
  | i, [] -> None
  | i, head :: tail -> if i > 0 then (at [@tailcall]) (i - 1) tail else Some head

(* 4. *)
let length list =
  let rec aux l i = match l, i with
  | [], i -> i
  | a :: b, i -> (aux [@tailcall]) b (i + 1)
  in aux list 0

(* 5. *)
let rev list =
  let rec aux list acc =
    match list, acc with
    | [], a -> a
    | h :: t, a -> (aux [@tailcall]) t (h :: a)
  in aux list []

(* 6. *)
let is_palindrome list =
  list = rev list
  (* the solution below works as well but is more complicated *)
  (* let len = (length list) / 2 in *)
  (* let rec second_half i acc = function *)
  (*   | [] -> acc *)
  (*   | hd :: tl when i < len -> (second_half [@tailcall]) (i + 1) [] tl *)
  (*   | hd :: tl -> (second_half [@tailcall]) (i + 1) (hd :: acc) tl *)
  (* in *)
  (* let reversed_tail = second_half 0 [] list in *)
  (* let rec compare list reversed = *)
  (*   match list, reversed with *)
  (*   | _, [] | [], _ -> true *)
  (*   | a :: t, ar :: tr when a <> ar -> false *)
  (*   | a :: t, ar :: tr -> (compare [@tailcall]) t tr *)
  (* in compare list reversed_tail *)

(* 7. *)
type 'a node =
  | TOne of 'a
  | TMany of 'a node list

let flatten tree =
  let rec aux acc = function
    | [] -> acc
    | TOne hd :: tail ->  (aux [@tailcall]) (hd :: acc) tail
    | TMany hd :: tail -> (aux [@tailcall]) (aux acc hd) tail
  in aux [] tree |> rev

(* 8. *)
let rec compress list =
  match list with
  | a :: (b :: _ as tail) when a = b -> compress tail
  | a :: (b :: _ as tail) -> a :: (compress tail)
  | a :: [] -> [a]
  | [] -> []

(* 9. *)
let pack list =
  let rec aux acc group = function
    | [] -> []
    | [a] -> (a :: group) :: acc |> rev
    | a :: (b :: _ as tail) when a = b -> (aux [@tailcall]) acc (a :: group) tail
    | a :: tail -> (aux [@tailcall]) ((a :: group) :: acc) [] tail
  in aux [] [] list

(* 10. *)
let encode list =
  let rec aux acc counter = function
    | [] -> []
    | [a] -> (counter, a) :: acc
    | a :: (b :: _ as tail) when a = b -> (aux [@tailcall]) acc (counter + 1) tail
    | a :: (b :: _ as tail) -> (aux [@tailcall]) ((counter, a) :: acc) 1 tail
  in aux [] 1 list |> rev

(* 11. && 13. *)
type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode_2 list =
  let to_entry value = function
    | 1 -> One value
    | counter -> Many (counter, value)
  in
  let rec aux acc counter = function
    | [] -> []
    | [a] -> to_entry a counter :: acc
    | a :: (b :: _ as tail) when a = b -> (aux [@tailcall]) acc (counter + 1) tail
    | a :: (b :: _ as tail) -> (aux [@tailcall]) (to_entry a counter :: acc) 1 tail
  in aux [] 1 list |> rev

let encode_2_to_string list = 
  let entry_to_string it = match it with
    | One x -> "One " ^ x
    | Many (i, x) -> "Many (" ^ Int.to_string i ^ ", " ^ x ^ ")"
  in
  list |> (List.map entry_to_string) |> los_to_string

(* 12. *)
let decode list =
  let rec expand acc = function
    | One x -> x :: acc
    | Many (i, x) when i = 1 -> x :: acc
    | Many (i, x) -> expand (x :: acc) (Many (i - 1, x))
  in
  let rec aux acc = function
    | [] -> []
    | [x] -> expand acc x
    | hd :: tl -> aux (expand acc hd) tl
  in aux [] list |> rev

let () =
  assert_equals oos_to_string (last ["a" ; "b" ; "c" ; "d"]) (Some "d") "last";
  assert_equals oos_to_string (last []) (None) "last";
  assert_equals
    (o_to_string (fun (a, b) -> "("^a^","^b^")"))
    (last_two ["a"; "b"; "c"; "d"])
    (Some ("c", "d"))
    "last_two";
  assert_equals
    (o_to_string (fun (a, b) -> "("^a^","^b^")"))
    (last_two ["a"])
    (None)
    "last_two";
  assert_equals oos_to_string (at 2 ["a"; "b"; "c"; "d"; "e"]) (Some "c") "at";
  assert_equals oos_to_string (at 2 ["a"]) (None) "at";
  assert_equals Int.to_string (length ["a"; "b"; "c"]) 3 "length";
  assert_equals Int.to_string (length []) 0 "length";
  assert_equals los_to_string (rev ["a"; "b"; "c"]) ["c"; "b"; "a"] "rev";
  assert_equals Bool.to_string (is_palindrome ["x"; "a"; "m"; "a"; "x"]) true "is_palindrome";
  assert_equals Bool.to_string (is_palindrome ["a"; "b"]) false "is_palindrome";
  assert_equals los_to_string
    (flatten [TOne "a"; TMany [TOne "b"; TMany [TOne "c" ;TOne "d"]; TOne "e"]])
    ["a"; "b"; "c"; "d"; "e"]
    "flatten";
  assert_equals los_to_string
    (compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"])
    ["a"; "b"; "c"; "a"; "d"; "e"]
    "compress";
  assert_equals lolos_to_string
    (pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"])
    [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]]
    "compress";
  assert_equals
    (l_to_string (fun (a, b) -> "("^(Int.to_string a)^","^b^")"))
    (encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"])
    [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
    "encode";
  assert_equals encode_2_to_string
    (encode_2 ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"])
    [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]
    "encode_2";
  assert_equals los_to_string
    (decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")])
    ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
    "decode";
  print_endline "success =)"
