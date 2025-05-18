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

(* let assert_equals a b msg = assert_fn eq a b msg *)

let list_to_string list =
  let rec aux acc = function
    | [] -> acc ^ "]"
    | a :: [] -> acc ^ a ^ "]"
    | a :: b -> aux (acc ^ a ^ "; ") b
  in aux "[" list

(* list of list of strings *)
let lolos_to_string list =
  list |> (List.map list_to_string) |> list_to_string

(* let assert_optional_equals = function *)
let assert_optional_equals actual expectation msg =
  match actual, expectation with
  | Some a, Some e when a = e -> ()
  | None, None -> ()
  | _ -> fail msg

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
  | One of 'a
  | Many of 'a node list

let flatten tree =
  let rec aux acc = function
    | [] -> acc
    | One hd :: tail ->  (aux [@tailcall]) (hd :: acc) tail
    | Many hd :: tail -> (aux [@tailcall]) (aux acc hd) tail
  in aux [] tree |> rev

(* 8. *)
let rec compress list =
  match list with
  | a :: (b :: _ as tail) -> if a = b then compress tail else a :: (compress tail)
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

let () =
  assert_optional_equals (last ["a" ; "b" ; "c" ; "d"]) (Some "d") "should have been d";
  assert_optional_equals (last []) (None) "should have been d";
  assert_optional_equals (last_two ["a"; "b"; "c"; "d"]) (Some ("c", "d")) "should have been (c, d)";
  assert_optional_equals (last_two ["a"]) (None) "should have been (c, d)";
  assert_optional_equals (at 2 ["a"; "b"; "c"; "d"; "e"]) (Some "c") "should have been c";
  assert_optional_equals (at 2 ["a"]) (None) "should have been None";
  (* assert_equals (length ["a"; "b"; "c"]) 3 "should have been 3"; *)
  assert_equals Int.to_string (length ["a"; "b"; "c"]) 3 "length";
  assert_equals Int.to_string (length []) 0 "length";
  assert_equals list_to_string (rev ["a"; "b"; "c"]) ["c"; "b"; "a"] "rev";
  assert_equals Bool.to_string (is_palindrome ["x"; "a"; "m"; "a"; "x"]) true "is_palindrome";
  assert_equals Bool.to_string (is_palindrome ["a"; "b"]) false "is_palindrome";
  assert_equals list_to_string
    (flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]])
    ["a"; "b"; "c"; "d"; "e"]
    "flatten";
  assert_equals list_to_string
    (compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"])
    ["a"; "b"; "c"; "a"; "d"; "e"]
    "compress";
  assert_equals lolos_to_string
    (pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"])
    [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]]
    "compress";
    (* "not packed correctly"; *)
  print_endline "success =)"
