exception AssertionFailure of string

let fail msg = raise @@ AssertionFailure msg

let eq a b = a = b

let assert_fn (fn: 'a -> 'b -> bool ) a b msg =
  if fn a b
  then ()
  else fail msg

let assert_equals a b msg = assert_fn eq a b msg

(* let assert_optional_equals = function *)
let assert_optional_equals actual expectation msg =
  match actual, expectation with
  | Some a, Some e when a = e -> ()
  | None, None -> ()
  | _ -> fail msg

let rec last = function
  | [] -> None
  | [head] -> Some head
  | head :: tail -> (last [@tailcall]) tail

let rec last_two = function
  | [] | [_] -> None
  | [a; b] -> Some (a, b)
  | _ :: b -> (last_two [@tailcall]) b

let rec at i list =
  match i, list with
  | i, [] -> None
  | i, head :: tail -> if i > 0 then (at [@tailcall]) (i - 1) tail else Some head

let length list =
  let rec aux l i = match l, i with
  | [], i -> i
  | a :: b, i -> (aux [@tailcall]) b (i + 1)
  in aux list 0

let rev list =
  let rec aux list acc =
    match list, acc with
    | [], a -> a
    | h :: t, a -> (aux [@tailcall]) t (h :: a)
  in aux list []

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

let () =
  assert_optional_equals (last ["a" ; "b" ; "c" ; "d"]) (Some "d") "should have been d";
  assert_optional_equals (last []) (None) "should have been d";
  assert_optional_equals (last_two ["a"; "b"; "c"; "d"]) (Some ("c", "d")) "should have been (c, d)";
  assert_optional_equals (last_two ["a"]) (None) "should have been (c, d)";
  assert_optional_equals (at 2 ["a"; "b"; "c"; "d"; "e"]) (Some "c") "should have been c";
  assert_optional_equals (at 2 ["a"]) (None) "should have been None";
  assert_equals (length ["a"; "b"; "c"]) 3 "should have been 3";
  assert_equals (length []) 0 "should have been 0";
  assert_equals (rev ["a"; "b"; "c"]) ["c"; "b"; "a"] "should have been [c; b; a]";
  assert_equals (is_palindrome ["x"; "a"; "m"; "a"; "x"]) true "should have been true";
  assert_equals (is_palindrome ["a"; "b"]) false "should have been false";
  print_endline "success =)"
