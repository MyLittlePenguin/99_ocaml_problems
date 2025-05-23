open Stringify

(* 1. *)
let rec last = function
  | [] -> None
  | [ head ] -> Some head
  | _ :: tail -> (last [@tailcall]) tail

(* 2. *)
let rec last_two = function
  | [] | [ _ ] -> None
  | [ a; b ] -> Some (a, b)
  | _ :: b -> (last_two [@tailcall]) b

(* 3. *)
let rec at i list =
  match (i, list) with
  | _, [] -> None
  | i, head :: tail ->
      if i > 0 then (at [@tailcall]) (i - 1) tail else Some head

(* 4. *)
let length list =
  let rec aux l i =
    match (l, i) with [], i -> i | _ :: b, i -> (aux [@tailcall]) b (i + 1)
  in
  aux list 0

(* 5. *)
let rev list =
  let rec aux list acc =
    match (list, acc) with
    | [], a -> a
    | h :: t, a -> (aux [@tailcall]) t (h :: a)
  in
  aux list []

(* 6. *)
let is_palindrome list = list = rev list

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
type 'a node = TOne of 'a | TMany of 'a node list

let flatten tree =
  let rec aux acc = function
    | [] -> acc
    | TOne hd :: tail -> (aux [@tailcall]) (hd :: acc) tail
    | TMany hd :: tail -> (aux [@tailcall]) (aux acc hd) tail
  in
  aux [] tree |> rev

(* 8. *)
let rec compress list =
  match list with
  | a :: (b :: _ as tail) when a = b -> compress tail
  | a :: (_ :: _ as tail) -> a :: compress tail
  | a :: [] -> [ a ]
  | [] -> []

(* 9. *)
let pack list =
  let rec aux acc group = function
    | [] -> []
    | [ a ] -> (a :: group) :: acc |> rev
    | a :: (b :: _ as tail) when a = b ->
        (aux [@tailcall]) acc (a :: group) tail
    | a :: tail -> (aux [@tailcall]) ((a :: group) :: acc) [] tail
  in
  aux [] [] list

(* 10. *)
let encode list =
  let rec aux acc counter = function
    | [] -> []
    | [ a ] -> (counter, a) :: acc
    | a :: (b :: _ as tail) when a = b ->
        (aux [@tailcall]) acc (counter + 1) tail
    | a :: (_ :: _ as tail) -> (aux [@tailcall]) ((counter, a) :: acc) 1 tail
  in
  aux [] 1 list |> rev

(* 11. && 13. *)
type 'a rle = One of 'a | Many of int * 'a

let encode_2 list =
  let to_entry value = function
    | 1 -> One value
    | counter -> Many (counter, value)
  in
  let rec aux acc counter = function
    | [] -> []
    | [ a ] -> to_entry a counter :: acc
    | a :: (b :: _ as tail) when a = b ->
        (aux [@tailcall]) acc (counter + 1) tail
    | a :: (_ :: _ as tail) ->
        (aux [@tailcall]) (to_entry a counter :: acc) 1 tail
  in
  aux [] 1 list |> rev

let encode_2_to_string list =
  let entry_to_string it =
    match it with
    | One x -> "One " ^ x
    | Many (i, x) -> "Many (" ^ Int.to_string i ^ ", " ^ x ^ ")"
  in
  list |> List.map entry_to_string |> los_to_string

(* 12. *)
let decode list =
  let rec expand acc = function
    | One x -> x :: acc
    | Many (i, x) when i = 1 -> x :: acc
    | Many (i, x) -> expand (x :: acc) (Many (i - 1, x))
  in
  let rec aux acc = function
    | [] -> []
    | [ x ] -> expand acc x
    | hd :: tl -> aux (expand acc hd) tl
  in
  aux [] list |> rev

(* 14. *)
let duplicate list =
  let rec aux acc = function
    | [] -> acc
    | a :: tail -> aux (a :: a :: acc) tail
  in
  aux [] list |> rev

(* 15. *)
let replicate list count =
  let rec repeat acc value = function
    | 0 -> acc
    | i -> repeat (value :: acc) value (i - 1)
  in
  let rec aux acc = function
    | [] -> acc
    | a :: tail -> aux (repeat acc a count) tail
  in
  aux [] list |> rev

(* 16. *)
let drop list nth =
  let rec aux acc i = function
    | [] -> acc
    | _ :: tail when i = 1 -> aux acc nth tail
    | a :: tail -> aux (a :: acc) (i - 1) tail
  in
  aux [] nth list |> rev

(* 17. *)
let split list len =
  let rec aux acc i list =
    match (acc, list) with
    | acc, [] -> acc
    | (fst, snd), hd :: tail when i <= len -> aux (hd :: fst, snd) (i + 1) tail
    | (fst, snd), hd :: tail -> aux (fst, hd :: snd) (i + 1) tail
  in
  aux ([], []) 1 list |> fun (f, s) -> (rev f, rev s)

(* 18. *)
let slice list start stop =
  let rec aux acc i = function
    | [] -> acc
    | _ when i > stop -> acc
    | _ :: tail when i < start -> aux acc (i + 1) tail
    | hd :: tail -> aux (hd :: acc) (i + 1) tail
  in
  aux [] 0 list |> rev

(* 19. *)
let rotate list places =
  let len = length list in
  let n = if places <= 0 then 0 else places mod len in
  let tail, head = split list n in
  head @ tail

(* 20. *)
let remove_at n list =
  let rec aux acc i = function
    | [] -> acc |> rev
    | hd :: tl when i > 0 -> aux (hd :: acc) (i - 1) tl
    | _ :: tl -> rev acc @ tl
  in
  aux [] n list

(* 21. *)
let insert_at value n list =
  let rec aux acc n = function
    | rest when n <= 0 -> rev acc @ (value :: rest)
    | hd :: tl -> aux (hd :: acc) (n - 1) tl
    | [] -> [ value ]
  in
  aux [] n list

(* 22. *)
let range start stop =
  let rec aux acc = function
    | i when i <= stop -> aux (i :: acc) (i + 1)
    | _ -> acc |> rev
  in
  aux [] start

(* 23. *)
let rand_select list n =
  Random.init 0;
  let rec select acc i = function
    | [] -> raise Not_found
    | hd :: tail when i > 0 -> select (hd :: acc) (i - 1) tail
    | hd :: tail -> (hd, acc @ tail)
  in
  let select_rand list = select [] (list |> length |> Random.int) list in
  let rec aux acc rest = function
    | 0 -> acc
    | i ->
        let it, rest = select_rand rest in
        aux (it :: acc) rest (i - 1)
  in
  aux [] list n

(* 24. *)
let lotto_select n max =
  rand_select (range 1 max) n
