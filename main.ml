exception AssertionFailure of string

let fail msg = raise @@ AssertionFailure msg

let assertOptionalEquals actual expectation msg = 
  match actual, expectation with
  | Some a, Some e when a = e -> ()
  | None, None -> ()
  | _ -> fail msg

let assertPrimitiveEquals actual expectation msg = 
  if actual = expectation
  then ()
  else fail msg

let assertListEquals actual expectation msg =
  if List.equal (fun a b -> a = b) actual expectation
  then ()
  else fail msg

let rec last list = match list with
  | [] -> None
  | head :: [] -> Some head
  | head :: tail -> last tail

let rec last_two list = match list with
  | [] -> None
  | [_] -> None
  | [a; b] -> Some (a, b)
  | _ :: b -> last_two b

let rec at i list = 
  match i, list with
  | 0, [a] -> Some a
  | 0, head :: tail -> Some head
  | j, [] -> None
  | j, head :: tail -> if j > 0 then at (j - 1) tail else None

let () = 
  let _ = assertOptionalEquals (last ["a" ; "b" ; "c" ; "d"]) (Some "d") "should have been d" in
  let _ = assertOptionalEquals (last []) (None) "should have been d" in
  let _ = assertOptionalEquals (last_two ["a"; "b"; "c"; "d"]) (Some ("c", "d")) "should have been (c, d)" in
  let _ = assertOptionalEquals (at 2 ["a"; "b"; "c"; "d"; "e"]) (Some "c") "should have been c" in
  let _ = assertOptionalEquals (at 2 ["a"]) (None) "should have been None" in
  print_endline "success =)"
