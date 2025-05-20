exception AssertionFailure of string

let fail msg = raise @@ AssertionFailure msg
let eq a b = a = b
let assert_fn (fn : 'a -> 'b -> bool) a b msg = if fn a b then () else fail msg

let assert_equals (fn : 'a -> string) a b txt =
  let a = fn a in
  let b = fn b in
  assert_fn eq a b (txt ^ ": " ^ a ^ " <> " ^ b)
