open Base
open O_my_problems.Stringify
open O_my_problems.List_problems

let tests () =
  print_endline "list tests";
  assert_equals oos_to_string (last [ "a"; "b"; "c"; "d" ]) (Some "d") "last";
  assert_equals oos_to_string (last []) None "last";
  assert_equals
    (o_to_string (fun (a, b) -> "(" ^ a ^ "," ^ b ^ ")"))
    (last_two [ "a"; "b"; "c"; "d" ])
    (Some ("c", "d"))
    "last_two";
  assert_equals
    (o_to_string (fun (a, b) -> "(" ^ a ^ "," ^ b ^ ")"))
    (last_two [ "a" ]) None "last_two";
  assert_equals oos_to_string (at 2 [ "a"; "b"; "c"; "d"; "e" ]) (Some "c") "at";
  assert_equals oos_to_string (at 2 [ "a" ]) None "at";
  assert_equals Int.to_string (length [ "a"; "b"; "c" ]) 3 "length";
  assert_equals Int.to_string (length []) 0 "length";
  assert_equals los_to_string (rev [ "a"; "b"; "c" ]) [ "c"; "b"; "a" ] "rev";
  assert_equals Bool.to_string
    (is_palindrome [ "x"; "a"; "m"; "a"; "x" ])
    true "is_palindrome";
  assert_equals Bool.to_string
    (is_palindrome [ "a"; "b" ])
    false "is_palindrome";
  assert_equals los_to_string
    (flatten
       [ TOne "a"; TMany [ TOne "b"; TMany [ TOne "c"; TOne "d" ]; TOne "e" ] ])
    [ "a"; "b"; "c"; "d"; "e" ]
    "flatten";
  assert_equals los_to_string
    (compress
       [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ])
    [ "a"; "b"; "c"; "a"; "d"; "e" ]
    "compress";
  assert_equals lolos_to_string
    (pack
       [
         "a";
         "a";
         "a";
         "a";
         "b";
         "c";
         "c";
         "a";
         "a";
         "d";
         "d";
         "e";
         "e";
         "e";
         "e";
       ])
    [
      [ "a"; "a"; "a"; "a" ];
      [ "b" ];
      [ "c"; "c" ];
      [ "a"; "a" ];
      [ "d"; "d" ];
      [ "e"; "e"; "e"; "e" ];
    ]
    "compress";
  assert_equals
    (l_to_string (fun (a, b) -> "(" ^ Int.to_string a ^ "," ^ b ^ ")"))
    (encode
       [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ])
    [ (4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e") ]
    "encode";
  assert_equals encode_2_to_string
    (encode_2
       [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ])
    [
      Many (4, "a");
      One "b";
      Many (2, "c");
      Many (2, "a");
      One "d";
      Many (4, "e");
    ]
    "encode_2";
  assert_equals los_to_string
    (decode
       [
         Many (4, "a");
         One "b";
         Many (2, "c");
         Many (2, "a");
         One "d";
         Many (4, "e");
       ])
    [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
    "decode";
  assert_equals los_to_string
    (duplicate [ "a"; "b"; "c"; "c"; "d" ])
    [ "a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d" ]
    "duplicate";
  assert_equals los_to_string
    (replicate [ "a"; "b"; "c" ] 3)
    [ "a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c" ]
    "replicate";
  assert_equals los_to_string
    (drop [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3)
    [ "a"; "b"; "d"; "e"; "g"; "h"; "j" ]
    "drop";
  let to_str (f, s) = "(" ^ los_to_string f ^ ", " ^ los_to_string s ^ ")" in
  assert_equals to_str
    (split [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3)
    ([ "a"; "b"; "c" ], [ "d"; "e"; "f"; "g"; "h"; "i"; "j" ])
    "split somewhere in the middle";
  assert_equals to_str
    (split [ "a"; "b"; "c"; "d" ] 5)
    ([ "a"; "b"; "c"; "d" ], [])
    "split after the list";
  assert_equals los_to_string
    (slice [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 2 6)
    [ "c"; "d"; "e"; "f"; "g" ]
    "slice";
  assert_equals los_to_string
    (rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3)
    [ "d"; "e"; "f"; "g"; "h"; "a"; "b"; "c" ]
    "rotate 3";
  assert_equals los_to_string
    (rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 11)
    [ "d"; "e"; "f"; "g"; "h"; "a"; "b"; "c" ]
    "rotate 11";
  assert_equals los_to_string
    (remove_at 1 [ "a"; "b"; "c"; "d" ])
    [ "a"; "c"; "d" ] "remove_at";
  assert_equals los_to_string
    (insert_at "alfa" 1 [ "a"; "b"; "c"; "d" ])
    [ "a"; "alfa"; "b"; "c"; "d" ]
    "insert at";
  assert_equals loi_to_string (range 4 9) [ 4; 5; 6; 7; 8; 9 ] "range";
  assert_equals los_to_string
    (rand_select [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3)
    [ "e"; "c"; "g" ] "rand_select";
  assert_equals loi_to_string (lotto_select 6 49) [21; 8; 28; 4; 34; 29]
  (* should have been [20 28; 45; 16; 24; 38] according to the ocaml exercises page but even the *)
  (* solution of the page spits out the same result as my solution *)
  "lotto_select";
  assert_equals los_to_string
    (permutation ["a"; "b"; "c"; "d"; "e"; "f"])
    (* ["c"; "d"; "f"; "e"; "b"; "a"] *)
    ["c"; "b"; "f"; "e"; "d"; "a"]
    "permutation";
  assert_equals lolos_to_string
    (extract 1 ["a"; "b"; "c"; "d"])
    [["a"]; ["b"]; ["c"]; ["d"]]
    "extract 1";
  assert_equals lolos_to_string
    (extract 2 ["a"; "b"; "c"; "d"])
    [["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"]; ["b"; "d"]; ["c"; "d"]]
    "extract 2";
  assert_equals lolos_to_string
    (extract 3 ["a"; "b"; "c"; "d"])
    [["a"; "b"; "c"]; ["a"; "b"; "d"]; ["a"; "c"; "d"]; ["b"; "c"; "d"]]
    "extract 3";
  assert_equals lolos_to_string
    (extract 0 ["a"; "b"; "c"; "d"])
    [[]]
    "extract 0";
