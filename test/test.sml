(* Bad syntax - missing 'in' *)
let 
    val x = 5
    val y = 10
    x + y
end

(* (* Mismatched parentheses *) *)
(* fun broken x = ( *)
(*     let *)
(*         val y = x + 1 *)
(*     in *)
(*         y * 2 *)
(*     (* Missing closing paren *) *)
(**)
(* (* Type error *) *)
(* val x: int = "hello" *)
(**)
(* (* Pattern match not exhaustive *) *)
(* fun incomplete [] = 0 *)
(*   | incomplete (x::xs) = x *)
(**)
(* (* Missing 'of' in datatype *) *)
(* datatype color = Red | Blue string *)
(**)
(* (* Invalid operator usage *) *)
(* val bad = 3 ++ 4 *)

(* Unclosed string *)
val str = "unclosed string
