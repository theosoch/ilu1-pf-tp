(* GRADE:  100% *)
let multiple_of n d =
  n mod d = 0 ;;

let integer_square_root n =
  int_of_float(sqrt(float_of_int(n))) ;;
  
let last_character s =
  s.[(String.length s) -1] ;;
  
let string_of_bool b =
  if b then "true" else "false" ;;
    
let pairwise_distinct (a, b, c, d) =
  if a <> b && a <> c && a <> d && b <> c && b <> d && c <> d then true else false ;;

let e1 = ((1, true), 0) ;;
let e2 = ((fun x -> x), 'x') ;;

let f1 = fun x b -> (x + 0, if b then b else b) ;;
let f2 = fun x -> true ;;
let f3 = fun x y -> if x = y then 1 else 0 ;; 
let f4 = fun (x, y) -> (y, x) ;; 
let f5 = fun g h a -> h a (g a) ;;
let f6 = fun (g, h, a) -> h (a, (g a));;
