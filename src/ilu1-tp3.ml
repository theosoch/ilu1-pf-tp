(* GRADE:  100% *)
(* Exercice 1 *)

(* Q.1 *)

(*
  let rec premierCh n =
    match n with
    | n when n >= 0 -> 
        let r = toutSaufDer n in
        if r = 0 then n else premierCh r
          
    | _ -> failwith "erreur"
  ;;
  *) 
  
let rec premierCh n =
  if n >= 0
  then 
    let r = toutSaufDer n in
    if r = 0 then n else premierCh r 
  else failwith "erreur"
;;
      
(*
  let rec seulementPrem n =
    match n with
    | n when n >= 0 -> 
        let r = toutSaufDer n in
        if r = 0 then n else (seulementPrem r) * 10

    | _ -> failwith "erreur"
  ;; 
  *)
  
let rec seulementPrem n =
  if n >= 0
  then
    let r = toutSaufDer n in
    if r = 0 then n else (seulementPrem r) * 10
  else failwith "erreur"
;;
  
let toutSaufPrem n = n - (seulementPrem n) ;;


(* --- *)

(* Q.2 *)

(*
  let rec estPalindrome n =
    match n with
    | n when n < 0 -> estPalindrome (~-n)
    | 0 -> true
    | n when dernierCh n <> premierCh n -> false
    | n -> estPalindrome (toutSaufPrem (toutSaufDer n))
  ;;
  *)

let rec estPalindrome n =
  if n < 0 then estPalindrome(~-n)
  else
    n = 0
    || (
      dernierCh n = premierCh n
      && estPalindrome (toutSaufPrem (toutSaufDer n))
    )
;;

(*
estPalindrome 0 ;;
estPalindrome 1 ;;
estPalindrome 11 ;;
estPalindrome 121 ;;
estPalindrome 12721 ;;
estPalindrome 127721 ;;

estPalindrome 12 ;;
estPalindrome 125 ;;
estPalindrome 12720 ;;
estPalindrome 127621 ;;
*)

(* --- *)

(* Q.3 *)

(*
  let rec nbOccs c n =
    match c, n with
    | c, n when c >= 0 && c <= 9 && n >= 0 -> 
        let toutSaufDerN = toutSaufDer n in
        (if dernierCh n = c then 1 else 0)
        + (if toutSaufDerN = 0 then 0 else nbOccs c toutSaufDerN)
      
    | _ -> failwith "erreur"
  ;;
  *)

let rec nbOccs c n =
  if 0 <= c && c <= 9 && n >= 0
  then 
    let toutSaufDerN = toutSaufDer n in
    (if dernierCh n = c then 1 else 0)
    + (if toutSaufDerN = 0 then 0 else nbOccs c toutSaufDerN)
  else failwith "erreur"
;;

(*
nbOccs 0 100 ;;
nbOccs 0 101 ;;
nbOccs 0 111 ;;

nbOccs 0 1000 ;;
nbOccs 0 1001 ;;
nbOccs 0 1010 ;;
nbOccs 0 1011 ;;
nbOccs 0 1111 ;;

nbOccs 2 25753 ;;
nbOccs 5 25753 ;;
nbOccs 7 25753 ;;
nbOccs 3 25753 ;;
*)

(* --- *)

(* Exercice 2 *)

(* Q.1 *)

let rec iterer n f x =
  if n = 0
  then x
  else iterer (n-1) f (f x)
;;

(*
iterer 2 (fun x -> x + 10) 3 ;;
*)

(* --- *)

(* Q.2.1 *)

let id x = x ;;

(* --- *)

(* Q.2.2 *)

let compose f g x = f (g x) ;;

let rec iterer2 n f = 
  if n = 0
  then id
  else compose f (iterer2 (n-1) f)
;;

(*
let f = iterer2 4 (fun x -> x + 10) in f 3 ;;
*)

(* --- *)

(* Q.2.3 *)

let rec itererBis f p x =
  if p x
  then x 
  else itererBis f p (f x) 
;;

(*
itererBis (fun x -> x / 2) (fun x -> x < 10) 45  =  5  ;;
*)

(* --- *)

(* Exercice 3 *)

(* Q.1 *)

(*
  let rec qqsoit n p = 
    match n, p with
    | n, p when n <= 0 -> true
    | n, p -> (p n) && (qqsoit (n-1) p)
  ;;
  *)

let rec qqsoit n p =
  n <= 0 || (p n) && (qqsoit (n-1) p)
;;

(* --- *)

(* Q.2 *)

let pair n = n - ((n lsr 1) lsl 1) = 0 ;;

(*
  let rec fastpow n e =
    if e < 0
    then failwith "erreur" 
    else
      match n, e with
      | n, 0 -> 1
      | n, 1 -> n
      | n, e when pair e -> fastpow (n * n) (e lsr 1)
      | n, e when not (pair e) -> n * (fastpow (n * n) ((e-1) lsr 1))
      | _, _ -> failwith "erreur"
  ;;
  *)

let rec fastpow n e =
  if e < 0 then failwith "erreur"
  else
    match e with
    | 0 -> 1
    | 1 -> n
    | _ ->
        if pair e then fastpow (n * n) (e lsr 1)
        else n * (fastpow (n * n) ((e-1) lsr 1))
;;

fastpow 0 0 ;;

(*
let n = Random.int 9 + 2 and e = Random.int 9 + 1 ;;
let r1 = int_of_float( float_of_int(n) ** float_of_int(e) ) ;;
let r2 = fastpow n e ;;
r1 = r2 ;;
*)

(* --- *)

(* Q.3 *)

(*
  let rec ack (m, n) =
    match m, n with
    | 0, n -> n + 1
    | m, n when m > 0 && n = 0 -> ack (m-1, 1)
    | m, n when m > 0 && n > 0 -> ack (m-1, ack (m, (n-1)) )
    | _, _ -> failwith "erreur"
  ;;
  *)

let rec ack (m, n) =
  if m < 0 || n < 0 then failwith "erreur"
  else if m = 0 then n + 1
  else if n = 0 then ack (m-1, 1)
  else ack (m-1, ack (m, (n-1)) )
;;

(*
ack (0, 0) = 1 ;;
ack (1, 0) = 2 ;;
ack (2, 0) = 3 ;;
ack (3, 0) = 5 ;;

ack (0, 1) = 2 ;;
ack (1, 1) = 3 ;;
ack (2, 1) = 5 ;;
ack (3, 1) = 13 ;;

ack (0, 2) = 3 ;;
ack (1, 2) = 4 ;;
ack (2, 2) = 7 ;;
ack (3, 2) = 29 ;;

ack (0, 3) = 4 ;;
ack (1, 3) = 5 ;;
ack (2, 3) = 9 ;;
ack (3, 3) = 61 ;;

ack (0, 0) = 1 ;;
ack (0, 1) = 2 ;;
ack (0, 2) = 3 ;;
ack (0, 3) = 5 ;;

ack (1, 0) = 2 ;;
ack (1, 1) = 3 ;;
ack (1, 2) = 4 ;;
ack (1, 3) = 5 ;;

ack (2, 0) = 3 ;;
ack (2, 1) = 5 ;;
ack (2, 2) = 7 ;;
ack (2, 3) = 9 ;;

ack (3, 0) = 5 ;;
ack (3, 1) = 13 ;;
ack (3, 2) = 29 ;;
ack (3, 3) = 61 ;;
*)

(* --- *)
