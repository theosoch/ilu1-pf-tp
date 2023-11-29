(* GRADE:  100% *)
(* Exercice 1 *)

(* Q.1a *)

let estZero_v1 n =
  match n with
  | 0 -> "zero"
;; 

(* --- *)

(* Q.1b *)

let estZero_v2 n =
  match n with
  | 0 -> "zero"
  | _ -> "nonZero"
;;

(* --- *)

(* Q.2 *)

let voyelle c =
  match c with
  | 'a' | 'e' | 'i' | 'o' | 'u' | 'y' -> true 
  | _ -> false
;;

(* --- *)

(* Exercice 2 *)

(* Q.1 *)

let rang d =
  match d with
  | "lundi" | "monday" -> 1
  | "mardi" | "tuesday" -> 2
  | "mercredi" | "wednesday" -> 3
  | "jeudi" | "thursday" -> 4
  | "vendredi" | "friday" -> 5
  | "samedi" | "saturday" -> 6
  | "dimanche" | "sunday" -> 7
  | _ -> 0
;;

(* --- *)

(* Q.2 *)

let inf d1 d2 = (rang d2)-1 = ((rang d1-1 +1) mod 7) && min (rang d1) (rang d2) > 0 ;;

(* --- *)

(* Q.3 *)

let jsem x =
  match x with
  | 1 -> "lundi"
  | 2 -> "mardi"
  | 3 -> "mercredi"
  | 4 -> "jeudi"
  | 5 -> "vendredi"
  | 6 -> "samedi"
  | 7 -> "dimanche"
  | _ -> "jour inconnu"
;;

(* --- *)

(* Q.4 *)

let jourSucc1 d =
  match d with
  | "lundi" -> "mardi"
  | "mardi" -> "mercredi"
  | "mercredi" -> "jeudi"
  | "jeudi" -> "vendredi"
  | "vendredi" -> "samedi"
  | "samedi" -> "dimanche"
  | "dimanche" -> "lundi"
  | _ -> "jour inconnu"
;;

let jourSucc2 d =
  let rangd = rang d in
  if rangd > 0
  then
    if rangd+1 > 7 
    then jsem 1 
    else jsem (rangd+1)
  else "jour inconnu"
;;

let jourSucc3 d =
  let rangd = rang d in
  if rangd > 0
  then jsem ( ( ( rangd (*rangd-1 +1*) ) mod 7 ) +1 )
  else "jour inconnu"
;;

let jourSucc d = jourSucc3 d ;;

(* jourSucc "samedi" ;; *)
(* jourSucc "dimanche" ;; *)
(* jourSucc "lundi" ;; *)
(* jourSucc "mardi" ;; *)

(* --- *)

(* Q.5 *)

let jourPred1 d =
  match d with
  | "lundi" -> "dimanche"
  | "mardi" -> "lundi"
  | "mercredi" -> "mardi"
  | "jeudi" -> "mercredi"
  | "vendredi" -> "jeudi"
  | "samedi" -> "vendredi"
  | "dimanche" -> "samedi"
  | _ -> "jour inconnu"
;;

let jourPred2 d =
  let rangd = rang d in
  if rangd > 0
  then
    if rangd-1 = 0
    then jsem 7
    else jsem (rangd-1)
  else "jour inconnu"
;;

let jourPred3 d =
  let rangd = rang d in
  if rangd > 0
  then jsem ( ( (rangd-1 +7 -1) mod 7 ) +1 )
  else "jour inconnu"
;;

let jourPred d = jourPred3 d ;;

(* jourPred "mardi" ;; *)
(* jourPred "lundi" ;; *)
(* jourPred "dimanche" ;; *)
(* jourPred "samedi" ;; *)

(* jourSucc (jourPred "lundi") ;; *)
(* jourPred (jourSucc "lundi") ;; *)

(* jourSucc (jourPred "dimanche") ;; *)
(* jourPred (jourSucc "dimanche") ;; *)

(* --- *)

(* Q.6 *)

let bissextile a = a mod 400 = 0 || ( a mod 100 <> 0 && a mod 4 = 0 ) ;; 

(* --- *)

(* Q.7 *)

(*
  let nbjour m a =
    match m, a with
    | m, a when m = 1 -> 31
    | m, a when m = 2 -> 28 + (if bissextile a then 1 else 0)
    | m, a when m > 2 && m < 8 -> 30 + ((m) mod 2)
    | m, a when m > 7 && m < 13 -> 30 + ((m+1) mod 2)
    | _, _ -> failwith "erreur"
  ;;
  *)

let nbjour m a =
  match m with
  | 2 -> 28 + (if bissextile a then 1 else 0)
  | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
  | 4 | 6 | 9 | 11 -> 30
  | _ -> failwith "erreur"
  
(* nbjour 1 2020 ;; *)
(* nbjour 2 1900 ;; *)
(* nbjour 2 2020 ;; *)
(* nbjour 3 2020 ;; *)
(* nbjour 4 2020 ;; *)
(* nbjour 5 2020 ;; *)
(* nbjour 6 2020 ;; *)
(* nbjour 7 2020 ;; *)
(* nbjour 8 2020 ;; *)
(* nbjour 9 2020 ;; *)
(* nbjour 10 2020 ;; *)
(* nbjour 11 2020 ;; *)
(* nbjour 12 2020 ;; *)

(* --- *)
