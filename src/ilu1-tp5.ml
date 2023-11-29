(* GRADE:  100% *)
let musee = [
  "Astérix", "bouclier";
  "Astérix", "casque";
  "Astérix", "bouclier";
  "Astérix", "casque";
  "Astérix", "casque";
  "Obélix", "bouclier";
  "Obélix", "casque"
] ;;

(* --- *)

(* q.1 *)

let rec insertObj o l =
  match l with
  | [] -> [(1, o)]
  | (nb, x) :: tl ->
      if x = o
      then (nb+1, o) :: tl
      else (nb, x) :: (insertObj o tl)
;; 

(* --- *)

(* q.2 *)

let rec insert p o lpo =
  match lpo with
  | [] -> [(p, (insertObj o []))]
  | (xp, lo) :: tlpo ->
      if xp = p
      then (xp, (insertObj o lo)) :: tlpo
      else (xp, lo) :: (insert p o tlpo)
;;

(* --- *)

(* q.3 *)

let rec decompte l =
  match l with
  | [] -> []
  | (p, o) :: tl ->
      insert p o (decompte tl)
;; 

decompte musee ;;

(* --- *)

(* q.4 *)

let rec longueur l =
  match l with
  | [] -> 0
  | (nb, x) :: tl ->
      nb + (longueur tl)
;;

(* --- *)

(* q.5 *)

let rec aumoins_indexed n lpo =
  match lpo with
  | [] -> []
  | (xp, lo) :: tlpo ->
      if (longueur lo) >= n
      then xp :: (aumoins_indexed n tlpo)
      else (aumoins_indexed n tlpo)
;;

(* --- *)

(* q.6 *)

let aumoins n l = aumoins_indexed n (decompte l) ;;

aumoins 2 musee ;;

(* --- *)
