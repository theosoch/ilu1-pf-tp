(* GRADE:  100% *)
(* Exercice A *)

(* Q.1 *)

let rec hanoi (source, temp, dest) n =
  if n <= 0 then []
  else
    (hanoi (source, dest, temp) (n-1))
    @ ( (source, dest) ::(hanoi (temp, source, dest) (n-1)) )
;;

hanoi (1,2,3) 2 = [(1, 2); (1, 3); (2, 3)] ;;

(* --- *)

(* Q.2 *)

(*
Explication (pour la séance de TP) :

Dans la fonction "hanoi", lorsqu'on demande 0 disque,
on obtient logiquement aucun déplacement.
Ainsi, le nombre de coup pour cette tour est de 0.

Lorsqu'on demande n disque (avec n > 0 donc), on obtient au moins 1 coup,
mais aussi les coups renvoyés par les appels récursifs de "hanoi".
Ainsi, on obtient :
  (nb coups à n-1) + 1 + (nb coups à n-1)
soit :
  2 * (nb coups à n-1) + 1

*)

let rec longueurHanoi n =
  if n <= 0 then 0
  else 2 * (longueurHanoi (n-1)) + 1
;;

longueurHanoi 2 ;;

(* --- *)

(* Exercice B *)

(* Q.1 *)

let rec map f l = 
  match l with
  | [] -> []
  | e :: tl -> (f e) :: (map f tl)
;;

(* --- *)

(* Q.2 *)

let longueurMap f l = List.length l ;;

(* --- *)

(* Q.3 *)

let ltest = [1; 2; 0; 4; -1; 5; 2] ;;

List.filter (fun n -> n mod 2 = 0) ltest ;;
List.for_all (fun n -> n mod 2 = 0) ltest ;;
List.exists (fun n -> n mod 2 = 0) ltest ;;

List.filter (fun n -> n > -2) ltest ;;
List.for_all (fun n -> n > -2) ltest ;;
List.exists (fun n -> n > -2) ltest ;;

List.filter (fun x -> x > 2) ltest ;;
List.for_all (fun x -> x >= 0) ltest ;;
List.exists (fun x -> x < 0) ltest ;;
List.exists (fun x -> x >= 0) ltest ;; 

(* --- *)

(* Exercice C *)

(* Q.1 *)

(*
let rec inserer e l i =
  match e, l, i with
  | e, l, i when i < 0 || i > (List.length l) -> failwith "erreur"
  | e, l, i when i = 0 -> e :: l
  | e, l, i when i > 0 -> List.hd l :: inserer e (List.tl l) (i-1)
  | e, l, i -> failwith "erreur"
;; 
*)

let rec inserer e l =
  match l with
  | [] -> [e]
  | x :: tl ->
      if x > e then e :: l
      else x :: (inserer e tl)
;; 

let rec recTriInsertion l sl =
  match l with
  | [] -> sl
  | e :: tl ->
      recTriInsertion tl (inserer e sl)
;;

let triInsertion l = recTriInsertion l [] ;;

ltest ;;
triInsertion ltest ;; 

(* --- *)

(* Q.2 *)

let rec partage_ l x =
  match l with
  | [] ->
      ([], [], x / 2) 
  | e :: tl ->
      let (lr, rr, xr) = partage_ tl (x+1) in
      if xr = 0
      then (e :: lr, rr, xr)
      else (lr, e :: rr, xr-1)
;;

let partage l = let (lr, rr, xr) = partage_ l 0 in (lr, rr) ;;
          
  
ltest ;;
partage ltest ;;

let rec merge l1 l2 =
  match l1, l2 with
  | [], l2 -> l2
  | l1, [] -> l1
  | e1 :: tl1, e2 :: tl2 ->
      if e1 <= e2
      then e1 :: merge tl1 l2
      else e2 :: merge l1 tl2
;;
  
let ltest_sorted = triInsertion ltest ;;
let ltest2 = [ 1; 3; 5; 5; 7; 8 ] ;;

merge ltest_sorted ltest2 ;;

let rec triFusion l =
  match l with
  | [] -> []
  | [x] -> [x]
  | l ->
      let (pl1, pl2) = partage l in
      let tl1 = triFusion pl1 and tl2 = triFusion pl2 in
      merge tl1 tl2
;; 


(*
triFusion [] ;;
triFusion [0] ;;
triFusion [7] ;;
triFusion [7; 0] ;;
triFusion [7; 7] ;;
triFusion [1; 7; 7] ;;
triFusion [7; 1; 7] ;;
triFusion [7; 7; 1] ;;

ltest ;; 
triFusion ltest ;;
*)      

(* --- *)

(* Exercice D *)

(* Q.0 *)

let fst c = let (a, b) = c in a ;;
let snd c = let (a, b) = c in b ;;

let ctest = (5, 3) ;;
fst ctest ;;
snd ctest ;;

(* --- *)

(* Q.1 *) 

let rec estFonction lc =
  match lc with
  | [] -> true
  | e :: tlc ->
      (
        List.for_all
          (fun c -> (fst c) <> (fst e))
          tlc
      )
      && estFonction tlc
;;

let lftest = [(1,3) ; (2,5) ; (3,7) ; (4,9) ; (5,11)] ;;
  
estFonction lftest ;;
estFonction (lftest @ [(2,4)]) ;;

(* --- *)

(* Q.2 *)

let rec image x rf =
  match rf with
  | [] -> failwith "erreur"
  | c :: trf ->
      if (fst c) = x then (snd c) else image x trf
;;

image 1 lftest = 3 ;;
image 4 lftest = 9 ;;

(* --- *)

(* Q.3 *)

let rec imageEns l rf =
  match l with
  | [] -> []
  | e :: tl -> (image e rf) :: (imageEns tl rf)
;;

imageEns [1; 4] lftest ;;

(* --- *)

(* Q.4 *) 

let estInjective rf =
  List.for_all
    (
      fun c1 ->
        List.for_all (fun c2 -> fst c1 = fst c2 || snd c1 <> snd c2) rf
    )
    rf
;;

estInjective lftest ;;
estInjective (lftest @ [(6, 5)]) ;;

(* --- *)

(* Q.5 *) 

let rec surcharge rf1 rf2 =
  (
    List.filter
      (fun c1 -> List.for_all (fun c2 -> fst c2 <> fst c1) rf2)
      rf1
  ) @ rf2
;;

lftest ;;
let lftest2 = [(5, 20); (6, 0); (7, -5); (0, 0); (1, 7)] ;;
surcharge lftest lftest2  ;;

(* --- *)

(* Q.6 *)

let isDef x rf = List.exists (fun c -> fst c = x) rf ;;

let composition rf1 rf2 = 
  map
    (fun (c_x, c_y) -> (c_x, image c_y rf1))
    (
      List.filter
        (fun (c_x, c_y) -> isDef c_y rf1)
        rf2
    )
;;

lftest ;;
lftest2 ;;
composition lftest2 lftest ;;
composition lftest lftest2 ;; 

(* --- *)

(* Q.7 *) 

let rec extraire_couples_antecedants rf1 rf2 =
  match rf1 with
  | [] -> []
  | rf1 ->
      (map (fun c -> (fst (List.hd rf1), fst c)) rf2)
      @ (extraire_couples_antecedants (List.tl rf1) rf2)
;;

let rec image_couples_antecedants lc rf1 rf2 =
  match lc with
  | [] -> []
  | xc :: tlc -> 
      let x1 = fst xc and x2 = snd xc in
      (
        (x1, x2),
        (image x1 rf1, image x2 rf2)
      ) :: (image_couples_antecedants tlc rf1 rf2)
;;

let rec produit rf1 rf2 =
  image_couples_antecedants
    (extraire_couples_antecedants rf1 rf2)
    rf1 rf2
;;


produit lftest lftest2 ;;

(* --- *)
