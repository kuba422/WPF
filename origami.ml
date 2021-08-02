type point = float * float

type kartka = point -> int

(* Dla epsilon = 0. program jest poprawny matematycznie,       *)
(* ale zdarza mu sie nie przejsc testu z powodu niedokladnosci *)
(* obliczen na liczbach zmiennoprzecinkowych                   *)
let epsilon = 0.00000001 

(* zwraca kwadrat liczby *)
let square (x : float) = 
  min (x *. x) max_float 

(* zwraca wartosc bezwzgledna floata *)
let float_abs (x : float) = 
  if x >= 0. then 
    x
  else
    (-1.) *. x 

(* dopasowuje rozmiar epsilona do rzedu wielkosci danych *)
let adjust_epsilon ((x1, y1) : point) ((x2, y2) : point) = 
  let mean = (float_abs x1 +. float_abs y1 +. float_abs x2 +. float_abs y2) /. 4.
  in 
  square (epsilon *. mean) 

(* sprawdza, czy a lezy miedzy x a y *)
let between (x : float) (y : float) (a : float) =
  min x y <= a && a <= max x y 

(* sprawdza, czy punkt (x, y) lezy w kole o srodku (x1, y1) i promieniu r *)
let in_circle ((x1, y1) : point) (r : float) ((x, y) : point) =
  (square (x1 -. x) +. square (y1 -. y) <= square ((1. +. epsilon) *. r))

(* sprawdza, czy punkt lezy na prostej wyznaczonej przez dwa punkty *)
let edge ((x1, y1) : point) ((x2, y2) : point) =
  fun ((x, y) : point) -> 
    let v = (y -. y1) *. (x2 -. x) -. (y2 -. y) *. (x -. x1) 
    in float_abs v <= adjust_epsilon (x1, y1) (x2, y2)
(* jezeli jeden z nawiasow jest rowny 0, to rownosc zachodzi           *)
(* iff (x, y) jest jednym z punktow (x1, y1), (x2, y2) albo gdy        *)
(* wartosc pewnej wspolrzednej wszystkich 3 punktow jest rowna,        *)
(* w przeciwnym wypadku rownosc jest rownowazna rownosci wspolczynnkow *)
(* kierunkowych prostych wyznaczanych przez odpowiednie pary punktow   *)

(* wersje znacznie szybsze, ale mniej dokladne: 

  let edge ((x1, y1) : point) ((x2, y2) : point) =
    fun ((x, y) : point) -> 
      let v = (y -. y1) *. (x2 -. x) -. (y2 -. y) *. (x -. x1) 
      in float_abs v <= square epsilon
  
  let edge ((x1, y1) : point) ((x2, y2) : point) =
    fun ((x, y) : point) -> 
      (y -. y1) *. (x2 -. x) = (y2 -. y) *. (x -. x1) 
*)

(* sprawdza, czy punkt lezy na prawo od prostej wyznaczonej przez dwa punkty *)
let right ((x1, y1) : point) ((x2, y2) : point) =
  fun ((x, y) : point) ->
  let cross_prod (a1, b1) (a2, b2) = a1 *. b2 -. a2 *. b1
  in 
  cross_prod (x2 -. x1, y2 -. y1) (x -. x1, y -. y1) < 0.
(* korzystamy ze znanych wlasnosci iloczynu wektorowego *)

(* zwraca odbicie punktu wzgledem prostej wyznaczonej przez dwa punkty *)
let reflect ((x1, y1) : point) ((x2, y2) : point) =
  fun ((x, y) : point) -> 
  if (x1 = x2) then
    (2. *. x1 -. x, y)
  else
    let a = (y2 -. y1) /. (x2 -. x1) and b = (x2 *. y1 -. x1 *. y2) /. (x2 -. x1)
    in
    (((1. -. square a) *. x +. 2. *. a *. (y -. b)) /. (1. +. square a),
    ((square a -. 1.) *. y +. 2. *. (a *. x +. b)) /. (1. +. square a))
(* korzystamy z prostych do wyprowadzenia wzorow na wspolrzedne *)
(* punktu bedacego obrazem w symetrii wzgledem danej prost      *)
  

let prostokat ((x1, y1) : point) ((x2, y2) : point) : kartka = 
  fun ((x, y) : point) ->
  assert (x1 <= x2 && y1 <= y2) ;
  if (between x1 x2 x && between y1 y2 y) then 
    1 
  else 
    0
  
let kolko (p1 : point) (r : float) : kartka = 
  fun (p : point) ->
  assert (r >= 0.) ;
  if in_circle p1 r p then 
    1 
  else 
    0

let zloz (p1 : point) (p2 : point) (sheet : kartka) : kartka =
  fun (p : point) ->
  assert (fst p1 <> fst p2 || snd p1 <> snd p2) ;
  if (edge p1 p2 p) then
    sheet p
  else if (right p1 p2 p) then
    0
  else
    sheet p + sheet (reflect p1 p2 p)

let skladaj (lst : (point * point) list) (sheet : kartka) : kartka =
  let pair_zloz sheet (p1, p2) = zloz p1 p2 sheet
  in List.fold_left pair_zloz sheet lst
