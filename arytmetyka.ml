(* autor:     Jakub Jagiella *)
(* recenzent: Antoni Puch    *)
(* projekt:   Arytmetyka     *)


(* funkcje pomocnicze *)

let modul (x : float) =
  if (x <= 0.) then (-1.) *. x else x


(* wartosc a b true  true  = [a, b] (a, b nie muszą być skonczone) *)
(* wartosc a b false true  = (neg_infinity, a] u [b, infinity)     *)
(* wartosc _ _ _____ false = nan                                   *)

type wartosc = float * float * bool * bool

let pierwszy  ((x, _, _, _) : wartosc) = x 
let drugi     ((_, x, _, _) : wartosc) = x
let przedzial ((_, _, x, _) : wartosc) = x
let liczba    ((_, _, _, x) : wartosc) = x

(* nomenklatura:                                        *)
(* przedzial w = true - w jest przedzialem              *)
(* przedzial w = false - w jest dopelnieniem przedzialu *)


(* konstruktory *)

let wartosc_dokladnosc (x : float) (p : float) =
  ((x -. (modul x) *. p /. 100., x +. (modul x) *. p /. 100., true, true) : wartosc)


let wartosc_od_do (x : float) (y : float) =
  ((x, y, true, true) : wartosc)


let wartosc_dokladna (x : float) = 
  ((x, x, true, true) : wartosc)


(* selektory *)

let in_wartosc (w : wartosc) (x : float) =
  if (not (liczba w)) then                  
    false 
  else if (przedzial w) then
    (x >= (pierwszy w) && x <= (drugi w))
  else
    (x <= (pierwszy w) || x >= (drugi w))


let min_wartosc (w : wartosc) =
  if (not (liczba w)) then
    nan
  else if (przedzial w) then
    pierwszy w
  else
    neg_infinity

    
let max_wartosc (w : wartosc) =
  if (not (liczba w)) then
    nan
  else if (przedzial w) then 
    drugi w
  else
    infinity


let sr_wartosc (w : wartosc) = 
  let min = min_wartosc w 
  and max = max_wartosc w 
  in
  if (min = neg_infinity && max = infinity) then
    nan
  else
    (min +. max) /. 2.


(* funkcje pomocnicze do modyfikatorow *)

(* suma w1 w2, gdy                                 *)
(* w1 jest przedzialem, w2 dopelnieniem przedzialu *)
let plus_pomocnicza (w1 : wartosc) (w2 : wartosc) =
  let a = pierwszy w1
  and b = drugi w1
  and x = pierwszy w2
  and y = drugi w2
in
  if ((x +. b) < (y +. a)) then
    ((x +. b, y +. a, false, true) : wartosc)
  else
    ((neg_infinity, infinity, true, true) : wartosc)

(* {a : -a \in w} *)
let wartosc_przeciwna (w : wartosc) =
  (((-1.) *. (drugi w), (-1.) *. (pierwszy w), przedzial w, liczba w) : wartosc)


(* razy w1 w2, gdy w1 i w2 to przedzialy *)
let razy_pomocnicza_1 (w1 : wartosc) (w2 : wartosc) =
  let a' = (pierwszy w1) *. (pierwszy w2)
  and b' = (pierwszy w1) *. (drugi w2)
  and c' = (drugi w1) *. (pierwszy w2)
  and d' = (drugi w1) *. (drugi w2)
  in  
  (* przyjmujemy, ze infinity * 0 = 0 *)
  let a = if (classify_float a') = FP_nan then 0. else a'
  and b = if (classify_float b') = FP_nan then 0. else b'
  and c = if (classify_float c') = FP_nan then 0. else c'
  and d = if (classify_float d') = FP_nan then 0. else d'
  in
  let najmniejsza = min a (min b (min c d))
  and najwieksza = max a (max b (max c d))
in 
  ((najmniejsza, najwieksza, true, true) : wartosc)


(* razy w1 w2, gdy                                      *)
(* w1 jest przedzialem, w2 jest dopelnieniem przedzialu *)
let razy_pomocnicza_2 (w1 : wartosc) (w2 : wartosc) =
  let a = pierwszy w1
  and b = drugi w1
  and x = pierwszy w2
  and y = drugi w2
in
  if (pierwszy w1 = 0. && drugi w1 = 0.) then
    ((0., 0., true, true) : wartosc)
  else if in_wartosc w1 0. then 
    ((neg_infinity, infinity, true, true) : wartosc)
  else if (a > 0.) then
    ((x *. a, y *. a, false, true) : wartosc)
  else 
    ((y *. b, x *. b, false, true) : wartosc)


(* razy w1 w2, gdy w1 i w2 to dopelnienia przedzialow *)
let razy_pomocnicza_3 (w1 : wartosc) (w2 : wartosc) =
  if (not (in_wartosc w1 0. || in_wartosc w2 0.)) then
    let a = (pierwszy w1) *. (drugi w2)
    and b = (drugi w1) *. (pierwszy w2)
    and c = (pierwszy w1) *. (pierwszy w2)
    and d = (drugi w1) *. (drugi w2)
  in
    ((max a b, min c d, false, true) : wartosc)
  else
    ((neg_infinity, infinity, true, true) : wartosc)

(* {1 / a : a \in w}                    *)
(* zakladamy, ze w jest rozne od [0, 0] *)
let wartosc_odwrotna (w : wartosc) = 
  if (przedzial w && not (in_wartosc w 0.)) then
    ((1. /. (drugi w), 1. /. (pierwszy w), true, true) : wartosc)
  else if (przedzial w && (pierwszy w = 0.)) then
    ((1. /. (drugi w), infinity, true, true) : wartosc)
  else if (przedzial w && (drugi w = 0.)) then
    ((neg_infinity, 1. /. (pierwszy w), true, true) : wartosc)
  else if (przedzial w) then
    ((1. /. (pierwszy w), 1. /. (drugi w), false, true) : wartosc)
  else if (pierwszy w < 0. && drugi w > 0.) then
    ((1. /. (pierwszy w), 1. /. (drugi w), true, true) : wartosc)
  else if (not (pierwszy w = 0. || drugi w = 0.) && in_wartosc w 0.) then
    ((1. /. (drugi w), 1. /. (pierwszy w), false, true) : wartosc)
  else if (pierwszy w = 0.) then
    ((neg_infinity, 1. /. (drugi w), true, true) : wartosc)
  else 
    ((1. /. (pierwszy w), infinity, true, true) : wartosc)


(* modyfikatory *)

let plus (w1 : wartosc) (w2 : wartosc) = 
  if (not (liczba w1) || not (liczba w2)) then
    ((0., 0., true, false) : wartosc)
  else if (przedzial w1 && przedzial w2) then
    ((min_wartosc w1 +. min_wartosc w2, max_wartosc w1 +. max_wartosc w2,
    true, true) : wartosc)
  else if (przedzial w1) then
    plus_pomocnicza w1 w2
  else if (przedzial w2) then
    plus_pomocnicza w2 w1
  else 
    ((neg_infinity, infinity, true, true) : wartosc)

  
let minus (w1 : wartosc) (w2 : wartosc) = 
  plus w1 (wartosc_przeciwna w2)


let razy (w1 : wartosc) (w2 : wartosc) = 
  if (not (liczba w1) || not (liczba w2)) then
    ((0., 0., true, false) : wartosc)
  else if (przedzial w1 && przedzial w2) then
    razy_pomocnicza_1 w1 w2
  else if (przedzial w1) then
    razy_pomocnicza_2 w1 w2
  else if (przedzial w2) then
    razy_pomocnicza_2 w2 w1
  else 
    razy_pomocnicza_3 w1 w2


let podzielic (w1 : wartosc) (w2 : wartosc) = 
  if (pierwszy w2 = 0. && drugi w2 = 0.) then
    ((0., 0., true, false) : wartosc)
  else
    razy w1 (wartosc_odwrotna w2)



  
