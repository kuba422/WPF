(* Projekt: Modyfikacja_Drzew *)
(* Autor: Jakub Jagiella      *)
(* Recenzent: Olaf Placha     *)

(* typ reprezentujacy przedzial            *)
(* ((x, y) : interval) reprezentuje [x, y] *)
type interval = int * int

(* typ reprezentujacy drzewo, zawiera odpowiednio:      *)
(* (lewe poddrzewo, przedzial z wezla, prawe poddrzewo, *)
(* liczbe liczb calkowitych w poddrzewach)              *)

(* stala zwiazana z balansowaniem drzew *)
let const = 2;;

type t =
  | Empty
  | Node of t * interval * t * int * int

let empty = Empty

let is_empty (s : t) = 
  s = Empty

(* dodawanie zabezpieczone przed wychodzeniem poza zakres inta *)
(* dziala w czasie O(1)                                        *)
let (++) (x : int) (y : int) = 
  if (x > 0 && y > 0 && x >= max_int - y) then
    max_int
  else if (x < 0 && y < 0 && x <= min_int - y) then
    min_int
  else
    x + y

(* zwraca wysokosc danego drzewa *)
(* dziala w czasie O(1)          *)
let height = function
  | Node (_, _, _, h, _) -> h
  | Empty -> 0

(* zwraca dlugosc przedzialu *)
(* dziala w czasie O(1)      *)
let width ((x, y) : interval) =
  if x = min_int then
    y ++ max_int ++ 2
  else
    y ++ (-x) ++ 1

(* zwraca liczbe liczb calkowitych w danym drzewie *)
(* dziala w czasie O(1)                            *)
let full_card = function
  | Node (_, k, _, _, c) -> c ++ width k
  | Empty -> 0 

(* sprawdza, czy element nalezy do przedzialu *)
(* dziala w czasie O(1)                       *)
let in_interval (n : int) ((x, y) : interval) =
  (x <= n && n <= y) 

(* tworzy drzewo z danych poddrzew i przedzialu    *)
(* zakladamy, ze wszystkie przedzialy sa rozlaczne *)
(* dziala w czasie O(1)                            *)
let make (l : t) (k : interval) (r : t) =
  Node (l, k, r, 1 ++ max (height l) (height r), full_card l ++ full_card r)

(* tworzy wywazone drzewo z dwoch drzew i przedzialu *)
(* dziala w czasie O(1)                              *)
let bal (l : t) (k : interval) (r : t) =
  let hl = height l in
  let hr = height r in
  if hl > hr + const then
    match l with
    | Node (ll, lk, lr, _, _) ->
        if height ll >= height lr then make ll lk (make lr k r)
        else
          (match lr with
          | Node (lrl, lrk, lrr, _, _) ->
              make (make ll lk lrl) lrk (make lrr k r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + const then
    match r with
    | Node (rl, rk, rr, _, _) ->
        if height rr >= height rl then make (make l k rl) rk rr
        else
          (match rl with
          | Node (rll, rlk, rlr, _, _) ->
              make (make l k rll) rlk (make rlr rk rr)
          | Empty -> assert false)
    | Empty -> assert false
  else make l k r

(* dodaje do s przedzial [x,y]                                  *)
(* zakladamy, ze jest on scisle mniejszy/wiekszy niz elementy s *)
(* dziala w czasie O(1)                                         *)
let aux_add ((x, y) : interval) (s : t) = 
  if (x > y) then s else
  match s with
  | Empty -> make Empty (x, y) Empty
  | Node (l, k, r, _, _) ->
    if (y < fst k) then
      bal Empty (x, y) s
    else
      bal s (x, y) Empty

(* laczy przedzial z drzewami                      *)
(* zakladamy, ze przedzial i zbiory reprezentowane *)
(* przez drzewa sa wzajemnie rozlaczne             *)
(* dziala w czasie O(log n), poniewaz przy kazdym  *)
(* kolejnym  wywolaniu rekurencyjnym zmniejsza     *)
(* sie suma wysokosci laczonych drzew              *)
let rec join (l : t) (k : interval) (r : t) =
  match (l, r) with
  | (Empty, _) -> aux_add k r
  | (_, Empty) -> aux_add k l
  | (Node(ll, lk, lr, lh, _), Node(rl, rk, rr, rh, _)) ->
      if lh > rh + const then 
        bal ll lk (join lr k r) 
      else if rh > lh + const then 
        bal (join l k rl) rk rr 
      else
        make l k r

(* zwraca najmniejszy przedzial                              *)
(* dziala w czasie O(log n), poniewaz wywolanie rekurencyjne *)
(* dotyczy drzewa o scisle mniejszej wysokosci               *)
let rec min_elt = function
  | Node (Empty, k, _, _, _) -> k
  | Node (l, _, _, _, _) -> min_elt l
  | Empty -> raise Not_found

(* usuwa najmniejszy przedzial                               *)
(* dziala w czasie O(log n), poniewaz wywolanie rekurencyjne *)
(* dotyczy drzewa o scisle mniejszej wysokosci               *)
let rec remove_min_elt = function
  | Node (Empty, _, r, _, _) -> r
  | Node (l, k, r, _, _) -> bal (remove_min_elt l) k r
  | Empty -> invalid_arg "ISet.remove_min_elt"

(* zwraca najwiekszy przedzial                               *)
(* dziala w czasie O(log n), poniewaz wywolanie rekurencyjne *)
(* dotyczy drzewa o scisle mniejszej wysokosci               *)
let rec max_elt = function
  | Node (_, k, Empty, _, _) -> k
  | Node (_, _, r, _, _) -> max_elt r
  | Empty -> raise Not_found

(* usuwa najwiekszy przedzial                                *)
(* dziala w czasie O(log n), poniewaz wywolanie rekurencyjne *)
(* dotyczy drzewa o scisle mniejszej wysokosci               *)
let rec remove_max_elt = function
  | Node (l, _, Empty, _, _) -> l
  | Node (l, k, r, _, _) -> bal l k (remove_max_elt r)
  | Empty -> invalid_arg "iSet.remove_min_elt"

(* laczy rozlaczne drzewa w jedno              *)
(* dziala w czasie O(log n), poniewaz  min_elt *)
(* i remove_min_elt dzialaja w czasie O(log n) *)  
let merge (s1 : t) (s2 : t) =
  match (s1, s2) with
  | (Empty, _) -> s2
  | (_, Empty) -> s1
  | _ -> bal s1 (min_elt s2) (remove_min_elt s2)

(* dziala w czasie O(log n), poniewaz wywolanie rekurencyjne *)
(* dotyczy drzewa o scisle mniejszej wysokosci               *)
let mem (x : int) (s : t) =
  let rec loop = function
    | Node (l, k, r, _, _) ->
      if (in_interval x k) then 
        true
      else if (x < fst k) then 
        loop l
      else 
        loop r
    | Empty -> false 
  in loop s

(* dziala w czasie O(log n), poniewaz wywolanie rekurencyjne *)
(* dotyczy drzewa o scisle mniejszej wysokosci               *)
let below (x : int) (s : t) = 
  let rec loop acc = function
    | Node (l, k, r, _, _) ->
      if (in_interval x k) then
        acc ++ (width (fst k, x)) ++ (full_card l)
      else if (x > snd k) then 
        loop (acc ++ (width k) ++ (full_card l)) r
      else 
        loop acc l
    | Empty -> acc
  in loop 0 s

(* dziala w czasie O(n), poniewaz wywoluje sie *) 
(* dla kazdego wierzcholka dokladnie raz       *)
let elements (s : t) = 
  let rec loop acc = function
    | Node(l, k, r, _, _) -> loop (k :: loop acc r) l 
    | Empty -> acc
  in loop [] s

(* dziala w czasie \Omega(n), poniewaz wywoluje *) 
(* sie dla kazdego wierzcholka dokladnie raz    *)
(* (gorne ograniczenie zalezy od zlozonosci f)  *)
let iter f (s : t) =
  let rec loop = function
    | Node (l, k, r, _, _) -> loop l; f k; loop r 
    | Empty -> ()
  in loop s

(* dziala w czasie \Omega(n), poniewaz wywoluje *) 
(* sie dla kazdego wierzcholka dokladnie raz    *)
(* (gorne ograniczenie zalezy od zlozonosci f)  *)
let fold f (s : t) acc =
  let rec loop acc = function
    | Node (l, k, r, _, _) -> loop (f k (loop acc l)) r 
    | Empty -> acc
  in loop acc s  

(* dziala w czasie O(log n), poniewaz aux_add wywola sie *)
(* co najwyzej dwa razy, a czas wszystkich wywolan       *)
(* sumuje sie do O(log n), bo koszt funkcji join jest    *)
(* proporcjonalny do roznicy wysokosci laczanych drzew   *)
let split (x : int) (s : t) = 
  let rec loop = function
    | Empty -> (Empty, false, Empty)
    | Node (l, k, r, _, _) ->
      if (in_interval x k) then
        if (x = max_int) then 
          (aux_add (fst k, x - 1) l, true, Empty)
        else if (x = min_int) then
          (Empty, true, aux_add (x + 1, snd k) r)
        else
          (aux_add (fst k, x - 1) l, true, aux_add (x + 1, snd k) r)
      else if x < fst k then
        let (ll, pres, rl) = loop l 
        in (ll, pres, join rl k r)
      else
        let (lr, pres, rr) = loop r 
        in (join l k lr, pres, rr)
  in loop s

(* dziala w czasie O(log n), poniewaz funkcje *)
(* split i merge dzialaja w czasie O(log n)   *)
let remove ((x, y) : interval) (s : t) = 
  let (left, _, r) = split x s in
  let (_, _, right) = split y r
  in merge left right

(* dziala w czasie O(log n), poniewaz wszystkie wywolywane *)
(* funkcje dzialaja w czasie co najwyzej O(log n)          *)
let add ((x, y) : interval) (s : t) = 
  let (lx, _, rx) = split x s
  and (ly, _, ry) = split y s
  in
  match (lx, ry) with
  | (Empty, Empty) -> make Empty (x, y) Empty
  | (Empty, _) -> 
    let (xr, yr) = min_elt ry in
    if (y ++ 1 = xr) then 
      aux_add (x, yr) (remove_min_elt ry)
    else
      aux_add (x, y) ry
  | (_, Empty) ->
    let (xl, yl) = max_elt lx in
    if (yl ++ 1 = x) then
      aux_add (xl, y) (remove_max_elt lx)
    else
      aux_add (x, y) lx
  | (_, _) ->
    let (xr, yr) = min_elt ry 
    and (xl, yl) = max_elt lx in
    if (y ++ 1 = xr && yl ++ 1 = x) then 
      join (remove_max_elt lx) (xl, yr) (remove_min_elt ry)
    else if (y ++ 1 = xr) then 
      join lx (x, yr) (remove_min_elt ry)
    else if (yl ++ 1 = x) then 
      join (remove_max_elt lx) (xl, y) ry
    else 
      join lx (x, y) ry
