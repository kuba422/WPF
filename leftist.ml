(* projekt:   Drzewa lewicowe *)
(* autor:     Jakub Jagiełła  *)
(* recenzent: Anna Pawłowska  *)

(* priorytet, prawa wysokosc, lewe poddrzewo, prawe poddrzewo *)
type 'a queue = 
  | Null
  | Node of 'a * int * 'a queue * 'a queue

(* funkcja pomocnicza *)
let height (q : 'a queue) = 
  match q with
  | Null -> 0
  | Node(_, h, _, _) -> h

let empty = Null

exception Empty

let is_empty (q : 'a queue) = 
  (q = Null)

let rec join (q1 : 'a queue) (q2 : 'a queue) =
  match (q1, q2) with
  | (Null, q) -> q
  | (q, Null) -> q
  | (Node(p1, _, l1, r1), Node(p2, _, _, _)) ->
    if (p1 > p2) then 
      join q2 q1 
    else
      let q3 = join r1 q2
      in
      if (is_empty q3) then
        Node(p1, 1, l1, Null)
      else if (is_empty l1) then
        Node(p1, 1, q3, Null)
      else if (height q3 > height l1) then
        Node(p1, height l1 + 1, q3, l1)
      else
        Node(p1, height q3 + 1, l1, q3)

let add (element : 'a) (q : 'a queue) = 
  join q (Node(element, 0, Null, Null))

let delete_min (q : 'a queue) =
  match q with 
  | Null -> raise Empty
  | Node(p, _, l, r) -> (p, join l r)
