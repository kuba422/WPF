exception Cykliczne ;;

let topol (lst : ('a * 'a list) list) = 
  (* tablica, ktora dla kazdego wierzcholka przechowuje liczbe krawedzi do niego wchodzacych *)
  let deg = Hashtbl.create (List.length lst)
  (* przeksztalcona w tablice wejsciowa lista *)
  and hash_lst = Hashtbl.create (List.length lst)
  (* kolejka (LIFO) wierzcholkow do sprawdzenia *)
  and queue = ref [] 
  (* lista posortowanych topologicznie wierzcholkow (w odwrotnej kolejnosci) *)
  and ans = ref []
  in
  (* tworzy stan poczatkowy deg *)
  List.iter
  (fun (element, neighbours) -> 
    begin
      if not (Hashtbl.mem deg element) then 
        Hashtbl.add deg element 0 ;
      List.iter 
      (fun key -> 
        match Hashtbl.find_opt deg key with
        | None -> Hashtbl.add deg key 1
        | Some value -> Hashtbl.replace deg key (value + 1))
      neighbours
    end)
  lst ;
  (* przetwarza wejsciowa liste na hash_lst *)
  List.iter
  (fun (element, neighbours) -> 
    match Hashtbl.find_opt hash_lst element with
    | None -> Hashtbl.add hash_lst element neighbours
    | Some value -> Hashtbl.replace hash_lst element (value @ neighbours))
  lst ;
  (* wstawiamy do kolejki wszystkie wierzcholki, do ktorych nie wchodza zadne krawedzie *)
  Hashtbl.iter 
  (fun key value ->
    if value = 0 then 
      queue := key :: !queue)
  deg ;
  while !queue <> [] do
    (* przetwarzamy pierwszy wierzcholek z kolejki *)
    let q = List.hd !queue 
    in
    (* usuwamy go z listy wierzcholkow do przetworzenia *)
    queue := List.tl !queue ;
    (* dodajemy go jako nastepny wierzcholek posortowanej topologicznie listy *)
    ans := q :: !ans ;
    (* jezeli z tego wierzcholka wychodzily jakies krawedzie, to je rowniez usuwamy *)
    if Hashtbl.mem hash_lst q then
      List.iter 
      (fun x -> 
        let value = Hashtbl.find deg x - 1
        in
        Hashtbl.replace deg x value ;
        (* jezeli do pewnego wierzcholka nie wchodza po tej operacji zadne krawedzie, to dodajemy go do kolejki *)
        if value = 0 then
          queue := x :: !queue)
      (Hashtbl.find hash_lst q) 
  done ;
  (* jezeli zostaly jakies nieprzetworzone krawedzie, to w grafie musial wystepowac cykl *) 
  Hashtbl.iter
  (fun _ value -> 
    if value <> 0 then
      raise Cykliczne)
  deg ;
  List.rev !ans ;;
