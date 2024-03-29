let przelewanka (a : (int * int) array) = 
  let n = Array.length a
  (* [Hashtbl.find stany s] reprezentuje minimalna liczbe operacji potrzebna do otrzymania stanu s ze stanu poczatkowego *)
  and stany = Hashtbl.create 100000 
  (* trzyma pary (stan, liczba operacji potrzebna do jego osiagniecia ze stanu poczatkowego), ktore nalezy sprawdzic *)
  and kolejka = Queue.create () 
  (* warunek konieczny do tego, zeby szukany stan mogl byc osiagalny przy danych pojemnosciach *)
  and warunek_konieczny = ref true 
  in
  (* zeby szukany stan byl osiagalny, to nwd pojemnosci wszystkich szklanek musi dzielic ilosc wody w kazdej szklance              *)
  (* dowod to prosta indukcja; przypadek bazowy: wszystkie szklanki sa puste                                                       *)
  (* krok indukcyjny: zakladamy, ze dla kazdego stanu osiagalnego w d krokach ten niezmiennik jest zachowany i pokazujemy,         *)
  (* ze po wykonaniu jednej operacji dla dowolnego takiego stanu otrzymamy stan, w ktorym ten niezmiennik rowniez bedzie zachowany *)
  let test_1 () = 
    let rec nwd x y = 
      if x >= y then
        if y = 0 then 
          x
        else 
          nwd y (x mod y)
      else
        nwd y x 
    in
    let nwd_pojemnosci = Array.fold_left (fun acc (x, _) -> nwd acc x) 0 a 
    in 
    if nwd_pojemnosci <> 0 then
      warunek_konieczny := !warunek_konieczny && Array.for_all (fun (_, y) -> y mod nwd_pojemnosci = 0) a
  (* zeby szukany stan byl osiagalny, to przynajmniej jedna szklanka musi byc pusta lub pelna; analogiczny dowod indukcyjny *)
  and test_2 () =
    warunek_konieczny := !warunek_konieczny && Array.exists (fun (x, y) -> y = 0 || y = x) a
  (* sprawdza, czy udalo sie juz osiagnac szukany stan; jezeli tak, to zatrzymuje poszukiwania czyszczac kolejke *)
  and czy_osiagniety () = 
    if Hashtbl.mem stany (Array.init n (fun i -> snd a.(i))) then
      Queue.clear kolejka 
  (* dla danego stanu sprawdza wszystkie stany, ktore mozna z niego otrzymac wykonujac 1 operacje pierwszego rodzaju *)
  and operacja_1 (s, d) =
    for i = 0 to n - 1 do
      let kopia_s = Array.copy s
      in
      kopia_s.(i) <- (fst a.(i)) ;
      if not (Hashtbl.mem stany kopia_s) then
      begin
        Queue.add (kopia_s, d + 1) kolejka ;
        Hashtbl.add stany kopia_s (d + 1) 
      end
    done
  (* dla danego stanu sprawdza wszystkie stany, ktore mozna z niego otrzymac wykonujac 1 operacje drugiego rodzaju *)
  and operacja_2 (s, d) = 
    for i = 0 to n - 1 do
      let kopia_s = Array.copy s
      in
      kopia_s.(i) <- 0 ;
      if not (Hashtbl.mem stany kopia_s) then
      begin
        Queue.add (kopia_s, d + 1) kolejka ;
        Hashtbl.add stany kopia_s (d + 1) 
      end
    done
  (* dla danego stanu sprawdza wszystkie stany, ktore mozna z niego otrzymac wykonujac 1 operacje trzeciego rodzaju *)
  and operacja_3 (s, d) = 
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do 
        if i <> j then
        let kopia_s = Array.copy s
        in
        begin
          if s.(i) + s.(j) <= (fst a.(j)) then
          begin
            kopia_s.(i) <- 0 ;
            kopia_s.(j) <- s.(i) + s.(j)
          end
          else 
          begin
            kopia_s.(i) <- s.(i) + s.(j) - (fst a.(j)) ;
            kopia_s.(j) <- (fst a.(j))
          end ;
          if not (Hashtbl.mem stany kopia_s) then
          begin
            Queue.add (kopia_s, d + 1) kolejka ;
            Hashtbl.add stany kopia_s (d + 1) 
          end 
        end
      done
    done
  in
  test_1 () ;
  test_2 () ;
  (* przeszukujemy graf wszerz, dopoki nie dojdziemy do szukanego stanu       *)
  (* lub nie przejdziemy calej spojnej skladowej zawierajacej stan poczatkowy *)
  if !warunek_konieczny || n = 0 then
  begin
    Queue.add (Array.make n 0, 0) kolejka ;
    Hashtbl.add stany (Array.make n 0) 0 ;
    while not (Queue.is_empty kolejka) do
      let (s, d) = Queue.pop kolejka 
      in
      operacja_1 (s, d) ;
      operacja_2 (s, d) ;
      operacja_3 (s, d) ; 
      czy_osiagniety ()  
    done 
  end ;
  (* sprawdzamy, czy szukany stan byl osiagalny ze stanu poczatkowego *)
  match Hashtbl.find_opt stany (Array.init n (fun i -> snd a.(i))) with
  | None -> -1
  | Some d -> d ;;
