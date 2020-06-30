(*---------------------------------DEFINITON DES TYPES---------------------------------*)
type ab =
  |Nil
  |Node of int * ab * ab
;;

(*-------------------------------------------------------------------------------------*)

(*-----------------------------------Affichage-----------------------------------------*)

let rec space k = 
  if k <= 0 then ""
  else " "^(space (k - 1))
;;
	     
let rec line k =  
  if k <= 0 then ""
  else "-"^(line (k - 1))
;;
  
let croisement l1 ls1 ln l2 ls2 =
  let spc_center = space ln in
  let spc_left   = (space l1)^spc_center
  and spc_right  = spc_center^(space l2) in
  let rec aux ls1 ls2 = match ls1, ls2 with
      [], [] -> []
    | s1::ls1', [] -> (s1^spc_right)::(aux ls1' [])
    | [], s2::ls2' -> (spc_left^s2)::(aux [] ls2')
    | s1::ls1', s2::ls2' -> (s1^spc_center^s2)::(aux ls1' ls2')
  in aux ls1 ls2
;;

let rec levels_of_tree t = match t with 
    Nil -> ((1, 0), ["*"]) 
  | Node(n, g, d) -> 
     let sn = string_of_int n in
    let ln = String.length sn in 
    let (l1, ofs1), ls1 =  levels_of_tree g 
    and (l2, ofs2), ls2 =  levels_of_tree d in 
    let sr_top = 
      (space (ofs1 + 1))^
	(line (l1 - ofs1 - 1))^
	  (sn)^
	    (line ofs2)^
	      (space (l2 - ofs2))  in
    let sr_bot = 
      (space ofs1)^"|"^
	(space (l1 - ofs1 - 1))^
	  (space ln)^
	    (space ofs2)^"|"^
	      (space (l2 - ofs2 - 1))  in
    
    let lc = croisement l1 ls1 ln l2 ls2  in 
    ((l1 + ln + l2, l1), sr_top::sr_bot::lc)
;;
  
let print_int_tree tree = 
  List.iter (fun s -> print_string (s^"\n")) (snd (levels_of_tree tree));;
  
(*-------------------------------------------------------------------------------------*)
  

(* fonctions qui permettent de trier une liste de nombres*)
let rec rev_append left right = match left with
    [] -> right
   |t::q -> rev_append q (t::right)
;;
  
let insert x l = 
  let rec aux ll m = match ll with
      [] -> rev_append m [x]
     |t::q -> if  x<=t then rev_append m (x::ll) 
              else aux q (t::m) 
  in aux l []
;;
  
let sort l = 
  let rec aux acc ll = match ll with
      [] -> acc
     |t::q -> aux (insert t acc) q
  in aux [] l
;;
(*-----------------------------------------------------------------------------------------------*)
  
  
(* formule de kraft sur 2-aire *)
let sum_lg tab_longueurs_mots =
  let rec aux ll acc = match ll with
    |[] -> acc
    |t::q -> aux q ((1. /. (2. ** t)) +. acc)
  in aux tab_longueurs_mots 0.
;;
  
(* retourne true s'il existe un code prefixe sinon false *)
let is_prefix tab_longueurs_mots =
  if sum_lg tab_longueurs_mots <= 1. then true else false
;;
  
(*fonction ramdom qui determinera vers quel fils aller, 0-->fils gauche et 1->fils droite*)
let leftorright() = Random.int(2);;
  
(*hauteur d'un arbre*)
let hauteur a =
  let rec aux a acc = match a with
    |Nil -> acc
    |Node(_,g,d) -> max (aux g (1+acc)) (aux d (1+acc))
  in aux a 0
;;
  
(*retourne true si un arbre est complet sinon false NB: cette version *)
let complet_partiel a = match a with
  |Nil -> false
  |Node(_,g,d) -> if (hauteur g = hauteur d) then true else false
;;
  
(*fonction qui prend la longueur d'un mot de code et construit l'arboresence d'un mot de code correspondant*)
let cons_prefix long_mot a =
  let rec aux_cons_prefix n a =
    if long_mot -. n = 0. then a
    else
      let bit = leftorright() in
      match a with
      |Nil -> a
      |Node(r,g,d) ->
	if bit=0 then	  
	  if complet_partiel g then
	    match d with
	    |Nil -> Node(r, g, (aux_cons_prefix (n +. 1.) (Node(1, Nil, Nil))))
	    |Node(_,_,_) -> Node(r, g, aux_cons_prefix (n +. 1.) d)
	  else
	    match g with
	    |Nil -> Node(r, (aux_cons_prefix (n +. 1.) (Node(0, Nil, Nil))), d)
	    |Node(_,_,_) -> Node(r, aux_cons_prefix (n +. 1.) g, d)
				
	else
	  if complet_partiel d then
	    match g with
	    |Nil -> Node(r, (aux_cons_prefix (n +. 1.) (Node(0, Nil, Nil))), d)
	    |Node(_,_,_) -> Node(r, aux_cons_prefix (n +. 1.) g, d)
	  else
	    match d with
	    |Nil -> Node(r, g, (aux_cons_prefix (n +. 1.) (Node(1, Nil, Nil))))
	    |Node(_,_,_) -> Node(r, g, aux_cons_prefix (n +. 1.) d)
				
  in aux_cons_prefix 0. a
;;
  
(*fonction qui construit l'arbre complet de la liste des longueurs des mots code donnée en parametre*)
let generate_ab list_mot_code =
  let rec aux l a = match l with
      [] -> a
     |t::q -> cons_prefix t (aux q a)
  in aux (List.rev (sort list_mot_code)) (Node(1010, Nil, Nil))      (*la racine aura arbitrairement la valeur 1010*)
;;
  
(* fonction qui prend en argument un arbre et retourne la liste des mots de code (tous les chemins de la racine à chaque feuille *)
let get_bits a =
  let rec aux a acc list = match a with
      Nil -> list
     |Node(r,g,d) ->
       match g,d with
       |Nil,Nil -> if r=1010 then acc::list else (List.rev (r::acc))::list
       |_,_ -> if r=1010 then aux g acc list @ aux d acc list
	       else aux g (r::acc) list @ aux d (r::acc) list
  in aux a [] []
;;

let rec affiche l = match l with
  |[] -> print_string " ; "
  |t::q -> print_int t; affiche q
;;


(*List.map affiche [[1];[0;1];[0;0;0]];;*)
  
  
(*let list = List.map float_of_string (Str.split_on_char ',' "1,2,2");;
let b = generate_ab list;;
  print_int_tree b;;
    get_bits b;;*)
      
      
      
(*------------------------------------------ MAIN -----------------------------------------------------*)  
let main () =
  try
    let list_long_mots = List.map float_of_string (String.split_on_char ',' Sys.argv.(1)) in
    if (is_prefix list_long_mots)
    then
      let arb = generate_ab list_long_mots in
      List.map affiche (get_bits arb);
      print_endline "";
      print_int_tree arb;
    else
      failwith "il n'existe pas des mots de code correspondants à ces longueurs"
  with
  |Sys_error _ -> failwith "erreur de parametres"
;;
  
 main ();;
