open Types
open List

let get_word_list_from_doc doc =
  let rec aux doc l =
    match doc with
    | [] -> l
    | (words, _) :: t ->
        aux t (l @ List.filter (fun e -> not (List.mem e l)) words)
  in
  aux doc []

(* Vérification de la pureté d'un document *)
let rec is_pure d = match d with [] -> true | (_, s) :: t -> s && is_pure t

(* Division du document selon la présence d'un mot *)
let rec sous_doc d w =
  match d with
  | [] -> ([], [])
  | (ws, s) :: t ->
      let yes, no = sous_doc t w in
      if List.mem w ws then ((ws, s) :: yes, no) else (yes, (ws, s) :: no)

(* Construction naïve d'un arbre de décision *)
let build_tree_naif d =
  let rec aux d l = match d with
    | [] -> Empty 
    | [ (_, s) ] -> print_endline "Doc 1 elem";Leaf s
    | _ -> print_endline "cas recursife"; match l with
        | [] -> failwith "Liste de mot vide\n"
        | fst :: rst ->
            let yes, no = sous_doc d fst in
            Node (fst, aux yes rst, aux no rst)
  in let words = get_word_list_from_doc d in
  aux d words

(*===============================================*)
(*     Arbre optimiser avec fonction de gain     *)
(*===============================================*)

let log2 x = if x = 0.0 then x else log x /. log 2.0

(* calcule de l'entropy d'un essemble *)
let entrpoy d =
  let n = length d in
  if n = 0 then 0.0
  else
    let n_pos = length (filter (fun (_, s) -> s) d) in
    let n_neg = n - n_pos in

    let p_pos =
      float_of_int n_pos
      /. float_of_int n (* proportion de positife dans le doc *)
    and p_neg = float_of_int n_neg /. float_of_int n in
    (-.p_pos *. log2 p_pos) -. (p_neg *. log2 p_neg)

let gain w d =
  let p, a = sous_doc d w in
  let e1 = entrpoy d in
  let e2 = entrpoy p in
  let e3 = entrpoy a in
  e1 -. e2 -. e3

(* Calucule de meillieur mot pour la separation d'un essemble *)
let get_best_word d =
  let rec aux l =
    match l with
    | [] -> failwith "No word found"
    | [ h ] -> h
    | h :: t ->
        let g = gain h d in
        if g <= gain (aux t) d then h else aux t
  in
  let words = get_word_list_from_doc d in
  aux words

let rec build_tree d =
    match d with
    | [] -> Empty
    | [ (_, s) ] -> Leaf s
    | (_, s) :: _ when is_pure d -> Leaf s
    | _ ->
        let w = get_best_word d in
        let yes, no = sous_doc d w in
        Node (w, build_tree yes, build_tree no)
