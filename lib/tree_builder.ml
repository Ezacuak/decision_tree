open Types


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
  let rec aux d l =
    match d with
    | [] -> Empty
    | [ (_, s) ] -> Leaf s
    | (_, s) :: _ when is_pure d -> Leaf s
    | _ -> (
        match l with
        | [] -> Empty
        | fst :: rst ->
            let yes, no = sous_doc d fst in
            if is_pure yes then Node (fst, Leaf true, aux no rst)
            else if is_pure no then Node (fst, aux yes rst, Leaf false)
            else Node (fst, aux yes rst, aux no rst))
  in
  let words = get_word_list_from_doc d in
  aux d words