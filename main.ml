(*
 ███████████ ███████████     ███████████ █████ ██████   █████   █████████   █████
░█░░░███░░░█░░███░░░░░███   ░░███░░░░░░█░░███ ░░██████ ░░███   ███░░░░░███ ░░███
░   ░███  ░  ░███    ░███    ░███   █ ░  ░███  ░███░███ ░███  ░███    ░███  ░███
    ░███     ░██████████     ░███████    ░███  ░███░░███░███  ░███████████  ░███
    ░███     ░███░░░░░░      ░███░░░█    ░███  ░███ ░░██████  ░███░░░░░███  ░███
    ░███     ░███            ░███  ░     ░███  ░███  ░░█████  ░███    ░███  ░███      █
    █████    █████           █████       █████ █████  ░░█████ █████   █████ ███████████
   ░░░░░    ░░░░░           ░░░░░       ░░░░░ ░░░░░    ░░░░░ ░░░░░   ░░░░░ ░░░░░░░░░░░

    Sujet: Arbre de décision

    Antonin Plard
*)

(* ======================================================== *)
(* =================== IMPORTS & UTILS ==================== *)
(* ======================================================== *)

open List


let red = "\027[31m"
let green = "\027[32m"
let yellow = "\027[33m"
let blue = "\027[34m"
let reset = "\027[0m"

let debug fn m = print_endline (blue ^ "[" ^ fn ^"]: " ^ reset ^ m)

(* ======================================================== *)
(* ============= DÉFINITION DES TYPES DE BASE ============= *)
(* ======================================================== *)

(* Structure de l'arbre de décision *)
type decision_tree =
    | Empty
    | Leaf of bool
    | Node of string * decision_tree * decision_tree

(* Format des données:
    Document -> data list
    data -> (word list, decision)
    word -> string
    decision -> bool
*)

type word = string
type decision = bool
type data = word list * decision
type document = data list

(* ======================================================== *)
(* ============= FONCTIONS D'AFFICHAGE ==================== *)
(* ======================================================== *)

let print_document (doc : (string list * bool) list) =
  List.iter (fun (ws, d) ->
    print_string "[ ";
    List.iter (fun w -> print_string (w ^ " ")) ws;
    print_string "]";
    print_string " -> ";
    print_endline (if d then "true" else "false")
  ) doc

let print_list l =
  print_string "[ ";
  List.iter (fun x -> print_string (x ^ " ")) l;
  print_string "]\n"

(* ======================================================== *)
(* ======== CONSTRUCTION DES ARBRES DE DÉCISION =========== *)
(* ======================================================== *)

(* Extraction de la liste de mots à partir d'un document *)
let get_word_list_from_doc doc =
    let rec aux doc l = match doc with
    | [] -> l
    | (words, _)::t -> aux t (l @ (filter (fun e -> not (mem e l)) words))
    in aux doc []

(* Vérification de la pureté d'un document 
   (si tous les éléments ont la même décision) *)
let rec is_pure d = match d with
    | [] -> true
    | (_, s)::t -> s && is_pure t


(* Division du document selon la présence d'un mot *)
let rec sous_doc d w = match d with
    | [] -> [], []
    | (ws, s)::t ->
        let (yes, no) = sous_doc t w in
        if (mem w ws) then ((ws, s)::yes, no)
        else (yes, (ws, s)::no)

(* Construction naïve d'un arbre de décision *)
let build_tree_naif d =
    let rec aux d l = match d with
        | [] -> Empty 
        | [(_, s)] -> Leaf s
        | (_, s)::_ when is_pure d -> Leaf s
        | h::t ->
            match l with
            | [] -> Empty 
            | fst::rst ->
                let (yes, no) = sous_doc d fst in
                Node(fst, (aux yes rst), (aux no rst))

    in let words = get_word_list_from_doc d
    in aux d words

(* Construction optimisée d'un arbre de décision *)
let build_tree_opti d =
    let rec aux d l = match d with
        | [] -> Empty 
        | [(_, s)] -> Leaf s
        | (_, s)::t when is_pure d -> Leaf s
        | h::t ->
            match l with
            | [] -> Empty 
            | fst::rst ->
                let (yes, no) = sous_doc d fst in
                if (is_pure yes) then Node(fst, Leaf true, aux no rst)
                else if (is_pure no) then Node(fst, aux yes rst, Leaf false)
                else Node(fst, (aux yes rst), (aux no rst))

    in let words = get_word_list_from_doc d
    in aux d words

(* ======================================================== *)
(* ============= CLASSIFICATION ET ÉVALUATION ============= *)
(* ======================================================== *)

(* Fonction d'affichage d'un arbre de décision *)
let rec print_tree tree depth =
  let indent = String.make (depth * 2) ' ' in
  match tree with
  | Empty -> print_endline (indent ^ "Empty")
  | Leaf value -> print_endline (indent ^ "Leaf " ^ (if value then "true" else "false"))
  | Node (word, left, right) -> 
      print_endline (indent ^ "Node (" ^ word ^ ")");
      print_string (indent ^ "  Si présent: ");
      print_tree left (depth + 1);
      print_string (indent ^ "  Si absent: ");
      print_tree right (depth + 1)

(* Classification d'une liste de mots avec un arbre de décision *)
let classify tree words =
  let rec aux t ws = match t with
    | Empty -> failwith "Impossible de classer: arbre vide"
    | Leaf b -> b
    | Node (word, left, right) ->
        if mem word ws then aux left ws else aux right ws
  in aux tree words

(* ======================================================== *)
(* ============= JEUX DE DONNÉES ET TESTS ================= *)
(* ======================================================== *)

let doc_1 = [
    (["mange"; "chocolat"; "toto"; "bras"], true);
    (["école"; "maîtresse"; "toto"], true);
    (["mange"; "chocolat"; "toto"; "maîtresse"], true);
    (["maîtresse"; "mange"], false);
    (["toto"; "chocolat"; "maîtresse"], true);
    (["chocolat"; "maîtresse"; "bras"], false);
]

let doc_1_tree = Node("mange",
    Node("chocolat",
        Leaf true,
        Leaf false
    ),
    Node("chocolat", 
        Node("toto",
            Leaf true,
            Leaf false
        ),
        Leaf false
    )
)

(* Test des fonctions de construction d'arbre *)
let test_tree_build doc =
  print_endline (yellow ^ "Document original:" ^ reset);
  print_document doc;
  
  print_endline (yellow ^ "\nConstruction de l'arbre naïf:" ^ reset);
  let tree_naif = build_tree_naif doc in
  print_tree tree_naif 0;
  
  print_endline (yellow ^ "\nConstruction de l'arbre optimisé:" ^ reset);
  let tree_opti = build_tree_opti doc in
  print_tree tree_opti 0;
  
  print_endline ""

(* Test de la classification d'exemples *)
let test_classify tree example expected =
  let words, _ = example in
  let result = classify tree words in
  print_string "Classification de ";
  print_list words;
  print_string "Résultat: ";
  if result = expected then
    print_endline (green ^ (if result then "true" else "false") ^ " (correct)" ^ reset)
  else
    print_endline (red ^ (if result then "true" else "false") ^ " (incorrect, attendu: " ^ (if expected then "true" else "false") ^ ")" ^ reset)

let sport_doc = [
    (["tournoi"; "Irlande";"retrouver";"goût"; "victoire"], true);
    (["ligue"; "champion";"Paris";"demi-finale"], true);
    (["AMD"; "gagner";"course";"processeur"], false);
    (["JO"; "Paris";"Romain Ntamak";"porter"; "flamme"; "olympique"; "Occitanie"], true);
    (["JO"; "Paris";"menace";"grève"; "SNCF"], false)
]

let sport_tree = Node("Plonger",
    Node("Tournoi", Leaf true,
        Node("Gagner", Leaf true, 
            Node("Final", Leaf true, Leaf false))),
    Leaf false)

(* ======================================================== *)
(* =================== FONCTION PRINCIPALE ================ *)
(* ======================================================== *)

(* Exécution des tests *)
let main () =
  print_endline (yellow ^ "=== Test avec doc_1 ===" ^ reset);
  test_tree_build doc_1;
  
  let test_examples = [
    (["mange"; "chocolat"], true);
    (["maîtresse"; "bras"], false);
    (["toto"; "chocolat"], true);
  ] in
  
  print_endline (yellow ^ "Tests de classification avec l'arbre naïf:" ^ reset);
  let tree_naif = build_tree_naif doc_1 in
  List.iter (fun (words, expected) -> test_classify tree_naif (words, expected) expected) test_examples;
  
  print_endline (yellow ^ "\nTests de classification avec l'arbre optimisé:" ^ reset);
  let tree_opti = build_tree_opti doc_1 in
  List.iter (fun (words, expected) -> test_classify tree_opti (words, expected) expected) test_examples;
  
  print_endline (yellow ^ "\n=== Test avec sport_doc ===" ^ reset);
  test_tree_build sport_doc

(* Point d'entrée du programme *)
let () = main ()