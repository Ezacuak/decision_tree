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

(* NOTE: Import *)

open List


let red = "\027[31m"
let green = "\027[32m"
let yellow = "\027[33m"
let blue = "\027[34m"
let reset = "\027[0m"

let debug fn m = print_endline (blue ^ "[" ^ fn ^"]: " ^ reset ^ m)

(***********************************************)
(**                                           **)
(**     Définition des types & structures     **)
(**                                           **)
(***********************************************)

(* NOTE: Type arbre de decision *)
type decision_tree =
    | Empty
    | Leaf of bool
    | Node of string * decision_tree * decision_tree

(* NOTE: Format des donnée sous la forme suivante: *)
(*
    Document -> data list
    data -> (word list, decision)
    word -> srting
    decision -> bool
*)

type word = string
type decision = bool
type data = word list * decision
type document = data list


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

(*************************************)
(**                                 **)
(**                                 **)
(**     Construction d'un arbre     **)
(**     A partire d'un document     **)
(**                                 **)
(**                                 **)
(*************************************)

(***)
let get_word_list_from_doc doc =
    let rec aux doc l = match doc with
    | [] -> l
    | (words, _)::t -> aux t (l @ (filter (fun e -> not (mem e l)) words))
    in aux doc []

(**
   val is_pure: document -> bool

   Verifie la pureté d'un document
   C'est à dire si il ne contient que des donnée de meme signe (decision)
*)
let rec is_pure d = match d with
    | [] -> true
    | (_, s)::t -> s && is_pure t


(*
alternative avec la fonction partition de List:

let sous_doc d w = partition (fun (ws, _) -> mem w s) d
*)
let rec sous_doc d w = match d with
    | [] -> [], []
    | (ws, s)::t ->
        let (yes, no) = sous_doc t w in
        if (mem w ws) then ((ws, s)::yes, no)
        else (yes, (ws, s)::no)

let build_tree d =
    let rec aux d l = match d with
        | [] -> Empty 
        | [(_, s)] -> Leaf s
        | (_, s)::_ when is_pure d -> Leaf s    (* si le document est pure -> Une feuille du signe de la decesion *)
        | h::t ->
            match l with
            | [] -> Empty 
            | fst::rst -> (* Ce cas de devrait pas arriver *)
                let (yes, no) = sous_doc d fst in
                Node(fst, (aux yes rst), (aux no rst))

    in let words = get_word_list_from_doc d
    in aux d words

(**************************)
(**                      **)
(**     Zone de test     **)
(**                      **)
(**************************)


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

