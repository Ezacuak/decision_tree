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

(* NOTE: Import des phases *)

#use "learn.ml";; (* Phase d'apprentissage *)
#use "test.ml";;  (* Phase de test *)

open List;;
(***********************************************)
(**                                           **)
(**     Définition des types & structures     **)
(**                                           **)
(***********************************************)

(* NOTE: Type arbre de decision *)
type decision_tree =
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

(*************************************)
(**                                 **)
(**                                 **)
(**     Construction d'un arbre     **)
(**     A partire d'un document     **)
(**                                 **)
(**                                 **)
(*************************************)


let get_word_list_from_doc doc =
    let rec aux doc l = match doc with
    | [] -> l
    | (words, _)::t -> aux t (l @ (filter (fun e -> not (mem e l)) words)))
    in aux doc []


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

