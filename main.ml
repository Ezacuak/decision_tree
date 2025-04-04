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

(***********************************************)
(**                                           **)
(**     Définition des types & structures     **)
(**                                           **)
(***********************************************)

(* NOTE: Type arbre de decision *)
type decision_tree =
    | Leaf of bool
    | Node of string * decision_tree * decision_tree


let () = print_endline "Hello World !"



(**************************)
(**                      **)
(**     Zone de test     **)
(**                      **)
(**************************)

let sport_tree = Node("Plonger",
    Node("Tournoi", Leaf true,
        Node("Gagner", Leaf true, 
            Node("Final", Leaf true, Leaf false))),
    Leaf false)

