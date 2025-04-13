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

open Types
open Utils
open Decision_tree
open Test_data

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