open Types
open List

(* Couleurs pour l'affichage *)
let red = "\027[31m"
let green = "\027[32m"
let yellow = "\027[33m"
let blue = "\027[34m"
let reset = "\027[0m"

let debug fn m = print_endline (blue ^ "[" ^ fn ^"]: " ^ reset ^ m)

(* Fonctions d'affichage *)
let print_document (doc : document) =
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

(* Fonction pour afficher un arbre de décision *)
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
