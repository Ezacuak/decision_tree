open Types

(* Fonction de calcul de l'entropie d'un ensemble de documents *)
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
let print_tree (tree : decision_tree) =
  let rec print_tree_aux indent tree =
    match tree with
    | Empty -> print_string (indent ^ "Empty\n")
    | Leaf value -> print_string (indent ^ "Leaf(" ^ (if value then "true" else "false") ^ ")\n")
    | Node (attribute, left, right) ->
        print_string (indent ^ "Node(" ^ attribute ^ ")\n");
        print_string (indent ^ "├── true: ");
        print_tree_aux (indent ^ "│   ") left;
        print_string (indent ^ "└── false: ");
        print_tree_aux (indent ^ "    ") right
  in
  print_tree_aux "" tree
