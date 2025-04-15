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
