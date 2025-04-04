(* learn.ml *)
(*

Cette phase consiste à fournir à l’ordinateur des exemples de documents avec leur catégorie,
« intéressant » ou « pas intéressant ». En analysant le contenu de chaque document,
l’ordinateur construit une fonction de décision. C’est la phase d’apprentissage ; l’ensemble
des documents utilisé pour cette phase est appelé Doc_appr (documents d’apprentissage);

*)

(*
 NOTE: Premiere phase, consiste a creer les donnée pour l'apprentisage avec:
    - Collecte des donnée (ici, de simple documents test, type article, livre, blog, ...)
    - Prétraitement des textes avec
        1. Tokenization
        2. Suppression des mots de liasion, ect
        3. Suppresion de character spéciaux
        4. Réduire les mots à leur forme de base
    - Construire les données
        1. Bag of Word
            dictionnaire de tous les mots uniques du corpus et représenter chaque document par
            un vecteur indiquant combien de fois chaque mot apparaît
        2. Ponderation des mots par leurs presence dans le corpus
*)

(* Lecture d'un document *)
let data_dir = "data/"
let file_name = "doc_1.txt"

let () =
    let file = data_dir ^ file_name in
    let ic = open_in file in
    try
        let line = input_line ic in
        print_endline line;
        flush stdout;
        close_in ic;
    with e ->
        close_in_noerr ic;
        raise e;
