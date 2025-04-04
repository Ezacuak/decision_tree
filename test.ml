(* test.ml *)
(*

Cette phase a pour objectif de valider la fonction de décision obtenue ; Il s’agit ici de réaliser
des tests avec des documents différents de ceux qui ont servi pour l’apprentissage, dont on
connaît la catégorie également. C’est la phase de test. La fonction de décision est considérée
comme « bonne » si elle trouve correctement les catégories des documents de validation.
L’ensemble de ces documents est appelé Doc_test ;

*)
let () = print_endline "Phase de test"
