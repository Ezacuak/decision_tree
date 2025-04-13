(* Types pour les arbres de décision *)
type decision_tree =
    | Empty
    | Leaf of bool
    | Node of string * decision_tree * decision_tree

(* Format des données *)
type word = string
type decision = bool
type data = word list * decision
type document = data list

