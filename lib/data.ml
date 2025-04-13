open Types

(* Jeux de données pour les tests *)
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
