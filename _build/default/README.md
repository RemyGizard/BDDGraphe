# BDDGraphe
## COMMANDE FACILE
exécuter la commande : "./commande_depart.sh" dans BDDGraphe .

si ça ne fonctionne pas , utilise cette alternative
## Commande a taper pour lancer le projet si la première version ne marche pas
  ### première commande
        opam init
  ### bibliothèques a installer
        opam install ppx_deriving
        opam install ocamlgraph
        opam install menhir
        opam install uTop
  ### Commande a taper
        eval $(opam env)
        opam install dune
        dune init  Proj_GraphDB
        dune exec Proj_GraphDB f test/tiny.q
        dune build 
        dune utop

# Commande a taper  lorsque le projet est lancer

          #use_output "dune ocaml top-module bin/main.ml" ;;
          
          open Proj_GraphDB ;;
          open Graphstruct ;;
          open Typing ;;
          
          let test_types =  
           DBG ([( DBN ("P", [("nom", Lang. StringT ); ("age", Lang.IntT )]));
                 (DBN ("E", [("nom", Lang. StringT ); ("pme", Lang. BoolT )]))] ,
             [( DBR ("P", "ami", "P"));
              (DBR ("P", "emp", "E"));
               (DBR ("E", "f", "E"))]) ;;
          
               check_graph_types test_types ;;
