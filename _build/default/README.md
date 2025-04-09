# Commande pour intencier le projet
exécuter la commande : "./commande_depart.sh" dans BDDGraphe .

si ça ne fonctionne pas , utilise cette alternative
## Commande a taper pour lancer le projet si la première version ne marche pas
  #### première commande
        opam init
  #### bibliothèques a installer
        opam install ppx_deriving
        opam install ocamlgraph
        opam install menhir
        opam install uTop
  #### Commande a taper
        eval $(opam env)
        opam install dune
        dune init  Proj_GraphDB
        dune exec Proj_GraphDB f test/tiny.q
        dune build 
        dune utop

#### Commande a taper  lorsque le projet est lancer

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
# Ce qui a été fait dans le  projet 
          
      Les tests ont été instancier dans le fichier interf.ml qui permettrons d'exécuter les commandes dans le parceur

![](https://github.com/RemyGizard/BDDGraphe/blob/main/Cha%C3%AEne%20d'ex%C3%A9cution(1)_page-0001.jpg)

  ## 1. parser.mly – Grammaire du langage (Analyseur syntaxique)
    
   #### Complété la grammaire du langage source :
      
          Toutes les instructions (CREATE, MATCH, DELETE, RETURN, SET, WHERE)      
          Prise en charge des expressions avec opérateurs (+, ==, &&, etc.)      
          Gestion complète des motifs de nœuds et relations, ainsi que des suppressions
          
   #### Rôle dans l’exécution :
  
    Traduit un programme source en une structure de données OCaml (Lang.prog) utilisable pour le typage.

  ## 2. typing.ml – Vérification des types (Analyse statique)

   #### Implémenté tout le système de typage :

        Vérification de la validité des types de graphe (check_graph_types)

        Typage des expressions et levée d’erreurs explicites

        Vérification de toutes les instructions (CREATE, MATCH, SET, etc.)

        Gestion d’un environnement typé et des variables

    Créé une batterie de tests unitaires pour valider les cas typiques et erreurs
  
   #### Rôle dans l’exécution :

    Garantit que les instructions sont bien typées avant exécution

    Stoppe le programme avec un message explicite en cas d’erreur
  ## 3. interf.ml – Interface principale (Point d’entrée du programme)

    Intégré la phase de parsing avec celle de typage

    Ajouté une fonction run_tests qui lance les tests définis dans typing.ml

   #### Modifié run_file pour :

        Parser un fichier source

        Le normaliser (Instr.normalize_prog)

        Vérifier les types

        Afficher le programme analysé

        Lancer les tests automatiquement

    L’ancienne exécution avec Sem.exec a été commentée (non utile ici)

   #### Rôle dans l’exécution :

    Sert de lanceur du projet

    Permet d'exécuter le fichier source en appelant le parser puis le vérificateur

    Gère l'affichage des erreurs de parsing et de typage

*Lors de notre projet nous avons utiliser ChatGPT de manière modéré pour nous débloquer sur les problèmes que nous avons rencontrés lors de la mise en oeuvre du projet*
