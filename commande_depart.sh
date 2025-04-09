#!/bin/bash

# Initialisation d'OPAM
echo "Initialisation d'OPAM..."
opam init -y

# Installation des bibliothèques nécessaires
echo "Installation des bibliothèques nécessaires..."
opam install ppx_deriving
opam install ocamlgraph
opam install menhir
opam install uTop

# Configuration de l'environnement OPAM
echo "Configuration de l'environnement OPAM..."
eval $(opam env)

# Installation de Dune
echo "Installation de Dune..."
opam install dune
dune init  Proj_GraphDB

# Exécution du projet avec Dune
echo "Exécution du projet avec Dune..."
dune exec Proj_GraphDB f test/tiny.q

# Construction du projet
echo "Construction du projet..."
dune build


