# Rapport de travail

par Rémi Langevin
et  Christophe Apollon-Roy

## Problèmes rencontrés

#### Trouver le format de base de Llambda

Au départ, nous avions décidé de transformer l'arbre de Sexp en liste. Au début,
cela facilitait la tâche. Toutefois, nous avons rapidement eu des difficultés
avec les cas plus complexes, dont le lambda. À partir de ce moment, nous avons
décidé d'y aller avec du pattern matching et déterminer les patterns associés à
chaque type de Sexp.

#### Gérer les Llet imbriqués et avec assignation multiples

Trouver les patterns associés et comment extraire de multiples déclarations dans un Llet fut une tâche complexe. Nous avons eu besoin de dessiner quelques arbres afin de bien généraliser.

## Surprises

Nous avons été surpris à quel point les cases, cons et if furent facile à
implémenter.

## Choix

Nous avons choisi de ne pas avoir de souplesse dans la syntaxe acceptée,
car cela devenait trop complexe de trouver les patterns associés à certain modèle.

## Options sciemment rejetées

Nous avons rejettés l'idée de ne pas remettre un travail complet.
