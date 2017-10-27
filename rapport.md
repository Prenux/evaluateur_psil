# Rapport de travail

par Rémi Langevin
et  Christophe Apollon-Roy

## Problèmes rencontrés

#### Trouver le format de base de Llambda

Au départ, nous avions décidé de transformer l'arbre de Sexp en liste. Au
début, cela facilitait la tâche. Toutefois, nous avons rapidement eu des
difficultés avec les cas plus complexes, dont le lambda. À partir de ce
moment, nous avons décidé d'y aller avec du pattern matching et
déterminer les patterns associés à chaque type de Sexp.

#### Gérer les Llet imbriqués et avec assignation multiples

Trouver les patterns associés et comment extraire de multiples
déclarations dans un Llet fut une tâche complexe. Nous avons eu besoin de
dessiner quelques arbres afin de bien généraliser.

#### Currying

Le currying nous a couté beaucoup de temps et de modifications dans le
code. À un certain point, d'un côté, nous pouvions faire fonctionner les
fonctions non-curried, de l'autre less fonctions curried, mais jamais les
deux en même temps. Cela se jouait au niveau du Lapp. Sans currying, nous
avions:
```
s2l (Scons Snil a) =
   case (s2l a) of
   ...
   (Lapp x y) -> Lapp x y
```
et avec currying:
```
s2l (Scons Snil a) =
   case (s2l a) of
   ...
   (Lapp x y) -> Lapp (Lapp x y) []
```
Nous avions donc eu du trouble à trouver une façon de faire qui
fonctionne pour les deux.

#### Unsweetner

Se débarasser du sucre syntaxique fut une autre fonction complexe à
implémenter sans briser le code déjà existant.

## Surprises

Nous avons été surpris à quel point les cases, cons et if furent facile à
implémenter. Autrement, pouvoir utiliser la structure de l'arbre
directement avec du pattern matching nous a surpris.

De plus, malgré la difficulté que le travail nous a donné et bien que
nous ne le ferions pas à nouveau, nous nous sommes surpris à apprécier
l'exercice.

Ensuite, nous avons été surpris à quel point nous pouvons construire par
dessus les fonctions existantes sans se soucier de comment elle allait 
agir lorsque celles-ci sont bien pensées à la base.

Malgré tout cela, les problèmes rencontrés nous ont couté beaucoup de
temps, ce qui a un peu terni l'expérience.

## Choix

Nous avons choisi de ne pas avoir de souplesse dans la syntaxe acceptée,
car cela devenait trop complexe de trouver les patterns associés à
certains modèles. Nous avons choisi, en fin de travail, de ne faire
aucune vrai différence entre les dlet et les llet dans eval (s2l fait la
différence), donc au final, il n'y a aucune différence entre les deux.

## Options sciemment rejetées

Nous avons sciemment rejeté l'idée de v
