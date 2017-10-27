\newcommand{\ket}[1]{\left|{#1}\right\rangle}
\newcommand{\bra}[1]{\left\langle{#1}\right|}

#Bits classiques et quantiques
****
### Classique: 0 et 1
* information is physical (Raff Landauer)
* Representation possible:
    * Courant
    * Interrupteur
    * Polarisation photon
    * atome de cesium (excite ou non)
    * Cavite micro-ondes (cavite vide: 0, cavite avec photon: 1)

### Qubit (bit quantique)
* etat de base:
    * |0> et |1>

Seule condition: il faut que |0> et |1> soient distinguables (en principe)
a tout coup et avec certitude.

Definition:

~~~
Deux etats distinguables (en principe) a tout coup et avec certitude sont othogonaux
~~~

Plus generalement, in qubit peut etre dans l'etat:
$|\psy> = \alpha|0> + \beta|1>$ ou $\alpha,\beta \in \bigc$ t.q $|\alpha|2+|\beta|2 = 1$

Un etat quantique $|\bigpsy>=\alpha|0> + \beta|1>$ peut etre mesure

$|\bigpsy>$ est 0 avec prob de $|\alpha|2$ et 1 avec prob $|\beta|2$

$$
\alpha = |\alpha|(cos(\Theta) + i*sin(\Theta))
=|\alpha|e^{i\Theta}
$$

$$
\alpha^{*} = |\alpha|e^{-i\Theta}
$$

$$
\alpha\beta = |\alpha||\beta|e^{i(\Theta+\Phi)}
$$

$$
e^{i\pi}=-1
$$

$$
|\alpha + \beta|^2 = |\alpha|^2+|\beta|^2 + 2Re(\alpha\beta^*)
$$


# 12 oct
une fct $f: \{0,1\}^n \to \{0,1\}^{n-1}$ est 2-dans-1 si
$$
\forall y \in \{0,1\}^{n-1}, \#f^{-1}(y)=2
$$

###Def:
Une famille de fonctions $f_i:\{0,1\}^n \to \{0,1\}^{n-1}, i \in I$ est resistante
au collision si:

#. chaque $f_i$ est 2-dans-1
#. etant donne $i$ et $x_i$, on peut calculer $f_i(x)$ efficacement
#. etant donne $i \in I$ aleatoire, il est impossible (sauf avec prob minime)
d'obtenir efficacement une paire $(x_0,x_1)$ t.q. $x_0 \neq x_1$ et $f_i(x_0)=f_i(x_1)$

## Transfert de phase (Phase KickBack)
Soit $f:\{0,1\}^n \to \{0,1\}$ une fonction booleenne.

On sait deja comment faire $(\ket x ,\ket y) \to (\ket x, \ket {y\oplus f(x)})$

Pour faire plus quantique, on veut :
$$
\ket x \xrightarrow{\text{(m)}} \boxed{C} \xrightarrow{\text{(n)}} (-1)^{f(x)}\ket x
$$

* voir notes

Probleme de Deutsch :

Soit $f$ une fonction :  $\{0,1\} \to \{0,1\}$
On s'interesse a "est-ce que $f(0)=f(1)$ ou pas?".

* voir notes
