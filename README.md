# Polish
A parser and interprer for a simple programming language nammed Polish.

# Identifiants
Les membres du groupes sont :

|       Nom         |  Prénom  | Identifiants gitlab | Numéro d'étudiant |
|       ---         |  ------  | ------------------- | ----------------- |
| Teixeira Medeiros | Claudio  | @teixeirc           | 21953762          |
| Maudet            | Benjamin | DaRealNim, @maudet  | 21957316          |

# Fonctionnalités
Ce projet permet pour le moment :
- La lecture et parsing d'un fichier polish (.p), comprenant une analyse
statique des erreurs de syntaxe et d'indentation
- Le réaffichage d'un programme polish lu, en filtrant les commentaires, les
espaces superflux ainsi que les lignes vides
- L'execution d'un programme polish

# Compilation et exécution
## Compilation
Polish peut être compilé de deux façons différente

### (1) Avec make
Pour compiler la source en executable depuis la racine:
```
make
```
ou alors
```
make binary
```
Cela va produire un executable `polish.exe` à la racine du projet.

Pour compler la source en script ocaml executable avec `ocamlrun`:
```
make byte
```
Cela va produire un script ocaml executable `polish.bc` à la racine du projet.

Vous pouvez nettoyer le dossier de build ainsi que l'executable ou le script
produit avec
```
make clean
```

### (2) Directement avec dune
Si make n'est pas disponible sur votre machine, vous pouvez lancer la compilation
directement avec dune.

Pour produire un executable:
```
dune build src/polish.exe
```

Pour produire un script ocaml executable:
```
dune build src/polish.bc
```

Pour nettoyer le dossier de build ainsi que l'executable ou le script produit:
```
dune clean
```

## Execution
L'executable polish se lance de la façon suivante:
```
./polish.exe --[reprint/eval/simpl/vars/signs] <programme_polish.p>
```
Dans le cas d'un script ocaml executable:
```
ocamlrun polish.bc --[reprint/eval/simpl/vars/signs] <programme_polish.p>
```
Où
- `--reprint prog.p` réaffiche le programme polish prog.p comme explicité plus haut
- `--eval prog.p` execute le programme polish prog.p
- `--simpl prog.p` affiche une version simplifiée de prog.p ou les conditions et opérations évidentes sont précalculées afin d'alleger le code source
- `--vars prog.p` affiche les variables présentes dans prog.p et sur une autre ligne, celles qui sont estimées dangereuses car potentiellement lues avant d'être initialisées
- `--signs prog.p` effectue une analyse statique des potentiels signes des variables de prog.p. Cette
option lève l'exception Not_found si une des variables du programme est dangereuse. Pour trouver laquelle, lancer l'option `--vars` sur le fichier.

# Découpage modulaire
Le projet Polish est découpé en 5 modules, tous dans le dossier src/ :
- **Polish** : le module principal, contenant la fonction main, l'évaluation des
arguments. Il fait appel aux autres modules pour les différents traitements.

- **AbstractSyntax** : ce module contient les définitions des types ocaml
permettant de représenter la syntaxe d'un programme polish en mémoire.

- **Parser** : ce module contient la fonction read_polish ainsi que toutes ses
fonctions auxiliaires, et permet la lecture d'un fichier polish, ainsi que sa
transformation en la syntaxe abstraite fournie par *AbstractSyntax*.
`read_polish : string -> program`

- **Printer** : ce module, contenant la fonction print_polish ainsi que ses
fonctions auxiliaires, permet le ré-affichage d'un programme polish depuis sa
syntaxe abstraite.
`print_polish : program -> unit`

- **VirtualMachine** : ce module, contenant la fonction eval_polish ainsi que
ses fonctions auxiliaires, permet l'évaluation, ou l'éxecution, d'un programme
polish depuis sa syntaxe abstraite. Cette "machine virtuelle" utilise une Map
ocaml pour associer les noms des variables du programme à leur valeur pendant
l'execution.
`eval_polish : program -> unit`

- **Vars** : ce module contient l'implementation de l'option --vars, 
qui permet de lister toutes les variables d'un programme ainsi que celles qui
peuvent être accédées avant d'avoir été initialisées, qu'on appelle "dangereuses".
`vars_polish : program -> unit`

- **Signs** : ce module permet de lister les variables du programme ainsi que 
leur liste de signes possible respective. Une variable peut être positive, 
négative, nulle ou bien une erreur (dans le cas d'une division par zéro 
par exemple).
`signs_polish : program -> unit`

- **Simplifier** : ce module permet de simplifier des programmes inutilement 
compliqués, en simplifiant des conditons insatisfiables ou valides et en 
réduisant l'écriture des expressions à leur valeur finale quand cela est possible.
`simplify_polish : program -> polish`

# Organisation du travail
-- RENDU 1 --

Le différentes taches à réaliser sur ce projet ont été un effort collectif. Ce
projet étant une introduction au monde des langages, de l'analyse syntaxique,
des interpréteurs, etc... pour les deux membres du groupe, un gros travail de
recherche et de réflexion commune sur chaque partie de ce projet fut nécéssaire.

On peut diviser ce projet en 4 parties majeures, la lecture, le parsing,
le ré-affichage et l'éxecution. La partie la plus conséquente en terme de
réflexion et de design étant la lecture et le parsing, elle a nécéssité le
travail des deux membres. Claudio Teixeira s'est chargé de réaliser la structure
du parser, Benjamin Maudet a fait de la correction de typage afin de rendre la
structure fonctionnelle.

Les deux autres parties, bien moins compliquées à réaliser, n'ont nécéssité que
le code d'un seul membre (Benjamin Maudet), bien que les idées ont souvent été
trouvées lors de discussions ou séances de programmation en binôme.

Le travail a commencé le 16 novembre et a avancé progressivement depuis.

-- RENDU 2 --

Trois fonctionnalités auxiliaires étaient demandées pour le deuxième rendu, 
correspondant aux modules **Vars**, **Signs** et **Simplifier**.

La réalisation du deuxième rendu s'est déroulée de façon à peu près équivalente 
à celle du premier:

Benjamin Maudet a travaillé sur le module **Simplifier** et Claudio Teixeira 
a travaillé sur le module **Vars**. Le module **Signs** est en 
majorité le résultat d'une programmation et reflexion du binôme.
