***Note : ce rapport, rédigé pour le 1er rendu du projet Polish, est évidemment
incomplet, et est susceptible de changer jusqu'au rendu final.***

# Identifiants
Les membres du groupes sont :
| Nom               | Prénom   | Identifiants gitlab | Numéro d'étudiant |
|-------------------|----------|---------------------|-------------------|
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
./polish.exe --[reprint/eval] <programme_polish.p>
```
Dans le cas d'un script ocaml executable:
```
ocamlrun polish.bc --[reprint/eval] <programme_polish.p>
```
Où `--reprint prog.p` réaffiche le programme polish prog.p comme explicité
plus haut et `--eval prog.p` execute le programme polish prog.p.

# Découpage modulaire
Le projet Polish est découpé en 5 modules, tous dans le dossier src/ :
- **Polish** : le module principal, contenant la fonction main, l'évaluation des
arguments et fait appel aux autres modules pour les différents traitements.
- **AbstractSyntax** : ce module contient les définitions des types ocaml
permettant de représenter la syntaxe d'un programme polish en mémoire.
- **Parser** : ce module contient la fonction read_polish ainsi que toutes ses
fonctions auxiliaires, et permet la lecture d'un fichier polish, ainsi que
l'analyse statique du programme et sa transformation en la syntaxe abstraite
fournie par *AbstractSyntax*.
- **Printer** : ce module, contenant la fonction print_polish ainsi que ses
fonctions auxiliaires, permet le ré-affichage d'un programme polish depuis sa
syntaxe abstraite.
- **VirtualMachine** : ce module, contenant la fonction eval_polish ainsi que
ses fonctions auxiliaires, permet l'évaluation, ou l'execution, d'un programme
polish depuis sa syntaxe abstraite. Cette "machine virtuelle" utilise une Map
ocaml pour associer les noms des variables du programme à leur valeur pendant
l'execution.

# Organisation du travail
Le différentes taches à réaliser sur ce projet ont été un effort collectif. Ce
projet étant une introduction au monde des langages, de l'analyse syntaxique,
des interpréteurs, etc... pour les deux membres du groupe, un gros travail de
recherche et de réflexion commune sur chaque partie de ce projet fut nécéssaire.

On peut diviser ce projet en 3 parties majeures, la lecture et le parsing,
le ré-affichage et l'éxecution. La partie la plus conséquente en terme de
réflexion et de design étant la lecture et le parsing, elle a nécéssité le
travail des deux membres. Claudio Teixeira s'est chargé de réaliser la structure
du parser, Benjamin Maudet à fait de la correction de typage afin de rendre la
structure fonctionnelle.

Les deux autres parties, bien moins compliquées à réaliser, n'ont nécéssité que
le code d'un seul membre (Benjamin Maudet), bien que les idées ont souvent été
trouvées lors de séances de réflexion communes.

Le travail a commencé le 16 novembre et a avancé progressivement depuis.
