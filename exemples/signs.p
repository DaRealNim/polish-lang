COMMENT Fichier pour tester le bon fonctionnement de --signs

a := 5
b := -3
IF a > b
  READ a
  a := / 5 a
  b := 0

COMMENT ici, la condition a > b est identifiée comme valide, donc b sera toujours nul
COMMENT de même, a peut être positif ou négatif, ou être une erreur si l'utilisateur rentre 0 pour a
COMMENT mais pas nul, car le seul moyen d'avoir un nul avec x/y est si x=0, or ici on a 5/a


COMMENT Ici, c peut être positif, négatif, ou nul
READ c
n := 0
IF c = 0
  COMMENT Ici, on sait que c est a 0, donc c - 4 est forcément négatif
  n := - c 4
ELSE
  COMMENT ici, n est paramétré a un nombre négatif
  n := -10

COMMENT du coup, n est forcément négatif a la fin de l'execution