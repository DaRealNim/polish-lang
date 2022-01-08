COMMENT Ce programme permet le test de l'options --vars de polish: il déclare et fait
COMMENT des opérations sur des variables, parfois de manière dangereuse, c'est a dire
COMMENT que certaines variables sont (ou peuvent êtres) lues avant d'être assignées.

good1 := 1
READ good2

IF bad1 < 10
  good3 := 10
  IF bad1 < 5
    good5 := 2
    bad4 := 3
  ELSE
    good5 := 2
    bad4 := 3
  PRINT good5
ELSE
  good4 := 5

PRINT bad4

WHILE bad2 < 10
  bad2 := bad3

bad3 := bad4