open AbstractSyntax;;

exception WrongIndentation
exception ExpectedIndentedBlock
exception ElseWithoutIf
exception WrongCondition
exception WrongExpression

val read_polish : string -> program
(* Lit un fichier et met son contenu dans un string, lit ensuite
   le contenu du fichier pour le transformer en blocs d'instructions,
   autrement dit un programme.*)
