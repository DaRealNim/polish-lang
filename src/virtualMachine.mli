open AbstractSyntax;;

(** Prend un programme en entrée et évalue ses expressions et conditions,
   c'est à dire, les remplace par leurs valeurs concrètes dans le programme. *)
val eval_polish : program -> unit
