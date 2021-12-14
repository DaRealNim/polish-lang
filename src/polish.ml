(** Projet Polish -- Analyse statique d'un mini-langage impératif *)
(* Fichier principal, la fonction main est appelée lors de l'execution*
de polish.exe *)
open AbstractSyntax;;
open Parser;;
open Printer;;
open VirtualMachine;;

let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: à documenter \n"

let main () =
  match Sys.argv with
  | [|_;"--reprint";file|] -> print_polish (read_polish file)
  | [|_;"--eval";file|] -> eval_polish (read_polish file)
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
