(** Projet Polish -- Analyse statique d'un mini-langage impératif *)
(* Fichier principal, la fonction main est appelée lors de l'execution*
de polish.exe *)
open AbstractSyntax;;
open Parser;;
open Printer;;
open VirtualMachine;;

let usage () =
  print_string "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣠⡯⠀⠀⠀⢠⡐⠀⠀⠀⠀⠀Polish Cow\n";
  print_string "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣰⣿⡟⠀⠀⠀⠀⠀⠀⠀⠀⠀\n";
  print_string "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣀⣠⣤⣤⣤⣴⣦⣾⣿⣿⣿⣶⣦⣴⣶⣶⠀⠀⠀⠀\n";
  print_string "⠀⠀⠀⠀⠀⠀⠀⠀⢀⣽⣿⣿⡟⢻⣿⣿⣿⡟⣿⣿⣿⣿⠉⠉⠁⠀⠀⠀⠀⠀\n";
  print_string "⠀⠀⠀⠀⠀⠀⠀⠀⣾⣿⣿⣿⣠⣾⣿⣿⣏⣤⣾⣿⣿⣿⡆⠀⠀⠀⠀⠀⠀⠀\n";
  print_string "⠀⠀⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡧⠀⠀⠀⠀⠀⠀⠀\n";
  print_string "⠀⠀⠀⠀⠀⠀⠀⠀⢻⡿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠿⠛⡄⠀⠀⠀⠀⠀⠀⠀\n";
  print_string "⠀⠀⠀⠀⠀⠀⠀⠀⣿⠁⠉⠙⠛⠛⢿⣿⠟⠋⠁⠀⠀⠀⠈⠀⠀⠀⠀⠀⠀⠀\n";
  print_string "⠀⠀⠀⠀⠀⠀⠀⠀⠁⠀⠀⠀⠀⠀⠸⣿⠀⠀⠀⠀⠀⠀⠀⠀⢀⣀⠀⠀⠀⠀\n";
  print_string "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢹⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀\n";
  print_string "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡘⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀\n";
  print_string "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀\n";
  print_string "\n";
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "Usage:\n";
  print_string "./polish[.exe/.bc] --reprint <file>: Réaffiche les éléments identifiés dans le fichier par le parser.\n";
  print_string "./polish[.exe/.bc] --eval <file>: Exécute le fichier.\n"

let main () =
  match Sys.argv with
  | [|_;"--reprint";file|] -> print_polish (read_polish file)
  | [|_;"--eval";file|] -> eval_polish (read_polish file)
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
