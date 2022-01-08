(** Projet Polish -- Analyse statique d'un mini-langage impératif *)
(* Fichier principal, la fonction main est appelée lors de l'execution*
de polish.exe *)
open AbstractSyntax;;
open Parser;;
open Printer;;
open VirtualMachine;;
open Simplifier;;
open Vars;;
open Signs;;

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
  | [|_;"--simpl";file|] -> read_polish file |> simplify_polish |> print_polish
  | [|_;"--vars";file|] -> read_polish file |> simplify_polish |> vars_polish
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
