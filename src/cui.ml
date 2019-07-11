open Eval
open Syntax

let read_ch_eval_print ic env =
  print_string "# ";
  flush stdout;
  let decl = Parser.toplevel Lexer.main ic in
  let (id_vals, newenv) = eval_decl env decl in
  List.iter (fun (id, v) -> Printf.printf "val %s = " id;
                            pp_val v; print_newline ();)
            id_vals;
  newenv

let rec read_stdin_eval_print env =
  try
    let ic = Lexing.from_channel stdin in
    let newenv = read_ch_eval_print ic env in
    read_stdin_eval_print newenv
  with e -> err_handler env e
and read_file_eval_print fp ic env =
  try
    let newenv = read_ch_eval_print ic env in
    read_file_eval_print fp ic newenv
  with e -> close_in fp; err_handler env e
and err_handler env = function
    Error msg -> print_endline msg; read_stdin_eval_print env
  | Failure msg -> print_endline msg; read_stdin_eval_print env
  | _ -> print_endline "Fatal error"; read_stdin_eval_print env
and read_eval_print env =
  if Array.length Sys.argv > 1 then
    try
      let fp = open_in Sys.argv.(1) in
      let ic = Lexing.from_channel fp in
      read_file_eval_print fp ic env
    with Sys_error msg -> print_endline msg; read_stdin_eval_print env
  else
    read_stdin_eval_print env

(* pre-defined val *)
let not_function =
  ProcV ("b", IfExp (Var("b"), BLit false, BLit true), ref Environment.empty)

let initial_env = Environment.extend "not" not_function Environment.empty
