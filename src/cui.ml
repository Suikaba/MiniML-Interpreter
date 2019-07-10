open Eval

let rec print_msg_and_continue msg env =
  print_endline msg;
  read_eval_print env

and read_eval_print env =
  try
    print_string "# ";
    flush stdout;
    let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
    let (id, newenv, v) = eval_decl env decl in
    Printf.printf "val %s = " id;
    pp_val v;
    print_newline();
    read_eval_print newenv
  with _ -> print_msg_and_continue "Fatal error" env

let initial_env = Environment.empty
