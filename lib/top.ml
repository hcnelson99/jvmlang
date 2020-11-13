open Core

type cmd_line_args =
  { verbose : bool
  ; dump_parsing : bool
  ; dump_ast : bool
  ; dump_ir : bool
  ; dump_assem : bool
  ; filename : string
  }

let cmd_line_term : cmd_line_args Cmdliner.Term.t =
  let open Cmdliner in
  let module Let_syntax = struct
    let return = Term.pure
    let map ~f a = Term.(return f $ a)
    let both a b = Term.(pure Tuple2.create $ a $ b)
  end
  in
  let flag info = Arg.value (Arg.flag info) in
  let%map verbose =
    let doc = "If present, print verbose debug information." in
    flag (Arg.info [ "v"; "verbose" ] ~doc)
  and dump_parsing =
    let doc = "If present, print debug informaton from parsing." in
    flag (Arg.info [ "dump-parsing" ] ~doc)
  and dump_ast =
    let doc = "If present, print the parsed ast." in
    flag (Arg.info [ "dump-ast" ] ~doc)
  and dump_ir =
    let doc = "If present, print the translated ir ast." in
    flag (Arg.info [ "dump-ir" ] ~doc)
  and dump_assem =
    let doc = "If present, print the final assembly." in
    flag (Arg.info [ "dump-assem" ] ~doc)
  and filename =
    let doc = "The source file $(docv) to compile." in
    Arg.(required (pos 0 (some non_dir_file) None (info [] ~doc ~docv:"FILE")))
  in
  { verbose; dump_parsing; dump_ast; dump_ir; dump_assem; filename }
;;

let say_if (v : bool) (f : unit -> string) = if v then prerr_endline (f ())

let compile (cmd : cmd_line_args) : unit =
  say_if cmd.verbose (fun () -> "Parsing... " ^ cmd.filename);
  if cmd.dump_parsing then ignore (Parsing.set_trace true : bool);
  let ast = Parse.parse cmd.filename in
  print_endline (Ast.Print.pp_program ast)
;;

let run (cmd : cmd_line_args) : unit =
  try compile cmd with
  | Error_msg.Error ->
    prerr_endline "Compilation failed.";
    exit 1
;;

let main () =
  let open Cmdliner in
  let cmd_line_info = Term.info "compile" ~doc:"Compile a source file." in
  match Term.eval (cmd_line_term, cmd_line_info) with
  | `Ok cmd_line -> run cmd_line
  | result -> Term.exit result
;;
