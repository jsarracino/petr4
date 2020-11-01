open Core
open Petr4
open Common

module Conf: Parse_config = struct
  let red s = s
  let green s = s

  let preprocess include_dirs p4file =
    let cmd =
      String.concat ~sep:" "
        (["cc"] @
         (List.map include_dirs ~f:(Printf.sprintf "-I%s") @
          ["-undef"; "-nostdinc"; "-E"; "-x"; "c"; p4file])) in
    let in_chan = Unix.open_process_in cmd in
    let str = In_channel.input_all in_chan in
    let _ = Unix.close_process_in in_chan in
    str
end

module Parse = Make_parse(Conf)

let parser_test include_dirs file =
  match Parse.parse_file include_dirs file false with
  | `Ok _ -> true
  | `Error _ -> false

let to_string pp : string =
  Format.fprintf Format.str_formatter "%a" Pp.to_fmt pp;
  Format.flush_str_formatter ()

let get_name include_dirs file =
  match Parse.parse_file include_dirs file false with 
  | `Ok prog -> prog |> Prettypp.format_program |> to_string 
  | `Error _ -> "121" 

let pp_round_trip_test include_dirs file =
  let way_there = match Parse.parse_file include_dirs file false with 
    | `Ok prog -> prog |> Prettypp.format_program |> to_string 
    | `Error _ -> "" in 
  let way_back = Parse.parse_string way_there in
  String.compare way_there (way_back |> Prettypp.format_program |> to_string) = 0 

let typecheck_test (include_dirs : string list) (p4_file : string) : bool =
  match Parse.parse_file include_dirs p4_file false with
  | `Ok prog ->
    begin
      try
        let prog, renamer = Elaborate.elab prog in
        let _ = Checker.check_program renamer prog in
        true
      with
      | Error.Type(info, err) ->
        Format.eprintf "%s: %a" (Info.to_string info) Error.format_error err;
        false
      | exn ->
        Format.eprintf "Unknown exception: %s" (Exn.to_string exn);
        false
    end
  | `Error (info, Lexer.Error s) -> false
  | `Error (info, Parser.Error) -> false
  | `Error (info, err) -> false

let get_files path =
  Sys.ls_dir path
  |> List.filter ~f:(fun name ->
      Core_kernel.Filename.check_suffix name ".p4")

let example_path l =
  let root = Filename.concat ".." "examples" in
  List.fold_left l ~init:root ~f:Filename.concat

let good_files = example_path ["checker_tests"; "good"] |> get_files

let bad_files = example_path ["checker_tests"; "bad"] |> get_files

let good_test f file () =
  Alcotest.(check bool) "good test" true
    (f ["../examples"] (example_path ["checker_tests"; "good"; file]))

let bad_test f file () =
  Alcotest.(check bool) "bad test" false
    (f ["../examples"] (example_path ["checker_tests"; "bad"; file]))

let pp_test () =
  Alcotest.(check bool) "printing test" true Printingtests.test

let example_path l =
  let root = Filename.concat ".." "examples" in
  List.fold_left l ~init:root ~f:Filename.concat

let () = 
  Format.printf "%s" (get_name ["../examples"] (example_path ["checker_tests"; "good"; "coree.p4"])); 
  Format.printf "%s" "----------";
  Format.printf "%s" ((Parse.parse_string (get_name ["../examples"] (example_path ["checker_tests"; "good"; "coree.p4"]))) |> Prettypp.format_program |> to_string); 
  let open Alcotest in
  run "Tests" [
    (* "parser tests good", (Stdlib.List.map (fun name ->
        test_case name `Quick (good_test parser_test name)) good_files);
       "typecheck tests good", (Stdlib.List.map (fun name ->
        test_case name `Quick (good_test typecheck_test name)) good_files);
       "typecheck tests bad", (Stdlib.List.map (fun name ->
        test_case name `Quick (bad_test typecheck_test name)) bad_files);
       "unit tests pp", [test_case "printing tests" `Quick pp_test];  *)
    (* "round trip pp tests good", (Stdlib.List.map (fun name ->
        test_case name `Quick (good_test pp_round_trip_test name)) good_files); *)
  ]



