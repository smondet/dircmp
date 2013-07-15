open Dircmp_internal_pervasives
open Dircmp

let digest () =

  let add_to_list l s = l := !l @ [s] in

  let to_parse_or_load = ref [] in
  let to_do = ref [] in
  let forget_specials = ref false in
  let read_links = ref true in
  let options = [
    ( "-parse-tree", 
      Arg.String (fun s -> add_to_list to_parse_or_load (`parse s)),
      "<path>\n\tDescend a file tree.");
    ( "-save-to",
      Arg.String (fun s -> add_to_list to_do (`save s)),
      "<path>\n\tSave all trees to a file (uses the Marshal module).");
    ( "-load",
      Arg.String (fun s -> add_to_list to_parse_or_load (`load s)),
      "<path>\n\tLoad file trees (coming from a -save-to invocation).");
    ( "-print",
      Arg.Unit (fun () -> add_to_list to_do `print),
      " \n\tPrint the parsed and loaded trees to standard output.");
    ( "-print-to",
      Arg.String (fun path -> add_to_list to_do (`print_to path)),
      "<path>\n\tPrint the parsed and loaded trees to a file.");
    ( "-forget-specials",
      Arg.Set forget_specials,
      "\n\tDo not keep track of “special” files and errors while parsing trees \
       \n\t(i.e. look only at regular files, symbolic links, and directories).");
    ( "-do-not-read-links",
      Arg.Clear read_links,
      "\n\tDo not track where the symbolic links point to.");
    ( "-version",
      Arg.Unit (fun () -> printf "%s\n" About.version),
      (sprintf "\n\tPrint version number on stdout (i.e. print %S)." About.version))
  ] in
  let anon s = eprintf "Do not know what to do with %S\n" s in
  let usage = "dircmp digest [OPTIONS]" in
  Arg.parse options anon usage;

  let actions =
    List.map (function
      | `print -> File_tree.print stdout
      | `print_to path ->
        let out = open_out path in
        at_exit (fun () -> close_out out);
        File_tree.print out
      | `save path -> 
        let action, to_do_at_exit = File_tree.file_saver path in
        at_exit to_do_at_exit;
        action
    ) !to_do in
  List.iter (function
    | `parse path ->
      File_tree.descend ~forget_specials:!forget_specials 
        ~read_links:!read_links ~actions path
    | `load path ->
      File_tree.load ~actions path
  ) !to_parse_or_load;

  exit 0

let diff () =

  let tmp_dir = ref None in
  let diff_cmd = ref "diff" in
  let options = [
    ("-tmp-dir",
     Arg.String (fun s -> tmp_dir := Some s),
     sprintf "<path>\n\tSet the temporary directory (default %S)." 
       Filename.temp_dir_name);
    ("-diff-cmd",
     Arg.Set_string diff_cmd,
     sprintf "<path>\n\tSet the command used for “diff” (default %S)."
       !diff_cmd);
  ] in
  let left_right = ref [] in
  let anon s = left_right := s :: !left_right in
  let usage = "dircmp diff [OPTIONS] left.{dircmp,txt} right.{dircmp,txt}" in
  Arg.parse options anon usage;

  let transform_dircmp f =
    let tmp =
      Filename.temp_file ?temp_dir:!tmp_dir "dircmpdiff" ".txt" in
    let out = open_out tmp in
    let actions = [ File_tree.print out ] in
    File_tree.load ~actions f;
    close_out out;
    tmp in

  let text_version f =
    if Filename.check_suffix f ".dircmp" then
      transform_dircmp f
    else
      f in

  begin match !left_right with
  | [ left; right ] ->
    let txt_left = text_version left in
    let txt_right = text_version right in
    let open Unix in    
    begin match system (sprintf "%s %s %s" !diff_cmd txt_left txt_right) with
    | WEXITED 127 -> 
      eprintf "The result WEXITED 127 indicates that the shell couldn't be executed."
    | WEXITED 0 -> ()
    | WEXITED n -> eprintf "%s returned %d\n" !diff_cmd n
    | WSIGNALED n -> eprintf "%s was killed by signal %d\n" !diff_cmd n
    | WSTOPPED n -> eprintf "%s was stopped by signal %d\n" !diff_cmd n
    end
  | _ ->
    eprintf "dircmp diff expects exactly two arguments, \n
              please do not mess up with that.\n"
  end


let () =
  let usage = "usage: dircmp {version,help,digest,diff} [OPTIONS]" in
  if Array.length Sys.argv < 2 then (
    eprintf "%s\n" usage;
    exit 1
  ) else (
    match Sys.argv.(1) with
    | "digest" -> 
      Arg.current := 1;
      digest ()
    | "diff" ->
      Arg.current := 1;
      diff ()
    | "help" | "-help" | "-h" | "--help" ->
      eprintf "%s\n" usage;
      exit 0
    | "version" | "-version" | "-v" | "--version" ->
      eprintf "Dircmp v. %s\n" About.version;
      exit 0
    | s -> 
      eprintf "unknown command %S\n%s\n" s usage;
      exit 1
  )

