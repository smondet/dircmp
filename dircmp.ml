open Printf

let version = "0+s"

module File_tree = struct

  type file_tree_item =
    | Root of string
    | Dir of string
    | File of string * Digest.t
    | Link of string * string
    | Char of string     (* Character device *) 
    | Block of string    (* Block device	*)	   
    | Pipe of string     (* Named pipe	*)   
    | Socket of string   (* Socket	*)   	 
    | Error of string * string

  let descend ?(forget_specials=false) ~actions root =
    let ls dir =
      let sort a = Array.fast_sort String.compare a; a in
      let dir_read = Sys.readdir (root ^ "/" ^ dir) in
      (Array.map (fun f -> dir ^ "/" ^ f) (sort dir_read)) in

    let do_actions o = List.iter (fun f -> f o) actions in
    let do_special o = if forget_specials then () else (do_actions o) in
    let rec explore path =
      try 
        let module ULF = Unix.LargeFile in
        let real_path = (root ^ "/" ^ path) in
        let lstat = ULF.lstat real_path in
        match lstat.ULF.st_kind with
        | Unix.S_REG -> do_actions (File (path, Digest.file real_path))
        | Unix.S_LNK -> do_actions (Link (path, Unix.readlink path))
        | Unix.S_DIR ->
          (do_actions (Dir path); (Array.iter explore (ls path)))
        | Unix.S_CHR  -> do_special (Char path) 
	| Unix.S_BLK  -> do_special (Block path) 
	| Unix.S_FIFO -> do_special (Pipe path) 
	| Unix.S_SOCK -> do_special (Socket path) 
      with
      | Sys_error msg ->
        eprintf "Warning: Ignoring path %s (%s)\n" path msg;
        do_special (Error (path, msg))
      | Unix.Unix_error (e, _, _) ->
        let msg = Unix.error_message e in
        eprintf "Warning: Ignoring path %s (%s)\n" path msg;
        do_special (Error (path, msg))
    in
    do_actions (Root root);
    explore "."

  let string_of_item ft =
    match ft with 
    | Root s -> sprintf "Root %s" s
    | Dir s -> sprintf "Dir %s" s
    | File (s, d) -> sprintf "File %s MD5:%s" s (Digest.to_hex d)
    | Link (s, d) -> sprintf "Link %s -> %s" s d
    | Error (s, m) -> sprintf "Error %s" m
    | Char   path 
    | Block  path
    | Pipe   path
    | Socket path -> sprintf "Other stuff %s" path
      
  let print out item =
    fprintf out "%s\n" (string_of_item item)

  let _file_tree_magic_string =
    sprintf "dircmp v %s\nOCaml %s\n" version Sys.ocaml_version

  let file_saver path =
    let o = open_out path in
    output_string o _file_tree_magic_string;
    let action = fun (item: file_tree_item) -> Marshal.to_channel o item [] in
    let at_exit = fun () -> close_out o in
    (action, at_exit)

  let load ~actions file =
    let do_actions o = List.iter (fun f -> f o) actions in
    let i = open_in file in
    let magick_length = (String.length _file_tree_magic_string) in
    let s = String.make magick_length '\000' in
    try
      ignore (input i s 0 magick_length);
      if s = _file_tree_magic_string then (
        try 
          while true do
            do_actions (Marshal.from_channel i : file_tree_item)
          done
        with End_of_file -> ()
      ) else (
        failwith (sprintf "The header of the file %S is wrong: %S." file s)
      )
    with e -> close_in i; raise e

end

let digest () =

  let add_to_list l s = l := !l @ [s] in

  let to_parse_or_load = ref [] in
  let to_do = ref [] in
  let forget_specials = ref false in
  let options = [
    ( "-parse-tree", 
      Arg.String (fun s -> add_to_list to_parse_or_load (`parse s)),
      "<path>\n\tDescend a file tree.");
    ( "-forget-specials",
      Arg.Set forget_specials,
      "\n\tDo not keep track of “special” files and errors while parsing trees \
       \n\t(i.e. look only at regular files, symbolic links, and directories).");
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
    ( "-version",
      Arg.Unit (fun () -> printf "%s\n" version),
      (sprintf "\n\tPrint version number on stdout (i.e. print %S)." version))
  ] in
  let anon s = eprintf "Do not know what to do with %S\n" s in
  let usage = "dircmp [OPTIONS]" in
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
      File_tree.descend ~forget_specials:!forget_specials ~actions path
    | `load path ->
      File_tree.load ~actions path
  ) !to_parse_or_load;

  exit 0


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
      eprintf "NOT IMPLEMENTED\n";
      exit 0
    | "help" | "-help" | "-h" | "--help" ->
      eprintf "%s\n" usage;
      exit 0
    | "version" | "-version" | "-v" | "--version" ->
      eprintf "Dircmp v. %s\n" version;
      exit 0
    | s -> 
      eprintf "unknown command %S\n%s\n" s usage;
      exit 1
  )

