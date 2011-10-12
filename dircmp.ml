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
  type file_tree = {
    ft_root: string;
    ft_tree: file_tree_item array;
  }

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
      
  let print ?(indent=0) item =
    let strindent = String.make indent ' ' in
    printf "%s%s\n" strindent (string_of_item item)
(*
  let _file_tree_magic_string =
    sprintf "dircmp v %s\nOCaml %s\n" version Sys.ocaml_version

  let save_to_file (ft : file_tree list) file =
    let o = open_out file in
    output_string o _file_tree_magic_string;
    Marshal.to_channel o ft [];
    close_out o

  let load_from_file file =
    let i = open_in file in
    let magick_length = (String.length _file_tree_magic_string) in
    let s = String.make magick_length '\000' in
    try
      ignore (input i s 0 magick_length);
      if s = _file_tree_magic_string then (
        let ft = (Marshal.from_channel i : file_tree list) in
        ft
      ) else (
        failwith (sprintf "The header of the file %S is wrong: %S." file s)
      )
    with e -> close_in i; raise e

  let rec fast_compare = function
    | [] | _ :: [] -> true
    | h1 :: h2 :: q ->
      compare h1.ft_tree h2.ft_tree = 0
      (* List.for_all2 fast_compare_two_items h1 h2 *)
      && (fast_compare (h2 :: q))

  let compare_two_items one_name one two_name two =
    match one, two with
    | File (s1, d1), File (s2, d2) when s1 = s2 && d1 = d2 -> ()
    | Link s1, Link s2 when s1 = s2 -> ()
    | Dir s1, Dir s2 when s1 = s2 -> ()
    | a, b ->
      eprintf "Those two are different:\n  - %s::%s\n  - %s::%s\n"
        one_name (string_of_item a) two_name (string_of_item b)

  let rec compare = function
    | [] | _ :: [] -> ()
    | h1 :: h2 :: q ->
      Array.iteri (fun i a ->
        compare_two_items h1.ft_root a h2.ft_root h2.ft_tree.(i)
      ) h1.ft_tree;
      compare (h2 :: q)
*)

end

let () =
(*  let set_opt_str o s = o := Some s in
  let opt_may o f = match o with None -> () | Some s -> f s in *)

  let add_to_list l s = l := !l @ [s] in

  let to_parse_or_load = ref [] in
  let to_do = ref [] in
  let forget_specials = ref false in
  let options = [
    ( "-parse-tree", 
      Arg.String (fun s -> add_to_list to_parse_or_load (`parse s)),
      "<path>\n\tDescend a file tree (i.e. the find+md5sum).");
    ( "-forget-specials",
      Arg.Set forget_specials,
      "\n\tDo not keep track of “special” files and errors while building trees \
       \n\t(i.e. keep only regular files, symbolic links, and directories).");
 (*   ( "-save-to",
      Arg.String (set_opt_str output_file),
      "<path>\n\tSave all trees to a file (uses the Marshal module).");
    ( "-load",
      Arg.String (fun s -> add_to_list to_build_or_load (`load s)),
      "<path>\n\tLoad file trees (coming from a -save-to invocation).");
    ( "-fast-compare",
      Arg.Set do_fast_compare,
      "\n\tDo a `fast comparison' of all the built/loaded trees\
        \n\t(will simply tell if they are all equal or not).");
    ( "-compare",
      Arg.Set do_compare,
      "\n\tDo an (experimental) detailed comparison.");
 *)    ( "-print",
      Arg.Unit (fun () -> add_to_list to_do `print),
      " \n\tPrint the parsed and loaded trees.");
    ( "-version",
      Arg.Unit (fun () -> printf "%s\n" version),
      (sprintf "\n\tPrint version number on stdout (i.e. print %S)." version))
  ] in
  let anon s = eprintf "Do not know what to do with %S\n" s in
  let usage = "dircmp [OPTIONS]" in
  Arg.parse options anon usage;

  let actions =
    List.rev_map (function | `print -> File_tree.print ~indent:0) !to_do in
  List.iter (function
    | `parse path ->
      File_tree.descend ~forget_specials:!forget_specials ~actions path
  ) !to_parse_or_load;

(*
  let trees =
    List.flatten 
    (List.map (function
      | `build b -> [File_tree.build ~forget_specials:!forget_specials b]
      | `load l -> File_tree.load_from_file l) !to_build_or_load) in
  if !print then List.iter File_tree.print trees;
  opt_may !output_file (File_tree.save_to_file trees);
  if !do_fast_compare then (
    if File_tree.fast_compare trees then
      printf "[dircmp] Fast compare: Yes, they are all equal.\n"
    else
      printf "[dircmp] Fast compare: No, they are not all equal.\n"
  );
  if !do_compare then (
    File_tree.compare trees
  ); *)
  exit 0
