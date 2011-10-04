open Printf

let version = "0"

module File_tree = struct

  type file_tree_item = 
    | Dir of string
    | File of string * Digest.t * int
    | Link of string
    | Char of string     (* Character device *) 
    | Block of string    (* Block device	*)	   
    | Pipe of string     (* Named pipe	*)   
    | Socket of string   (* Socket	*)   	 
    | Error of string * string
  type file_tree = {
    ft_root: string;
    ft_tree: file_tree_item array;
  }

  let build root =
    let ls dir =
      let sort a = Array.fast_sort String.compare a; a in
      let dir_read = Sys.readdir (root ^ "/" ^ dir) in
      Array.to_list (Array.map (fun f -> dir ^ "/" ^ f) (sort dir_read)) in
    let rec explore path =
      try 
        let real_path = (root ^ "/" ^ path) in
        let lstat = Unix.lstat real_path in
        match lstat.Unix.st_kind with
        | Unix.S_REG -> [File (path, Digest.file real_path, lstat.Unix.st_size)]
        | Unix.S_LNK -> [Link path]
        | Unix.S_DIR -> 
          Dir path ::
            (List.flatten (List.map explore (ls path)))
        | Unix.S_CHR  -> [Char   path] 
	| Unix.S_BLK  -> [Block	 path] 
	| Unix.S_FIFO -> [Pipe	 path] 
	| Unix.S_SOCK -> [Socket path] 
      with
      | Sys_error msg ->
          eprintf "Warning: Ignoring path %s (%s)\n" path msg;
          [Error (path, msg)]
      | Unix.Unix_error (e, _, _) ->
        let msg = Unix.error_message e in
        eprintf "Warning: Ignoring path %s (%s)\n" path msg;
        [Error (path, msg)]
    in
    {ft_root = root; ft_tree = Array.of_list (explore ".")}

  let print_item ft =
    match ft with 
    | Dir s -> printf "Dir: %s\n" s
    | File (s, d, z) -> printf "File: %s (%d Bytes; MD5: %s)\n" s z (Digest.to_hex d)
    | Link s -> printf "Link %s\n" s
    | Error (s, m) -> printf "Error: %s\n" m
    | Char   path 
    | Block  path
    | Pipe   path
    | Socket path -> printf "Other stuff: %s\n" path
      
  let print ft =
    printf "Root: %s\n" ft.ft_root;
    Array.iter print_item ft.ft_tree

  let save_to_file (ft : file_tree list) file =
    let o = open_out file in
    Marshal.to_channel o ft [];
    close_out o

  let load_from_file file =
    let i = open_in file in
    let ft = (Marshal.from_channel i : file_tree list) in
    close_in i;
    ft

  let rec fast_compare_two_items one two =
    match one, two with
    | File (s1, d1, z1), File (s2, d2, z2)
      when s1 = s2 && d1 = d2 && z1 = z2 -> true
    | Link s1, Link s2 when s1 = s2 -> true
    | Dir s1, Dir s2 when s1 = s2 -> true
    | a, b ->
      eprintf "Those are different:\n";
      print_item a;
      print_item b;
      false

  let rec fast_compare = function
    | [] | _ :: [] -> true
    | h1 :: h2 :: q ->
      compare h1.ft_tree h2.ft_tree = 0
      (* List.for_all2 fast_compare_two_items h1 h2 *)
      && (fast_compare (h2 :: q))

end

let () =
  let set_opt_str o s = o := Some s in
  let opt_may o f = match o with None -> () | Some s -> f s in

  let add_to_list l s = l := !l @ [s] in

  let build_dir_trees = ref [] in
  let output_file = ref None in
  let input_files = ref [] in
  let print = ref false in
  let do_fast_compare = ref false in
  let options = [
    ( "-build-tree", 
      Arg.String (add_to_list build_dir_trees),
      "<path>\n\tBuild a file tree (i.e. the find+md5sum).");
    ( "-save-to",
      Arg.String (set_opt_str output_file),
      "<path>\n\tSave all trees to a file (uses the Marshal module).");
    ( "-load",
      Arg.String (add_to_list input_files),
      "<path>\n\tLoad file trees (coming from a -save-to invocation).");
    ( "-fast-compare",
      Arg.Set do_fast_compare,
      "\n\tDo a `fast comparison' of all the built/loaded trees\
        \n\t(will simply tell if they are all equal or not).");
    ( "-print",
      Arg.Set print,
      " \n\tPrint the built and loaded trees.")
  ] in
  let anon s = eprintf "Do not know what to do with %S\n" s in
  let usage = "dircmp [OPTIONS]" in
  Arg.parse options anon usage;

  let trees =
    (List.map File_tree.build !build_dir_trees) @
      (List.flatten (List.map File_tree.load_from_file !input_files))
  in
  if !print then List.iter File_tree.print trees;
  opt_may !output_file (File_tree.save_to_file trees);
  if !do_fast_compare then (
    if File_tree.fast_compare trees then
      printf "[dircmp] Fast compare: Yes, they are all equal.\n"
    else
      printf "[dircmp] Fast compare: No, they are not all equal.\n"
  );
  exit 0
