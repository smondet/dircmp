open Dircmp_internal_pervasives
module About = Dircmp_about

type file_tree_item =
| Root of string
| Dir of string
| File of string * Digest.t
| Link of string * string option
| Char of string     (* Character device *)
| Block of string    (* Block device	*)
| Pipe of string     (* Named pipe	*)
| Socket of string   (* Socket	*)
| Error of string * string

let descend ?(forget_specials=false) ?(read_links=true) ~actions root =
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
      | Unix.S_LNK ->
        do_actions (Link (path,
                          if read_links then
                            Some (Unix.readlink real_path) else None))
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
  | Link (s, d) ->
    sprintf "Link %s%s" s
      (match d with None -> "" | Some s -> sprintf " -> %s" s)
  | Error (s, m) -> sprintf "Error %s" m
  | Char   path
  | Block  path
  | Pipe   path
  | Socket path -> sprintf "Other stuff %s" path

let print out item =
  fprintf out "%s\n" (string_of_item item)

let _file_tree_magic_string =
  sprintf "dircmp v %s\nOCaml %s\n" About.version Sys.ocaml_version

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
