open Dircmp_internal_pervasives
open Dircmp

let digest : Command.t = Command.basic
  ~summary:"compute digest of a directory tree"
  Command.Spec.(
    empty
    +> flag "parse-tree" (optional string) ~doc:" descend a file tree"
    +> flag "save-to" (optional string) ~doc:" save all trees to a file (uses the Marshal module)"
    +> flag "load" (optional string) ~doc:" load file trees (coming from a -save-to invocation)"
    +> flag "print" no_arg ~doc:" print the parsed and loaded trees to standard output"
    +> flag "print-to" (optional string) ~doc:" print the parsed and loaded trees to a file"
    +> flag "forget-specials" no_arg ~doc:" do not keep track of “special” files and errors while parsing trees (i.e. look only at regular files, symbolic links, and directories)"
    +> flag "do-not-read-links" no_arg ~doc:" do not track where the symbolic links point to"
  )
  (fun parse_tree save_to load print print_to forget_specials_arg do_not_read_links () ->
    let add_to_list l s = l := !l @ [s] in
    let to_parse_or_load = ref [] in
    let to_do = ref [] in
    let forget_specials = ref false in
    let read_links = ref true in

    (match parse_tree with
    | None -> ()
    | Some s -> add_to_list to_parse_or_load (`parse s)
    );

    (match save_to with
    | None -> ()
    | Some s -> add_to_list to_do (`save s)
    );

    (match load with
    | None -> ()
    | Some s -> add_to_list to_parse_or_load (`load s)
    );

    (if print then add_to_list to_do `print);

    (match print_to with
    | None -> ()
    | Some path -> add_to_list to_do (`print_to path)
    );

    (if forget_specials_arg then forget_specials := true);
    (if do_not_read_links then read_links := false);

    let actions =
      List.map ~f:(function
      | `print -> File_tree.print stdout
      | `print_to path ->
        let out = open_out path in
        at_exit (fun () -> Out_channel.close out);
        File_tree.print out
      | `save path ->
        let action, to_do_at_exit = File_tree.file_saver path in
        at_exit to_do_at_exit;
        action
      ) !to_do
    in

    List.iter ~f:(function
    | `parse path ->
      File_tree.descend ~forget_specials:!forget_specials
        ~read_links:!read_links ~actions path
    | `load path ->
      File_tree.load ~actions path
    ) !to_parse_or_load;
  )


let diff : Command.t = Command.basic
  ~summary:"compute diff"
  Command.Spec.(
    empty
    +> flag "tmp-dir" (optional_with_default Filename.temp_dir_name string) ~doc:"path set the temporary directory"
    +> flag "diff-cmd" (optional_with_default "diff" string) ~doc:"path set the command used for “diff”"
    +> anon ("left" %: string)
    +> anon ("right" %: string)
  )
  (fun tmp_dir diff_cmd left right () ->
    let transform_dircmp f =
      let tmp = Filename.temp_file ~in_dir:tmp_dir "dircmpdiff" ".txt" in
      let out = open_out tmp in
      let actions = [ File_tree.print out ] in
      File_tree.load ~actions f;
      Out_channel.close out;
      tmp
    in

    let text_version f =
      if Filename.check_suffix f ".dircmp" then
        transform_dircmp f
      else
        f
    in

    let txt_left = text_version left in
    let txt_right = text_version right in
    let open Unix in
    begin match system (sprintf "%s %s %s" diff_cmd txt_left txt_right) with
    | Result.Ok () -> ()
    | Error (`Exit_non_zero 127) ->
      eprintf "The result WEXITED 127 indicates that the shell couldn't be executed."
    | Error (`Exit_non_zero n) -> eprintf "%s returned %d\n" diff_cmd n
    | Error (`Signal s) -> eprintf "%s was killed by signal %s\n" diff_cmd (Signal.to_string s)
    end
  )

let main = Command.group
  ~summary:About.description
  [
    "digest", digest;
    "diff", diff;
  ]

;;
try Command.run ~version:About.version main
with e -> eprintf "%s\n" (Exn.to_string e)
