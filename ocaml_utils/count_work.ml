(** 
    count_work Counts the files and lines that includes in
    a list of locations.
    
    The file location are all relative to the current working
    directory.

*)

open String
open Sys
open Filename
open Unix

(* My own utilities *)
open File_util
open Util

module StringSet = Set.Make(String)

(* Count file or dir *)
let count_file_or_dir (exc:StringSet.t) (d:string) (f:string) (
  cl:FileCounter.t list): unit =
  let fullfile = Filename.concat d f in
    if not (is_symbolic_link fullfile) then 
      if (not (is_directory fullfile ) ) &&
           (not (StringSet.mem fullfile exc )) then
	List.iter (fun (fc:FileCounter.t) -> 
                   FileCounter.process fc fullfile ) cl
      else
	count_dir fullfile cl 
    else ()

(* Whether the line is empty after removing comment *)
let is_empty_line line = 
  let reg = Str.regexp "[ ]+" in
  ((String.length line)==0) || (Str.string_match reg line 0)

(* Parse the string as an exlusion line, if success return the path,
 else return None *)
let parse_exclude_line line = 
  let reg = Str.regexp "-[ ]+" in
  let matched = Str.string_match reg line 0 in
  if matched then
    let start = Str.match_end () in
    let len = (String.length line) - start in
    let path = String.sub line start len in
    Some path
  else 
    None
  

(* Remove comments *)
let trim_line (line:string): string = 
  if String.contains line '#' then
    (* Remove comments *)
    let idx = String.index line '#' in
    let no_comments = String.sub line 0 idx in
    String.trim no_comments
  else 
    String.trim line

(* Parse the exclusion lines (lines beginning with '- ') *)
let parse_exclusion (d:string) (l:string list) : StringSet.t = 
  let excludeList = StringSet.empty in
  List.fold_left (fun (s:StringSet.t) (line:string) ->
                 let x = trim_line line in
                 if not(is_empty_line x) then
                   let excOpt = parse_exclude_line x in
                   match excOpt with 
                   | Some excFile -> 
                      let fullfile = Filename.concat d excFile in
                      StringSet.add fullfile s 
                   | None -> s 
                 else s
                ) excludeList l 

(* Process each line of the file, skip the ones in the exclusion set.
 And then invoke the counter objects to count the lines *)
let process_lines (exc:StringSet.t) (d:string) (l:string list) (
  cl:FileCounter.t list): unit = 
  List.iter (fun line -> 
    let x = trim_line line in
    if not(is_empty_line x ) then
      let excOpt = parse_exclude_line x in
      match excOpt with
      | Some excFile -> ()
      | None -> count_file_or_dir exc d x cl
    else () ) l

let main () = 
  if Array.length Sys.argv > 1 then
    let profile = Sys.argv.(1) in
    let ic = open_in profile in
    let cwd = Sys.getcwd () in
    let lines = read_lines ic in
    (* let _ = List.iter (fun x -> print_string x) lines in *)
    let is_cpp = fun n ->
      (check_suffix n ".cpp") || (check_suffix n ".hpp" ) ||
        (check_suffix n ".c") || (check_suffix n ".h")
    in
    let cpp_counter = FileCounter.create is_cpp "Cpp source code" in
    let is_matlab = fun n -> check_suffix n ".m" in
    let matlab_counter = FileCounter.create is_matlab "Matlab source code" in
    let cl = [cpp_counter; matlab_counter] in
    let exc = parse_exclusion cwd lines in 
    begin
      process_lines exc cwd lines cl; 
      List.iter (fun c -> FileCounter.print c ) cl ;
      let total_files = List.fold_left (fun a b -> a+ b.FileCounter.f) 0 cl in
      let total_lines = List.fold_left (fun a b -> a+ b.FileCounter.l) 0 cl in
      Printf.printf "Total %d files, %d lines \n" total_files total_lines  
    end
  else 
     Printf.printf "Usage: count_work profile"

let _ = main ()




