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

(* Count file or dir *)
let count_file_or_dir (d:string) (f:string) (
  cl:FileCounter.t list): unit =
  let fullfile = Filename.concat d f in
    if not (is_symbolic_link fullfile) then 
      if not (is_directory fullfile ) then
	List.iter (fun (fc:FileCounter.t) -> 
	  FileCounter.process fc fullfile ) cl
      else
	count_dir fullfile cl 
    else ()

let is_empty_line line = 
  let reg = Str.regexp "[ ]+" in
  ((String.length line)==0) || (Str.string_match reg line 0)

let trim_line (line:string): string = 
  if String.contains line '#' then
    (* Remove comments *)
    let idx = String.index line '#' in
    let no_comments = String.sub line 0 idx in
    String.trim no_comments
  else 
    String.trim line

let process_lines (d:string) (l:string list) (
  cl:FileCounter.t list): unit = 
  List.iter (fun line -> 
    let x = trim_line line in
    if not(is_empty_line x ) then
      count_file_or_dir d x cl
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
    begin
      process_lines cwd lines cl; 
      List.iter (fun c -> FileCounter.print c ) cl ;
      let total_files = List.fold_left (fun a b -> a+ b.FileCounter.f) 0 cl in
      let total_lines = List.fold_left (fun a b -> a+ b.FileCounter.l) 0 cl in
      Printf.printf "Total %d files, %d lines \n" total_files total_lines  
    end
  else 
     Printf.printf "Usage: count_work directory"

let _ = main ()




