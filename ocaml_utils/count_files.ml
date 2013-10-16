(** 
    count_files Count the files and lines in all the subdirectories. 
    Adapted from previous python code.
*)

open String
open Filename
open Sys
open Unix
open File_util

let count_cwd () =
  let is_ocaml = fun (n:string) -> 
    (check_suffix n ".ml") || (check_suffix n ".mli")  in 
  let ocaml_counter = FileCounter.create is_ocaml "OCaml source code" in 

  let is_cpp = fun n ->
    (check_suffix n ".cpp") || (check_suffix n ".hpp" ) ||
      (check_suffix n ".c") || (check_suffix n ".h")
  in
  let cpp_counter = FileCounter.create is_cpp "Cpp source code" in

  let is_python = fun n -> check_suffix n ".py" in
  let python_counter = FileCounter.create is_python "Python source code" in

  let is_scala = fun n -> check_suffix n ".scala" in
  let scala_counter = FileCounter.create is_scala "Scala source code" in

  let is_matlab = fun n -> check_suffix n ".m" in
  let matlab_counter = FileCounter.create is_matlab "Matlab source code" in

  let is_antlr = fun n -> 
    (check_suffix n ".g") || (check_suffix n ".stg") 
  in
  let antlr_counter = FileCounter.create is_antlr "Antlr source code" in
  
  
  let d = Sys.getcwd () in
  let cl = [ocaml_counter; 
	    cpp_counter; 
	    python_counter;
	    scala_counter;
	    matlab_counter;
	    antlr_counter] in 
  count_dir d cl ;
  List.iter (fun c -> FileCounter.print c ) cl ;

  (* Calculate total *)
  let total_files = List.fold_left (fun a b -> a+ b.FileCounter.f) 0 cl in
  let total_lines = List.fold_left (fun a b -> a+ b.FileCounter.l) 0 cl in
    Printf.printf "Total %d files, %d lines \n" total_files total_lines

let _ = count_cwd ()

  
