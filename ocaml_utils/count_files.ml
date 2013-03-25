(** 
    count_files Count the files and lines in all the subdirectories. 
    Adapted from previous python code.
*)
open String
open Filename
open Sys
open Unix

module FileCounter = struct
  (* Mutable data members to mimic object behavior in OCaml *)
  type t = { mutable f: int; 
	     mutable l: int; 
	     is: string->bool;
	     desc: string }

  (* Count the number of lines in a source file *)
  let count (this:t) (filename:string):unit = 
    let ic = open_in filename in
    this.f <- this.f +1; 
    try
      while true do
        let _ = input_line ic in
        this.l <- this.l + 1;
        ()
      done
    with End_of_file -> 
      close_in ic ;
      ()
      
  let create (isType: string->bool) (description:string)  =
    {f = 0; l = 0; is = isType; desc = description }
    
  let process (this:t) (name:string):unit = 
    if this.is name then
      count this name 
    else
      ()

  let print (this:t) : unit =
    Printf.printf "%s:\n %d files, %d lines\n" this.desc this.f this.l
    
end

(* sbt copy files to 'target' directory *)
let is_target f = (f = "target")

(* ocamlbuild creates '_build' directory *)
let is_hidden (f:string) = (f = "_build") || (f = "CMakeFiles")



(* Symbolic link *)
let is_symbolic_link (fullname:string):bool = 
  let st = lstat fullname in
  st.st_kind  = S_LNK

(** Count *)
let rec count_dir (d:string) (cl:FileCounter.t list) : unit = 
  let files = Sys.readdir d in
  Array.iter (fun (f:string) ->
    let fullfile = Filename.concat d f in
    if not (is_symbolic_link fullfile) then 
      if not (is_directory fullfile ) then
	List.iter (fun (fc:FileCounter.t) -> 
	  FileCounter.process fc fullfile ) cl
      else
	if (not (is_hidden f))  &&  
	  (not (is_target f)) then
	  count_dir fullfile cl
	else ()
    else ()
  )
    files

let main () =
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

let _ = main ()

  
