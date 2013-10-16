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
    if this.f > 0 then
      Printf.printf "%s:\n %d files, %d lines\n" this.desc this.f this.l
    else ()
    
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

