exception PathException of string

open Str

let rec concat_parts (s:string list) (out:string) = 
  match s with
  | h:: tl -> concat_parts tl (out ^ "/" ^ h) 
  | [] -> out
                           

let rec process_parts (s: string list) (scanned: string list) = 
  match s with
  | sh :: stl -> 
     (
       match sh with 
       | ".." -> 
          (
            match scanned with
            | h :: tl -> process_parts stl tl
            | [] -> raise (PathException "At root level")
          )
       | f -> process_parts stl (f::scanned)
     )
  | [] -> (List.rev scanned)
  


let concat_path a b = 
  let reg = regexp "/" in
  let asplit = split reg a in
  let bsplit = split reg b in
  let full = asplit @ bsplit in
  let result = process_parts full [] in
  concat_parts result ""
  

let main () = 
  if Array.length Sys.argv == 3 then
    let a = Sys.argv.(1) in
    let b = Sys.argv.(2) in
    let c = concat_path a b in
    begin
      print_string "Result is ";
      print_endline c
    end
                 
let _ = main ()
