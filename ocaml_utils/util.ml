(* Process input file *)
exception WrongInput of string

(** Read all lines in a file and return it in a list *)
let read_lines ic = 
  let lines = ref [] in
  try
    while true do
      let line = input_line ic in
      lines := line :: !lines
    done ;
    !lines
  with End_of_file -> (List.rev !lines)
 

