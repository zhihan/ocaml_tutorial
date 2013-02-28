
let process_line (line:string) = 
  if String.contains line '=' then
    let i = String.index line '=' in
    let lhs = String.sub line 1 (i-1) in
    let rhslen  = (String.length line) - i - 1 in
    let rhs = String.sub line (i+1) rhslen in
    (lhs, rhs)
  else
    ("", "")

  
let read_inputs filename = 
  let ic = open_in filename  in
  (* let ocamlroot = ref "" in
  let proverlib = ref "" in *)
  try
    while true  do
      let line = input_line ic in
      let (lhs, rhs) = process_line(line) in
      Printf.printf "%s == :%s: \n" lhs rhs
    done
  with End_of_file ->
    close_in ic ;
    ()
    
let _ = read_inputs Sys.argv.(1) ;;
  

