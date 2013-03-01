(* Trim leading and trailing spaces *)
let str_trim s =
  let is_space = function
    | ' ' | '\012' | '\n' | '\r' | '\t' -> true
    | _ -> false in
  let len = String.length s in
  let i = ref 0 in
  while !i < len && is_space (String.get s !i) do
    incr i
  done;
  let j = ref (len - 1) in
  while !j >= !i && is_space (String.get s !j) do
    decr j
  done;
  if !i = 0 && !j = len - 1 then
    s
  else if !j >= !i then
    String.sub s !i (!j - !i + 1)
  else
    ""

(* Process a line and separate the two sides of '=' *)
let process_line (line:string) = 
  if String.contains line '=' then
    let i = String.index line '=' in
    let lhs = String.sub line 0 i in
    let rhslen  = (String.length line) - i - 1 in
    let rhs = String.sub line (i+1) rhslen in
   (str_trim lhs, str_trim rhs)
  else
    ("", "")

(* Read input and divide the lines separated by '=' *)  
let read_inputs filename = 
  let ic = open_in filename  in
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
  

