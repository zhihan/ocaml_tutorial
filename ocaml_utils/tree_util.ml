type node = Leaf of string | Branch of string * (node list)

let rec print_it (prefix:string) (n:node) = 
  match n with 
    | Leaf x -> print_endline (prefix ^ x)
    | Branch (s, l)  -> List.iter (fun x -> print_it (prefix ^ s) x ) l
   
let main () = 
  let x = Branch ("r", [ Leaf "a"; Branch("b", [ Leaf "c"; Leaf "d"])]) in
  print_it "" x

let _ = main ()
    
