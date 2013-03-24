let parse_opt () = 
  let usage = "Usage: " ^ Sys.argv.(0) ^ " filename -c constraint " in
  let con = ref "" in
  let filename = ref [] in  
  let speclist = [
    ("-c", Arg.String (fun s -> con := s ), "constraint" )] in
  ( 
    Arg.parse 
      speclist
      (fun s -> filename := s :: !filename)
      usage ;
    Printf.printf "filename: %s\n" (String.concat "," !filename) ; 
    Printf.printf "constraint: %s\n" !con
  )

let _ = parse_opt ()

