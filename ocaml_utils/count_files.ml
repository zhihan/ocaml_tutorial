open String
open Filename

module FileCounter = struct
  type t = {mutable f: int; mutable l: int; is: string->bool}

  let count (this:t) (filename:string):unit = 
    let ic = open_in filename in
    this.f <- this.f +1; 
    try
      while true do
        let _ = input_line ic in
        this.l <- this.l + 1;
        ()
      done
    with End_of_file -> ()
      
  let create (isType: string->bool) =
    {f = 0; l = 0; is = isType }
    
  let process (this:t) (name:string):unit = 
    if this.is name then
      count this name 
    else
      ()
    
end


let main () =
  let is_ocaml = fun (n:string) -> check_suffix n ".ml" in 
  let fc = FileCounter.create is_ocaml in 
  let d = Sys.getcwd () in
  let files = Sys.readdir d in
  Array.iter (FileCounter.process fc) files;
  Printf.printf "Count %d %d\n" fc.FileCounter.f fc.FileCounter.l
    
let _ = main ()

  
