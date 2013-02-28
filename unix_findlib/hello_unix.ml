open Unix


let main () = 
  execvp "date" [| "" |]

let _ = main()
