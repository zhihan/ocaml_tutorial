open OUnit
(* This simple code demonstrate the internals of oUnit *)

let test_fun () = ()

let test_with_setup_cleanup =
  bracket 
    ( fun () -> Printf.printf "setup\n" )
    ( fun () -> Printf.printf "test\n") 
    ( fun () -> Printf.printf "cleanup\n")

let suite = 
  "root" >:::  (* Labeled list *)
    [ "a" >:: test_fun ;  (* Labeled test case *)
      "b" >:: test_fun ;
      "c" >::: [ 
	"c1" >:: test_fun;
	"c2" >:: test_fun];
      "d" >: ("a" >:: test_with_setup_cleanup)  (* Label only *)
    ] 

let _ = run_test_tt_main suite
