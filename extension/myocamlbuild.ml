open Ocamlbuild_plugin;;

(* *)
dispatch begin function
  | Before_options -> ()
  | After_rules ->
    begin
      flag ["c"; "use_my"; "compile"]
	( S[A"-ccopt";
	    A"-I ../lib"]
	);

      flag ["ocaml"; "byte"; "use_my"; "link"]
	(S[ A"-custom";
            A"-cclib"; A"-lstdc++"; (* Add C++ to linker*)
            (* OCaml requires the link ordering: stub lib first and then library *)
            A"-cclib"; A"-L ."; A"-cclib"; A"-lmystub";
            A"-cclib"; A"-L ../lib"; A"-cclib"; A"-lmy" (* The actual C++ library *)
             ]);

      flag ["ocaml"; "native"; "use_my"; "link"]
	(S[
          A"-cclib"; A"-lstdc++"; (* Add C++ to linker*)
            (* OCaml requires the link ordering: stub lib first and then library *)
          A"-cclib"; A"-L ."; A"-cclib"; A"-lmystub";
          A"-cclib"; A"-L ../lib"; A"-cclib"; A"-lmy" (* The actual C++ library *)
	]);

      flag ["ocaml"; "byte"; "mktop"; "use_my"]
	(S[ A"-custom"; 
            A"-cclib"; A"-lstdc++"; (* Add C++ to linker*)
            (* OCaml requires the link ordering: stub lib first and then library *)
            A"-cclib"; A"-L ."; A"-cclib"; A"-lmystub";
            A"-cclib"; A"-L ../lib"; A"-cclib"; A"-lmy" (* The actual C++ library *)
             ]);
      
      dep["ocaml"; "use_my"] ["libmystub.a"]

    end
  | _ -> ()
end;;
