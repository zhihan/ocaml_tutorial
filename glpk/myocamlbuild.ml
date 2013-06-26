open Ocamlbuild_plugin;;

(* Makerules are encoded in the dispatch function *)
dispatch begin function
  | Before_options -> ()
  | After_rules ->
    begin
      flag ["c"; "use_glpk"; "compile"] 
	(S[A"-ccopt"; A"-I /opt/local/include"]);

      flag ["ocaml"; "byte"; "use_glpk"; "link"]
	(S[ A"-custom";
            (* OCaml requires the link ordering: stub lib first and then library *)
	    A"-cclib"; A"-L/opt/local/lib"; (* macport install of glpk *)
            A"-cclib"; A"-L."; A"-cclib"; A"-lglpk_stub";
            A"-cclib"; A"-lglpk"
             ]);

      flag ["ocaml"; "native"; "use_glpk"; "link"]
	(S[
	  A"-cclib"; A"-L/opt/local/lib"; (* macport install of glpk *)
          A"-cclib"; A"-L."; A"-cclib"; A"-lglpk_stub";
           A"-cclib"; A"-lglpk" (* The actual library *)
	]);

      flag ["ocaml"; "byte"; "mktop"; "use_glpk"]
	(S[ A"-custom"; 
	    A"-cclib"; A"-L/opt/local/lib"; (* macport install of glpk *)
	    A"-cclib"; A"-L."; A"-cclib"; A"-lglpk_stub";
            A"-cclib"; A"-lglpk"
          ]);
      
      dep["ocaml"; "use_glpk"] ["libglpk_stub.a"]

    end
  | _ -> ()
end;;
