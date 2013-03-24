(*
   My first OCAML script for manipulating Binary Decision Diagrams
*)
module Bdd = struct
  (*
    The main type of Bdd is the vertex
    There are terminals and regular (non-terminal) vertices
  *)

  type terminal = One | Zero

  type t = Terminal of terminal | Regular of regular  
  and regular = {
    lo: t; 
    hi: t;
    index: int;
    id:   int;   (* For now require to be unique *)
    mark: bool
  }

  let compare_int x y =
    if x < y then -1 else if x = y then 0 else 1

  let compare x y = 
    match x, y with
      | (Terminal One, Terminal Zero) -> 1
      | (Terminal Zero, Terminal One) -> -1
      | (Terminal Zero, Terminal Zero) -> 0
      | (Terminal One, Terminal One) -> 0
      | (Regular xr, Regular yr) -> compare_int xr.id yr.id
      | (Regular r, Terminal t) -> 1
      | (Terminal t, Regular r) -> -1

  let tlabel v = 
    match v with 
      | Terminal One -> "1"
      | Terminal Zero -> "0"
      | Regular r -> ""

  let tid v =
    match v with 
      | Terminal One -> "T1"
      | Terminal Zero -> "T0"
      | Regular r -> "N" ^ string_of_int(r.id)
    
end


module BSet = Set.Make(Bdd) ;;
(*
  Collect all vertices in a set. Requires the 
  Ids of the vertices are different.
*)
let rec collect_all(v) = 
  let m = BSet.empty in
    match v with
      | Bdd.Terminal t -> BSet.add v m
      | Bdd.Regular r ->
          let m = BSet.add v m in
          let lo_n = collect_all r.Bdd.lo in
          let hi_n = collect_all r.Bdd.hi in
            BSet.union (BSet.union lo_n m) hi_n
;;

let dot_node v = 
  let vbuf = 
    Bdd.tid(v) ^ 
    match v with
    | Bdd.Terminal t-> 
        (
        match t with 
          | Bdd.One -> "[label=\"1\"][shape=\"box\"]"
          | Bdd.Zero -> "[label=\"0\"][shape=\"box\"]"
        )
    | Bdd.Regular r -> ""
  in
  let ebuf = match v with 
    |Bdd.Terminal t -> " "
    |Bdd.Regular r -> 
       Bdd.tid(v) ^ "--" ^ Bdd.tid(r.Bdd.lo) ^ "\n" ^
       Bdd.tid(v) ^ "--" ^ Bdd.tid(r.Bdd.hi) 
  in 
  vbuf^"\n" ^ebuf ^"\n"
;;

let rec dot v = 
  "graph G {\n" ^ 
  let s = collect_all(v) in
  let l = BSet.elements s in
  let str_list = List.map dot_node l in
    String.concat "" str_list
  ^ "}\n" 
;;

(*
  Main driver as the test case
*)
let main() = 
  let a = Bdd.Regular {
    Bdd.lo = Bdd.Terminal Bdd.Zero;
    Bdd.hi = Bdd.Terminal Bdd.One;
    Bdd.index = 0;
    Bdd.id = 0;
    Bdd.mark = false;
  } in
  print_string (dot a)
;;

main();;


