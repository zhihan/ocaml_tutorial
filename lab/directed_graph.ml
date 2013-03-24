type inport = {inputIdx: int}
type internal = {id: int}
type outport = {outputIdx: int}
type vertex = Inport of inport | Internal of internal | Outport of outport

let v_string v = 
  match v with
    | Inport i -> "i" ^ string_of_int(i.inputIdx)
    | Outport o -> "o" ^ string_of_int(o.outputIdx)
    | Internal j -> string_of_int(j.id)

let rec vl_string vl = 
  match vl with
    | vh :: vt -> v_string(vh) ^ "\n" ^ vl_string(vt)
    | [] -> ""

type edge = vertex * vertex

let e_string e = 
  match e with
    | (s,t) -> v_string(s) ^ "->" ^ v_string(t)

let rec el_string el =
  match el with 
    | [] -> ""
    | eh::et -> e_string(eh) ^ "\n" ^ el_string(et)

type dg = (vertex list)*(edge list) 

let g_dot g = 
  "digraph G {\n" ^ match g with 
    | (vl, el) -> vl_string(vl) ^ el_string(el) 
  ^ "}\n"   

(* successor *)
let succ g v = 
  match g with
    | (vl, el) -> 
   List.map (fun e -> 
     match e with (s, t) -> t )
     ( List.filter (fun x -> 
       match x with (s,t) -> s = v 
     ) el )

(* predecessor *)
let pre g v = 
  match g with
    | (vl, el) -> 
   List.map (fun e -> 
     match e with (s, t) -> s )
     ( List.filter (fun x -> 
       match x with (s,t) -> t = v 
     ) el )

let vertices g = match g with (vl, el) -> vl 

let edges g = match g with (vl, el) -> el 

let inputs g = match g with (vl, el ) ->
  List.filter ( fun v -> 
    match v with 
      | Inport i -> true
      | _ -> false
  ) vl

let outputs g = match g with (vl, el ) ->
  List.filter ( fun v -> 
    match v with 
      | Outport o -> true
      | _ -> false
  ) vl

let is_subset_of subset superset = 
  match subset with
    | [] -> true
    | _-> List.for_all (fun element -> 
                           List.mem element superset ) subset

let is_frontier aLabel allLabels = 
  not (List.exists (fun x ->
                     (is_subset_of aLabel x ) &&
                       (not (is_subset_of x aLabel)) ) allLabels )
    
class reach = object
  val h  = Hashtbl.create 1 
  method init(vl: vertex list)  = 
    List.iter (fun v -> Hashtbl.add h v [] ) vl

  method reached i = 
    let t = Hashtbl.find h i in
    List.mem i t
  
  method add_reach (v:vertex) (i:vertex) = 
    let t = Hashtbl.find h v in
    let newt = if (List.mem i t) then t else i::t in
      Hashtbl.replace h v newt
    
  method print () = 
    Hashtbl.iter (fun v r ->
                    begin
                      print_string(v_string(v));
                      print_string ": [";
                      List.iter ( fun x -> 
                                    print_string(v_string(x) ^ " ") ) r ;
                      print_string "]\n"
                    end
                 ) h
  method get vtx = Hashtbl.find h vtx


  method frontier () = 
    let allFrontier = ref [] in
    let allReach = ref [] in
      begin
        Hashtbl.iter (fun v labels -> 
                        allReach := labels :: (!allReach) ) h;
        
        Hashtbl.iter (fun v labels -> 
                      match v with 
                        | Internal i ->
                            if is_frontier labels (!allReach) then
                              if not (List.mem labels (!allFrontier)) then 
                              allFrontier := labels :: (!allFrontier) 
                        | _ -> () ) h;
        (!allFrontier)
      end

  method partition labels = 
    let allParts = ref [] in
    let aPart = ref [] in
      List.iter ( fun superset -> 
                    aPart := [];
        Hashtbl.iter (fun v labels ->
          match v with 
            | Internal i ->
                if (is_subset_of labels superset) then
                  aPart := v :: (!aPart)
            | _ -> ()
            ) h; 
        
         allParts := (!aPart) :: (!allParts)
        ) labels;
      !allParts

end

class visited = object (s)
  val t = Hashtbl.create 1
  method init(vl: vertex list) = 
    List.iter (fun v -> Hashtbl.add t v false) vl

  method visited(v: vertex) = Hashtbl.find t v
  method set_visited(v: vertex) = Hashtbl.replace t v true
  method print () = 
   Hashtbl.iter (fun k v -> 
     if (v) then print_string(v_string(k)) ) t  

  method update_labels (r:reach) (i:vertex) = 
    Hashtbl.iter ( fun v visited ->
                    if visited then r#add_reach v i ) t

end

let bfs next g input = 
  let q = Queue.create() in 
  let vis = new visited in
    begin
      vis#init(vertices(g)) ;
      let n = next g input in
      List.iter (fun v -> 
           vis#set_visited v;
           Queue.add v q ) n ;
      while not (Queue.is_empty q) do
        let v = Queue.take q in
        let n = next g v in 
        List.iter (fun this_v ->
        if not (vis#visited this_v) then
           (vis#set_visited this_v;
            Queue.add this_v q) ) n ;
      done;
      vis
    end

let propagate g = 
  let reachLabels = new reach in 
    reachLabels#init(vertices(g));
  let inports = inputs g in
      List.iter( fun i -> 
                  let vis = bfs succ g i in
                   vis#update_labels reachLabels i 
               ) inports ;
  let outports = outputs g in
      List.iter( fun o -> 
                  let vis = bfs pre g o in
                   vis#update_labels reachLabels o 
               ) outports ;
  reachLabels

(*
  Unit tests 
*)

(* A trivial example *)
let graph1 () = 
  let a = Inport {inputIdx = 1} in
  let b = Outport {outputIdx = 1} in
  let c = Internal {id = 1} in
  let ac = (a,c) in
  let cb = (c, b) in
  let g = ([a;b;c], [ac;cb]) in
    g 

(* 
   The test example according to Mohamed and Alongkrit 
*)

let example1 () = 
  let i1 = Inport {inputIdx = 1} in
  let i2 = Inport {inputIdx = 2} in
  let i3 = Inport {inputIdx = 3} in
  let o1 = Outport {outputIdx = 1} in
  let o2 = Outport {outputIdx = 2} in
  let o3 = Outport {outputIdx = 3} in
  let a = Internal {id =1 } in
  let b = Internal {id =2 } in
  let c = Internal {id =3 } in
  let d = Internal {id =4 } in
  let e = Internal {id =5 } in
  let f = Internal {id =6 } in
  let g = Internal {id =7 } in
  let h = Internal {id =8 } in
  let j = Internal {id =9 } in
  let grph = ([i1;i2;i3;o1;o2;o3;a;b;c;d;e;f;g;h;j], 
              [(i1,a); (a,b); (b,c); (c,o1); 
              (i2,d); (d,e); (b,e); (e,f); 
              (f, o2); (i3, g); (g,h); (h,j); (e,j); (j,o3)]) in
    grph 

let graph2 () = 
  let a = Inport {inputIdx = 1} in
  let b = Outport {outputIdx = 1} in
  let c = Internal {id = 1} in
  let d = Internal {id = 2} in
  let ac = (a,c) in
  let cd = (c, d) in
  let db = (d, b) in
    ([a;b;c;d], [ac;cd;db])

let test1() =
  print_string("test_dot\n");
  let g = graph1 () in
    print_string(g_dot(g))

let test2() = 
  print_string("test_succ\n");
  let g = graph1 () in
  let vl = vertices(g) in 
    List.iter (fun a -> 
                 begin
                   print_string("succ(");
                   print_string(v_string(a));
                   print_string(")=");
                   List.iter (fun v -> print_string(v_string (v)) ) (succ g a);
                   print_string("\n");
                   () 
                 end
              ) vl

let test3() = 
  print_string("test_pre\n");
  let g = graph1() in
  let vl = vertices(g) in
    List.iter (fun a -> 
                 begin
                   print_string("pre(");
                   print_string(v_string(a));
                   print_string(")=");
                   List.iter (fun v -> print_string(v_string (v)) ) (pre g a);
                   print_string("\n");
                   () 
                 end
              ) vl 
      
let testall() =
  begin
    print_string("Regression testing ........................... \n");
    test1();
    test2();
    test3();
    print_string("End of Regression testing ---------------------\n\n");
  end

(* Main driver *)
let main() =
  begin
    print_string("1) Compute Reachability \n");
    let g = example1() in
    let r = propagate g in
      r#print();
      let f = r#frontier() in
        print_string("2) Compute Reachability Frontier\n");
        List.iter (fun labels ->
                     List.iter (fun label ->
                                  print_string(v_string(label) ^",")) 
                       labels ;
                  print_string("\n")) 
          f;
        print_string("3) Partition\n");
        let parts = r#partition f in
          List.iter (fun part ->
                       List.iter (fun v -> 
                                    print_string(v_string(v) ^ ",") 
                                 ) part;
                    print_string("\n")) parts;
        ()
      
  end
;;

main();;


(*
 todo :
   - Add functions to create quotient
*)
