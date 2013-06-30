(* Simple test for simplex solver *)
open Glpk
open Glp_prob
open Matrix
open Parse_input
open Util

let edgeIdx i j n = 
  if i < j then
    (n-1 + n-i ) *i /2 + (j-i) -1
  else
    (n-1 + n-j) * j/ 2 + (i-j) -1

let print_edges x n =
  for i = 0 to n-1 do
    for j = 0 to n-2 do
      let jIdx = if j< i then j else j+1 in
      if x.(i*(n-1) + j ) = 1 then
        Printf.printf "%d -> %d \n" i jIdx
      else ()
    done
  done

let compute_edges x y = 
  let n = Array.length x in 
  let nE = n * (n - 1) / 2 in
  let c = Array.create nE 0. in
  (
    for i = 0 to (Array.length x) -2 do
      for j = i+1 to (Array.length x) -1 do
        c.(edgeIdx i j n) <- sqrt( 
          (x.(i) -. x.(j)) *. (x.(i) -. x.(j)) +. 
            (y.(i) -. y.(j)) *. (y.(i) -. y.(j))) 
      done
    done;
    (nE, c) 
  )

(* Create the distance data structure  *)
let create_dist nV (coords: (float*float) list) = 
  let x = Array.create nV 0. in
  let y = Array.create nV 0. in
  let rec loop i remain  = 
    match remain with
      | [] -> ()
      | h::tl -> 
        begin
          x.(i) <- fst h;
          y.(i) <- snd h;
          loop (i+1) tl
        end 
  in
  begin
    loop 0 coords;
    compute_edges x y
  end
  

let solve_tsp_mip (n:int) (nE:int) (c:float array) =
  (* Solve TSP problem using mixed integer programming *)
  let mip = create () in
  let nC = (n-1)*(n-2) in (* Additional sub-tour constraints *)

  begin 
    set_name mip "sample mip";
    set_obj_dir mip Min;

    (* Objective *)
    (* x_ij *)
    add_cols mip nE ;
    for i = 0 to n -1 do
      for j = 0 to n-2 do
	let jIdx = if j < i then j else (j+1) in 
	begin
	  set_col_kind mip (i*(n-1) +j+ 1) Boolean;
	  set_col_name mip (i*(n-1) +j+ 1) ("x" ^ 
				     (string_of_int i)^"_"^ (string_of_int j));
	  set_obj_coeff mip (i*(n-1)+j + 1) c.(edgeIdx i jIdx n);
	end
      done;
    done;
    
    (* u_i *)
    add_cols mip (n-1);
    for i = 0 to n-2 do
      set_col_kind mip (i+nE+1) Real;
      set_col_name mip (i+nE+1) ("u" ^ (string_of_int i));
      set_col_bnds_lower mip (i+nE+1) 0.0;
      set_obj_coeff mip (i+nE+1) 0.0
    done;

    (* Tour constraint *)
    add_rows mip (2*n) ;
    let a_mat = Triplet.create (2*n) (nE + n -1) (2*nE) in 
    let off = ref 0 in
    for i = 0 to (n - 1) do
      set_row_name mip (2*i+1) ("enter" ^ (string_of_int (i+1)));
      set_row_bnds_fixed mip (2*i+1) 1.0;
      set_row_name mip (2*i+2) ("leave" ^ (string_of_int (i+1)));
      set_row_bnds_fixed mip (2*i+2) 1.0;
      for j = 0 to n-2 do
        begin
          Triplet.set a_mat !off (2*i+1) (i*(n-1)+j +1) 1.0 ;
          off := !off + 1;
	  let jIdx = if j<i then j else j+1 in
	  let iIdx = if jIdx > i then i else i-1 in
	  Triplet.set a_mat !off (2*i+2) (jIdx*(n-1)+iIdx+1) 1.0;
	  off := !off + 1;
        end

      done;
    done;
    Triplet.normalize a_mat;
    
    (* sub-tour elimination *)
    add_rows mip nC ;
    let b_mat = Triplet.create nC (nE +n -1) (3*nC) in
    let rowIdx = ref 0 in
    let off = ref 0 in
    for i = 1 to (n-1) do
      for j = 1 to (n-1) do
	if i<> j then
	  begin
            set_row_name mip (!rowIdx + 1 + 2*n) (* Index here is global*) 
              ("sub-tour" ^ (string_of_int (!rowIdx)));
            set_row_bnds_upper mip (!rowIdx + 1 + 2*n) (float_of_int (n-1));
	    
            Triplet.set b_mat (!off) (!rowIdx +1) (nE + i) 1.0;
            off := !off + 1;
            Triplet.set b_mat (!off) (!rowIdx +1) (nE + j) (-1.0);
            off := !off + 1;
	    let jIdx = if i < j then j else j+1 in
            Triplet.set b_mat (!off) (!rowIdx +1)  (i*(n-1)+jIdx) (float_of_int n);
            off := !off + 1;
            rowIdx := !rowIdx + 1;
	  end
	else ()
      done;
    done;
    Triplet.normalize b_mat;

    let ab = Triplet.vertcat a_mat b_mat in
    load_matrix mip (Triplet.nnz ab) (
      Triplet.row_index ab) (Triplet.col_index ab) (
      Triplet.value ab);

    let opt = Glp_iocp.init () in
    let _ = Glp_iocp.enable_presolver opt in
    let out = intopt mip opt in 
    let v = Array.create nE 0 in
    for i= 0 to nE -1 do
      v.(i) <- int_of_float( mip_col_val mip (i+1))
    done;
    let cost = mip_obj_val mip in
    (out, v, cost)
  end

let parse_result nV x = 
  let visited = Array.create (nV) false in
  (* Find the next unvisited vertex i where (start, i) is in the
     result. *)
  let find_next_unvisited src = 
    let result = ref 0 in
    begin
      for i = 0 to nV-2 do
	if x.(src* (nV-1) + i) = 1 then
	  result := if i < src then i else i+1
	else()
      done;
      if visited.(!result) then
	None
      else
	Some !result
    end
  in
  let result = Array.create (nV) 0 in
  let latest = ref 0 in
  let offset = ref 0 in
  let stop = ref false in
  begin
    visited.(!latest) <- true;
    result.(!offset) <- !latest;
    offset := !offset + 1;
    while not(!stop) do
      let nextOpt = find_next_unvisited !latest in
      match nextOpt with
	| Some next -> 
	  begin
            visited.(next) <- true;
	    result.(!offset) <- next;
            offset := !offset + 1;
            latest := next
          end
	| None -> 
	  stop := true
    done;
    result
  end
  

let write_out nV cost x = 
  let filename = "result.dat" in
  let oc = open_out filename in
  let result = parse_result nV x in
  begin
    Printf.fprintf oc "%2.4f %d\n" cost 0; 
    Array.iter (fun x -> Printf.fprintf oc "%d " x) result; 
    close_out oc
  end
 

let _ = 
    if Array.length Sys.argv > 1 then
    let filename = Filename.concat (Sys.getcwd()) Sys.argv.(1) in
    let (nV, coords) = process_input filename in
    let (_, c) = create_dist nV coords in
    let (out, x, cost) = solve_tsp_mip nV (nV*(nV-1)) c in
    begin
      write_out nV cost x;
      Printf.printf "Return code %d\n" out;
      print_edges x nV;
      Array.iteri (fun i x -> Printf.printf "x(%d) = %d " (i+1) x) x; 
      Printf.printf "\n";
      Printf.printf "Optimal solution %2.4f\n" cost
    end
    

