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
    for j = i+1 to n-1 do
      if x.(edgeIdx i j n) = 1 then
        Printf.printf "%d - %d \n" i j
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
  let nC = (n-1)*(n-2)/2 in (* Additional sub-tour constraints *)

  begin 
    set_name mip "sample mip";
    set_obj_dir mip Min;

    (* Objective *)
    (* x_ij *)
    add_cols mip nE ;
    for i = 0 to nE -1 do
      set_col_kind mip (i+1) Boolean;
      set_col_name mip (i+1) ("edge" ^ (string_of_int i));
      set_obj_coeff mip (i+1) c.(i)
    done;
    
    (* u_i *)
    add_cols mip (n-1);
    for i = 0 to n-2 do
      set_col_kind mip (i+nE+1) Real;
      set_col_name mip (i+nE+1) ("u" ^ (string_of_int i));
      set_col_bnds_free mip (i+nE+1);
      set_obj_coeff mip (i+nE+1) 0.0
    done;

    (* Tour constraint *)
    add_rows mip n ;
    let a_mat = Triplet.create n (nE + n -1) (2*nE) in 
    let off = ref 0 in
    for i = 0 to (n - 1) do
      set_row_name mip (i+1) ("visited" ^ (string_of_int i));
      set_row_bnds_fixed mip (i+1) 2.0;
      for j = 0 to n-1 do
        if j <> i then
          begin
            Triplet.set a_mat !off (i+1) ((edgeIdx i j n) +1) 1.0 ;
            off := !off + 1
          end
        else ()
      done;
    done;

    (* sub-tour elimination *)
    add_rows mip nC ;
    let b_mat = Triplet.create nC (nE +n -1) (3*nC) in
    let rowIdx = ref 0 in
    let off = ref 0 in
    for i = 1 to (n-1) do
      for j = (i+1) to (n-1) do
        set_row_name mip (!rowIdx + 1 + n) (* Index here is global*) 
          ("sub-tour" ^ (string_of_int (!rowIdx)));
        set_row_bnds_upper mip (!rowIdx + 1 + n) (float_of_int (n-1));

        Triplet.set b_mat (!off) (!rowIdx +1) (nE + i) 1.0;
        off := !off + 1;
        Triplet.set b_mat (!off) (!rowIdx +1) (nE + j) (-1.0);
        off := !off + 1;
        Triplet.set b_mat (!off) (!rowIdx +1) 
          ((edgeIdx i j n)+1) (float_of_int n);
        off := !off + 1;
        rowIdx := !rowIdx + 1;

      done;
    done;

    let ab = Triplet.vertcat a_mat b_mat in
    load_matrix mip (Triplet.nnz ab) (
      Triplet.row_index ab) (Triplet.col_index ab) (
      Triplet.value ab);

    let opt = Glp_iocp.init () in
    let _ = Glp_iocp.enable_presolver opt in
    let out = intopt mip opt in 
    let v = Array.create nE 0 in
    for i= 0 to nE -1 do
      v.(i) <- int_of_float( mip_col_val mip (i+1));
    done;
    let cost = mip_obj_val mip in
    (out, v, cost)
  end

let parse_result nV x = 
  let visited = Array.create (nV) false in
  let find_next start src = 
    let found = ref false in
    let current = ref start in
    begin
      found := (x.(edgeIdx src (!current) nV) = 1);
      while not(!found) && (!current < nV) do
        current := !current + 1;
        if (!current) <> src then 
          current := !current + 1
        else ();
        found := (x.(edgeIdx src (!current) nV) = 1);
      done;
      !current
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
      let next = find_next 0 !latest in
      if not(visited.(next)) then
        begin
          visited.(next) <- true;
          result.(!offset) <- next;
          offset := !offset + 1;
          latest := next
        end
      else
        let next = find_next (next+1) !latest in
        begin
          visited.(next) <- true;
          result.(!offset) <- next;
          offset := !offset + 1;
          latest := next
        end
    done;
    result
  end
  

let _ = 
    if Array.length Sys.argv > 1 then
    let filename = Filename.concat (Sys.getcwd()) Sys.argv.(1) in
    let (nV, coords) = process_input filename in
    let (nE, c) = create_dist nV coords in
    let (out, x, cost) = solve_tsp_mip nV nE c in
    let result = parse_result nV x in
    begin
      Printf.printf "Return code %d\n" out;
      print_edges x nV;
      Array.iter (fun x -> Printf.printf "%d " x) x; 
      Printf.printf "\n";
      Array.iter (fun x -> Printf.printf "%d " x) result; 
      Printf.printf "\n"; 
      Printf.printf "Optimal solution %2.4f\n" cost
    end
    

