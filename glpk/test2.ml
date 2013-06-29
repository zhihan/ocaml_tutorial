(* Simple test for simplex solver *)
open Glpk
open Glp_prob
open Matrix


let edgeIdx i j n = 
  if i < j then
    (n-1 + n-i ) *i /2 + (j-i) -1
  else
    (n-1 + n-j) * j/ 2 + (i-j) -1
  

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
  

let test () = 
  let x = [| 0. ; 0. ; 0. ;1.; 1.|] in
  let y = [| 0.; 0.5; 1.; 1.; 0. |] in
  let n = Array.length x in
  let (nE, c) = compute_edges x y in

  let mip = create () in
  let nC = (n-1)*(n-2)/2 in (* Additional sub-tour constraints *)

  begin 
    print_endline ("nc="^ (string_of_int nC));
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
      Printf.printf "Set up %d\n" i;
      set_col_name mip (i+nE+1) ("u" ^ (string_of_int i));
      set_col_bnds_free mip (i+nE+1);
      set_obj_coeff mip (i+nE+1) 0.0
    done;

    (* Tour constraint *)
    add_rows mip n ;
    let a_mat = Triplet.create n (nC+n) (2*nE) in 
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
    print_endline ("a="^ (Triplet.to_string a_mat));

    (* sub-tour elimination *)
    add_rows mip nC ;
    let b_mat = Triplet.create nC (nC+n) (3*nC) in
    let rowIdx = ref 0 in
    let off = ref 0 in
    for i = 1 to (n-1) do
      for j = (i+1) to (n-1) do
        set_row_name mip (!rowIdx + 1 + n) (* Index here is global*) 
          ("sub-tour" ^ (string_of_int (!rowIdx)));
        set_row_bnds_upper mip (!rowIdx + 1 + n) (float_of_int (n-1));
        Printf.printf "Upper %d %1.1f\n" (!rowIdx + 1 + n) (float_of_int (n+1));

        Triplet.set b_mat (!off) (!rowIdx +1) (nE + i) 1.0;
        off := !off + 1;
        Triplet.set b_mat (!off) (!rowIdx +1) (nE + j) (-1.0);
        off := !off + 1;
        Triplet.set b_mat (!off) (!rowIdx +1) 
          ((edgeIdx i j n)+1) (float_of_int n);
        off := !off + 1;
        rowIdx := !rowIdx + 1
      done;
    done;
    print_endline ("b="^ (Triplet.to_string b_mat));

    let ab = Triplet.vertcat a_mat b_mat in
    print_endline (Triplet.to_string ab);
    load_matrix mip (Triplet.nnz ab) (
      Triplet.row_index ab) (Triplet.col_index ab) (
      Triplet.value ab);

    let opt = Glp_iocp.init () in
    let _ = Glp_iocp.enable_presolver opt in
    let out = intopt mip opt in
    Printf.printf "Return code %d\n" out;

    for i= 0 to (nE+n-2) do
      let v = mip_col_val mip (i+1) in
      Printf.printf "x(%d) = %1.1f\n " (i+1) v
    done;
    Printf.printf "\n"
  end

let _ = test ()
