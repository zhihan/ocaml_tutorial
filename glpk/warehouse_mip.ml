open Parse_input_warehouse
open Util
open Matrix

open Glpk
open Glp_prob

let solve_warehouse_mip m n cap setup demand trans = 
  let mip = create () in
  
  begin
    set_name mip "Warehouse mip";
    set_obj_dir mip Min;
    
    add_cols mip m; (* s_i, i = 0...m-1*)
    for i=0 to m-1 do
      set_col_kind mip (i+1) Boolean;
      set_col_name mip (i+1) ("s" ^ (string_of_int i));
      set_obj_coeff mip (i+1) setup.(i)
    done;

    add_cols mip (m*n); (* x_ij, i= 0...m-1, j=0...n-1 *)
    for i=0 to m-1 do (* warehouse *)
      for j=0 to n-1 do (* demand *)
        set_col_kind mip (m + i*n+j+1) Boolean;
        set_col_name mip (m + i*n+j+1) ("x" ^ 
                                       (string_of_int i) ^
                                       "_" ^ 
                                       (string_of_int j));
        set_obj_coeff mip (m + i*n+j+1) (Dense.get trans j i);
      done;
    done;

    add_rows mip n; (* Demands are met *) 
    let demand_mat = Triplet.create n (m + m*n) (m*n) in
    let off = ref 0 in
    for j=0 to n-1 do
      set_row_name mip (j+1) ("Demands at " ^ (string_of_int j));
      set_row_bnds_fixed mip (j+1) 1.0; (* sum of Bool is 1 *)
      for i = 0 to m-1 do
        Triplet.set demand_mat !off (j+1) (m + i*n+j+1) 1.0;
        off := !off+1
      done;
    done;

    add_rows mip m; (* Capacity is not violated *)
    let capacity_mat = Triplet.create m (m+m*n) (m*n+m) in
    let off = ref 0 in
    for i = 0 to m-1 do
      set_row_name mip (n+i+1) ("Capacity at " ^ (string_of_int i));
      set_row_bnds_upper mip (n+i+1) 0.0; (* demands - capacity <=0 *)
      
      Triplet.set capacity_mat !off (i+1) (i+1) (-. cap.(i));
      off := !off + 1;
      for j = 0 to n-1 do
        Triplet.set capacity_mat !off (i+1) (m + i*n+j+1) (demand.(j));
        off := !off +1;
      done
    done;

    let a = Triplet.vertcat demand_mat capacity_mat in
    load_matrix mip (Triplet.nnz a) (
      Triplet.row_index a) (Triplet.col_index a) (Triplet.value a);

    let opt = Glp_iocp.init () in
    let _ = Glp_iocp.enable_presolver opt in
    let out = intopt mip opt in 
    let s = Array.create m 0 in
    for i= 0 to m-1 do
      s.(i) <- int_of_float( mip_col_val mip (i+1))
    done;
    
    let x = Array.create (m*n) 0 in
    for i=0 to m*n-1 do
      x.(i) <- int_of_float (mip_col_val mip (m + i+1))
    done;

    let cost = mip_obj_val mip in
    (out, s, x,  cost)
  end

let parse_result m n x = 
  let result= Array.create n 0 in
  begin
    for j = 0 to n-1 do
      for i= 0 to m-1 do
        if x.(i*n + j) = 1 then
          result.(j) <- i
        else ()
      done
    done;
    result
  end

let write_out m n s x cost = 
  let filename = "result.dat" in
  let oc = open_out filename in
  let result = parse_result m n x in
   begin
    Printf.fprintf oc "%2.4f %d\n" cost 0; 
    Array.iter (fun x -> Printf.fprintf oc "%d " x) result; 
    close_out oc
  end

let _ = 
 if Array.length Sys.argv > 1 then
    let filename = Filename.concat (Sys.getcwd()) Sys.argv.(1) in
    let (m,n, cap, setup, demand, trans) = process_input filename in
    let cap = Array.of_list cap in
    let setup = Array.of_list setup in
    let demand = Array.of_list demand in
    let trans = Dense.of_list n m trans in
    begin
      let (out, s, x, cost) = solve_warehouse_mip m n cap setup demand trans in
      begin
        write_out m n s x cost;
        Printf.printf "Optimal cost %2.4f \n" cost
      end
    end
