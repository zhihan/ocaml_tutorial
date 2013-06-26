(* Simple test for simplex solver *)
open Glpk

open Glp_prob

let test () = 
  let lp = create () in
  begin
    set_name lp "sample";
    set_obj_dir lp Max;
    add_rows lp 3;
    set_row_name lp 1 "p";
    set_row_bnds_upper lp 1 100.0;
    set_row_name lp 2 "q";
    set_row_bnds_upper lp 2 600.0;
    set_row_name lp 3 "r";
    set_row_bnds_upper lp 3 300.0;
    add_cols lp 3;
    set_col_name lp 1 "x1";
    set_col_bnds_lower lp 1 0.0;
    set_obj_coeff lp 1 10.;
    set_col_name lp 2 "x2";
    set_col_bnds_lower lp 2 0.0;
    set_obj_coeff lp 2 6.;
    set_col_name lp 3 "x3";
    set_col_bnds_lower lp 3 0.0;
    set_obj_coeff lp 3 4.;
    let i = [|1;1;1;2;3;2;3;2;3|] in
    let j = [|1;2;3;1;1;2;2;3;3|] in
    let a = [|1.;1.;1.;10.;2.;4.;2.;5.;6.|] in
    load_matrix lp 9 i j a;
    let opt = Glp_smcp.init () in
    Glp_smcp.disable_display opt;
    let out = simplex lp opt in
    Printf.printf "Return code %d\n" out;
    let obj = get_obj_val lp in
    Printf.printf "Value %2.4f\n" obj;
    for i=1 to 3 do
      let x = get_col_prim lp i in
      Printf.printf "%2.4f " x 
    done;
    print_endline ""
  end    

let _ = test () 
