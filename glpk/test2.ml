(* Simple test for simplex solver *)
open Glpk

open Glp_prob

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
  begin 
    set_name mip "sample mip";
    set_obj_dir mip Min;

    (* Objective *)
    add_cols mip nE;
    for i = 0 to nE -1 do
      set_col_kind mip (i+1) Boolean;
      set_col_name mip (i+1) ("c" ^ (string_of_int i));
      set_obj_coeff mip (i+1) c.(i)
    done;

    (* Tour objective *)
    add_rows mip n ;
    let ai = Array.create (n*(n-1)) 0 in
    let aj = Array.create (n*(n-1)) 0 in
    let a = Array.create (n*(n-1)) 1.0 in
    let off = ref 0 in

    (* *)
    for i = 0 to (n - 1) do
      set_row_name mip (i+1) ("v" ^ (string_of_int i));
      set_row_bnds_fixed mip (i+1) 2.0;
      for j = 0 to n-1 do
        if j != i then
          begin
            ai.(!off) <- i + 1 ;
            aj.(!off) <- (edgeIdx i j n) + 1;
            Printf.printf "offset=%d, i=%d,j=%d\n" 
              (!off)
              (ai.(!off)) (aj.(!off)); 
            off := !off + 1
          end
        else ()
      done;
    done;


    load_matrix mip (n*(n-1)) ai aj a;
    let opt = Glp_iocp.init () in
    let _ = Glp_iocp.enable_presolver opt in
    let out = intopt mip opt in
    Printf.printf "Return code %d\n" out;

    for i= 0 to nE-1 do
      let v = mip_col_val mip (i+1) in
      Printf.printf "x(%d) = %1.1f\n " i v
    done;
    Printf.printf "\n"
  end

let _ = test ()
