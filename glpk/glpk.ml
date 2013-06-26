module Glp_smcp = struct
  type t
  external init: unit -> t = "glp_smcp_init_stub"
  external disable_display: t -> unit = "glp_smcp_disable_display"
end


module Glp_prob = struct
  type t

  external create: unit -> t = "create_prob_stub"
  external set_name: t -> string -> unit = "set_prob_name_stub"
  external get_name: t -> string = "get_prob_name_stub"
  
  type obj_dir = Min | Max  (* *)      
  external set_obj_min: t->unit = "set_prob_obj_min_stub"
  external set_obj_max: t->unit = "set_prob_obj_max_stub"
  let set_obj_dir (p:t) (o:obj_dir)  =  
    match o with
      | Max -> set_obj_max p 
      | Min -> set_obj_min p
  external get_obj_dir: t -> obj_dir = "get_prob_obj_dir_stub"

  (* Row operations *)
  external add_rows: t -> int -> unit = "add_rows_stub" 
  external set_row_name: t -> int -> string -> unit = "set_row_name_stub"

  (* Row bounds *)
  type row_bnds = Free | Lower | Upper | Double | Fixed

  external set_row_bnds_free: t -> int -> unit = 
    "set_row_bnds_free_stub"
  external set_row_bnds_lower: t -> int -> float-> unit = 
    "set_row_bnds_lower_stub"
  external set_row_bnds_upper: t -> int -> float->unit = 
    "set_row_bnds_upper_stub"
  external set_row_bnds_fixed: t -> int -> float->unit = 
    "set_row_bnds_fixed_stub"
  external set_row_bnds_double: t -> int -> float-> float ->unit = 
    "set_row_bnds_double_stub"
   
  (* Column operations *)
  external add_cols: t -> int -> unit = "add_cols_stub"
  external set_col_name: t -> int -> string -> unit = "set_col_name_stub"
  external get_col_name: t -> int -> string = "get_col_name_stub"

  external set_col_bnds_free: t -> int -> unit = 
    "set_col_bnds_free_stub"
  external set_col_bnds_lower: t -> int -> float-> unit = 
    "set_col_bnds_lower_stub"
  external set_col_bnds_upper: t -> int -> float->unit = 
    "set_col_bnds_upper_stub"
  external set_col_bnds_fixed: t -> int -> float->unit = 
    "set_col_bnds_fixed_stub"
  external set_col_bnds_double: t -> int -> float-> float ->unit = 
    "set_col_bnds_double_stub"
  
  (* Objective coefficients *)
  external set_obj_coeff: t -> int -> float -> unit =
    "set_obj_coeff_stub" 

  external load_matrix: t -> int -> int array -> int array -> float array -> unit =
    "load_matrix_stub"

  (* Mixed-Integer programming support *)
  type kind = Real | Integer | Boolean  
  external set_col_kind: t -> int -> kind -> unit = 
      "glp_set_col_kind_stub"
      

  (* Main drivers *)
  external simplex: t -> Glp_smcp.t -> int = 
    "simplex_stub" 

  (* Results *)
  external get_obj_val: t -> float = "get_obj_val_stub"
  external get_col_prim: t -> int -> float = "glp_get_col_prim_stub"

end

