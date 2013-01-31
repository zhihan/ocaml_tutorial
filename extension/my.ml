(* This module is implemented in the stubbed C library *)
module MyVector = struct
  type t
  external create: unit -> t = 
      "new_vector_stub"
  external delete: t -> unit = 
      "delete_vector_stub"
  external append: t -> float -> unit = 
      "vector_append_stub"
  external get: t -> int -> float = 
      "vector_get_stub"
  external size: t-> int =
      "vector_size_stub"

  (* Get a string for the vector *)
  let to_string (v:t) = 
    let n = size v in
    let a = ref "" in  
    for i= 0 to n-1 do
      if i=0 then
        a :=  "[" ^ string_of_float (get v i)
      else
        a := !a ^ "; " ^ (string_of_float (get v i))
    done ;
    !a ^ "]"

  

end

