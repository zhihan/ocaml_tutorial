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
end
