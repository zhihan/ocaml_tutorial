type 'a item = Nil | Item of 'a * 'a item

let copy_list (l: a' item) = 
  let interleave (l: a' item) = 
    match l with
    | 


