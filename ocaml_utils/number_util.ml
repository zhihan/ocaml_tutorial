(** Multiply long integers stored in lists  *)
let rec mult1 (a: int list) (b: int list) (result:int) = 
  match (a,b) with 
    | (ah::at, bh::bt) -> 
      begin
	print_int ah;
	print_string " * " ;
	print_int bh ;
	print_endline "";
	mult1 at bt (ah*bh + result)
      end
    | ([], _) -> result
    | (_, []) -> result

let rec multx (x:int list) (stack:int list) (result:int) =
  match x with
    | xh :: xt -> 
      let a = mult1 x stack 0 in
      multx xt stack (result* 10 + a)
    | [] -> result
  

let rec multy (x:int list) (y:int list) (stack: int list) (result:int) = 
  match y with
    | yh :: yt -> 
      let newstack = yh::stack in
      let a = mult1 x stack 0 in
      multy x yt newstack (result* 10 + a)
    | [] -> 
       multx x stack result

let mult_long (x:int list) (y:int list) = 
  multy x y [] 0
