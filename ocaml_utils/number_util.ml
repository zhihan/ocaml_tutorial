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

(** Multiply numbers except its own position *)
(* Example mult_other [|2;3;1;4|] *)
let mult_other (orig:int array) = 
  let n = Array.length orig in
  let before = Array.make n 1 in
  let after = Array.make n 1 in

  let rec cumprod (x:int array) (i:int) (p:int) = 
    if i < n then
      let prod = p * orig.(i) in
      begin
        x.(i) <- prod;
        cumprod x (i+1) prod
      end
    else ()
  in
  let rec cumprod2 (x:int array) (i:int) (p:int) = 
    if i >=0 then
      let prod = p * orig.(i) in
      begin
        x.(i) <- prod;
        cumprod2 x (i-1) prod
      end
    else ()
  in
  begin
    cumprod before 0 1;
    cumprod2 after (n-1) 1;
    let result = Array.make n 1 in
    begin
      result.(0) <- after.(1); 
      result.(n-1) <- before.(n-2);
      let update (k:int) =
        if k > 0 then
          result.(k) <- before.(k-1) * after.(k+1)
        else ()
      in
      update (n-2) ;
      result
    end
  end
                                            
                 
             
  
