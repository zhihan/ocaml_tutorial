open My
let main () = 
  let a = MyVector.create () in
  begin
    MyVector.append a 1.0;
    MyVector.append a 2.0;
    
    Printf.printf "a[0] = %f\n" (MyVector.get a 0);
    Printf.printf "a[1] = %f\n" (MyVector.get a 1);
  end 
    

let _ = main ()

