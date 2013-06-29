open Matrix

let simple_matrix () = 
  let a = Triplet.create 2 3 6 in
  let idx = ref 0 in
  begin
    for i = 1 to 2 do
      for j = 1 to 3 do
        Triplet.set a !idx i j 1.0;
        idx := !idx + 1
      done
    done;
    a
  end  

let test1 () = 
  let a = simple_matrix () in 
  print_endline (Triplet.to_string a)

let test_zip () = 
  let a = [|1;2;3|] in
  let b = [|1;2;3|] in
  let z = zip_array a b in
  Array.iter (fun (x,y) -> Printf.printf "%d %d\n" x y) z

let test_unzip () = 
  let a = [|1;2;3|] in
  let b = [|1;2;3|] in
  let z = zip_array a b in
  let c,d = unzip_array z in
  begin
    Array.iter (fun x -> Printf.printf "%d " x) c;
    Printf.printf "\n";
    Array.iter (fun x -> Printf.printf "%d " x) c;
    Printf.printf "\n"
  end

let test_normalize () = 
  let ai = [|1;1;2;2|] in
  let aj = [|2;1;1;2|] in
  let ar = [|1.;2.;3.;4.|] in
  let mat = Triplet.of_arrays 2 2 ai aj ar in
  begin
    Triplet.normalize mat;
    print_endline (Triplet.to_string mat)
  end
  
let test_horzcat () = 
  let a = simple_matrix () in
  let b = Triplet.horzcat a a in
  print_endline (Triplet.to_string b)

let test_vertcat () = 
  let a = simple_matrix () in
  let b = Triplet.vertcat a a in
  print_endline (Triplet.to_string b)

let _ = 
  begin
    test1 ();
    test_zip ();
    test_unzip ();
    test_normalize ();
    test_horzcat ();
    test_vertcat ()
  end
    
