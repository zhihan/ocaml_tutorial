type terminal = { tname: string }
type node = Terminal of terminal | Regular of regular
and regular = {left: node; right: node; name: string}
                                                        
let rec pp a = 
  match a with
    | Terminal t-> print_string(t.tname)
    | Regular r -> 
        begin
          print_string(r.name);
          print_string("<left>");
          pp(r.left);
          print_string("<right>");
          pp(r.right);
          print_string("\n")
        end 

let main() = 
  let a = Terminal {tname="a"} in
  let c = Terminal {tname="c"} in
  let b = Regular {left=a; right=c; name="b"} in
    pp(b);;

main();;
