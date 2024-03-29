open String
open Filename
open Unix

(* HandBrake setting module *)
module HB = struct
  exception HBException of string

  let get_source_target (fullpath:string) : (string*string) = 
    (* Local utility function *)
    let remove_trailing_slash (p:string) = 
      let n = length p in
      if (sub p (n-2) 1) = dir_sep then 
        sub p 0 (n-1)
      else 
        p 
    in

    let pathname = remove_trailing_slash fullpath in
    if Sys.is_directory pathname then
      (* directory *)
      let source = pathname ^ dir_sep ^ "VIDEO_TS" in
      let filename = lowercase (basename pathname) in
      (source, filename)
    else 
      if Sys.file_exists pathname then
      (* existing file *)
        let filename = lowercase (basename pathname) in
        (pathname, chop_extension filename)
      else
        raise (HBException "File not exists")

  let common_options() = ["-e"; "x264"; "--two-pass"; "-T"]

  let nokia_options () = ["--format mp4"; "-b"; "2000"; "-E"; "copy:ac3"]
 
  let iphone_options () = ["--format mp4"; "-b"; "2000"; "-E"; "faac"]

  let strict_anamorphic () = ["--strict-anamorphic"]
     
end

let substr s pt = 
  let n = length pt in
  if (length s) < n then false
  else 
    begin
      let i = ref 0 in
      let off = ref (-1) in
      begin
	while (!i <= (length s) - n) && (!off < 0) do
	  if (sub s !i n) = pt then
	    off := !i
	  else
	    i := !i + 1
	done;
	!off >= 0 
      end
    end

let main () = 
  if Array.length Sys.argv > 1 then
    (* Parse args *)
    let filename = ref "" in
    let title = ref "1" in 
    let speclist = [
      ("-t", Arg.String (fun s -> title := s), "title")] in
    Arg.parse 
      speclist
      (fun (s:string) -> filename := s)
      "Usage dvd2mp4 file|directory [-t <title>] " ;
      
    let (source,target) = HB.get_source_target !filename in
    let prog = "HandBrakeCLI" in 
    
    let audio_options = if (substr source "VIDEO_TS") then
	HB.nokia_options () 
      else
	HB.iphone_options () in
    let args = ["-i"; source; "-t"; !title] @ (HB.common_options ()) 
      @ audio_options  @  ["-o"; target^".mp4" ] in

    let _ = Sys.command (prog ^ " " ^ (String.concat " " args)) in
    ()
    
  else
    Printf.printf "Usage: dvd2mp4 directory"

let _ = main ()
