(* Use Str and Unix library *)

(** *)
module FilenameUtil = struct
  let is_unix_hidden (name:string):bool = 
    let bname = Filename.basename name in
    if (String.length bname ) > 0 then
      bname.[0] = '.' 
    else
      false

  let has_extension (name:string) = 
    (String.contains name '.') && ((String.length name) > 1)

  let is_substr sub str : bool = 
    let re = Str.regexp_string sub in
    try
      let _ = Str.search_forward re str 0 in
      true
    with Not_found -> false
      
  let split_ext (name:string) : string*string = 
    let base = Filename.chop_extension name in
    let m = String.length base in
    let n = String.length name in
    let ext = String.sub name (m+1) (n-m-1) in
    (base, ext)

  let is_matlab_dump (fname:string):bool = 
    let bname = Filename.basename fname in
    is_substr "matlab_crash_dump" bname

  let is_desktop_ini (fname:string):bool =
    let bname = Filename.basename fname in
    bname = "desktop.ini"

  let is_latex_temp (fname:string):bool =
    let bname = Filename.basename fname in
    (Filename.check_suffix bname ".aux") ||
      (Filename.check_suffix bname ".blg") ||
      (Filename.check_suffix bname ".bak") ||
      (Filename.check_suffix bname ".bbl") ||
      (Filename.check_suffix bname ".log") ||
      (Filename.check_suffix bname ".dvi") 

  let is_emacs_temp (ext:string):bool = 
    ((String.length ext) > 0) && (ext.[(String.length ext) -1] = '~')

  let is_temp_file (fname:string):bool =
    let bname = Filename.basename fname in
    if has_extension bname then
      let (name, ext) = split_ext bname in
      match (String.lowercase ext) with
        | "asv" -> true
        | "autosave" -> true
        | _ -> (is_matlab_dump bname) || 
          (is_desktop_ini bname) ||
          (is_latex_temp bname) ||
          (is_emacs_temp ext)
    else
      (* Can't tell from extension *)
      false
    
  let is_ocamlbuild_dir (name:string):bool=
    let bname = Filename.basename name in
    bname = "_build"
 
  let is_sldv_output_dir (fname:string): bool =
    let bname = Filename.basename fname in
    bname = "sldv_output"

  let is_slprj_dir (fname:string): bool =
    let bname = Filename.basename fname in
    bname = "slprj"

  let is_temp_dir (fname:string): bool = 
    (is_ocamlbuild_dir fname) || (is_sldv_output_dir fname) || 
      (is_slprj_dir fname)

end

open FilenameUtil
open Unix

let is_symbolic_link (fullname:string):bool = 
  let st = lstat fullname in
  st.st_kind  = S_LNK


let rec delete_temp_files ?force:(all=false) (dir:string) : unit =
  let files = Sys.readdir dir in
  let process_file f : unit = 
    let fullfile = Filename.concat dir f in
    if (not (is_symbolic_link fullfile)) && 
      (not (is_unix_hidden f)) && 
      (Sys.is_directory fullfile ) then
      if (is_temp_dir fullfile) then
        delete_temp_files ~force:true fullfile
      else
        delete_temp_files fullfile
    else 
      if (not (is_symbolic_link fullfile)) then
        if (is_temp_file fullfile) || all then
          Sys.remove fullfile
      else ()
  in
  begin
    Array.iter process_file files;
    let remains = Sys.readdir dir in
    if not (is_symbolic_link dir) then
      if (Array.length remains) = 0 then
        Unix.rmdir dir
      else
        ()
    else ()
  end
    
let _ = 
  let d = Sys.getcwd () in
  delete_temp_files d


