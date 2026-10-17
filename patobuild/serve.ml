open Printf
open Unix

(** Minimal HTTP/1.1 static file server for `patoline --serve`.

    This module intentionally keeps external dependencies to the OCaml
    standard library only.  It serves a single file (or a directory
    listing) over plain HTTP/1.1, which is enough for a local
    documentation server. *)

type request = {
  path : string;
}

let content_type_of_path path =
  if Filename.check_suffix path ".pdf" then "application/pdf"
  else if Filename.check_suffix path ".html" || Filename.check_suffix path ".htm"
  then "text/html; charset=utf-8"
  else if Filename.check_suffix path ".css" then "text/css; charset=utf-8"
  else if Filename.check_suffix path ".js" then "application/javascript"
  else "application/octet-stream"

let parse_request ic =
  let req = ref None in
  try
    while !req = None do
      let line = input_line ic in
      if String.starts_with ~prefix:"GET " line then
        let path =
          let line = String.trim line in
          let parts = String.split_on_char ' ' line in
          match parts with
          | _ :: p :: _ -> p
          | _ -> "/"
        in
        req := Some { path }
    done;
    !req
  with End_of_file -> !req

let serve_directory dir =
  if not (Sys.is_directory dir)
  then Some (Filename.basename dir)
  else
    let files = Sys.readdir dir |> Array.to_list |> List.sort String.compare in
    let rec aux = function
      | [] -> None
      | f :: rest ->
        let full = Filename.concat dir f in
        if Sys.file_exists full && not (Sys.is_directory full)
        then Some f
        else aux rest
    in
    aux files

let respond fd status content_type body =
  let len = String.length body in
  let status_msg = match status with
    | 200 -> "OK"
    | 404 -> "Not Found"
    | 500 -> "Internal Server Error"
    | _ -> string_of_int status
  in
  let headers = sprintf
      "HTTP/1.0 %d %s\r\n\
       Server: patoline/0.1\r\n\
       Content-Type: %s\r\n\
       Content-Length: %d\r\n\
       Connection: close\r\n\
       \r\n"
      status status_msg content_type len
  in
  let hbytes = Bytes.of_string headers in
  let bbytes = Bytes.of_string body in
  let _ = Unix.write fd hbytes 0 (Bytes.length hbytes) in
  if len > 0 then ignore (Unix.write fd bbytes 0 len)

let serve_connection root fd =
  let ic = in_channel_of_descr fd in
  (match parse_request ic with
   | None ->
     close_in ic; respond fd 404 "text/plain" "Not Found"
   | Some req ->
     let path =
       let p = if req.path = "/" then "/index.html" else req.path in
       Filename.concat root p
     in
     close_in ic;
     if Sys.file_exists path && not (Sys.is_directory path)
     then
       let content_type = content_type_of_path path in
       let body =
         let ic = open_in_bin path in
         let len = in_channel_length ic in
         let buf = Bytes.create len in
         really_input ic buf 0 len;
         close_in ic;
         Bytes.to_string buf
       in
       respond fd 200 content_type body
     else if Sys.is_directory path
     then
       (match serve_directory path with
        | Some fname ->
          let resolved = Filename.concat path fname in
          if Sys.file_exists resolved && not (Sys.is_directory resolved)
          then
            let content_type = content_type_of_path resolved in
            let body =
              let ic = open_in_bin resolved in
              let len = in_channel_length ic in
              let buf = Bytes.create len in
              really_input ic buf 0 len;
              close_in ic;
              Bytes.to_string buf
            in
            respond fd 200 content_type body
          else respond fd 404 "text/plain" (sprintf "Not Found: %s\n" req.path)
        | None -> respond fd 404 "text/plain" (sprintf "Not Found: %s\n" req.path))
     else
       respond fd 404 "text/plain" (sprintf "Not Found: %s\n" req.path));
  close fd

let serve ?(port = 8080) ?(root = ".") () =
  let addr = ADDR_INET (inet_addr_any, port) in
  let sock = socket PF_INET SOCK_STREAM 0 in
  try
    setsockopt sock SO_REUSEADDR true;
    bind sock addr;
    listen sock 8;
    Printf.eprintf "[serve] listening on http://localhost:%d/ (root=%s)\n%!" port root;
    while true do
      let (client, _) = accept sock in
      ignore (Thread.create (fun () ->
        try serve_connection root client
        with exn ->
          Printf.eprintf "[serve] connection error: %s\n%!" (Printexc.to_string exn)
      ) ())
    done
  with exn ->
    close sock;
    raise exn

let pdf_root_from_arg = function
  | Some fn -> if Sys.file_exists fn then Filename.dirname fn else "."
  | None -> "."

let default_pdf = function
  | Some fn -> Some fn
  | None ->
    let candidates = [ "patobook.pdf"; "patobook/patobook.pdf"; "index.pdf" ] in
    let rec aux = function
      | [] -> None
      | c :: rest ->
        if Sys.file_exists c then Some c else aux rest
    in
    aux candidates
