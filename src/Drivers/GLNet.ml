(*
  Copyright Florian Hatat, Tom Hirschowitz, Pierre Hyvernat,
  Pierre-Etienne Meunier, Christophe Raffalli, Guillaume Theyssier 2012.

  This file is part of Patoline.

  Patoline is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Patoline is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Patoline.  If not, see <http://www.gnu.org/licenses/>.
*)
open Netcgi
open Nethttpd_reactor
open Printf

type cmd = Next_page | Next_state | Prev_page | Prev_state | Goto of int * int | Refresh

let opened_fd = ref []
let connection_id = ref 0
 
let rec service master_sock netcgi_processor =
  try
  let (infd,_,errfd) =
    let fds = List.map fst !opened_fd in
    Unix.select (master_sock::fds) [] fds 0.0 
  in
  if errfd <> [] then
    opened_fd := List.filter (fun (fd,(id,reactor)) -> 
      if List.mem fd errfd then (
	Printf.fprintf stderr "Closing connection %d\n" id;
	flush stderr;
	false)
      else
	true) !opened_fd
  else begin
    let pending_connection = List.mem master_sock infd in
    let infd = List.filter (fun fd -> fd <> master_sock) infd in
    match infd with
    [] ->
      if pending_connection then begin
	let cid = !connection_id in
	incr connection_id;
	Printf.fprintf stderr "Accepting connection %d\n" cid;
	flush stderr;
	let conn_sock, _ = Unix.accept master_sock in
	Unix.set_nonblock conn_sock;
	let config = new modify_http_reactor_config
	  ~modify_http_processor_config:(
	    new modify_http_processor_config
	      ~config_timeout:0.25
	      ~config_timeout_next_request:0.0)
	  Nethttpd_reactor.default_http_reactor_config
	in
	let reactor = new http_reactor config conn_sock in
	opened_fd := (conn_sock,(cid,reactor))::!opened_fd;  
      end
  | fd::_ ->
    let cid, reactor = List.assoc fd !opened_fd in
    Printf.fprintf stderr "Request on connection %d\n" cid;
    flush stderr;
    match reactor # next_request () with
    | Some req ->
      ( try
	  req # accept_body();   (* Always! *)
	  let env =
	    req # environment in
	  let cgi = 
	    Netcgi_common.cgi_with_args 
	      (new Netcgi_common.cgi)
	      (env :> Netcgi.cgi_environment)
	      Netcgi.buffered_transactional_outtype
	      env#input_channel
	      (fun _ _ _ -> `Automatic) in
	  netcgi_processor cgi
	with
	  e ->
	    Printf.fprintf stderr "Uncaught exception in request: %s\n" (Printexc.to_string e);
	    flush stderr;
	    if e = Sys.Break then raise e
      );
      req # finish();
      
    | None ->
      opened_fd := List.filter (fun (fd', _) -> fd <> fd') !opened_fd;
      reactor # close ();

  end
  with
    e ->
      Printf.fprintf stderr "Uncaught exception in treatment: %s\n" (Printexc.to_string e);
      flush stderr;
      if e = Sys.Break then raise e



let text = Netencoding.Html.encode_from_latin1
(* This function encodes "<", ">", "&", double quotes, and Latin 1 characters 
 * as character entities. E.g. text "<" = "&lt;", and text "ä" = "&auml;"
 *)

let make_content = ref (fun out id cmd fmt width height format -> (assert false:unit))

let fetch_file cgi name =
  let out = cgi # output # output_string in
  let ic = open_in name in
  let n = in_channel_length ic in
  let s = String.create n in
  really_input ic s 0 n;
  close_in ic;
  out s    

let make_page cgi id cmd fmt width height format=
  (* Output the beginning of the page with the passed [title]. *)
  let out = cgi # output # output_string in
  out "<html>";
  out "<head>";
(*  out "<link rel=\"stylesheet\" type=\"text/css\" href=\"/themes/alpinux/handheld.css\" media=\"screen and (max-device-width: 480px)\" />";
  out "<link rel=\"stylesheet\" type=\"text/css\" href=\"/themes/alpinux/handheld.css\" media=\"handheld\" />"*)
  out "<meta name=\"viewport\" content=\"width=device-width, height=device-height\"/>";
  out "</head>";

  out "<body>";
  !make_content out id cmd fmt width height format;
  out "</body>";
  out "</html>"


let generate_page (cgi : cgi_activation) =
  (* Check which page is to be displayed. This is contained in the CGI
   * argument "page".
   *)
  let url = cgi # url () in
  Printf.fprintf stderr "Url is %s\n" url;
  let id = 
    try
      Str.search_forward (Str.regexp "/id\\([0-9]+\\)$") url 0;
      int_of_string (Str.matched_group 1 url)
    with Not_found -> 0
  in
  Printf.fprintf stderr "id = %d\n" id;
  List.iter (fun arg ->
    Printf.fprintf stderr "%s = %s  " (arg # name) (arg # value))
    cgi # arguments;
  flush stderr;
  
  try
    let filename = (cgi # argument "file") # value in
    fetch_file cgi filename
  with
    Not_found ->
  let page = 
    try Some (int_of_string ((cgi # argument "page") # value))
    with Not_found | Failure _ -> None
  in
  let state = 
    try Some (int_of_string ((cgi # argument "state") # value))
    with Not_found | Failure _ -> None
  in
  let next =
    try Some ((cgi # argument "next") # value)
    with Not_found -> None
  in
  let prev =
    try Some ((cgi # argument "prev") # value)
    with Not_found -> None
  in
  let cmd = match page, state, next, prev with
    | Some n, Some p, _, _ -> Goto(n,p)
    | Some n, None, _, _ -> Goto(n,0)
    | _, _, Some _, _ -> Next_state
    | _, _, _, Some _ -> Prev_state
    | _, _, _, _ -> Refresh
  in
  let fmt =
    try Some ((cgi # argument "fmt") # value)
    with Not_found -> None
  in
  let width =
    try Some (int_of_string ((cgi # argument "width") # value))
    with Not_found  | Failure _ -> None
  in
  let height =
    try Some (int_of_string ((cgi # argument "height") # value))
    with Not_found  | Failure _ -> None
  in
  let format =
    try Some ((cgi # argument "format") # value)
    with Not_found  | Failure _ -> None
  in
 

  make_page cgi id cmd fmt width height format



let process (cgi : cgi_activation) =
  (* A [cgi_activation] is an object that allows us to program pages
   * in a quite abstract way. By creating the [std_activation] object
   * the CGI/1.1 protocol is used to communicate with the outer world.
   * The CGI arguments are read in, and further properties of the protocol
   * are available by method calls.
   *
   * The parameter [~operating_type] specifies that the generated HTML
   * page is buffered, and sent to the browser when it is complete. This
   * has the advantage that you can catch errors while the page is generated,
   * and can output error messages. Other [~operating_type]s make it
   * possible that the HTML page is buffered in a temporary file, and it
   * can also be specified that the HTML page is not buffered at all.
   *)
  
  (* The [try] block catches errors during the page generation. *)
  try
    (* Set the header. The header specifies that the page must not be
     * cached. This is important for dynamic pages called by the GET
     * method, otherwise the browser might display an old version of
     * the page.
     * Furthermore, we set the content type and the character set.
     * Note that the header is not sent immediately to the browser because
     * we have enabled HTML buffering.
     *)
    cgi # set_header 
      ~cache:`No_cache 
      ~content_type:"text/html; charset=\"iso-8859-1\""
      ();

    generate_page cgi;

    (* After the page has been fully generated, we can send it to the
     * browser. 
     *)
    cgi # output # commit_work();
  with
      error -> ()
(*
	(* An error has happened. Generate now an error page instead of
	 * the current page. By rolling back the output buffer, any 
	 * uncomitted material is deleted.
	 *)
	cgi # output # rollback_work();

	(* We change the header here only to demonstrate that this is
	 * possible.
	 *)
	cgi # set_header 

	  ~status:`Forbidden                  (* Indicate the error *)
	  ~cache:`No_cache 
	  ~content_type:"text/html; charset=\"iso-8859-1\""
	  ();

	begin_page cgi "Software error";
        cgi # output # output_string "While processing the request an O'Caml exception has been raised:<BR>";
        cgi # output # output_string ("<TT>" ^ text(Printexc.to_string error) ^ "</TT><BR>");
	end_page cgi;

	(* Now commit the error page: *)
	cgi # output # commit_work()
*)


let conf_debug() =
  (* Set the environment variable DEBUG to either:
       - a list of Netlog module names
       - the keyword "ALL" to output all messages
       - the keyword "LIST" to output a list of modules
     By setting DEBUG_WIN32 additional debugging for Win32 is enabled.
   *)
  let debug = try Sys.getenv "DEBUG" with Not_found -> "" in
  if debug = "ALL" then
    Netlog.Debug.enable_all()
  else if debug = "LIST" then (
    List.iter print_endline (Netlog.Debug.names());
    exit 0
  )
  else (
    let l = Netstring_str.split (Netstring_str.regexp "[ \t\r\n]+") debug in
    List.iter
      (fun m -> Netlog.Debug.enable_module m)
      l
  );
  if (try ignore(Sys.getenv "DEBUG_WIN32"); true with Not_found -> false) then
    Netsys_win32.Debug.debug_c_wrapper true


(* main: *)

let _ = conf_debug()

let handle_one port=
  let master_sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt master_sock Unix.SO_REUSEADDR true;
  Unix.bind master_sock (Unix.ADDR_INET(Unix.inet_addr_any, port));
  Unix.listen master_sock 100;
  Printf.fprintf stderr "Listening on port %d\n" port;
  flush stderr;
  (fun () -> service master_sock process)


