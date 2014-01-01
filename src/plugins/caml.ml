open Build
open Util2

let lines x=
  let rec lines i0 i l=
    if i>=String.length x then
      if i>i0 then List.rev ((String.sub x i i0)::l) else List.rev l
    else
      if x.[i]='\n' then
        lines (i+1) (i+1) (String.sub x i0 (i-i0) :: l)
      else
        lines i0 (i+1) l
  in
  lines 0 0 []

exception Line

let _=
  Printf.fprintf stderr "caml dynlinked\n";flush stderr;
  Build.macros:=
    StrMap.add "caml" (fun x->
      let l=lines x in
      let buf=Buffer.create 1000 in
      let ch0,ch1=Unix.open_process "ocaml" in

      (* pass all commands to ocaml toplevel *)
      List.iter (fun cmd->
        output_string ch1 cmd;
        let rec needs_eol i=
          if i>=0 then (
            if cmd.[i]='\n' || cmd.[i]='\t' || cmd.[i]=' ' then
              needs_eol (i-1)
            else
              (if cmd.[i]=';' then
                  i>1 && cmd.[i-1]<>';'
               else true)
          ) else true
        in
        if needs_eol (String.length cmd-1) then(
          output_string ch1 ";;\n"
        )
      ) l;
      close_out ch1;

      (* Skip initial ocaml advertising *)
      let was_prompt=ref false in
      (try
         while true do
           let c=input_char ch0 in
           if c='#' then (
             let _=input_char ch0 in
             was_prompt:=true;
             raise Line
           ) else (
             let _=(if c<>'\n' then input_line ch0 else "") in
             was_prompt:=false
           )
         done
       with
           Line->());

      (* For each of the commands, make a paragraph for it, then
         another paragraph for ocaml's answer. *)
      let rec read_all l=match l with
          []->()
        | h::s->
          begin
            Buffer.add_string buf
              (Printf.sprintf
                 "let _=newPar D.structure ~environment:(fun env->verbEnv {env with par_indent=[]}) Complete.normal Patoline_Format.parameters [tT \"# \";tT %S];;\n" h);
            (try
               while true do
                 let c=input_char ch0 in
                 if c='#' then (
                   let _=input_char ch0 in
                   was_prompt:=true;
                   raise Line
                 ) else (
                   was_prompt:=false;
                   let l=input_line ch0 in
                   Buffer.add_string buf
                     (Printf.sprintf
                        "let _=newPar D.structure ~environment:(fun env->verbEnv {env with par_indent=[]}) Complete.normal Patoline_Format.parameters [tT \"%c\";tT %S];;\n" c l)
                 )
               done
             with
                 Line | End_of_file->());
            read_all s
          end
      in
      read_all l;
      Buffer.contents buf
    )
    !Build.macros
