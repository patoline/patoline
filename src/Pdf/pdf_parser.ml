(* module IntMap=Map.Make(struct type t=int let compare=compare end) *)
(* module IntSet=Set.Make(struct type t=int let compare=compare end) *)
(* module StrMap=Map.Make(struct type t=string let compare=compare end) *)

open Pdfutil
open Typography.OutputCommon
open Typography.OutputPaper
open Typography.Util

let buf=String.create 10000
let buf_start=ref 1
let buf_end=ref 0
let mmap f i=
  if !buf_start=(-1) || !buf_start>i || !buf_end <=i then (
    let p=max 0 (min i (in_channel_length f - String.length buf)) in
    seek_in f p;
    let read=input f buf 0 (String.length buf) in
    buf_start:=p;buf_end:=p+read;
  );
  buf.[i - !buf_start]

type page_tree=Node of page_tree list | Page of obj

let rec print_tree t=match t with
    Node l->Printf.printf "Node [";List.iter print_tree l;Printf.printf "]\n";
  | Page l->Printf.printf "Page (";Pdfutil.print_obj l;Printf.printf ")"

let parse file=
  let f=open_in_bin file in
  let buf=String.create 80 in
  let is_space x=x='\n' || x=' ' || x='\t' || x='\r' in
  let pos=ref 0 in
  let rec skip_while predicate f i0=
    let rec skip i=
      if i>= in_channel_length f || not (predicate (mmap f i)) then i else skip (i+1)
    in
    skip i0
  in
  let backward_find_string a i0=
    let rec find i j=
      if i<0 then i else
        if j>=String.length a then i else
          if a.[j]=mmap f (i+j) then find i (j+1) else
            find (i-1) 0
    in
    find i0 0
  and forward_find_string a i0=
    let rec find i j=
      if i+j>=in_channel_length f then (-1) else
        if j>=String.length a then i else
          if a.[j]=mmap f (i+j) then find i (j+1) else
            find (i+1) 0
    in
    find i0 0
  in
  let read_int pos0=
    let rec read i x=
      let c=mmap f i in
      if c>='0' && c<='9' then
        read (i+1) (x*10 + (int_of_char c-int_of_char '0'))
      else
        if i>pos0 then (
          pos:=i;
          x
        ) else (
          Printf.fprintf stderr "'%c%c%c'\n" (mmap f pos0) (mmap f (pos0+1)) (mmap f (pos0+2));
          flush stderr;
          failwith "read_int : not an int"
        )
    in
    read pos0 0
  in
  let rec find_xref f pos=
    if pos<=0 then failwith "startxref not found" else (
      let pos1=backward_find_string "startxref" (in_channel_length f-9) in
      if pos1>=0 then (
        let pos2=pos1+9 in
        let pos3=skip_while is_space f pos2 in
        read_int pos3
      ) else
        find_xref f (pos-String.length buf)
    )
  in
  let xref_pos=find_xref f (in_channel_length f) in
  let pos0=skip_while is_space f (xref_pos+4) in
  let _=read_int pos0 in

  let pos0=skip_while is_space f !pos in
  let num_objs=read_int pos0 in
  (* Printf.fprintf stderr "xref : %d %d\n" vers num_objs;flush stderr; *)

  let pos0=skip_while (fun c->c<>'\n') f !pos in
  let _=read_int (pos0+1) in            (* 0000…000 *)
  let _=read_int (!pos+1) in            (* 65535 *)
  let pos0=skip_while (fun c->c<>'\n') f !pos in

  let rec parse_xref pos0 i xref=
    if i>=num_objs then (
      pos:=pos0;
      xref
    ) else (
      let off=read_int pos0 in
      let _=read_int (!pos+1) in
      (* Printf.fprintf stderr "%d %d\n" off ver;flush stderr; *)
      let pos1=skip_while (fun c->c<>'\n') f !pos in
      parse_xref (1+pos1) (i+1) (IntMap.add i off xref)
    )
  in
  let xref=parse_xref (1+pos0) 1 IntMap.empty in
  (* IntMap.iter (fun k a->Printf.fprintf stderr "xref : %d %d\n" k a;flush stderr) xref; *)
  (* Normalement, il est écrit "trailer" ici *)
  for i=0 to 6 do
    if mmap f (!pos+i) <> "trailer".[i] then failwith "keyword trailer expected"
  done;
  let trailer_dict=
    seek_in f (!pos+7);
    let lexbuf = Lexing.from_channel f in
    match Obj_parser.main Obj_lexer.token lexbuf with
        Dict x->x
      | _->failwith "trailer dictionary not a dictionary"
  in
  let root=try StrMap.find "/Root" trailer_dict with Not_found->failwith "key /Root not found in trailer dict" in

  let rec resolve_object x s=
    match x with
        Indirect (i,_)->
          if IntSet.mem i s then failwith "cyclic pdf" else resolve_ref i (IntSet.add i s)
      | y->y
  and resolve_ref x s=
    let off=IntMap.find x xref in
    let off'=forward_find_string "obj" off in
    seek_in f (off'+4);
    let lexbuf = Lexing.from_channel f in
    resolve_object (Obj_parser.main Obj_lexer.token lexbuf) s
  in
  let pages_obj=
    StrMap.find "/Pages"
      (match resolve_object root IntSet.empty with
          Dict x->x
        | _->failwith "root not a dictionary")
  in
  let n_pages=ref 0 in
  let rec make_pages obj=
    let obj_resolved=resolve_object obj IntSet.empty in
    match obj_resolved with
        Dict x->(
          try
            match StrMap.find "/Type" x with
                Name "/Pages"->(
                  match StrMap.find "/Kids" x with
                      Array a->
                        Node (List.map make_pages a)
                    | _->failwith "/Kids not an array"
                )
              | Name "/Page"->(
                incr n_pages;
                Page obj_resolved
              )
              | _->failwith "unknown object type"
          with
            Not_found->failwith "/Type not found"
        )
      | _->failwith "/Pages not a dictionary"
  in
  let pages=make_pages pages_obj in
  let pages_arr=Array.make !n_pages { pageFormat=(0.,0.); pageContents=[] } in

  let parse_number i=
    let int=read_int i in
    if mmap f !pos='.' then (
      let pos0= !pos+1 in
      let float=read_int pos0 in
      let rec pow x i y=if i=0 then y else pow x (i-1) (y*.x) in
      let num=(float_of_int int)+.(float_of_int float)/.(pow 10. (!pos-pos0) 1.) in
      num
    ) else float_of_int int
  in
  let parse_page cont x0 y0=match cont with
      Indirect (i,_)->(
        let obj=
          let off=IntMap.find i xref in
          let off'=forward_find_string "obj" off in
          seek_in f (off'+4);
          let lexbuf = Lexing.from_channel f in
          pos:=lexbuf.Lexing.lex_abs_pos;
          Obj_parser.main Obj_lexer.token lexbuf
        in
        let pos0= !pos in
        let dict=match resolve_ref i IntSet.empty with
            Dict x->x
          | _->failwith "stream dictionary not a dictionary"
        in
        let pos0= !pos in
        let len=
          match resolve_object (StrMap.find "/Length" dict) IntSet.empty with
              Number x->x
            | _->failwith "stream length not a number"
        in
        let str=forward_find_string "stream" pos0 in
        let pos_stream=skip_while is_space f (str+7) in
        let curx=ref 0.
        and cury=ref 0.
        and curw=ref 1.
        and cur_fill=ref black
        and cur_stroke=ref black
        and cur_path=ref []
        and cur_paths=ref []
        and contents=ref []
        in

        let rec make_next_part i stack=
          (
          let pos0=skip_while is_space f i in
          if pos0<pos_stream+int_of_float len then (
            let c=mmap f pos0 in
            if (c>='0' && c<='9') || c='.' then
              let n=parse_number pos0 in
              (* Printf.fprintf stderr "%g\n" n; *)
              make_next_part (skip_while is_space f !pos) (n::stack)
            else (
              let b=Buffer.create 10 in
              let rec make_op j=
                let cc=mmap f j in
                if is_space cc then (
                  pos:=j;
                  Buffer.contents b
                ) else (
                  Buffer.add_char b cc;
                  make_op (j+1)
                )
              in
              let op=make_op pos0 in
              (* Printf.fprintf stderr "operator %S\n" op;flush stderr; *)
              (match op with
                  "RG"->(match stack with
                      b::g::r::_->cur_stroke:=Typography.OutputCommon.rgb r g b
                    | _->failwith "not enough operands for operator RG"
                  )
                | "rg"->(match stack with
                    b::g::r::_->cur_fill:=Typography.OutputCommon.rgb r g b
                    | _->failwith "not enough operands for operator RG"
                )
                | "w"->(match stack with
                    w::_ -> curw:=w
                    | _->failwith "not enough operands for operator w")
                | "f"->(
                  cur_paths:=(!cur_path)::(!cur_paths);
                  cur_path:=[];
                  contents:=
                    Path ({ default with strokingColor=None;fillColor=Some !cur_fill },
                          List.map (fun x->Array.of_list (List.rev x)) !cur_paths)::(!contents);
                  cur_path:=[]
                )
                | "m"->(match stack with
                    y::x::_ -> (
                      cur_paths:=(!cur_path)::(!cur_paths);
                      cur_path:=[];
                      curx:=x;cury:=y
                    )
                    | _->failwith "not enough operands for operator m")
                | "l"->(match stack with
                    y::x::_ -> (
                      cur_path:=([|mm_of_pt (!curx-.x0);
                                   mm_of_pt (x-.x0)|],
                                 [|mm_of_pt (!cury-.y0);
                                   mm_of_pt (y-.y0)|])::(!cur_path);
                      curx:=x;cury:=y
                    )
                    | _->failwith "not enough operands for operator l")
                | "c"->(match stack with
                    y3::x3::y2::x2::y1::x1::_ -> (
                      cur_path:=([|mm_of_pt (!curx-.x0);
                                   mm_of_pt (x1-.x0);
                                   mm_of_pt (x2-.x0);
                                   mm_of_pt (x3-.x0)|],
                                 [|mm_of_pt (!cury-.y0);
                                   mm_of_pt (y1-.y0);
                                   mm_of_pt (y2-.y0);
                                   mm_of_pt (y3-.y0)|])::(!cur_path);
                      curx:=x3;cury:=y3
                    )
                    | _->failwith "not enough operands for operator l")
                | op->(
                  Printf.fprintf stderr "unsupported operator %S\n" op
                )
              );
              make_next_part !pos []
            )
          ))
        in
        make_next_part pos_stream [];
        !contents
      )
    | _->failwith "page not an indirect reference"
  in
  let rec make_pages i t=match t with
      Page x->(
        match x with
            Dict y->(
              try
                let cont=StrMap.find "/Contents" y in
                match StrMap.find "/MediaBox" y with
                    Array (h0::h1::h2::h3::_)->(
                      let h0=match resolve_object h0 IntSet.empty with
                          Number n->n
                        | _->failwith "invalid mediabox"
                      in
                      let h1=match resolve_object h1 IntSet.empty with
                          Number n->n
                        | _->failwith "invalid mediabox"
                      in
                      let h2=match resolve_object h2 IntSet.empty with
                          Number n->n
                        | _->failwith "invalid mediabox"
                      in
                      let h3=match resolve_object h3 IntSet.empty with
                          Number n->n
                        | _->failwith "invalid mediabox"
                      in
                      let w=h2-.h0 in
                      let h=h3-.h1 in
                      pages_arr.(i)<-{pageFormat=(mm_of_pt w,mm_of_pt h);
                                      pageContents=parse_page cont h0 h1};
                      (i+1)
                    )
                  | _->failwith "invalid /MediaBox"
              with
                  Not_found->failwith "/Contents or /MediaBox not found in page"
            )
          | _->failwith "page not a dictionary"
      )
    | Node n->List.fold_left make_pages i n
  in
  let _=make_pages 0 pages in
  pages_arr
