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
open Patoraw
open Patfonts
open Patutil
open Unicodelib
open Fonts
open Printf
open Extra
open FTypes
open RawContent
open Color
open Driver

let pt_of_mm = Util.pt_of_mm

let pdf_type3_only = ref false

let driver_options =
  Arg.[("--type3-only", Set pdf_type3_only, "Use Type3 fonts only.")]

let filter_options argv =
  let is_ours flag = List.exists (fun (f, _, _) -> f = flag) driver_options in
  let (ours, theirs) = List.partition is_ours (Array.to_list argv) in
  let (ours, theirs) = (Array.of_list ours, Array.of_list theirs) in
  let current = ref 0 in
  let anon _ = assert false in
  Arg.parse_argv ~current ours driver_options anon "PDF driver options:";
  theirs

module FloatMap = Map.Make(
  struct
    type t = float
    let compare = compare
  end)

type pdfFont =
  {         font          : Fonts.font
  ; mutable fontVariants  : Fonts.glyph IntMap.t IntMap.t
  ; mutable pdfObjects    : int IntMap.t
  ; mutable fontGlyphs    : (int * int * Fonts.glyph) IntMap.t
  ; mutable revFontGlyphs : Fonts.glyph IntMap.t }

type state_data =
  { strokingColor      : color option
  ; nonStrokingColor   : color option
  ; strokingOpacity    : int
  ; nonstrokingOpacity : int
  ; lineWidth          : float
  ; lineJoin           : lineJoin
  ; lineCap            : lineCap
  ; dashPattern        : float list }

let initial_state =
  { strokingColor      = None
  ; nonStrokingColor   = None
  ; strokingOpacity    = 0
  ; nonstrokingOpacity = 0
  ; lineWidth          = 1.0
  ; lineJoin           = Miter_join
  ; lineCap            = Butt_cap
  ; dashPattern        = [] }

type state = state_data ref

let stream buf=
  let out_buf = Buffer.create 100000 in
  let buf_pos = ref 0 in
  Zlib.compress (fun zbuf ->
    let m = min (Buffer.length buf - !buf_pos) (Bytes.length zbuf) in
    for i = 0 to m-1 do
      Bytes.set zbuf i (Buffer.nth buf (!buf_pos));
      incr buf_pos
    done; m
  )
    (fun buf len -> Buffer.add_subbytes out_buf buf 0 len);
  "/Filter [/FlateDecode]", out_buf


let output ?(structure:structure=empty_structure) pages fname =
  let pages = if pages <> [||] then pages else [| empty_page (0.0, 0.0) |] in
  let fname = (try Filename.chop_extension fname with _ -> fname) ^ ".pdf" in
  let oc = open_out_bin fname in

  let pageBuf=Buffer.create 100000 in
  let xref=ref (IntMap.singleton 1 0) in (* Le pagetree est toujours l'objet 1 *)
  (* let fonts=ref StrMap.empty in *)
  let resumeObject n=
    flush oc;
    xref:=IntMap.add n (pos_out oc) !xref;
    fprintf oc "%d 0 obj\n" n
  in
  let beginObject ()=
    let n=IntMap.cardinal !xref in
      resumeObject (n+1);
      n+1
  in
  let futureObject ()=
    let n=IntMap.cardinal !xref in
      xref:=IntMap.add (n+1) (-1) !xref;
      n+1
  in
  let endObject _ = fprintf oc "\nendobj\n" in
  let pdf_string_buf=Buffer.create 1000 in
  let pdf_string utf=
    Buffer.clear pdf_string_buf;
    let rec fill i=
      try
        let code= (UTF8.look utf i) in
        (if UChar.is_space code then
            Buffer.add_string pdf_string_buf "20"
         else if UChar.code code<=0xff then
           Buffer.add_string pdf_string_buf (sprintf "%02x" (UChar.code code)));
        fill (UTF8.next utf i)
      with
          _->()
    in
    fill 0;
    Buffer.contents pdf_string_buf
  in
  let pageObjects=Array.make (Array.length pages) 0 in
  for i=0 to Array.length pageObjects-1 do pageObjects.(i)<-futureObject ()
  done;
  fprintf oc "%%PDF-1.7\n%%ãõẽũ\n";


  let rec are_valid x i=
    if i>=Array.length x then true else
      if x.(i) < infinity && x.(i)> -.infinity then are_valid x (i+1) else false
  in
  let writePath buf pt_of_mm paths params =
    List.iter (fun path->
      if Array.length path > 0 then (
        let (x0,y0)=path.(0) in
        if are_valid x0 0 && are_valid y0 0 then (
          Buffer.add_string buf (sprintf "%f %f m " (pt_of_mm x0.(0)) (pt_of_mm y0.(0)));
          Array.iter (
            fun (x,y)->if are_valid x 0 && are_valid y 0 then (
              if Array.length x<=2 && Array.length y<=2 then (
                let x1=if Array.length x=2 then x.(1) else x.(0) in
                let y1=if Array.length y=2 then y.(1) else y.(0) in
                Buffer.add_string buf (sprintf "%f %f l " (pt_of_mm x1) (pt_of_mm y1));
              ) else if Array.length x=3 && Array.length y=3 then (
                Buffer.add_string buf (sprintf "%f %f %f %f %f %f c "
                                          (pt_of_mm ((x.(0)+.2.*.x.(1))/.3.)) (pt_of_mm ((y.(0)+.2.*.y.(1))/.3.))
                                          (pt_of_mm ((2.*.x.(1)+.x.(2))/.3.)) (pt_of_mm ((2.*.y.(1)+.y.(2))/.3.))
                                          (pt_of_mm x.(2)) (pt_of_mm y.(2)));
              ) else if Array.length x=4 && Array.length y=4 then (
                Buffer.add_string buf (sprintf "%f %f %f %f %f %f c "
                                          (pt_of_mm x.(1)) (pt_of_mm y.(1))
                                          (pt_of_mm x.(2)) (pt_of_mm y.(2))
                                          (pt_of_mm x.(3)) (pt_of_mm y.(3)));
              )
            )
          ) path
        ))
    ) paths;
    match RawContent.(params.fillColor, params.strokingColor) with
    | None  , None                     -> Buffer.add_string pageBuf "n "
    | None  , Some _ when params.close -> Buffer.add_string pageBuf "s "
    | None  , Some _                   -> Buffer.add_string pageBuf "S "
    | Some _, None                     -> Buffer.add_string pageBuf "f "
    | Some _, Some _ when params.close -> Buffer.add_string pageBuf "b "
    | Some _, Some _                   -> Buffer.add_string pageBuf "B "
  in

  let fonts=ref StrMap.empty in

  let glyphEncoding gl=
    let f=Fonts.glyphFont gl in
    let num=Fonts.glyphNumber gl in
    let name=Fonts.uniqueName f in
    let pdffont=try StrMap.find name !fonts with Not_found->(
      let ff={ font=f;fontGlyphs=IntMap.empty;revFontGlyphs=IntMap.empty;
               fontVariants=IntMap.empty;
               pdfObjects=IntMap.empty
             }
      in
      fonts:=StrMap.add name ff !fonts;
      ff
    )
    in
    try
      let (a,variant,_)=IntMap.find num.glyph_index pdffont.fontGlyphs in
      (a,IntMap.find variant pdffont.pdfObjects)
    with
        Not_found->(
          let glyphutf=(Fonts.glyphNumber gl).glyph_utf8 in
          let glyphutf=if String.length glyphutf>0 then glyphutf else " " in
          let enc,variant,variants=
            let naturalEnc=
              if UTF8.next glyphutf 0 >=String.length glyphutf then (
                let x=UChar.code (UTF8.look glyphutf 0) in
                x<=0xff && (
                  try
                    let m=IntMap.find 0 pdffont.fontVariants in
                    let gl'=IntMap.find x m in
                    (Fonts.glyphNumber gl').glyph_index=num.glyph_index
                  with
                      Not_found->true
                )
              ) else false
            in
            if naturalEnc then (
              let x=UChar.code (UTF8.look glyphutf 0) in
              let m=try IntMap.find 0 pdffont.fontVariants with Not_found->IntMap.empty in
              if not (IntMap.mem 0 pdffont.pdfObjects) then (
                let obj=futureObject () in
                pdffont.pdfObjects<-IntMap.add 0 obj pdffont.pdfObjects;
              );
              x,0,IntMap.add 0 (IntMap.add x gl m) pdffont.fontVariants
            ) else (
              if IntMap.is_empty pdffont.fontVariants then (
                let obj=futureObject () in
                pdffont.pdfObjects<-IntMap.add 1 obj pdffont.pdfObjects;
                1,1,IntMap.singleton 1 (IntMap.singleton 1 gl)
              )  else (
                let last,mvalue=IntMap.max_binding pdffont.fontVariants in
                let value,_=IntMap.max_binding mvalue in
                if value>=0xff then (
                  let obj=futureObject () in
                  pdffont.pdfObjects<-IntMap.add (last+1) obj pdffont.pdfObjects;
                  1,(last+1),IntMap.add (last+1) (IntMap.singleton 1 gl) pdffont.fontVariants
                ) else (
                  (value+1),last,IntMap.add last (IntMap.add (value+1) gl mvalue) pdffont.fontVariants
                )
              )
            )
          in
          pdffont.fontVariants<-variants;
          pdffont.fontGlyphs<-IntMap.add num.glyph_index (enc,variant,gl) pdffont.fontGlyphs;
          (enc,IntMap.find variant pdffont.pdfObjects)
        )
  in
  for page=0 to Array.length pages-1 do
    Buffer.reset pageBuf;
    let pageLinks=ref [] in
    let pageImages=ref [] in
    let pageFonts=ref IntMap.empty in
    let currentFont=ref (-1) in
    let currentSize=ref (-1.) in
    (* Texte *)
    let isText=ref false in
    let openedWord=ref false in
    let openedLine=ref false in
    let xt=ref 0. in
    let yt=ref 0. in
    let xline=ref 0. in

    (* Dessins *)
    let state = ref initial_state in
    let opacitiesCounter=ref 0 in
    let sopacities=ref (FloatMap.singleton 1. 0) in
    let nsopacities=ref (FloatMap.singleton 1. 0) in

    let close_line ()=
      if !openedWord then (Buffer.add_string pageBuf ")"; openedWord:=false);
      if !openedLine then (Buffer.add_string pageBuf " ] TJ ";
                           openedLine:=false; xline:=0.);
    in
    let close_text ()=
      close_line ();
      if !isText then (
        currentFont:=(-1);
        currentSize:=(-.infinity);
        state := {!state with nonStrokingColor = None; strokingColor = None};
        Buffer.add_string pageBuf " ET "; isText:=false
      );
      xt:=0.; yt:=0.
    in
    let change_stroking_color col =
      if (Some col)<> (!state).strokingColor then (
        close_text();
        let (cr,cg,cb,ca) = to_rgba col in
        close_text ();
        let r=max 0. (min 1. cr) in
        let g=max 0. (min 1. cg) in
        let b=max 0. (min 1. cb) in
        let alpha=try FloatMap.find ca !sopacities with
            Not_found->(
              incr opacitiesCounter;
              sopacities:=FloatMap.add ca !opacitiesCounter !sopacities;
              !opacitiesCounter
            )
        in
        if alpha <> (!state).strokingOpacity then (
          state := {!state with strokingOpacity = alpha};
          Buffer.add_string pageBuf (sprintf "/GS%d gs " alpha);
        );
        state := {!state with strokingColor = Some col};
        Buffer.add_string pageBuf (sprintf "%f %f %f RG " r g b);
      )
    in
    let change_non_stroking_color col =
      if Some col<> (!state).nonStrokingColor then (
        close_text();
        let (cr,cg,cb,ca) = to_rgba col in
        close_text ();
        let r=max 0. (min 1. cr) in
        let g=max 0. (min 1. cg) in
        let b=max 0. (min 1. cb) in
        let alpha=try FloatMap.find ca !nsopacities with
            Not_found->(
              incr opacitiesCounter;
              nsopacities:=FloatMap.add ca !opacitiesCounter !nsopacities;
              !opacitiesCounter
            )
        in
        if alpha <> (!state).nonstrokingOpacity then (
          state := {!state with nonstrokingOpacity = alpha};
          Buffer.add_string pageBuf (sprintf "/GS%d gs " alpha);
        );
        state := {!state with nonStrokingColor = Some col};
        Buffer.add_string pageBuf (sprintf "%f %f %f rg " r g b);
      )
    in
    let set_line_join j=
      if j<> (!state).lineJoin then (
        close_text ();
        state := {!state with lineJoin = j};
        Buffer.add_string pageBuf (
          match j with
              Miter_join->" 0 j "
            | Round_join->" 1 j "
            | Bevel_join->" 2 j "
          (* | _->"" *)
        )
      )
    in
    let set_line_cap c=
      if c<> (!state).lineCap then (
        close_text ();
        state := {!state with lineCap = c};
        Buffer.add_string pageBuf (
          match c with
              Butt_cap->" 0 J "
            | Round_cap->" 1 J "
            | Proj_square_cap->" 2 J "
          (* | _->"" *)
        )
      )
    in
    let set_line_width w =
      if w <> (!state).lineWidth then (
        close_text (); (* FIXME is this line required? *)
        state := {!state with lineWidth = w};
        Buffer.add_string pageBuf (sprintf "%f w " w);
      )
    in
    let set_dash_pattern l=
      if l<> (!state).dashPattern then (
        close_text ();
        state := {!state with dashPattern = l};
        match l with
            []->(Buffer.add_string pageBuf "[] 0 d ")
          | _::_->(
            Buffer.add_string pageBuf " [";
            List.iter (fun x->Buffer.add_string pageBuf (sprintf "%f " (pt_of_mm x))) l;
            Buffer.add_string pageBuf (sprintf "] 0. d ");
          )
      )
    in
    let rec output_contents c =
      match c with
      | Animation a ->
        List.iter output_contents (a.anim_contents.(a.anim_default))
      | Glyph gl->(
        change_non_stroking_color gl.glyph_color;
        if not !isText then Buffer.add_string pageBuf " BT ";
        isText:=true;
        let gx=pt_of_mm gl.glyph_x in
        let gy=pt_of_mm gl.glyph_y in
        let gx=match classify_float gx with FP_nan | FP_infinite->0.
          | _->gx
        in
        let gy=match classify_float gy with FP_nan | FP_infinite->0.
          | _->gy
        in
        let size=pt_of_mm gl.glyph_size in
        let enc,pdfObj=glyphEncoding gl.glyph in
        (* Inclusion de la police sur la page *)
        let idx=
          try
            IntMap.find pdfObj !pageFonts
          with
              Not_found->(
                let card=IntMap.cardinal !pageFonts in
                pageFonts := IntMap.add pdfObj card !pageFonts;
                card
              )
        in
        if idx <> !currentFont || size <> !currentSize then (
          close_line ();
          Buffer.add_string pageBuf (sprintf "/F%d %f Tf " idx size);
          currentFont:=idx;
          currentSize:=size;
        );
        if !yt<>gy || (not !openedLine) then (
          close_line ();
          Buffer.add_string pageBuf (sprintf "%f %f Td " (gx-. !xt) (gy-. !yt));
          xline:=0.;
          xt:=gx;yt:=gy
        );

        if not !openedLine then (Buffer.add_string pageBuf "["; openedLine:=true; xline:=0.);

        if !xt +. !xline <> gx then (
          let str=sprintf "%f" (1000.*.(!xt+. !xline -. gx)/.size) in
          let i=ref 0 in
          while !i<String.length str && (str.[!i]='0' || str.[!i]='.' || str.[!i]='-') do incr i done;
          if !i<String.length str then (
            if !openedWord then (Buffer.add_string pageBuf ")"; openedWord:=false);
            Buffer.add_string pageBuf str;
            xline:= !xline -. size*.(float_of_string str)/.1000.;
          )
        );
        if not !openedWord then (Buffer.add_string pageBuf "("; openedWord:=true);
        let c=char_of_int enc in
        if c='\\' || c='(' || c=')' then (
          Buffer.add_char pageBuf '\\';
          Buffer.add_char pageBuf c;
        ) else (
          Buffer.add_char pageBuf c
        );
        xline:= !xline +. size*.Fonts.glyphWidth gl.glyph/.1000.
      )
      | Path (_,[])->()
      | Path (params,paths) ->(
        close_text ();
        set_line_join RawContent.(params.lineJoin);
        set_line_cap RawContent.(params.lineCap);
        set_line_width (pt_of_mm RawContent.(params.lineWidth));
        set_dash_pattern RawContent.(params.dashPattern);
        (match RawContent.(params.strokingColor) with
            None->()
          | Some col -> change_stroking_color col);
        (match RawContent.(params.fillColor) with
            None->()
          | Some col -> change_non_stroking_color col);
        writePath pageBuf pt_of_mm paths params
      )
      | Link l->(
        pageLinks:= l:: !pageLinks;
        List.iter output_contents l.link_contents
      )
      | Image i->(
        pageImages:=i::(!pageImages);
        let num=List.length !pageImages in
        close_text ();
        Buffer.add_string pageBuf
          (Printf.sprintf "q %f 0 0 %f %f %f cm /Im%d Do Q "
             (pt_of_mm i.image_width) (pt_of_mm i.image_height)
             (pt_of_mm i.image_x) (pt_of_mm i.image_y) num);
      )
      | States s->List.iter output_contents s.states_contents
      | Dynamic d->List.iter output_contents (d.dyn_contents ())
      | Affine aff->(
        close_text ();
        let saved_state= { !state with strokingColor= (!state).strokingColor } in
        Buffer.add_string pageBuf
          (Printf.sprintf "q %f %f %f %f %f %f cm "
             aff.affine_matrix.(0).(0)
             aff.affine_matrix.(1).(0)
             aff.affine_matrix.(0).(1)
             aff.affine_matrix.(1).(1)
             (pt_of_mm aff.affine_matrix.(0).(2))
             (pt_of_mm aff.affine_matrix.(1).(2)));
        List.iter output_contents aff.affine_contents;
        close_text ();
        Buffer.add_string pageBuf "Q ";
        state:=saved_state
      )
      | Video _-> Printf.fprintf stderr "Video not support by Pdf driver\n%!"
    in
    let rec sort_contents c=
      let x=List.fold_left (fun m x->
        let m'=try IntMap.find (drawing_order x) m with Not_found->[] in
        IntMap.add (drawing_order x) (x::m') m
      ) IntMap.empty c
      in
      let comp a b =
        match a,b with
        | (Glyph ga, Glyph gb) ->
            if ga.glyph_y=gb.glyph_y then compare ga.glyph_x gb.glyph_x
            else compare gb.glyph_y ga.glyph_y
        | (Glyph _ , _       ) -> -1
        | (_       , Glyph _ ) -> 1
        | (_       , _       ) -> 0
      in
      let subsort a=match a with
          Link l->Link { l with link_contents=sort_contents l.link_contents }
        | States l->States { l with states_contents=sort_contents l.states_contents }
        | Dynamic l->Dynamic { l with dyn_contents=(fun ()->sort_contents (l.dyn_contents ())) }
        | Affine l->Affine { l with affine_contents=sort_contents l.affine_contents }
        | b->b
      in
      IntMap.fold (fun _ a x->x@a)
        (IntMap.map (fun l->(List.sort comp (List.map subsort l))) x) []
    in
    List.iter output_contents (sort_contents pages.(page).contents);
        close_text ();
        (* Objets de la page *)
        let contentObj=beginObject () in
        let filt, data=stream pageBuf in
        let len=Buffer.length data in
          fprintf oc "<< /Length %d %s>>\nstream\n" len filt;
          Buffer.output_buffer oc data;
          fprintf oc "\nendstream";
          endObject ();
          resumeObject pageObjects.(page);
          let w,h=pages.(page).size in
          fprintf oc "<< /Type /Page /Parent 1 0 R /MediaBox [ 0 0 %f %f ] " (pt_of_mm w) (pt_of_mm h);
          fprintf oc "/Group << /Type /Group /S /Transparency /I false /K true /CS /DeviceRGB >>";
          fprintf oc "/Resources << /ProcSet [/PDF /Text%s] "
            (if !pageImages=[] then "" else " /ImageB");
          if FloatMap.cardinal !sopacities > 1 || FloatMap.cardinal !nsopacities>1 then (
            fprintf oc "/ExtGState << ";
            fprintf oc "/GS%d <</Type /ExtGState /ca %f /CA %f>> " 0 1. 1.;
              FloatMap.iter (fun k a->
                if a>0 then
                  fprintf oc "/GS%d <</Type /ExtGState /CA %f>> " a k
              ) !sopacities;
              FloatMap.iter (fun k a->
                if a>0 then
                  fprintf oc "/GS%d <</Type /ExtGState /ca %f>> " a k
              ) !nsopacities;
              fprintf oc ">> "
            );
            if !pageImages<>[] then fprintf oc " /XObject << ";
            let ii=ref 1 in
            let actual_pageImages=
              List.map (fun i->
                          let obj=futureObject () in
                          fprintf oc "/Im%d %d 0 R" !ii obj;
                          incr ii;
                          (obj, !ii, i)) (List.rev !pageImages)
            in
            if !pageImages<>[] then fprintf oc ">>";
            if IntMap.cardinal !pageFonts >0 then (
              fprintf oc " /Font << ";
              IntMap.iter (fun a b->fprintf oc "/F%d %d 0 R " b a) !pageFonts;
              fprintf oc ">> "
            );
            fprintf oc ">> /Contents %d 0 R " contentObj;

            if !pageLinks <> [] then (
              fprintf oc "/Annots [ ";
              List.iter (fun l->
                let inf0=match classify_float l.link_x0 with FP_nan | FP_infinite->false
                  | _->true in
                let inf1=match classify_float l.link_x1 with FP_nan | FP_infinite->false
                  | _->true in
                let inf2=match classify_float l.link_y0 with FP_nan | FP_infinite->false
                  | _->true in
                let inf3=match classify_float l.link_y1 with FP_nan | FP_infinite->false
                  | _->true in
                if inf0 && inf1 && inf2 && inf3 then
                  match l.link_kind with
                    Intern(label,dest_page,dest_x,dest_y)->
                      if dest_page>=0 && dest_page<Array.length pageObjects then (
                        fprintf oc
                          "<< /Type /Annot /Subtype /Link /Rect [%f %f %f %f] /Dest [ %d 0 R /XYZ %f %f null] /Border [0 0 0]  >> "
                          (pt_of_mm l.link_x0) (pt_of_mm l.link_y0)
                          (pt_of_mm l.link_x1) (pt_of_mm l.link_y1) pageObjects.(dest_page)
                          (pt_of_mm dest_x) (pt_of_mm dest_y)
                      ) else (
                        Printf.fprintf stderr "Please report: Pdf.ml Intern link problem %S %d/%d\n"
                          label
                          dest_page (Array.length pageObjects);
                        flush stderr;
                      )
                  | Extern(uri) ->
                    fprintf oc
                      "<< /Type /Annot /Subtype /Link /Rect [%f %f %f %f] /F 4 /A <</Type /Action /S /URI /URI (%s)>> /Border [0 0 0]  >> "
                      (pt_of_mm l.link_x0) (pt_of_mm l.link_y0)
                      (pt_of_mm l.link_x1) (pt_of_mm l.link_y1)
                      uri
                  | _ -> ()
              ) !pageLinks;
              fprintf oc "]";
            );
            fprintf oc ">> ";
            endObject ();

            if !pageImages<>[] then (
              List.iter Image.(fun (obj,_,i)->
                let image = ImageLib_unix.openfile i.image_file in
                let w=image.width and h=image.height in
                let bits_per_component =
                  if image.max_val <= 255 then 8 else 16 in
                let device, nbc = match image.pixels with
                    RGB _ | RGBA _ -> "RGB", 3
                  | Grey _ | GreyA _ -> "Gray", 1
                in
                let img_buf=Buffer.create (w*h*nbc*(bits_per_component/8)) in
                let alpha_buf =
      match image.pixels with
      | RGB _  | Grey _  -> None
      | RGBA _ | GreyA _ -> Some (Buffer.create (w*h*(bits_per_component/8)))
                in
                if device = "RGB" then
                  for j=0 to h-1 do
                    for i=0 to w-1 do
                      Image.read_rgba image i j (fun r g b a ->
                        if image.max_val <= 255 then (
                          let r = (r * 255 * 2 + 1) / (2 * image.max_val) in
                          let g = (g * 255 * 2 + 1) / (2 * image.max_val) in
                          let b = (b * 255 * 2 + 1) / (2 * image.max_val) in

                          Buffer.add_char img_buf (char_of_int r);
                          Buffer.add_char img_buf (char_of_int g);
                          Buffer.add_char img_buf (char_of_int b))
                        else (
                          let r = (r * 65535 * 2 + 1) / (2 * image.max_val) in
                          let g = (g * 65535 * 2 + 1) / (2 * image.max_val) in
                          let b = (b * 65535 * 2 + 1) / (2 * image.max_val) in
                          let r0 = char_of_int (r land 255) in
                          let r1 = char_of_int (r lsr 8) in
                          let g0 = char_of_int (g land 255) in
                          let g1 = char_of_int (g lsr 8) in
                          let b0 = char_of_int (b land 255) in
                          let b1 = char_of_int (b lsr 8) in
                          Buffer.add_char img_buf r1;
                          Buffer.add_char img_buf r0;
                          Buffer.add_char img_buf g1;
                          Buffer.add_char img_buf g0;
                          Buffer.add_char img_buf b1;
                          Buffer.add_char img_buf b0);
                        match alpha_buf with
                        | None -> ()
                        | Some buf ->
                          if image.max_val <= 255 then (
                            let a = (a * 255 * 2 + 1) / (2 * image.max_val) in
                            Buffer.add_char buf (char_of_int a))
                          else (
                            let a = (a * 65535) / image.max_val in
                            let a0 = char_of_int (a land 255) in
                            let a1 = char_of_int (a lsr 8) in
                            Buffer.add_char buf a1;
                            Buffer.add_char buf a0))
                    done
                  done
                else
                  for j=0 to h-1 do
                    for i=0 to w-1 do
                      Image.read_greya image i j (fun g a ->
                        if image.max_val <= 255 then (
                          let g = (g * 255) / image.max_val in
                          Buffer.add_char img_buf (char_of_int g))
                        else (
                          let g = (g * 65535) / image.max_val in
                          let g0 = char_of_int (g land 255) in
                          let g1 = char_of_int (g lsr 8) in
                          Buffer.add_char img_buf g1;
                          Buffer.add_char img_buf g0);
                        match alpha_buf with
                        | None -> ()
                        | Some buf ->
                          if image.max_val <= 255 then (
                            let a = (a * 255 * 2 + 1) / (2 * image.max_val) in
                            Buffer.add_char buf (char_of_int a))
                          else (
                            let a = (a * 65535) / image.max_val in
                            let a0 = char_of_int (a land 255) in
                            let a1 = char_of_int (a lsr 8) in
                            Buffer.add_char buf a1;
                            Buffer.add_char buf a0))
                    done
                  done;
                let mask = match alpha_buf with
                | None -> ""
                | Some buf ->
                  let a,b=stream buf in
                  let m = beginObject () in
                  fprintf oc "<< /Type /XObject /Subtype /Image /Width %d /Height %d /ColorSpace /DeviceGray /BitsPerComponent %d /Length %d %s>>\nstream\n" w h bits_per_component (Buffer.length b) a;
                  Buffer.output_buffer oc b;
                  let res = sprintf "/SMask %d 0 R" m in
                  fprintf oc "\nendstream";
                  endObject ();
                  res
                in
                resumeObject obj;
                let a,b=stream img_buf in
                fprintf oc "<< /Type /XObject /Subtype /Image /Width %d /Height %d /ColorSpace /Device%s /BitsPerComponent %d /Length %d %s %s>>\nstream\n" w h device bits_per_component (Buffer.length b) mask a;
                Buffer.output_buffer oc b;
                fprintf oc "\nendstream";
                endObject ()
              ) actual_pageImages
            )
  done;


  let writeEncoding enc varGlyphs=
    resumeObject enc;
    fprintf oc "<< /Type /Encoding /Differences [";
    IntMap.iter (fun enc _->
      fprintf oc "%d /uni%d "
        enc enc
      (* (UChar.code (UTF8.look utf8 0)) *)
      (* (UChar.code (UTF8.look utf8 0)) *)
    ) varGlyphs;
    fprintf oc "]>>";
    endObject ();
  in

  (* Type 3 *)
  let pdftype3 font var=
    try
      let pdffont=IntMap.find var font.pdfObjects in
      resumeObject pdffont;
      fprintf oc "<< /Type /Font /Subtype /Type3 ";
      (* fprintf oc "/Name %s " name; *)
      let varGlyphs=IntMap.find var font.fontVariants in
      let x0=ref infinity and x1=ref (-.infinity) and y0=ref infinity and y1=ref (-.infinity) in
      IntMap.iter (fun _ glyph->
        List.iter (List.iter (fun (x,y)->
          Array.iter (fun u->x0:=min u !x0;x1:=max u !x1) x;
          Array.iter (fun u->y0:=min u !y0;y1:=max u !y1) y
        )) (Fonts.outlines glyph)
      ) varGlyphs;
      (if !x0<infinity && !x1>(-.infinity) && !y0<infinity && !y1>(-.infinity) then
          fprintf oc "/FontBBox [%f %f %f %f] " !x0 !y0 !x1 !y1
       else
          fprintf oc "/FontBBox [0 0 0 0] ");
      fprintf oc "/FontMatrix [0.001 0 0 0.001 0 0] ";
      let charprocs_obj=futureObject () in
      fprintf oc "/CharProcs %d 0 R " charprocs_obj;
      let enc=futureObject () in
      fprintf oc "/Encoding %d 0 R " enc;

      let firstChar,_=IntMap.min_binding varGlyphs in
      let lastChar,_=IntMap.max_binding varGlyphs in
      fprintf oc "/FirstChar %d " firstChar;
      fprintf oc "/LastChar %d " lastChar;
      let widths=futureObject () in
      fprintf oc "/Widths %d 0 R " widths;
      fprintf oc ">>";
      endObject();


      resumeObject widths;
      fprintf oc "[";
      for i=firstChar to lastChar do
        fprintf oc "%f " (try Fonts.glyphWidth (IntMap.find i varGlyphs) with Not_found->0.)
      done;
      fprintf oc "]";
      endObject ();


      resumeObject charprocs_obj;
      fprintf oc "<< ";
      let charprocs=IntMap.fold (fun enc glyph m->
        let o=futureObject () in
        let w=Fonts.glyphWidth glyph in
        let outlines=Fonts.outlines glyph in
        fprintf oc "/uni%d %d 0 R " enc o;(* (UChar.code (UTF8.look utf8 0)) o; *)
        (o,w,outlines)::m
      ) varGlyphs []
      in
      fprintf oc ">>";
      endObject ();

      let bu=Buffer.create 1000 in
      List.iter (fun (o,w,outlines)->
        Buffer.clear bu;
        x0:=infinity;
        x1:=(-.infinity);
        y0:=infinity;
        y1:=(-.infinity);
        List.iter (List.iter (fun (x,y)->
          Array.iter (fun u->x0:=min u !x0;x1:=max u !x1) x;
          Array.iter (fun u->y0:=min u !y0;y1:=max u !y1) y
        )) outlines;
        Buffer.add_string bu (sprintf "%f %d %f %f %f %f d1 " w 0 !x0 !y0 !x1 !y1);
        writePath bu (fun x->x)(List.map (Array.of_list) outlines) default_path_param;
        Buffer.add_string bu "f ";
        let filt, data=stream bu in
        let len=Buffer.length data in
        resumeObject o;
        fprintf oc "<< /Length %d %s>>\nstream\n" len filt;
        Buffer.output_buffer oc data;
        fprintf oc "\nendstream";
        endObject ();
      ) charprocs;

      writeEncoding enc varGlyphs;
    with
        Not_found->()
  in
  (* /Type 3 *)

  (* Type 1C (CFF) *)
  let pdftype1c font var=
    try
      let cff = cff_only font.font in
      let pdffont=IntMap.find var font.pdfObjects in
      resumeObject pdffont;
      fprintf oc "<< /Type /Font /Subtype /Type1 ";
      let name=CFF.fontName cff in
      fprintf oc "/BaseFont /%s " name.postscript_name;
      let varGlyphs=IntMap.find var font.fontVariants in
      let firstChar,_=IntMap.min_binding varGlyphs in
      let lastChar,_=IntMap.max_binding varGlyphs in
      fprintf oc "/FirstChar %d " firstChar;
      fprintf oc "/LastChar %d " lastChar;
      let widths=futureObject () in
      fprintf oc "/Widths %d 0 R " widths;
      let descr=futureObject () in
      fprintf oc "/FontDescriptor %d 0 R " descr;
      let enc=futureObject () in
      fprintf oc "/Encoding %d 0 R " enc;
      fprintf oc ">>";
      endObject();

      writeEncoding enc varGlyphs;

      resumeObject descr;
      fprintf oc "<< /Type /FontDescriptor ";
      fprintf oc "/FontName /%s " name.postscript_name;
      fprintf oc "/Flags 4 ";

      let x0=ref infinity and x1=ref (-.infinity) and y0=ref infinity and y1=ref (-.infinity) in
      IntMap.iter (fun _ glyph->
        List.iter (List.iter (fun (x,y)->
          Array.iter (fun u->x0:=min u !x0;x1:=max u !x1) x;
          Array.iter (fun u->y0:=min u !y0;y1:=max u !y1) y
        )) (Fonts.outlines glyph)
      ) varGlyphs;
      (if !x0<infinity && !x1>(-.infinity) && !y0<infinity && !y1>(-.infinity) then
          fprintf oc "/FontBBox [%f %f %f %f] " !x0 !y0 !x1 !y1
       else
          fprintf oc "/FontBBox [0 0 0 0] ");
      fprintf oc "/ItalicAngle 0 /Ascent %f /Descent %f /CapHeight %f " (max 0. !y1) (min 0. !y0) (max 0. !y1);
      let fontfile=futureObject () in
      fprintf oc "/StemV 10 /FontFile3 %d 0 R >>" fontfile;
      endObject ();

      resumeObject widths;
      fprintf oc "[";
      for i=firstChar to lastChar do
        fprintf oc "%f " (try Fonts.glyphWidth (IntMap.find i varGlyphs) with Not_found->0.)
      done;
      fprintf oc "]";
      endObject ();

      resumeObject fontfile;
      let fontinfo=CFF.fontInfo cff in
      let glyphs=Array.make (1+IntMap.cardinal varGlyphs) {glyph_index=0;glyph_utf8=""} in
      let _,enc=IntMap.fold (fun enc glyph (i,m)->
        glyphs.(i)<-(Fonts.glyphNumber glyph);
        (i+1,IntMap.add enc i m)
      ) varGlyphs (1,IntMap.empty)
      in
      let cffdata=CFF.subset cff fontinfo enc glyphs in
      let filt, data=stream cffdata in
      let len=Buffer.length data in
      fprintf oc "<< /Length %d /Subtype /Type1C %s>>\nstream\n" len filt;
      Buffer.output_buffer oc data;
      fprintf oc "\nendstream";
      endObject ();
    with
        Not_found->()
  in
  (* /Type 1C (CFF) *)

  let fn _ pdffont =
    let pdftype =
      if not !pdf_type3_only && is_cff pdffont.font then pdftype1c
      else pdftype3
    in
    IntMap.iter (fun k _-> pdftype pdffont k) pdffont.pdfObjects
  in
  StrMap.iter fn !fonts;


  (*
    (* Tous les dictionnaires de unicode mapping *)
    let defaultutf8=String.make 1 (char_of_int 0) in
    let glyphutf8 x=
      let n=Fonts.glyphNumber x in
      if n.glyph_utf8="" then defaultutf8 else n.glyph_utf8
    in
    StrMap.iter (fun _ x->
      if x.fontToUnicode>=0 then (
        let buf=Buffer.create 100000 in
        Buffer.add_string buf "/CIDInit /ProcSet findresource begin\n12 dict begin\nbegincmap\n";
        Buffer.add_string buf "/CIDSystemInfo << /Registry (Adobe) /Ordering (UCS) /Supplement 0 >> def\n";
        Buffer.add_string buf "/CMapName /Adobe-Identity-UCS def\n/CMapType 2 def\n";
        Buffer.add_string buf "1 begincodespacerange\n<0000> <FFFF>\nendcodespacerange\n";
        let range=ref [] in
        let one=ref [] in
        let multRange=ref [] in
        let rec make_cmap glyphs=
          if not (IntMap.is_empty glyphs) then (
            (* On commence par partitionner par premier octet (voir adobe technical note #5411) *)
            let m0,g0=IntMap.min_binding glyphs in
            let a,b=
              let a,gi,b=IntMap.split (m0 lor 0x00ff) glyphs in
              (match gi with Some ggi->IntMap.add (m0 lor 0x00ff) ggi a | _->a), b
            in
            let one_char, mult_char=IntMap.partition (fun _ gl->
              let utf8=(glyphutf8 gl) in
              UTF8.next utf8 0 > String.length utf8) a
            in
            (* recuperer les intervalles et singletons *)
            let rec unicode_diff a0=
              if not (IntMap.is_empty a0) then (
                let idx0,m0=IntMap.min_binding a0 in
                let num0=glyphutf8 m0 in
                let u,v=IntMap.partition (fun idx x->let num=glyphutf8 x in
                                                     idx-(UChar.code (UTF8.get num 0)) =
                    idx0-(UChar.code (UTF8.get num0 0))
                ) a0
                in
                let idx1,m1=IntMap.max_binding u in
                if IntMap.cardinal u > 1 then (
                  range:=(idx0,idx1,UTF8.get num0 0)::(!range)
                ) else (
                  one:=(idx0, num0)::(!one)
                );
                unicode_diff v
              )
            in
            unicode_diff one_char;
            if not (IntMap.is_empty mult_char) then (
              let idx0,m0=IntMap.min_binding mult_char in
              let first=ref idx0 in
              let last=ref (idx0-1) in
              let cur=ref [] in
              IntMap.iter (fun idx a->
                let num=glyphutf8 a in
                if idx > (!last)+1 then (
                  (match !cur with
                      _::_::_->multRange:=(!first, List.rev !cur)::(!multRange)
                    | [h]->one:=(!first, h)::(!one)
                    | []->());
                  cur:=[]
                );
                if !cur=[] then
                  first:=idx;
                cur:=num::(!cur);
                last:=idx
              ) mult_char;

              match !cur with
                  _::_::_->multRange:=(!first, List.rev !cur)::(!multRange)
                | [h]->one:=(!first, h)::(!one)
                | []->()

            );
            make_cmap b
          )
        in
        make_cmap x.revFontGlyphs;
        let rec print_utf8 utf idx=
          try
            Buffer.add_string buf (sprintf "%04x" (UChar.code (UTF8.look utf idx)));
            print_utf8 utf (UTF8.next utf idx)
          with
              _->()
        in
        let one_nonempty=List.filter (fun (_,b)->b<>"") !one in
        if one_nonempty<>[] then (
          Buffer.add_string buf (sprintf "%d beginbfchar\n" (List.length !one));
          List.iter (fun (a,b)->
            Buffer.add_string buf (sprintf "<%04x> <" a);
            print_utf8 b (UTF8.first b);
            Buffer.add_string buf ">\n"
          ) one_nonempty;
          Buffer.add_string buf "endbfchar\n"
        );

        let mult_nonempty=List.filter (fun (_,b)->b<>[])
          (List.map (fun (a,b)->a, List.filter (fun c->c<>"") b) !multRange) in

        if !range<>[] || mult_nonempty<>[] then (
          Buffer.add_string buf (sprintf "%d beginbfrange\n" (List.length !range+List.length !multRange));
          List.iter (fun (a,b,c)->Buffer.add_string buf (sprintf "<%04x> <%04x> <%04x>\n"
                                                            a b (UChar.code c))) !range;
          List.iter (fun (a,b)->
            Buffer.add_string buf (sprintf "<%04x> <%04x> [" a (a+List.length b-1));
            List.iter (fun c->
              Buffer.add_string buf "<";
              print_utf8 c (UTF8.first c);
              Buffer.add_string buf ">") b;
            Buffer.add_string buf "]\n"
          ) mult_nonempty;
          Buffer.add_string buf "endbfrange\n"
        );
        Buffer.add_string buf "endcmap\nCMapName currentdict /CMap defineresource pop\nend end\n";

        resumeObject x.fontToUnicode;
        let filt, data=stream buf in
        let len=Buffer.length data in
        fprintf oc "<< /Length %d %s>>\nstream\n" len filt;
        Buffer.output_buffer oc data;
        fprintf oc "\nendstream";
        endObject ()
      )
    ) !fonts;

  *)

    (* Ecriture du pageTree *)
    flush oc;
    xref:=IntMap.add 1 (pos_out oc) !xref;
    fprintf oc "1 0 obj\n<< /Type /Pages /Count %d /Kids [" (Array.length pages);
    Array.iter (fun a->fprintf oc " %d 0 R" a) pageObjects;
    fprintf oc "] >>";
    endObject ();

    (* Ecriture du catalogue *)

    let rdf=Buffer.create 1000 in
    Buffer.add_string rdf"<?xpacket begin='' id='W5M0MpCehiHzreSzNTczkc9d'?>\n";

    Buffer.add_string rdf "<rdf:RDF
xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
xmlns:dc=\"http://purl.org/dc/elements/1.1/\">\n";
    Buffer.add_string rdf "<rdf:Description rdf:about=\"\" xmlns:pdfaid=\"http://www.aiim.org/pdfa/ns/id/\">\n<pdfaid:part>1</pdfaid:part>\n<pdfaid:conformance>A</pdfaid:conformance>\n</rdf:Description>\n";

    Buffer.add_string rdf "<rdf:Description rdf:about=\"\">\n";


    List.iter (fun (meta, cont)->
      let descr=match meta with
          Contributor->"contributor"
        | Coverage->"coverage"
        | Creator->"creator"
        | Date->"date"
        | Description->"description"
        | Format->"format"
        | Identifier->"identifier"
        | Language->"language"
        | Publisher->"publisher"
        | Relation->"relation"
        | Rights->"rights"
        | Source->"source"
        | Subject->"subject"
        | Title->"title"
        | Type->"type"
      in
      Buffer.add_string rdf (sprintf "<dc:%s>%s</dc:%s>\n" descr cont descr)
    ) structure.metadata;

    Buffer.add_string rdf "</rdf:Description>\n</rdf:RDF>\n";
    Buffer.add_string rdf "<?xpacket end='w'?>\n";

    let metadata=beginObject () in
    fprintf oc "<< /Length %d /Type /Metadata /Subtype /XML >>\nstream\n"
      (Buffer.length rdf);
    Buffer.output_buffer oc rdf;
    fprintf oc "\nendstream\n";
    endObject ();

    let markinfo="<< /Marked true /UserProperties null /Suspects null >>" in
    let outputIntents="/OutputIntents [ << /Info (none) /Type /OutputIntent /S /GTS_PDFX /OutputConditionIdentifier (Blurb.com) /RegistryName (http://www.color.org/) >> ]"
    in


    (* Encore plus de metadonnees ! structtree *)
    let structTreeRoot=beginObject () in
    fprintf oc "<< /Type /StructTreeRoot /S /Document >>\n";
    endObject ();

    let cat=futureObject () in
    if structure.name="" && Array.length structure.children=0 then (
      resumeObject cat;
      fprintf oc "<< /Type /Catalog /Pages 1 0 R /Metadata %d 0 R /MarkInfo %s %s /StructTreeRoot %d 0 R >>" metadata markinfo outputIntents structTreeRoot;
      endObject ()
    ) else (
      let count=ref 0 in
      let rec make_outlines str par=
        let hijosObjs=Array.map (fun _-> futureObject ()) str.children in
        for i=0 to Array.length str.children-1 do
          let (a,b)=make_outlines str.children.(i) hijosObjs.(i) in
          incr count;

          resumeObject hijosObjs.(i);
          fprintf oc "<< /Title <%s> /Parent %d 0 R " (pdf_string str.children.(i).name) par;
          if i>0 then fprintf oc "/Prev %d 0 R " hijosObjs.(i-1);
          if i<Array.length str.children-1 then fprintf oc "/Next %d 0 R " hijosObjs.(i+1);
          if a>0 then
            fprintf oc "/First %d 0 R /Last %d 0 R /Count %d "
              a b (Array.length str.children.(i).children);

          if str.children.(i).page>=0 && str.children.(i).page<Array.length pageObjects then
            fprintf oc "/Dest [%d 0 R /XYZ %f %f null] " pageObjects.(str.children.(i).page)
              (pt_of_mm str.children.(i).struct_x)
              (pt_of_mm str.children.(i).struct_y);
          fprintf oc ">> ";
          endObject ()
        done;
        if Array.length hijosObjs>0 then
          (hijosObjs.(0),hijosObjs.(Array.length hijosObjs-1))
        else
          (-1,-1)
      in


      let outlines=futureObject () in
      let a,b=make_outlines { name="";
                              page=0; struct_x=0.; struct_y=0.;
                              children=[|structure|];
                              metadata=[];
                              tags=[];
                              raw_name=[] } outlines
      in
      (*let a,b=make_outlines structure outlines in*)

      resumeObject outlines;
      fprintf oc "<< /Type /Outlines /First %d 0 R /Last %d 0 R /Count %d >>" a b !count;
      endObject ();
      resumeObject cat;
      fprintf oc "<< /Type /Catalog /Pages 1 0 R /Outlines %d 0 R /Metadata %d 0 R /MarkInfo %s %s /StructTreeRoot %d 0 R >>" outlines metadata markinfo outputIntents structTreeRoot;
      endObject ()
    );

    (* Ecriture de xref *)
    flush oc;
    let xref_pos=pos_out oc in
    fprintf oc "xref\n0 %d \n0000000000 65535 f \n" (1+IntMap.cardinal !xref);
    IntMap.iter (fun _ a->fprintf oc "%010d 00000 n \n" a) !xref;

    (* Trailer *)
    let file_id = Digest.to_hex (Digest.string fname) in
    fprintf oc "trailer\n";
    fprintf oc "<< /Size %d /Root %d 0 R /ID [(%s) (%s)] >>\n"
      (1+IntMap.cardinal !xref) cat file_id file_id;
    fprintf oc "startxref\n";
    fprintf oc "%d\n" xref_pos;
    fprintf oc "%%EOF\n";
    close_out oc

let output' = output_to_prime output

let _ =
  Hashtbl.add DynDriver.drivers "Pdf" (
    module struct
      let output  = output
      let output' = output'
    end:OutputDriver)
