
module Rope=Batteries.Rope

module type Driver = sig
  type driver
  type params=string
  val init : params -> driver
  val close : driver -> unit

  val begin_page : driver -> (float*float) ->unit
  val end_page : driver -> unit

  val moveto : driver->(float*float) -> unit
  val lineto : driver->(float*float) -> unit
  val curveto : driver->(float*float) -> (float*float) -> (float*float) -> unit

  val close:driver->unit
  val stroke:driver->unit
  val fill:driver->unit
  val fill_stroke:driver->unit
  val close_stroke:driver->unit
  val closePath:driver->unit

  val text:driver->(float*float)->Fonts.glyph list->unit

end

module IntMap=Map.Make (struct type t=int let compare=compare end)


module Pdf = 
  (struct
     type params=string
     type driver= { out_chan:out_channel;
                    mutable xref:int IntMap.t;
                    mutable pages:int IntMap.t;
                    mutable current_page:Rope.t;
                    mutable current_pageSize:float*float;
                    mutable vpos:float*float;
                    mutable pos:float*float;
                    mutable isText:bool
                  }
         
     let pageTree=1
       
     let init file=
       let out_chan=open_out file in
         output_string out_chan ("%PDF-1.7\n%"^String.make 4 (char_of_int 128)^"\n");
         { out_chan=out_chan; pages=IntMap.empty; xref=IntMap.singleton pageTree 0;
           current_pageSize=(0.,0.);
           current_page=Rope.empty;
           vpos=(0.,0.);
           pos=(infinity,infinity);
           isText=false }
           
     let resumeObject pdf n=
       flush pdf.out_chan;
       pdf.xref<-IntMap.add n (pos_out pdf.out_chan) pdf.xref;
       output_string pdf.out_chan ((string_of_int n)^" 0 obj\n")

     let beginObject pdf=
       let n=IntMap.cardinal pdf.xref in
         resumeObject pdf (n+1);
         n+1

     let endObject pdf=output_string pdf.out_chan "\nendobj\n"



       


     let close pdf=

       (* Ecriture du pageTree *)
       resumeObject pdf pageTree;(
         output_string pdf.out_chan
           ("<< /Type /Pages /Count "^(string_of_int (IntMap.cardinal pdf.pages))^
              " /Kids ["^(IntMap.fold (fun _ a str->str^(if String.length str=0 then "" else " ")^(string_of_int a)^" 0 R") pdf.pages "")^
              "] >>"));
       endObject pdf;
       
       (* Metadata stream *)
       let meta=beginObject pdf in
         output_string pdf.out_chan "<< /Type /Metadata /Subtype XML /Length 0 >>\nstream\nendstream";
         endObject pdf;
       
       (* Ecriture du catalogue *)
       let cat=beginObject pdf in
         output_string pdf.out_chan ("<< /Type /Catalog /Metadata "^(string_of_int meta)^" 0 R /Pages "^(string_of_int pageTree)^" 0 R >>");
         endObject pdf;
       

         (* Ecriture de xref *)
         flush pdf.out_chan;
         let xref=pos_out pdf.out_chan in
           output_string pdf.out_chan ("xref\n0 "^(string_of_int (1+IntMap.cardinal pdf.xref))^"\n0000000000 65535 f \n");
           IntMap.iter (fun _ a->
                          let str=string_of_int a in
                            output_string pdf.out_chan ( (String.make (10-String.length str) '0')^str^" 00000 n \n")
                       ) pdf.xref;

           (* Trailer *)
           output_string pdf.out_chan ("trailer\n<< /Size "^(string_of_int (1+IntMap.cardinal pdf.xref))^
                                         " /Root "^(string_of_int cat)^" 0 R >>\nstartxref\n"^(string_of_int xref)^
                                         "\n%%EOF\n");
           close_out pdf.out_chan


     let begin_page pdf size=
       pdf.current_page<-Rope.empty;
       pdf.current_pageSize<-size
           
     let end_page pdf=
       if pdf.isText then pdf.current_page<-Rope.append pdf.current_page (Rope.of_string " ET ");
       let str=Rope.to_string pdf.current_page in
       let contentObject=beginObject pdf in
         output_string pdf.out_chan ("<< /Length "^(string_of_int (String.length str))^" >>\nstream\n");
         output_string pdf.out_chan str;
         output_string pdf.out_chan "\nendstream";
         endObject pdf;

         let pageObject=beginObject pdf in
           output_string pdf.out_chan ("<< /Type /Page /Parent "^(string_of_int pageTree)^" 0 R /MediaBox [ 0 0 "^
                                         (string_of_int (int_of_float (fst pdf.current_pageSize)))^" "^
                                         (string_of_int (int_of_float (snd pdf.current_pageSize)))^" ] ");
           output_string pdf.out_chan "/Resources <<";
           output_string pdf.out_chan " /ProcSet [/PDF /Text] ";
           
           output_string pdf.out_chan (">> /Contents "^(string_of_int contentObject)^" 0 R >>");
           
           endObject pdf;
           
           pdf.pages<-IntMap.add (1+IntMap.cardinal pdf.pages) pageObject pdf.pages


     let moveto pdf pos=pdf.vpos<-pos

     let really_move pdf=
       if pdf.isText then pdf.current_page<-Rope.append pdf.current_page (Rope.of_string " ET ");
       if pdf.vpos <> pdf.pos then
         pdf.current_page <- Rope.append pdf.current_page 
           (Rope.of_string ((string_of_float (fst pdf.vpos)) ^ " " ^ (string_of_float (snd pdf.vpos)) ^ " m "))
     let end_path pdf=
       pdf.pos<-(0.,0.);
       pdf.vpos<-(infinity,infinity)

     let lineto pdf ((x,y) as pos)=
       really_move pdf;
       pdf.current_page <- Rope.append pdf.current_page
         (Rope.of_string ((string_of_float x)^" "^(string_of_float y)^" l "));
       pdf.pos<-pos;
       pdf.vpos<-pos

     let curveto pdf (x1,y1) (x2,y2) ((x3,y3) as pos)=
       really_move pdf;
       pdf.current_page <- Rope.append pdf.current_page
         (Rope.of_string ((string_of_float x1)^" "^(string_of_float y1)^(string_of_float x2)^" "^
                            (string_of_float y2)^" "^(string_of_float x3)^" "^(string_of_float y3)^" "^" c "));
       pdf.pos<-pos;
       pdf.vpos<-pos
         
     let closePath pdf=
       end_path pdf;
       pdf.current_page <- Rope.append pdf.current_page (Rope.of_string " h ")
     let stroke pdf=
       end_path pdf;
       pdf.current_page <- Rope.append pdf.current_page (Rope.of_string " S ")
     let close_stroke pdf=
       end_path pdf;
       pdf.current_page <- Rope.append pdf.current_page (Rope.of_string " s ")
     let fill_stroke pdf=
       end_path pdf;
       pdf.current_page <- Rope.append pdf.current_page (Rope.of_string " b ")
     let fill pdf=
       end_path pdf;
       pdf.current_page <- Rope.append pdf.current_page (Rope.of_string " f ")

     let text pdf (x,y) glyphs=
       if not pdf.isText then pdf.current_page<-Rope.append pdf.current_page (Rope.of_string " BT ");
       

   end:Driver)

let _=
  let drv=Pdf.init "test.pdf" in
    Pdf.begin_page drv (100.,100.);
    Pdf.moveto drv (10.,10.);
    Pdf.text drv (110.,110.) [];
    Pdf.stroke drv;
    Pdf.end_page drv;
    Pdf.close drv;
