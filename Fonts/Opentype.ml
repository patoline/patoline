open FTypes
open Constants
open Binary
open CFF
open CamomileLibrary
let offsetTable=12
let dirSize=16
exception Table_not_found


let tableLookup table file off=
  seek_in file (off+4);
  let numTables=readInt2 file in
  let tableName="    " in
  let rec lookup i j=
    let middle=(i+j) / 2 in
      seek_in file (off+offsetTable+middle*dirSize);
      really_input file tableName 0 4;
      if middle<=i then
        if tableName=table then
          ((seek_in file (off+offsetTable+i*dirSize+8);readInt4 file),
           (seek_in file (off+offsetTable+i*dirSize+12);readInt4 file))
        else
          raise Table_not_found
      else
        if compare tableName table <=0 then
          lookup middle j
        else
          lookup i middle
  in
    lookup 0 numTables
let tableList file off=
  seek_in file (off+4);
  let numTables=readInt2 file in
  let rec getTables n l=
    if n=offsetTable then l else
      (seek_in file (off+n);
       let newTable=String.create 4 in
         really_input file newTable 0 4;
         getTables (n-dirSize) (newTable::l))
  in
    getTables (off+dirSize*(numTables-1)+offsetTable) []
type font = CFF of (CFF.font*int)
let loadFont ?offset:(off=0) ?size:(_=0) file=
  let f=open_in file in
  let typ=String.create 4 in
    seek_in f off;
    really_input f typ 0 4;
    match typ with
        "OTTO"->
          let (a,b)=tableLookup "CFF " f off in
            CFF (CFF.loadFont file ~offset:(off+a) ~size:b, off)
      | _->failwith ("OpenType : format non reconnu : "^typ)

type glyph = CFFGlyph of (font*CFF.glyph)

let glyph_of_uchar font char0=
  match font with
      CFF (font,offset0)->
        let file=font.file in
        let char=UChar.code char0 in
        let (a,b)=tableLookup "cmap" file offset0 in
          seek_in file (a+2);
          let numTables=readInt4 file in
          let table=ref 0 in
          let cid=ref 0 in

            while !cid=0 && !table<numTables do
              seek_in file (a+8+8* !table);
              let offset=a+readInt4 file in
                seek_in file offset;
                let t=readInt2 file in
                  (match t with
                       0->if char<256 then (
                         seek_in file (offset+6+char);
                         cid:=input_byte file)
                     | 2->
                         (let i=(char lsr 8) land 0xff in
                          let j=char land 0xff in
                          let k=(seek_in file (offset+6+i*2); readInt2 file) lsr 3 in
                          let subHeaders=offset+6+256*2+k*8 in
                            if k=0 then
                              (seek_in file (subHeaders+6);
                               let idRangeOffset=readInt2 file in
                                 seek_in file (subHeaders+idRangeOffset+i*2);
                                 cid:=readInt2 file
                              )
                            else
                              (let firstCode=seek_in file subHeaders; readInt2 file in
                               let entryCount=seek_in file (subHeaders+2); readInt2 file in
                               let idDelta=seek_in file (subHeaders+4); readInt file 2 in
                               let idRangeOffset=seek_in file (subHeaders+6); readInt2 file in
                                 if j>=firstCode && j < (firstCode+entryCount) then
                                   (let p=seek_in file (subHeaders+8+idRangeOffset+j*2); readInt2 file in
                                      cid:=if p=0 then p else p+idDelta))
                         )
                     | 4->
                         (let sc2=seek_in file (offset+6); readInt2 file in
                          let rec smallestEnd i j=
                            if j<=i then i else
                              let middle=((i+j) lsr 1) land 0xfffe in
                              let end_=seek_in file (offset+14+middle); readInt2 file in
                                if char>end_ then
                                  smallestEnd (middle+2) j
                                else
                                  smallestEnd i middle
                          in
                          let seg=smallestEnd 0 (sc2-2) in
                          let start=seek_in file (offset+16+sc2+seg); readInt2 file in
                          let delta=seek_in file (offset+16+2*sc2+seg); readInt2 file in
                          let p_idrOffset=offset+16+3*sc2+seg in
                          let idrOffset=seek_in file p_idrOffset; readInt2 file in
                            if char<start then table:=numTables else
                              if idrOffset=0 then
                                cid:=(char+delta) land 0xffff
                              else
                                (seek_in file (idrOffset+2*(char-start)+p_idrOffset); cid:=readInt2 file)
                         )
                     | 6->
                         (let first=seek_in file (offset+6); readInt2 file in
                          let entryCount=seek_in file (offset+8); readInt2 file in
                            if first<=char && char <first+entryCount then
                              (seek_in file (offset+10+(char-first)*2); cid:=readInt2 file)
                         )
                     | x->failwith ("cmap : type "^(string_of_int t)^" unsupported (yet)")
                  );
                  incr table
            done;
            !cid

let glyph_of_char f c=glyph_of_uchar f (UChar.of_char c)


let glyphFont f=match f with
    CFFGlyph (x,_)->x
let loadGlyph f ?index:(idx=0) gl=
  match f with
      CFF (x,_)->CFFGlyph (f, CFF.loadGlyph x ~index:idx gl)
let outlines gl=match gl with
    CFFGlyph (_,x)->CFF.outlines x

let glyphNumber gl=match gl with
    CFFGlyph (_,x)->CFF.glyphNumber x


let glyphWidth gl=
  match gl with
      CFFGlyph (CFF(f, offset),x)->
        (let num=(CFF.glyphNumber x).glyph_index in
         let (a,_)=tableLookup "hhea" f.CFF.file offset in
         let nh=(seek_in (f.CFF.file) (a+34); readInt2 f.CFF.file) in
         let (b,_)=tableLookup "hmtx" f.CFF.file offset in
           seek_in (f.CFF.file) (if num>nh then b+4*(nh-1) else b+4*num);
           (float_of_int (readInt2 f.CFF.file)))
let fontName ?index:(idx=0) f =
  match f with
      CFF (x,_)->CFF.fontName x ~index:idx

let otype_file font=match font with
    CFF (font,offset0)->font.file, offset0


let coverageIndex file off glyph=
  let format=seek_in file off; readInt2 file in
  let count=readInt2 file in
  let rec format1 x0 x1=
    if x0>=x1 then raise Not_found else
      if x1=x0+1 then
        (let x2=(x0+x1)/2 in
         let current=seek_in file (off+4+2*x2); readInt2 file in
           if current=glyph then x2 else raise Not_found)
      else
        (let x2=(x0+x1)/2 in
         let current=seek_in file (off+4+2*x2); readInt2 file in
           if glyph<current then format1 x0 x2 else format1 x2 x1)
  in
  let rec format2 x0 x1=
    if x0>=x1 then raise Not_found else
      if x1=x0+1 then
        (let start=seek_in file (off+6*x0+4); readInt2 file in
         let final=readInt2 file in
         let cvIdx=readInt2 file in
           if glyph>=start && glyph<=final then
             cvIdx+glyph-start
           else
             raise Not_found)

      else
        (let x2=(x0+x1)/2 in
         let final=seek_in file (off+6*x0+6); readInt2 file in
           if glyph>final then
             format2 x2 x1
           else
             format2 x0 x2)
  in
    if format=1 then format1 0 count else
      if format=2 then format2 0 count else
        (Printf.printf "format : %d\n" format; raise Not_found)


let class_def file off glyph=
  let format=seek_in file off; readInt2 file in
    if format=1 then (
      let startGlyph=readInt2 file in
      let glyphCount=readInt2 file in
        if glyph>=startGlyph && glyph<startGlyph+glyphCount then
          (seek_in file (off+6+2*(glyph-startGlyph)); readInt2 file)
        else
          0
    ) else if format=2 then (
      let classRangeCount=readInt2 file in
      let off0=off+4 in
      let rec format2 x0 x1=
        let x2=(x0+x1)/2 in
        let rstart=seek_in file (off0+6*x2); readInt2 file in
        let rend=readInt2 file in
          if glyph<rstart then
            if x1-x0<=1 then 0 else format2 x0 x2
          else
            if glyph>rend then
              if x1-x0<=1 then 0 else format2 x2 x1
            else
              readInt2 file
      in
        format2 0 classRangeCount
    ) else 0

(************* Layout tables : GSUB, GPOS, etc. ***************)

let readCoverageIndex file off=
  let format=seek_in file off; readInt2 file in
  let count=readInt2 file in
  let rec format1 i result=
    if i>=count then result else (
      let c=readInt2 file in
        format1 (i+1) ((c,i)::result)
    )
  in
  let rec format2 i result=

    if i>=count then result else (
      let start=readInt2 file in
      let final=readInt2 file in
      let cvIdx=readInt2 file in
      let rec make_range i result=
        if i>final then result else (
          make_range (i+1) ((i,i+cvIdx-start)::result)
        )
      in
        format2 (i+1) (make_range start result)
    )
  in
    if format=1 then format1 0 [] else
      if format=2 then format2 0 [] else
        []

let readClass file off=
  let format=seek_in file off; readInt2 file in
    if format=1 then (
      let startGlyph=readInt2 file in
      let glyphCount=readInt2 file in
      let rec classValues i result=if i>=glyphCount then List.rev result else
        (classValues (i-1) ((startGlyph+i, readInt2 file)::result))
      in
        classValues 0 []
    ) else if format=2 then (
      let classRangeCount=readInt2 file in
      let rec format2 i result=
        if i>=classRangeCount then result else (
          let startGlyph=readInt2 file in
          let endGlyph=readInt2 file in
          let cl=readInt2 file in
          let rec make_range i r= if i>endGlyph then r else (make_range (i+1) ((i,cl)::r)) in
            format2 (i+1) (make_range startGlyph result)
        )
      in
        format2 0 []
    ) else []
(*********************************)


#define GSUB_SINGLE 1
#define GSUB_MULTIPLE 2
#define GSUB_ALTERNATE 3
#define GSUB_LIGATURE 4
#define GSUB_CONTEXT 5
#define GSUB_CHAINING 6


let rec readLookup file gsubOff i=
  let subst=ref [] in
  let lookup= seek_in file (gsubOff+8); readInt2 file in
  let offset0=seek_in file (gsubOff+lookup+2+i*2); gsubOff+lookup+(readInt2 file) in

  let lookupType=seek_in file offset0; readInt2 file in
    (* let lookupFlag=seek_in file (offset0+2); readInt2 file in *)
  let subtableCount=seek_in file (offset0+4); readInt2 file in
    for subtable=0 to subtableCount-1 do
      let offset1=seek_in file (offset0+6+subtable*2); offset0+(readInt2 file) in

        match lookupType with
            GSUB_SINGLE->(
              let format=seek_in file offset1;readInt2 file in
              let coverageOff=readInt2 file in
                if format=1 then (
                  let delta=readInt2 file in
                  let cov=readCoverageIndex file (offset1+coverageOff) in
                    List.iter (fun (a,_)->subst:=(Subst { original_glyphs=[|a|]; subst_glyphs=[|a+delta|]})::(!subst)) cov
                ) else if format=2 then (
                  let cov=readCoverageIndex file (offset1+coverageOff) in
                    List.iter (fun (a,b)->
                                 let gl=seek_in file (offset1+6+b*2); readInt2 file in
                                   subst:=(Subst { original_glyphs=[|a|]; subst_glyphs=[|gl|]})::(!subst)) cov
                )
            )
          | GSUB_MULTIPLE->(
              let coverageOff=seek_in file (offset1+2); readInt2 file in
              let cov=readCoverageIndex file (offset1+coverageOff) in
                List.iter (fun (first_glyph,alternate)->
                             let offset2=seek_in file (offset1+6+alternate*2); offset1+readInt2 file in
                             let glyphCount=seek_in file offset2; readInt2 file in
                             let arr=Array.make glyphCount 0 in
                               for comp=0 to glyphCount-1 do
                                 arr.(comp)<-readInt2 file;
                               done;
                               subst:=(Subst { original_glyphs=[|first_glyph|]; subst_glyphs=arr})::(!subst)
                          ) cov
            )
          | GSUB_ALTERNATE->(
              let coverageOff=seek_in file (offset1+2); readInt2 file in
              let cov=readCoverageIndex file (offset1+coverageOff) in
                List.iter (fun (first_glyph,alternate)->
                             let offset2=seek_in file (offset1+6+alternate*2); offset1+readInt2 file in
                             let glyphCount=seek_in file offset2; readInt2 file in
                             let arr=Array.make (1+glyphCount) first_glyph in
                               for comp=1 to glyphCount do
                                 arr.(comp)<-readInt2 file;
                               done;
                               subst:=(Alternative arr)::(!subst)
                          ) cov
            )
          | GSUB_LIGATURE->(
              let coverageOff=seek_in file (offset1+2); readInt2 file in
              let cov=readCoverageIndex file (offset1+coverageOff) in
                (* let ligSetCount=seek_in file (offset1+4); readInt2 file in *)
                List.iter (fun (first_glyph,ligset)->
                             (* for ligset=0 to ligSetCount-2 do *)
                             let offset2=seek_in file (offset1+6+ligset*2); offset1+readInt2 file in
                             let ligCount=seek_in file offset2; readInt2 file in
                               for lig=0 to ligCount-1 do
                                 let offset3=seek_in file (offset2+2+lig*2); offset2+readInt2 file in
                                 let ligGlyph=seek_in file offset3; readInt2 file in
                                 let compCount=readInt2 file in
                                 let arr=Array.make compCount first_glyph in
                                   for comp=1 to compCount-1 do
                                     arr.(comp)<-readInt2 file
                                   done;
                                   subst:=(Ligature { ligature_glyphs=arr; ligature=ligGlyph })::(!subst)
                               done
                                 (* done *)
                          ) cov
            )
          | GSUB_CONTEXT->(
              let format=seek_in file offset1; readInt2 file in
                if format=1 then (
                  let coverageOff=readInt2 file in
                  let cov=readCoverageIndex file (offset1+coverageOff) in
                    List.iter (fun (first_glyph, subruleSet)->
                                 let offset2=offset1+6+subruleSet*2 in
                                 let subruleCount=seek_in file offset2; readInt2 file in
                                   for j=0 to subruleCount-1 do
                                     let subruleOff=seek_in file (offset2+2+j*2); readInt2 file in

                                     let glyphCount=seek_in file (offset2+subruleOff); readInt2 file in
                                     let substCount=readInt2 file in
                                     let arr=Array.make glyphCount (first_glyph,[]) in
                                       for i=1 to glyphCount-1 do
                                         arr.(i)<-(readInt2 file, [])
                                       done;
                                       for i=0 to substCount do
                                         let seqIdx=readInt2 file in
                                         let lookupIdx=readInt2 file in
                                           arr.(seqIdx)<-(fst arr.(i), (readLookup file gsubOff lookupIdx)@(snd arr.(i)))
                                       done;
                                       subst:=(Context arr)::(!subst)
                                   done
                              ) cov
                ) else if format=2 then (

                ) else if format=3 then (

                )
            )
          | GSUB_CHAINING->(
              let format=seek_in file offset1; readInt2 file in
                if format=1 then (

                ) else if format=2 then (

                ) else if format=3 then (
                  let backCount=readInt2 file in
                  let back_arr=Array.make backCount IntSet.empty in
                    for i=1 to backCount do
                      let covOff=seek_in file (offset1+2+i*2); readInt2 file in
                      let cov=readCoverageIndex file (offset1+covOff) in
                        List.iter (fun (a,_)->back_arr.(backCount-i)<-IntSet.add a back_arr.(backCount-i)) cov
                    done;
                    let offset2=offset1+4+backCount*2 in
                    let inputCount=seek_in file offset2; readInt2 file in
                    let input_arr=Array.make inputCount IntSet.empty in
                      for i=0 to inputCount-1 do
                        let covOff=seek_in file (offset2+2+i*2); readInt2 file in
                        let cov=readCoverageIndex file (offset1+covOff) in
                          List.iter (fun (a,_)->input_arr.(i)<-IntSet.add a input_arr.(i)) cov
                      done;
                      let offset3=offset2+2+inputCount*2 in
                      let aheadCount=seek_in file offset3; readInt2 file in
                      let ahead_arr=Array.make aheadCount IntSet.empty in
                        for i=0 to aheadCount-1 do
                          let covOff=seek_in file (offset3+2+i*2); readInt2 file in
                          let cov=readCoverageIndex file (offset1+covOff) in
                            List.iter (fun (a,_)->ahead_arr.(i)<-IntSet.add a ahead_arr.(i)) cov
                        done;
                        subst:=(Chain {before=back_arr; input=input_arr; after=ahead_arr})::(!subst)
                );
            )
          | _->()
    done;
    List.rev !subst


let read_gsub font=
  let (file,off0)=otype_file font in
  let (gsubOff,_)=tableLookup "GSUB" file off0 in
  let lookup= seek_in file (gsubOff+8); readInt2 file in
  let lookupCount= seek_in file (gsubOff+lookup); readInt2 file in
    (* Iteration sur les lookuptables *)
  let arr=Array.make lookupCount [] in
    for i=0 to lookupCount-1 do
      arr.(i)<-readLookup file gsubOff i
    done;
    arr

let read_lookup font i=
  let (file,off0)=otype_file font in
  let (gsubOff,_)=tableLookup "GSUB" file off0 in
    readLookup file gsubOff i


let str_tag=function
    Alternates -> "aalt"
  | SmallCapitals -> "c2sc"
  | CaseSensitiveForms -> "case"
  | DiscretionaryLigatures -> "dlig"
  | Denominators -> "dnom"
  | Fractions -> "frac"
  | StandardLigatures -> "liga"
  | LiningFigures -> "lnum"
  | LocalizedForms -> "locl"
  | Numerators -> "numr"
  | OldStyleFigures -> "onum"
  | Ordinals -> "odrn"
  | Ornaments -> "ornm"
  | ProportionalFigures -> "pnum"
  | StylisticAlternates -> "salt"
  | ScientificInferiors -> "sinf"
  | SmallCapitals -> "smcp"
  | Subscript -> "subs"
  | Superscript -> "sups"
  | Titling -> "titl"
  | TabularFigures -> "tnum"
  | SlashedZero -> "zero"
  | _ -> ""

let tag_str=function
    "aalt" -> Alternates
  | "c2sc" -> SmallCapitals
  | "case" -> CaseSensitiveForms
  | "dlig" -> DiscretionaryLigatures
  | "dnom" -> Denominators
  | "frac" -> Fractions
  | "liga" -> StandardLigatures
  | "lnum" -> LiningFigures
  | "locl" -> LocalizedForms
  | "numr" -> Numerators
  | "onum" -> OldStyleFigures
  | "ordn" -> Ordinals
  | "ornm" -> Ornaments
  | "pnum" -> ProportionalFigures
  | "salt" -> StylisticAlternates
  | "sinf" -> ScientificInferiors
  | "smcp" -> SmallCapitals
  | "subs" -> Subscript
  | "sups" -> Superscript
  | "titl" -> Titling
  | "tnum" -> TabularFigures
  | "zero" -> SlashedZero
  | x -> raise (Unknown_feature x)

let select_features font feature_tags=
  let (file,off0)=otype_file font in
  let (gsubOff,_)=tableLookup "GSUB" file off0 in
  let features=seek_in file (gsubOff+6); readInt2 file in
  let featureCount=seek_in file (gsubOff+features);readInt2 file in

  let tags_str=List.map str_tag feature_tags in

  let feature_tag=String.create 4 in
  let rec select i result=
    if i>=featureCount then result else (
        seek_in file (gsubOff+features+2+i*6);
        let _=input file feature_tag 0 4 in
        let lookupOff=readInt2 file in
        let lookupCount=seek_in file (gsubOff+features+lookupOff+2); readInt2 file in
        let rec read lookup s=
          if lookup>=lookupCount then s else (
            let l=readInt2 file in read (lookup+1) (IntSet.add l s)
          )
        in
          if List.mem feature_tag tags_str then
            select (i+1) (read 0 result)
          else
            select (i+1) result
    )
  in
    List.concat (List.map (fun lookup->readLookup file gsubOff lookup) (IntSet.elements (select 0 IntSet.empty)))

module FeatSet=Set.Make (struct type t=features let compare=compare end)

let read_features font=
  let (file,off0)=otype_file font in
  let (gsubOff,_)=tableLookup "GSUB" file off0 in
  let features=seek_in file (gsubOff+6); readInt2 file in
  let featureCount=seek_in file (gsubOff+features);readInt2 file in
  let buf=String.create 4 in
  let rec make_features i result=
    if i>=featureCount then result else (
      seek_in file (gsubOff+features+2+i*6);
      let _=input file buf 0 4 in
        try
          make_features (i+1) (FeatSet.add (tag_str buf) result)
        with
            Unknown_feature _ -> make_features (i+1) result
    )
  in
    FeatSet.elements (make_features 0 FeatSet.empty)

let read_scripts font=
  let (file,off0)=otype_file font in
  let (gsubOff,_)=tableLookup "GSUB" file off0 in
  let scripts=seek_in file (gsubOff+4); readInt2 file in
  let scriptCount=seek_in file (gsubOff+scripts); readInt2 file in
  let arr=Array.make scriptCount "" in
    for i=0 to scriptCount-1 do
      let scriptTag=String.create 4 in
        seek_in file (gsubOff+scripts+2+i*6);
        let _=input file scriptTag 0 4 in
        let off=readInt2 file in
          Printf.printf "\n%s\n" scriptTag;
          let offset1=gsubOff+scripts+off in
          let langSysCount=seek_in file (offset1+2); readInt2 file in
            for langSys=0 to langSysCount-1 do
              let langSysTag=String.create 4 in
                seek_in file (offset1+4+langSys*6);
                let _=input file langSysTag 0 4 in
                  Printf.printf "lang : %s\n" langSysTag
          done
    done



let gsub font glyphs0=

  let (file,off0)=otype_file font in
  let (gsubOff,_)=tableLookup "GSUB" file off0 in

  let lookup= seek_in file (gsubOff+8); readInt2 file in
  let lookupCount= seek_in file (gsubOff+lookup); readInt2 file in
  let glyphs=ref glyphs0 in
    (* Iteration sur les lookuptables *)
    for i=1 to lookupCount do
      let offset=seek_in file (gsubOff+lookup+i*2); readInt2 file in

      let lookupType=seek_in file (gsubOff+lookup+offset); readInt2 file in
      (* let lookupFlag=seek_in file (gsubOff+lookup+offset+2); readInt2 file in *)
      let subtableCount=seek_in file (gsubOff+lookup+offset+4); readInt2 file in

      let maxOff=gsubOff+lookup+offset + 6+subtableCount*2 in

      let rec lookupSubtables off gl=
        if off>=maxOff then gl else
          let subtableOff=seek_in file off; readInt2 file in
          let offset=gsubOff+lookup+offset+subtableOff in

          let rec gsub glyphs=match lookupType with
              GSUB_LIGATURE-> (
                match glyphs with
                    []->[],[]
                  | h_::s->(
                      let h=h_.glyph_index in
                        (* let substFormat=seek_in file offset ; readInt2 file in *)
                      let coverageOffset=seek_in file (offset+2); readInt2 file in
                        (* let ligSetCount=readInt2 file in *)
                        try
                          let coverage=coverageIndex file (offset+coverageOffset) h in
                          let ligatureSetOff=seek_in file (offset+6+coverage*2); readInt2 file in
                          let ligatureCount=seek_in file (offset+ligatureSetOff); readInt2 file in
                          let initOff=offset+ligatureSetOff in
                          let rec ligatureSet off i l=match l with
                              []->[],[]
                            | h::s when i=0 -> [h],s
                            | _::s->
                                (let ligOff=seek_in file off; readInt2 file in
                                 let result=seek_in file (initOff+ligOff) ; readInt2 file in
                                 let compCount=readInt2 file in
                                 let buf=UTF8.Buf.create 1 in
                                   UTF8.Buf.add_string buf (h_.glyph_utf8);
                                   let rec compareComps c l=match l with
                                       l' when c<=0 -> true, l'
                                     | []->false, []
                                     | h::s->
                                         (let comp=readInt2 file in
                                            if comp=h.glyph_index then (
                                              UTF8.Buf.add_string buf  h.glyph_utf8;
                                              compareComps (c-1) s
                                            ) else false, [])
                                   in
                                   let applies, next=compareComps (compCount-1) s in
                                     if applies then [{glyph_utf8=UTF8.Buf.contents buf;glyph_index=result}], next else
                                       ligatureSet (off+2) (i-1) l
                                )
                          in
                          let a,b=ligatureSet (offset+ligatureSetOff+2) ligatureCount glyphs in
                          let a',b'=gsub b in
                            a@a', b'
                        with
                            Not_found->
                              (let a,b=gsub s in
                                 (h_::a, b))
                    )
              )

            | _-> [],glyphs
          in

          let u,v=gsub gl in
            u@(lookupSubtables (off+2) v)
      in
        glyphs:=lookupSubtables (gsubOff+lookup+offset + 6) !glyphs
    done;
    !glyphs



#define GPOS_SINGLE 1
#define GPOS_PAIR 2

let rec gpos font glyphs0=
  let (file,off0)=otype_file font in
  let (gposOff,_)=tableLookup "GPOS" file off0 in

  let lookup= seek_in file (gposOff+8); readInt2 file in
  let lookupCount= seek_in file (gposOff+lookup); readInt2 file in
  let glyphs=ref glyphs0 (* (List.map (fun x->GlyphID x) glyphs0) *) in
    (* Iteration sur les lookuptables *)
    for i=1 to lookupCount do
      let offset=seek_in file (gposOff+lookup+i*2); readInt2 file in

      let lookupType=seek_in file (gposOff+lookup+offset); readInt2 file in
      (* let lookupFlag=seek_in file (gposOff+lookup+offset+2); readInt2 file in *)
      let subtableCount=seek_in file (gposOff+lookup+offset+4); readInt2 file in
      let maxOff=gposOff+lookup+offset + 6+subtableCount*2 in

      let rec lookupSubtables off gl=
        if off>=maxOff then gl else
          let subtableOff=seek_in file off; readInt2 file in
          let offset=gposOff+lookup+offset+subtableOff in


          let rec gpos glyphs=
            (* Printf.printf "lookupType=%d\n" lookupType; *)
            match glyphs with

                id_h::id_h'::s->(
                  let h=glyph_id_cont id_h in
                  let h'=glyph_id_cont id_h' in
                  match lookupType with
                      GPOS_PAIR->(
                        let format=seek_in file offset; readInt2 file in
                          (* Printf.printf "format : %d\n" format; *)
                        let coverageOffset=readInt2 file in
                        let valueFormat1=readInt2 file in
                        let valueFormat2=readInt2 file in

                        let rec compute_size x r=if x=0 then r else compute_size (x lsr 1) (r+(x land 1)) in
                        let size1=compute_size valueFormat1 0 in
                        let size2=compute_size valueFormat2 0 in
                        let readAll format gl=
                          { kern_x0=if (format land 0x1) <> 0 then float_of_int (int16 (readInt2 file)) else 0.;
                            kern_y0=if (format land 0x2) <> 0 then float_of_int (int16 (readInt2 file)) else 0.;
                            advance_width=if (format land 0x4) <> 0 then float_of_int (int16 (readInt2 file)) else 0.;
                            advance_height=if (format land 0x8) <> 0 then float_of_int (int16 (readInt2 file)) else 0.;
                            kern_contents=gl }
                        in
                          try
                            let coverage=coverageIndex file (offset+coverageOffset) h in
                              if format=1 then (
                                let rec pairSetTable off0 x0 x1=
                                  let x2=(x0+x1)/2 in
                                  let gl=seek_in file (off0+x2*(1+size1+size2)*2); readInt2 file in
                                    if x1-x0<=1 then
                                      if gl=h' then readAll valueFormat1 id_h, readAll valueFormat2 id_h' else raise Not_found
                                    else
                                      if gl>h' then pairSetTable off0 x0 x2 else pairSetTable off0 x2 x1
                                in
                                let pairSetOffset=seek_in file (offset+10+coverage*2); readInt2 file in
                                let count=seek_in file (offset+pairSetOffset); readInt2 file in
                                let a,b=pairSetTable (offset+pairSetOffset+2) 0 count in
                                  (if valueFormat1<>0 then KernID a else id_h)::
                                    (gpos ((if valueFormat2<>0 then KernID b else id_h')::s))
                              ) else if format=2 then (

                                let classdef1=seek_in file (offset+8); class_def file (offset+readInt2 file) h in
                                let classdef2=seek_in file (offset+10); class_def file (offset+readInt2 file) h' in
                                let class1count=seek_in file (offset+12); readInt2 file in
                                let class2count=seek_in file (offset+14); readInt2 file in
                                  if classdef1>class1count || classdef2>class2count then
                                    glyphs
                                  else
                                    (let index=16
                                       + (class2count*2*(size1+size2))*classdef1
                                       + 2*(size1+size2)*h'
                                     in
                                       seek_in file (offset+index);
                                       let a=readAll valueFormat1 id_h in
                                       let b=readAll valueFormat2 id_h' in
                                         (if valueFormat1<>0 then KernID a else id_h)::
                                           (gpos ((if valueFormat2<>0 then KernID b else id_h')::s))
                                    )
                              ) else glyphs
                          with
                              Not_found->glyphs
                      )
                    | _->glyphs
                )
              | [h]->[h]
              | [] -> []
          in
            lookupSubtables (off+2) (gpos gl)
      in
        glyphs:=lookupSubtables (gposOff+lookup+offset + 6) !glyphs
    done;
    !glyphs


let substitutions=gsub
let positioning=gpos
