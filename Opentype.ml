open Constants
open Binary
open CFF
open CamomileLibrary
let offsetTable=12
let dirSize=16
  
exception Table_not_found

let tableLookup table file off=
  seek_in file (off+4);
  let numTables=readInt file 2 in
  let tableName="    " in
  let rec lookup i j=
    let middle=(i+j) / 2 in
      seek_in file (off+offsetTable+middle*dirSize);
      really_input file tableName 0 4;
      
      if middle<=i then
        if tableName=table then
          ((seek_in file (off+offsetTable+i*dirSize+8);readInt file 4),
           (seek_in file (off+offsetTable+i*dirSize+12);readInt file 4))
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
  let numTables=readInt file 2 in
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

let glyph_of_char font char0=
  match font with
      CFF (font,offset0)->
        let file=font.file in
        let char=UChar.code char0 in
        let (a,b)=tableLookup "cmap" file offset0 in
          seek_in file (a+2);
          let numTables=readInt file 4 in
          let table=ref 0 in
          let cid=ref 0 in

            while !cid=0 && !table<numTables do
              
              seek_in file (a+8+8* !table);
              let offset=a+readInt file 4 in
                
                seek_in file offset;
                let t=readInt file 2 in
                  (match t with
                       0->if char<256 then (
                         seek_in file (offset+6+char);
                         cid:=input_byte file)
                     | 2->
                         (let i=(char lsr 8) land 0xff in
                          let j=char land 0xff in
                          let k=(seek_in file (offset+6+i*2); readInt file 2) lsr 3 in
                          let subHeaders=offset+6+256*2+k*8 in
                            if k=0 then
                              (seek_in file (subHeaders+6);
                               let idRangeOffset=readInt file 2 in
                                 seek_in file (subHeaders+idRangeOffset+i*2);
                                 cid:=readInt file 2
                              )
                            else
                              (let firstCode=seek_in file subHeaders; readInt file 2 in
                               let entryCount=seek_in file (subHeaders+2); readInt file 2 in
                               let idDelta=seek_in file (subHeaders+4); readInt file 2 in
                               let idRangeOffset=seek_in file (subHeaders+6); readInt file 2 in
                                 if j>=firstCode && j < (firstCode+entryCount) then
                                   (let p=seek_in file (subHeaders+8+idRangeOffset+j*2); readInt file 2 in
                                      cid:=if p=0 then p else p+idDelta))
                         )
                     | 4->
                         (let sc2=seek_in file (offset+6); readInt file 2 in
                          let rec smallestEnd i j=
                            if j<=i then i else
                              let middle=((i+j) lsr 1) land 0xfffe in
                              let end_=seek_in file (offset+14+middle); readInt file 2 in
                                if char>end_ then
                                  smallestEnd (middle+2) j
                                else
                                  smallestEnd i middle
                          in
                          let seg=smallestEnd 0 (sc2-2) in
                          let start=seek_in file (offset+16+sc2+seg); readInt file 2 in
                          let delta=seek_in file (offset+16+2*sc2+seg); readInt file 2 in
                          let p_idrOffset=offset+16+3*sc2+seg in
                          let idrOffset=seek_in file p_idrOffset; readInt file 2 in
                            if char<start then table:=numTables else
                              if idrOffset=0 then
                                cid:=(char+delta) land 0xffff
                              else
                                (seek_in file (idrOffset+2*(char-start)+p_idrOffset); cid:=readInt file 2)
                         )
                     | 6->
                         (let first=seek_in file (offset+6); readInt file 2 in
                          let entryCount=seek_in file (offset+8); readInt file 2 in
                            if first<=char && char <first+entryCount then
                              (seek_in file (offset+10+(char-first)*2); cid:=readInt file 2)
                         )
                     | x->failwith ("cmap : type "^(string_of_int t)^" unsupported (yet)")
                  );
                  incr table
            done;
            !cid
              

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
        (let num=CFF.glyphNumber x in
         let (a,_)=tableLookup "hhea" f.CFF.file offset in
         let nh=(seek_in (f.CFF.file) (a+34); readInt f.CFF.file 2) in
         let (b,_)=tableLookup "hmtx" f.CFF.file offset in
           seek_in (f.CFF.file) (if num>nh then b+4*(nh-1) else b+4*num);
           (float_of_int (readInt f.CFF.file 2)))
          
let fontName ?index:(idx=0) f =
  match f with
      CFF (x,_)->CFF.fontName x ~index:idx

let otype_file font=match font with
    CFF (font,offset0)->font.file, offset0





(************* Layout tables : GSUB, GPOS, etc. ***************)



let lookups font apply glyphs0=
  let (file,off0)=otype_file font in
  let (gsubOff,_)=tableLookup "GSUB" file off0 in
    
  let lookup= seek_in file (gsubOff+8); readInt file 2 in
  let lookupCount= seek_in file (gsubOff+lookup); readInt file 2 in
  let glyphs=ref glyphs0 in
    (* Iteration sur les lookuptables *)
    for i=1 to lookupCount do
      
      let offset=seek_in file (gsubOff+lookup+i*2); readInt file 2 in

      let lookupType=seek_in file (gsubOff+lookup+offset); readInt file 2 in
      (* let lookupFlag=seek_in file (gsubOff+lookup+offset+2); readInt file 2 in *)
      let subtableCount=seek_in file (gsubOff+lookup+offset+4); readInt file 2 in

      let maxOff=gsubOff+lookup+offset + 6+subtableCount*2 in

      let rec lookupSubtables off gl=
        if off>=maxOff then gl else
          let subtableOff=seek_in file off; readInt file 2 in

          let u,v=apply file (gsubOff+lookup+offset+subtableOff) lookupType gl in
            u@(lookupSubtables (off+2) v)
      in
        glyphs:=lookupSubtables (gsubOff+lookup+offset + 6) !glyphs
    done;
    !glyphs


let coverageIndex file off glyph=
  let format=seek_in file off; readInt file 2 in
  let count=readInt file 2 in
  let rec format1 x0 x1=
    if x0>=x1 then raise Not_found else
      if x1=x0+1 then
        (let x2=(x0+x1)/2 in
         let current=seek_in file (off+4+2*x2); readInt file 2 in
           if current=glyph then x2 else raise Not_found)
      else
        (let x2=(x0+x1)/2 in
         let current=seek_in file (off+4+2*x2); readInt file 2 in
           if glyph<current then format1 x0 x2 else format1 x2 x1)
  in
  let rec format2 x0 x1=
    if x0>=x1 then raise Not_found else
      if x1=x0+1 then
        (let start=seek_in file (off+6*x0+4); readInt file 2 in
         let final=readInt file 2 in
         let cvIdx=readInt file 2 in
           if glyph>=start && glyph<=final then
             cvIdx+glyph-start
           else
             raise Not_found)

      else
        
        (let x2=(x0+x1)/2 in
         let final=seek_in file (off+6*x0+6); readInt file 2 in
           if glyph>final then
             format2 x2 x1
           else
             format2 x0 x2)
  in    
    if format=1 then format1 0 count else
      if format=2 then format2 0 count else
        (Printf.printf "format : %d\n" format; raise Not_found)


#define LIGATURE 4

let rec gsub file offset lookupType glyphs=
  if lookupType <> LIGATURE then [],glyphs else (
    match glyphs with
        []->[],[]
      | h::s->(
          (* let substFormat=seek_in file offset ; readInt file 2 in *)
          let coverageOffset=seek_in file (offset+2); readInt file 2 in
          (* let ligSetCount=readInt file 2 in *)
            try
              let coverage=coverageIndex file (offset+coverageOffset) h in
              let ligatureSetOff=seek_in file (offset+6+coverage*2); readInt file 2 in
              let ligatureCount=seek_in file (offset+ligatureSetOff); readInt file 2 in
              let initOff=offset+ligatureSetOff in
              let rec ligatureSet off i l=match l with
                  []->[],[]
                | h::s when i=0 -> [h],s
                | h::s->
                    (let ligOff=seek_in file off; readInt file 2 in
                     let result=seek_in file (initOff+ligOff) ; readInt file 2 in
                     let compCount=readInt file 2 in
                     let rec compareComps c l=match l with
                         l' when c<=0 -> true, l'
                       | []->false, []
                       | h::s->
                           (let comp=readInt file 2 in
                              if comp=h then compareComps (c-1) s else false, [])
                     in
                     let applies, next=compareComps (compCount-1) s in
                       if applies then [result], next else
                         ligatureSet (off+2) (i-1) l
                    )
              in
              let a,b=ligatureSet (offset+ligatureSetOff+2) ligatureCount glyphs in
              let a',b'=gsub file offset LIGATURE b in
                a@a', b'
                                     
            with
                Not_found->
                  (let a,b=gsub file offset LIGATURE s in
                     (h::a, b))
        )
  )
