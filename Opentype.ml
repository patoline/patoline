open Binary
open CFF
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
      
      
type font = CFF of (CFF.font*string*int*int)
  
let loadFont ?offset:(off=0) file=
  let f=open_in file in
  let typ=String.create 4 in
    seek_in f off;
    really_input f typ 0 4;
    match typ with
        "OTTO"->
          let (a,b)=tableLookup "CFF " f off in
            CFF (CFF.loadFont file ~offset:(off+a),file,off+a,b)
      | _->failwith ("OpenType : format non reconnu : "^typ)
          
type glyph = CFFGlyph of (font*CFF.glyph)
let glyphFont f=match f with
    CFFGlyph (x,_)->x
  
let loadGlyph f ?index:(idx=0) gl=
  match f with
      CFF (x,_,_,_)->CFFGlyph (f, CFF.loadGlyph x ~index:idx gl)
        
let outlines gl=match gl with
    CFFGlyph (_,x)->CFF.outlines x
          
let fontName ?index:(idx=0) f =
  match f with
      CFF (x,_,_,_)->CFF.fontName x ~index:idx
