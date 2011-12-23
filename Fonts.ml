open Binary

module type Font = sig
  type font
  type glyph
  val loadFont : in_channel->int->font
  val loadGlyph : font->int->int->glyph
  val outlines : glyph->Bezier.curve list
end



module CFF = (struct

      
  exception Index
  exception Type2Int of int
    
  let readType2Int f=
    let b0=(int_of_char (input_char f)) in
      if b0<28 || b0==31 then
        raise (Type2Int b0)
      else
        if b0=30 then
          
          let first=ref true in
          let next=ref 0 in
          let input_next f=
            if !first then
              (let a=int_of_char (input_char f) in
                 first:=false;
                 next:=a land 0xf;
                 a lsr 8)
            else
              (first:=true;
               !next)
          in
            
          let rec parseInt x=
            let a=input_next f in
              if a<=9 then parseInt (x*10+a) else (float_of_int x,a)
          in
          let (a0,b0)=
            let a=input_next f in
              if a = 0xe then let (u,v)=parseInt 0 in (-. u, v) else parseInt a
          in
          let (a1,b1)=
            if b0=0xa then
              let rec makeDecimal x=if (abs_float x)<1. then x else makeDecimal (x /. 10.) in
              let (a1',b1')=parseInt 0 in
                (makeDecimal a1', b1')
            else (0.,b0)
          in
          let (a2,b2)=
            if b1=0xb then parseInt 0 else
              if b1=0xc then
                let (a2',b2')=parseInt 0 in
                  (-. a2',b2')
              else
                (0.,b1)
          in
            (a0+.a1) *. (10. ** a2)
              
        else
          float_of_int (
            if b0>=32 then
              if b0<=246 then b0-139 else 
                let b1=(int_of_char (input_char f)) in
                  if b0<=250 then
                    (((b0-247) lsl 8) lor b1) + 108
                  else
                    - ((((b1-251) lsl 8) lor b1) + 108)
            else
              let b1=(int_of_char (input_char f)) in
              let b2=(int_of_char (input_char f)) in
                if b0=28 then
                  (b1 lsl 8) lor b2
                else
                  let b3=(int_of_char (input_char f)) in
                  let b4=(int_of_char (input_char f)) in
                    (((((b1 lsl 8) lor b2) lsl 8) lor b3) lsl 8) lor b4
          )

  let index f idx_off=
    seek_in f idx_off;
    let count=readInt f 2 in
    let idx_offSize=int_of_char (input_char f) in
    let idx_arr=Array.make (count+1) 0 in
      for i=0 to count do
        idx_arr.(i) <- idx_off+2+(count+1)*idx_offSize + (readInt f idx_offSize);
      done;
      idx_arr
        
  let strIndex f idx_off=
    let off=index f idx_off in
    let str=Array.create (Array.length off-1) "" in
      for i=0 to Array.length off-2 do
        seek_in f (off.(i));
        str.(i)<-String.create (off.(i+1)-off.(i));
        really_input f (str.(i)) 0 (off.(i+1)-off.(i))
      done;
      str
        
  let indexGet f idx_off idx=
    seek_in f idx_off;
    let count=readInt f 2 in
      if idx>=count || idx<0 then raise Not_found else
        (let idx_offSize=int_of_char (input_char f) in
           seek_in f (idx_off+3+idx*idx_offSize);
           let off0=readInt f idx_offSize in
           let off1=readInt f idx_offSize in
           let buf=String.create (off1-off0) in
             seek_in f (idx_off+2+(count+1)*idx_offSize+off0);
             really_input f buf 0 (off1-off0);
             buf
        )
  let dict f a b=
    let rec dict' stack l=
      if pos_in f >= b then l else
        try 
          let op=readType2Int f in
            dict' (op::stack) l
        with
            (Type2Int b0)->
              let op=
                if b0=12 then
                  let next=int_of_char (input_char f) in
                    (12 lsl 8) lor next
                else
                  b0
              in
                dict' [] ((op,stack)::l)
    in
      seek_in f a;
      dict' [] []
        
  let findDict f a b key=
    let rec dict' stack=
      if pos_in f >= b then raise Not_found else
        try 
          let op=readType2Int f in dict' (op::stack)
        with
            (Type2Int b0)->
              let op=
                if b0=12 then
                  let next=int_of_char (input_char f) in (12 lsl 8) lor next
                else
                  b0
              in
                if op=key then stack else dict' []
    in
      seek_in f a;
      dict' [] 


        
        
  type font= { file:in_channel; offset:int; offSize:int; nameIndex:int array;
              dictIndex:int array; stringIndex:int array; subrIndex:(string array) array;
              gsubrIndex:string array }
  let loadFont f off=
    seek_in f (off+2);
    let hdrSize=int_of_char (input_char f) in
    let offSize=int_of_char (input_char f) in
    let nameIndex=index f (off+hdrSize) in
    let dictIndex=index f (nameIndex.(Array.length nameIndex-1)) in
    let stringIndex=index f (dictIndex.(Array.length dictIndex-1)) in
      
    let subrIndex=
      let subrs=Array.create (Array.length dictIndex-1) [||] in
        for idx=0 to Array.length dictIndex-2 do
          let privOffset=findDict f (dictIndex.(idx)) (dictIndex.(idx+1)) 18 in
            match privOffset with
                offset::size::_->
                  let subrsOffset=int_of_float (List.hd (findDict f (off+int_of_float offset) (off+int_of_float (offset+.size)) 19)) in
                    subrs.(idx) <- strIndex f (off+int_of_float offset+subrsOffset)
              | _->()
        done;
        subrs
    in
    let gsubrIndex=
      let offIndex=index f (stringIndex.(Array.length stringIndex-1)) in
      let gsubr=Array.create (Array.length offIndex-1) "" in
        for i=0 to Array.length gsubr-1 do
          seek_in f (offIndex.(i));
          gsubr.(i)<-String.create (offIndex.(i+1)-offIndex.(i));
          really_input f gsubr.(i) 0 (offIndex.(i+1)-offIndex.(i))
        done;
        gsubr
    in
      { file=f; offset=off; offSize=offSize; nameIndex=nameIndex; dictIndex=dictIndex; stringIndex=stringIndex; gsubrIndex=gsubrIndex; subrIndex=subrIndex }
        
  
  type glyph= { type2:string; matrix:float array; subrs:string array; gsubrs:string array }
      
  let loadGlyph font idx gl=
    let charStrings=int_of_float (List.hd (findDict font.file font.dictIndex.(idx) font.dictIndex.(idx+1) 17)) in
    let fontMatrix=
      try
        findDict font.file font.dictIndex.(idx) font.dictIndex.(idx+1) 3079
      with
          Not_found->[0.001;0.;0.;0.001]
    in
      { type2=indexGet font.file (font.offset+charStrings) gl;
        matrix=Array.of_list fontMatrix;
        subrs=font.subrIndex.(idx);
        gsubrs=font.gsubrIndex }

  let outlines gl=
    

        
end:Font)




module Opentype = (struct

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
        

  type font = CFF of CFF.font

  let loadFont f off=
    let typ=String.create 4 in
      seek_in f off;
      really_input f typ 0 4;
      match typ with
          "OTTO"->
            let (a,b)=tableLookup "CFF " f off in
              CFF (CFF.loadFont f (off+a))
        | _->failwith ("OpenType : format non reconnu : "^typ)

  type glyph = CFFGlyph of CFF.glyph

  let loadGlyph f idx gl=
    match f with
        CFF x->CFFGlyph (CFF.loadGlyph x idx gl)

  let outlines gl=match gl with
      CFFGlyph x->CFF.outlines x
          
end:Font)
