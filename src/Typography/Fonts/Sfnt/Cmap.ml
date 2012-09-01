open CamomileLibrary
open Util

let write_cmap_table cmap format lang file=
  match format with
      0->(
        bufInt2 file 0;               (* format *)
        bufInt2 file 262;             (* length *)
        bufInt2 file lang;            (* language *)
        for i=0 to 255 do
          try
            let x=IntMap.find i cmap in
            bufInt1 file x
          with
              Not_found->bufInt1 file 0
        done
      )

    | 2->(
      (* Il faut commencer par trier les codes de caractères par
         premier octet, puis à préparer les structures pour réutiliser
         des rangs *)
      let firstCode=String.create 256 in
      let entryCount=String.make 256 (char_of_int 0) in
      let idDelta=Array.make 256 0 in
      let arr=Array.init 256 (fun _->Buffer.create 0) in
      let last=String.make 256 (char_of_int 0) in
      let sh_length=ref 0 in            (* Longueur des subHeaders *)
      let ga_length=ref 0 in            (* Longueur du glyphIndexArray *)

      IntMap.fold (fun k a _->
        if k<=0xffff && a<=0xffff then (
          let i=k lsr 8 in
          (if int_of_char (entryCount.[i])=0 then (
            firstCode.[i]<-char_of_int (k land 0xff);
            idDelta.(i)<-a;
            sh_length:= !sh_length+8
           ));
          for j=int_of_char last.[i]+1 to (k land 0xff)-1 do
            Buffer.add_char arr.(i) (char_of_int 0);
            Buffer.add_char arr.(i) (char_of_int 0);
            ga_length:= !ga_length+2
          done;
          last.[i]<-char_of_int (k land 0xff);

          entryCount.[i]<-char_of_int ((k land 0xff) - int_of_char firstCode.[i] + 1);
          let x=a-idDelta.(i) in
          Buffer.add_char arr.(i) (char_of_int (x lsr 8));
          Buffer.add_char arr.(i) (char_of_int (x land 0xff));
          ga_length:= !ga_length+2
        )
      ) cmap ();

      (* Ecriture de la table *)
      bufInt2 file 2;                                       (* format *)
      bufInt2 file (518+ !sh_length+ !ga_length);           (* length *)
      bufInt2 file lang;                                    (* langage *)
      let indexArrays=ref StrMap.empty in
      let subHeaders=
        if !sh_length<8*256 then(
          sh_length:= !sh_length+8;
          let buf=Buffer.create !sh_length in
          for i=0 to 7 do Buffer.add_char buf (char_of_int 0) done;
          buf
        ) else
          Buffer.create !sh_length
      in
      let glyphIndex=Buffer.create !ga_length in
      let buffers=Array.map Buffer.contents arr in
      (* Ecriture des subHeaderKeys, il y en a toujours 256 *)
      for i=0 to 255 do
        if String.length buffers.(i)=0 then (
          bufInt2 file 0
        ) else (
          bufInt2 file (Buffer.length subHeaders);
          let idRangeOffset_=           (* Offset par rapport au début du glyphIndexArray *)
            try StrMap.find buffers.(i) !indexArrays with
                Not_found->(
                  indexArrays:=StrMap.add buffers.(i) (Buffer.length glyphIndex) !indexArrays;
                  let x=Buffer.length glyphIndex in
                  Buffer.add_string glyphIndex buffers.(i);
                  x
                )
          in
          let idRangeOffset= (!sh_length - (Buffer.length subHeaders+8)) + idRangeOffset_ in
          (* firstCode *)
          Buffer.add_char subHeaders (char_of_int 0);
          Buffer.add_char subHeaders (firstCode.[i]);
          (* entryCount *)
          Buffer.add_char subHeaders (char_of_int 0);
          Buffer.add_char subHeaders (entryCount.[i]);
          (* idDelta *)
          Buffer.add_char subHeaders (char_of_int (idDelta.(i) lsr 8));
          Buffer.add_char subHeaders (char_of_int (idDelta.(i) land 0xff));
          (* idRangeOffset *)
          Buffer.add_char subHeaders (char_of_int (idRangeOffset lsr 8));
          Buffer.add_char subHeaders (char_of_int (idRangeOffset land 0xff));
        )
      done;
      (* Ecriture des subHeaders *)
      Buffer.add_buffer file subHeaders;
      (* Ecriture du glyphIndexArray *)
      Buffer.add_buffer file glyphIndex
    )
    | 4->(
      let bind=IntMap.bindings cmap in
      let rec make_ranges lastk ranges segCount l=match l with
          [] when lastk=0xffff->(ranges, segCount)
        | [] ->([0xffff,0]::ranges, 1+segCount)
        | (k,a)::s->(
          match ranges with
              rh::rs when k<=lastk+8->
                make_ranges
                  k
                  (((k,a)::rh)::rs)
                  segCount
                  s
            | _->
                make_ranges
                  k
                  ([k,a]::ranges)
                  (segCount+1)
                  s
        )
      in
      let _ranges,segCount=make_ranges 0 [] 0 bind in
      let ranges=List.rev (
        List.map (fun r->
          let rr=List.rev r in
          (fst (List.hd rr), fst (List.hd r), rr)
        ) _ranges)
      in

      bufInt2 file 4;
      bufInt2 file (16+8*segCount+2*IntMap.cardinal cmap);
      bufInt2 file lang;

      let segCountX2=segCount lsl 1 in
      let rec log2 x y=if x<=1 then y else log2 (x lsr 1) (y+1) in
      let entrySel=log2 segCount 0 in
      let searchRange=(2 lsl entrySel) in
      let rangeShift=segCountX2 - searchRange in

      bufInt2 file segCountX2;
      bufInt2 file searchRange;
      bufInt2 file entrySel;
      bufInt2 file rangeShift;
      List.iter (fun (_,x,_)->bufInt2 file x) ranges; (* endCount *)
      bufInt2 file 0;                                 (* Reserved padding ! *)
      List.iter (fun (x,_,_)->bufInt2 file x) ranges; (* startCount *)
      List.iter (fun _->bufInt2 file 0) ranges;       (* idDelta *)

      let off=ref (segCount*2) in
      List.iter (fun (u,v,x)->
        bufInt2 file !off;
        off:= !off+(v-u+1)*2 - 2;
      ) ranges; (* idRangeOffset *)
      List.iter (fun (u,v,x)->
        let rec write_range k l=
          if k>v then () else (
            match l with
                []->(
                  bufInt2 file 0;
                  write_range (k+1) []
                )
              | (h,a)::s when h=k->(
                bufInt2 file a;
                write_range (k+1) s
              )
              | _->(
                bufInt2 file 0;
                write_range (k+1) l
              )
          )
        in
        write_range u x;
      ) ranges; (* glyphIdArray *)
    )
    | 6->(
      let rec find_min m=
        let a,b=IntMap.min_binding m in
        if a>0xffff || b>0xffff then find_min (IntMap.remove a m) else a
      in
      let m_min=find_min cmap in
      let rec find_max m=
        let a,b=IntMap.max_binding m in
        if a>0xffff || b>0xffff then find_max (IntMap.remove a m) else a
      in
      let m,_,_=IntMap.split 0x10000 cmap in
      let m_max=find_max m in

      bufInt2 file 6;
      bufInt2 file ((m_max-m_min+1) lsl 1 + 10);
      bufInt2 file lang;
      bufInt2 file m_min;
      bufInt2 file (m_max-m_min+1);
      for i=m_min to m_max do
        bufInt2 file (try IntMap.find i cmap with Not_found->0)
      done
    )
    | _ -> failwith ("write_cmap : unknown format "^string_of_int format)

let write_cmap ?(formats=[0;4;6]) ?(lang=0) cmap buf0=
  let bufs=List.map (fun x->
    let buf=Buffer.create 256 in
    write_cmap_table cmap x lang buf;
    buf) formats
  in
  bufInt2 buf0 0;
  bufInt2 buf0 (List.length formats);
  let total=ref (4+List.length formats*8) in
  List.iter (fun b->
    bufInt2 buf0 3;                     (* windows *)
    bufInt2 buf0 1;                     (* unicode bmp *)
    bufInt4 buf0 (!total);
    total:= !total+Buffer.length b
  ) bufs;
  List.iter (Buffer.add_buffer buf0) bufs

let cmap file a char=
  seek_in file (a+2);
  let numTables=readInt2 file in
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
            let idDelta=seek_in file (subHeaders+4); readInt2 file in
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
