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
open CamomileLibrary
open UsualMake
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
      let arr=Array.init 256 (fun _->Rbuffer.create 0) in
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
            Rbuffer.add_char arr.(i) (char_of_int 0);
            Rbuffer.add_char arr.(i) (char_of_int 0);
            ga_length:= !ga_length+2
          done;
          last.[i]<-char_of_int (k land 0xff);

          entryCount.[i]<-char_of_int ((k land 0xff) - int_of_char firstCode.[i] + 1);
          let x=a-idDelta.(i) in
          Rbuffer.add_char arr.(i) (char_of_int (x lsr 8));
          Rbuffer.add_char arr.(i) (char_of_int (x land 0xff));
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
          let buf=Rbuffer.create !sh_length in
          for i=0 to 7 do Rbuffer.add_char buf (char_of_int 0) done;
          buf
        ) else
          Rbuffer.create !sh_length
      in
      let glyphIndex=Rbuffer.create !ga_length in
      let buffers=Array.map Rbuffer.contents arr in
      (* Ecriture des subHeaderKeys, il y en a toujours 256 *)
      for i=0 to 255 do
        if String.length buffers.(i)=0 then (
          bufInt2 file 0
        ) else (
          bufInt2 file (Rbuffer.length subHeaders);
          let idRangeOffset_=           (* Offset par rapport au début du glyphIndexArray *)
            try StrMap.find buffers.(i) !indexArrays with
                Not_found->(
                  indexArrays:=StrMap.add buffers.(i) (Rbuffer.length glyphIndex) !indexArrays;
                  let x=Rbuffer.length glyphIndex in
                  Rbuffer.add_string glyphIndex buffers.(i);
                  x
                )
          in
          let idRangeOffset= (!sh_length - (Rbuffer.length subHeaders+8)) + idRangeOffset_ in
          (* firstCode *)
          Rbuffer.add_char subHeaders (char_of_int 0);
          Rbuffer.add_char subHeaders (firstCode.[i]);
          (* entryCount *)
          Rbuffer.add_char subHeaders (char_of_int 0);
          Rbuffer.add_char subHeaders (entryCount.[i]);
          (* idDelta *)
          Rbuffer.add_char subHeaders (char_of_int (idDelta.(i) lsr 8));
          Rbuffer.add_char subHeaders (char_of_int (idDelta.(i) land 0xff));
          (* idRangeOffset *)
          Rbuffer.add_char subHeaders (char_of_int (idRangeOffset lsr 8));
          Rbuffer.add_char subHeaders (char_of_int (idRangeOffset land 0xff));
        )
      done;
      (* Ecriture des subHeaders *)
      Rbuffer.add_buffer file subHeaders;
      (* Ecriture du glyphIndexArray *)
      Rbuffer.add_buffer file glyphIndex
    )
    | 4->(
      let _,_,cmap=IntMap.split 0x19 cmap in
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
      let _ranges,segCount=make_ranges 0 [] 0 (IntMap.bindings cmap) in
      let ranges=List.rev (
        List.map (fun r->
          let rr=List.rev r in
          (fst (List.hd rr), fst (List.hd r), rr)
        ) _ranges)
      in

      let segCountX2=segCount lsl 1 in
      let rec log2 x y=if x<=1 then y else log2 (x lsr 1) (y+1) in
      let entrySel=log2 segCount 0 in
      let searchRange=(2 lsl entrySel) in
      let rangeShift=segCountX2 - searchRange in

      let buf_segments=Rbuffer.create 256 in

      bufInt2 buf_segments lang;
      bufInt2 buf_segments segCountX2;
      bufInt2 buf_segments searchRange;
      bufInt2 buf_segments entrySel;
      bufInt2 buf_segments rangeShift;
      List.iter (fun (_,x,_)->bufInt2 buf_segments x) ranges; (* endCount *)
      bufInt2 buf_segments 0;                                 (* Reserved padding ! *)
      List.iter (fun (x,_,_)->bufInt2 buf_segments x) ranges; (* startCount *)

      let off=ref segCountX2 in
      let buf_idDelta=Rbuffer.create 256 in
      let buf_idRangeOffset=Rbuffer.create 256 in
      let buf_array=Rbuffer.create 256 in
      List.iter (fun (u,v,x)->
        (* if List.length x=v-u+1 then ( *)
        (*   flush stderr; *)
        (*   let delta=snd (List.hd x) - u in *)
        (*   Printf.fprintf stderr "delta : %d\n" delta; *)
        (*   bufInt2 buf_idDelta (if delta<0 then delta+0x8000 else delta);    (\* idDelta *\) *)
        (*   bufInt2 buf_idRangeOffset 0;                                      (\* idRangeOffset *\) *)
        (*   off:= !off-2 *)
        (* ) else *) (
          bufInt2 buf_idDelta 0;                      (* idDelta *)
          bufInt2 buf_idRangeOffset (!off+Rbuffer.length buf_array); (* idRangeOffset *)
          off:= !off-2;
          let rec write_range k l=
            if k>v then () else (
              match l with
                  (h,a)::s when h=k->(
                    bufInt2 buf_array a;
                    write_range (k+1) s
                   )
                | _->(
                  bufInt2 buf_array 0;
                  write_range (k+1) l
                )
            )
          in
          write_range u x;
        )
      ) ranges;


      Rbuffer.add_buffer buf_segments buf_idDelta;
      Rbuffer.add_buffer buf_segments buf_idRangeOffset;
      Rbuffer.add_buffer buf_segments buf_array;

      bufInt2 file 4;
      bufInt2 file (4+Rbuffer.length buf_segments);
      Rbuffer.add_buffer file buf_segments
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
    let buf=Rbuffer.create 256 in
    write_cmap_table cmap x lang buf;
    buf) formats
  in
  bufInt2 buf0 0;
  bufInt2 buf0 (List.length formats);
  let total=ref (4+List.length formats*8) in
  List.iter (fun b->
    bufInt2 buf0 3;                     (* windows *)
    bufInt2 buf0 1;                     (* unicode bmp *)
    bufInt4_int buf0 (!total);
    total:= !total+Rbuffer.length b
  ) bufs;
  List.iter (Rbuffer.add_buffer buf0) bufs


let read_cmap file a=
  seek_in file (a+2);
  let numTables=readInt2 file in
  let rec read_cmap table cmap=
    if table>=numTables then cmap else (
      seek_in file (a+8+8*table);
      let offset=a+readInt4_int file in
      seek_in file offset;
      let t=readInt2 file in
      (match t with
          0->(
            let rec read_glyphs i m=
              if i>=256 then m else
                read_glyphs (i+1) (IntMap.add i (input_byte file) m)
            in
            seek_in file (offset+6);
            read_cmap (table+1) (read_glyphs 0 cmap)
          )
        | 2->
          (let rec read_subHeaders i m=
             if i>=256 then m else (
               let k=(seek_in file (offset+6+i*2); readInt2 file) lsr 3 in
               let subHeaders=offset+6+256*2+k*8 in
               let firstCode=seek_in file subHeaders; readInt2 file in
               let entryCount=seek_in file (subHeaders+2); readInt2 file in
               let idDelta=seek_in file (subHeaders+4); readInt2 file in
               let idDelta=if idDelta>=0x8000 then idDelta-0x8000 else idDelta in
               let idRangeOffset=seek_in file (subHeaders+6); readInt2 file in
               let rec read_subArray j mm=
                 if k=0 then (
                   if j>=firstCode+entryCount then mm else (
                     let p=seek_in file (subHeaders+8+idRangeOffset+j*2); readInt2 file in
                     read_subArray (j+1)
                       (if p=0 then mm else IntMap.add j (idDelta+p) mm)
                   )
                 ) else (
                   if j>=firstCode+entryCount then mm else (
                     let p=seek_in file (subHeaders+8+idRangeOffset+j*2); readInt2 file in
                     read_subArray (j+1)
                       (if p=0 then mm else IntMap.add ((i lsl 8) lor j) (idDelta+p) mm)
                   )
                 )
               in
               read_subHeaders (i+1) (read_subArray firstCode m)
             )
           in
           read_cmap (table+1) (read_subHeaders 0 cmap)
          )
        | 4->
          (let sc2=seek_in file (offset+6); readInt2 file in
           let rec read_glyphs i m=
             if i>=sc2 then m else (
               seek_in file (offset+14+i);
               let endCount=readInt2 file in
               seek_in file (offset+16+sc2+i);
               let startCount=readInt2 file in
               seek_in file (offset+16+2*sc2+i);
               let idDelta=readInt2 file in
               let idDelta=if idDelta>=0x8000 then idDelta-0x8000 else idDelta in
               let p_idrOffset=offset+16+3*sc2+i in
               seek_in file p_idrOffset;
               let idRangeOffset=readInt2 file in

               let rec read_range c mm=
                 if c>endCount then mm else (
                   if idRangeOffset=0 then (
                     if (idDelta+c) mod 0x8000 = 0 then
                       read_range (c+1) mm
                     else
                       read_range (c+1) (IntMap.add c ((idDelta+c) mod 0x8000) mm)
                   ) else (
                     seek_in file (idRangeOffset+2*(c-startCount)+p_idrOffset);
                     let g=readInt2 file in
                     read_range (c+1)
                       (if (idDelta+g) mod 0x8000 =0 then mm else
                           IntMap.add c ((idDelta+g) mod 0x8000) mm)
                   )
                 )
               in
               read_glyphs (i+2) (read_range startCount m)
             )
           in
           read_cmap (table+1) (read_glyphs 0 cmap)
          )
        | 6->
          (seek_in file (offset+6);
           let firstCode=readInt2 file in
           let entryCount=readInt2 file in
           let rec read i m=
             if i>=entryCount then m else (
               let x=readInt2 file in
               read (i+1) (if x=0 then m else IntMap.add (firstCode+i) x m)
             )
           in
           read_cmap (table+1) (read 0 cmap)
          )
        | x->read_cmap (table+1) cmap
      )
    )
  in
  read_cmap 0 IntMap.empty
