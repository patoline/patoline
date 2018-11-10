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

open Patutil.Extra
open FUtil

(* Deux scripts ne peuvent pas avoir un langage en commun (ou alors il
   faut le dupliquer, sinon on ne peut pas calculer les offsets
   relatifs au début de la table de scripts/de langage.
*)

type language_system={
  features:int list;
  req_feature:int
}
type scripts_table=
    (language_system StrMap.t               (* un système de langage *)
    ) StrMap.t                              (* une table de scripts *)

type feature={ tag:string; lookups:int list }

let write_layout (scripts:scripts_table) features lookups=
  let buf_scripts=Buffer.create 256 in

  (* Premère étape : écrire les scripts et les langages *)
  begin
    let buf_scriptTable=Buffer.create 256 in
    let buf_languageSys=Buffer.create 256 in
    bufInt2 buf_scripts (StrMap.cardinal scripts);
    StrMap.iter (fun script languageSystems->
      (* script tag, en quatre lettres *)
      for i=0 to min 3 (String.length script-1) do
        Buffer.add_char buf_scripts script.[i]
      done;
      for _ = String.length script to 3 do
        Buffer.add_char buf_scripts ' '
      done;
      (* script offset *)
      bufInt2 buf_scripts (2+(StrMap.cardinal scripts)*6+Buffer.length buf_scriptTable);
      bufInt2 buf_scriptTable 0;          (* DefaultLangSys *)
      bufInt2 buf_scriptTable (StrMap.cardinal languageSystems);
      StrMap.iter (fun lang b->
        (* language tag, en quatre lettres *)
        for i=0 to min 3 (String.length lang-1) do
          Buffer.add_char buf_scriptTable lang.[i]
        done;
        for _ = String.length lang to 3 do
          Buffer.add_char buf_scriptTable ' '
        done;
        (* language offsets *)
        bufInt2 buf_scriptTable (6+(StrMap.cardinal scripts)*6
                                 +(StrMap.cardinal languageSystems)*6
                                 +Buffer.length buf_languageSys);
        (* langSysTable *)
        let feat=List.filter (fun x->
          x<Array.length features &&
            x<>b.req_feature) b.features
        in
        bufInt2 buf_languageSys 0;
        bufInt2 buf_languageSys
          (if b.req_feature<Array.length features then b.req_feature
           else 0xffff);
        bufInt2 buf_languageSys (List.length feat);
        List.iter (bufInt2 buf_languageSys) feat
      ) languageSystems
    ) scripts;
    Buffer.add_buffer buf_scripts buf_scriptTable;
    Buffer.add_buffer buf_scripts buf_languageSys
  end;

  let buf_features=Buffer.create 256 in
  begin
    let buf_featureTable=Buffer.create 256 in
    bufInt2 buf_features (Array.length features);
    for i=0 to Array.length features-1 do

      (* featureList table *)
      let tag=(features.(i)).tag in
      for i=0 to min 3 (String.length tag-1) do
        Buffer.add_char buf_features tag.[i]
      done;
      for _ = String.length tag to 3 do
        Buffer.add_char buf_features ' '
      done;
      bufInt2 buf_features (2+(Array.length features)*6
                            +Buffer.length buf_featureTable);

      (* feature table *)
      let lookups=(features.(i)).lookups in
      bufInt2 buf_featureTable 0;
      bufInt2 buf_featureTable (List.length lookups);
      List.iter (bufInt2 buf_featureTable) lookups
    done;
    Buffer.add_buffer buf_features buf_featureTable
  end;

  let buf_lookups=Buffer.create 256 in
  begin
    bufInt2 buf_lookups (Array.length lookups);
    let buf_lookupTable=Buffer.create 256 in
    for i=0 to Array.length lookups-1 do
      bufInt2 buf_lookups (2+2*(Array.length lookups)+Buffer.length buf_lookupTable);
      Buffer.add_buffer buf_lookupTable lookups.(i)
    done;
    Buffer.add_buffer buf_lookups buf_lookupTable
  end;

  let global_buf=Buffer.create 256 in
  bufInt4 global_buf 0x00010000l;
  bufInt2 global_buf 10;
  let a=10+Buffer.length buf_scripts in
  let b=a+Buffer.length buf_features in
  bufInt2 global_buf a;
  bufInt2 global_buf b;
  Buffer.add_buffer global_buf buf_scripts;
  Buffer.add_buffer global_buf buf_features;
  Buffer.add_buffer global_buf buf_lookups;
  Buffer.contents global_buf

#define RIGHT_TO_LEFT             0x0001
#define IGNORE_BASE_GLYPHS        0x0002
#define IGNORE_LIGATURES          0x0004
#define IGNORE_MARKS              0x0008
#define USE_MARK_FILTERING_SET    0x0010
(* #define RESERVED 0x00E0 *)
#define MARK_ATTACHMENT_TYPE      0xFF00

(* Format 1 de coverage *)
let coverage cov=
  let buf=Buffer.create (4+2*IntMap.cardinal cov) in
  bufInt2 buf 1;
  bufInt2 buf (IntMap.cardinal cov);
  IntMap.iter (fun k _->bufInt2 buf k) cov;
  buf

let make_ligatures ligarray=
  let buf=Buffer.create 256 in
  bufInt2 buf 4;                        (* ligature *)
  bufInt2 buf 0;                        (* flags *)
  bufInt2 buf (Array.length ligarray);  (* subtable count *)

  let buf_subtables=Buffer.create 256 in

  for i=0 to Array.length ligarray-1 do
    bufInt2 buf (8+Buffer.length buf_subtables);   (* offset vers la subtable *)

    let ligatures=ligarray.(i) in
    let ligs=List.fold_left (fun m (lig,gl)->
      match lig with
          h::s->
            let autres=try IntMap.find h m with Not_found->[] in
            IntMap.add h ((s,gl)::autres) m
        | _->m
    ) IntMap.empty ligatures
    in


    (* Maintenant on écrit la subtable *)

    let coverage_buffer=coverage ligs in
    bufInt2 buf 1;                               (* SubstFormat *)
    bufInt2 buf (6+2*IntMap.cardinal ligs);      (* coverage *)
    bufInt2 buf (IntMap.cardinal ligs);          (* ligSetCount *)
    let ligatureset_buf=Buffer.create 256 in
    IntMap.iter (fun _ l->
      bufInt2 buf_subtables (6+2*IntMap.cardinal ligs
                             +Buffer.length coverage_buffer
                             +Buffer.length ligatureset_buf);

      (* l est un ensemble de ligatures commençant par le même glyph *)
      bufInt2 ligatureset_buf (List.length l);
      let ligs_buf=Buffer.create 256 in
      let off0=2+2*(List.length l) in
      List.iter (fun (suite,gl)->
        bufInt2 ligatureset_buf (off0+Buffer.length ligs_buf);
        bufInt2 ligs_buf gl;
        bufInt2 ligs_buf (1+List.length suite);
        List.iter (bufInt2 ligs_buf) suite
      ) l;
      Buffer.add_buffer ligatureset_buf ligs_buf
    ) ligs;
    Buffer.add_buffer buf_subtables coverage_buffer;
    Buffer.add_buffer buf_subtables ligatureset_buf;
  done;
  Buffer.add_buffer buf buf_subtables;
  buf

open FTypes
let make_kerning pairs=
  let buf=Buffer.create 256 in
  bufInt2 buf 2;                        (* pair adjustment *)
  bufInt2 buf 0;                        (* flags *)
  bufInt2 buf 1;                        (* subtable count *)

  let buf_subtables=Buffer.create 256 in
  bufInt2 buf (8+Buffer.length buf_subtables);   (* offset vers la subtable *)


  let valueformat1=List.fold_left (fun k (_,_,k1,_)->
    (if k1.kern_x0<>0.        then 1 else 0) lor
    (if k1.kern_y0<>0.        then 2 else 0) lor
    (if k1.advance_width<>0.  then 4 else 0) lor
    (if k1.advance_height<>0. then 8 else 0)
  ) 0 pairs
  and valueformat2=List.fold_left (fun k (_,_,_,k2)->
    (if k2.kern_x0<>0.        then 1 else 0) lor
    (if k2.kern_y0<>0.        then 2 else 0) lor
    (if k2.advance_width<>0.  then 4 else 0) lor
    (if k2.advance_height<>0. then 8 else 0)
  ) 0 pairs
  in
(*  let nformat1=
    (if valueformat1 land 1=0 then 0 else 1)+
    (if valueformat1 land 2=0 then 0 else 1)+
    (if valueformat1 land 4=0 then 0 else 1)+
    (if valueformat1 land 8=0 then 0 else 1)
  and nformat2=
    (if valueformat2 land 1=0 then 0 else 1)+
    (if valueformat2 land 2=0 then 0 else 1)+
    (if valueformat2 land 4=0 then 0 else 1)+
    (if valueformat2 land 8=0 then 0 else 1)
  in*)

  (* Maintenant on écrit la subtable *)
  let ipairs=List.fold_left (fun m k->
    let (a,b,x,y)=k in
    let autres=try IntMap.find a m with Not_found->[] in
    IntMap.add a (k::autres) m
  ) IntMap.empty pairs
  in
  let coverage_buffer=coverage ipairs in
  bufInt2 buf_subtables 1;                               (* SubstFormat *)
  bufInt2 buf_subtables (10+2*IntMap.cardinal ipairs);   (* coverage *)
  bufInt2 buf_subtables valueformat1;
  bufInt2 buf_subtables valueformat2;
  bufInt2 buf_subtables (IntMap.cardinal ipairs);
  let pairs_buf=Buffer.create 256 in
  let make_value buffer format k=
    Printf.fprintf stderr "make_value : %f %f %f %f\n" k.kern_x0 k.kern_y0 k.advance_width k.advance_height;flush stderr;
    if format land 1<>0 then (
      bufInt2 buffer (round k.kern_x0);
    );
    if format land 2<>0 then (
      bufInt2 buffer (round k.kern_y0);
    );
    if format land 4<>0 then (
      bufInt2 buffer (round k.advance_width);
    );
    if format land 8<>0 then (
      bufInt2 buffer (round k.advance_height);
    );
  in
  IntMap.iter (fun _ p->
    let p=List.sort (fun (_,a,_,_) (_,b,_,_)->compare a b) p in
    bufInt2 buf_subtables (10+2*IntMap.cardinal ipairs
                           +Buffer.length coverage_buffer
                           +Buffer.length pairs_buf);
    (* p est une liste de paires commençant par le même glyph *)
    bufInt2 pairs_buf (List.length p);
    (*let off0=2+(nformat1+nformat2)*2*(List.length p) in*)
    List.iter (fun (_,b,x,y)->
      bufInt2 pairs_buf b;
      make_value pairs_buf valueformat1 x;
      make_value pairs_buf valueformat2 y;
    ) p;
  ) ipairs;
  Buffer.add_buffer buf_subtables coverage_buffer;
  Buffer.add_buffer buf_subtables pairs_buf;
  Buffer.add_buffer buf buf_subtables;
  buf
