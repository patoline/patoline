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
open Printf
open Sqlite3
open UsualMake

let fields=[
  "id","INTEGER PRIMARY KEY AUTOINCREMENT";
  "booktitle", "TEXT";
  "chapter", "TEXT";
  "crossref", "INTEGER";
  "date", "DATETIME";
  "doi", "TEXT";
  "edition", "TEXT";
  "eprint", "TEXT";
  "institution", "INTEGER";
  "isbn", "TEXT";
  "journal", "INTEGER";
  "number", "TEXT";
  "organization", "INTEGER";
  "pages", "TEXT";
  "publisher", "INTEGER";
  "school", "INTEGER";
  "series", "TEXT";
  "title", "TEXT";
  "type", "TEXT";
  "url", "TEXT";
  "volume", "TEXT"
]
let field_num x=
  let rec f i=function
      []->fprintf stderr "No such field \"%s\"\n" x; raise Not_found
    | (h,_)::_ when h=x -> i
    | _::s->f (i+1) s
  in
    f 0 fields
let create db=
  let sqls =
    [
      "DROP TABLE IF EXISTS authors";
      "CREATE TABLE authors("^
        "id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, author INTEGER, name_format TEXT)";
      "DROP TABLE IF EXISTS institutions";
      "CREATE TABLE institutions("^
        "id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, type TEXT, address TEXT, url TEXT)";
      "DROP TABLE IF EXISTS publishers";
      "CREATE TABLE publishers("^
        "id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, address TEXT, url TEXT)";
      "DROP TABLE IF EXISTS journals";
      "CREATE TABLE journals("^
        "id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, publisher INTEGER)";
      "DROP TABLE IF EXISTS bibliography";
      "CREATE TABLE bibliography ("^(String.concat ", " (List.map (fun (a,b)->a^" "^b) fields))^")";

      "DROP TABLE IF EXISTS authors_publications";
      "CREATE TABLE authors_publications (id INTEGER PRIMARY KEY AUTOINCREMENT, author INTEGER, article INTEGER, ordre INTEGER)";

      "DROP TABLE IF EXISTS editors_publications";
      "CREATE TABLE editors_publications (id INTEGER PRIMARY KEY AUTOINCREMENT, author INTEGER, article INTEGER, ordre INTEGER)"
    ]
  in
    List.iter (fun sql ->match exec db sql with
                 | Rc.OK -> ()
                 | r ->fprintf stderr "%s\n%s\n" (Rc.to_string r) (errmsg db))
      sqls

exception Auteur of string
exception Notfound of string
let set_auteurs db art aut=
  (match exec db (sprintf "DELETE FROM authors_articles WHERE article=%Ld" art) with
       Rc.OK -> ()
     | r ->fprintf stderr "%s\n%s\n" (Rc.to_string r) (errmsg db); flush stderr;raise Not_found);
  List.iter (fun x->ignore (exec db
                              (sprintf "INSERT INTO authors_publications(author, article) VALUES (%Ld,%Ld)" x art)
                           )
            ) aut

let gets table db art=
  let auteurs=ref [] in
  let cb row _=match row.(0) with Some a->(
    auteurs:=a::(!auteurs)
  ) | None -> () in
    match exec db ~cb:cb (sprintf "select name from authors,%s_publications where %s_publications.article=%Ld and %s_publications.author=authors.id order by ordre ASC" table table art table) with
        Rc.OK -> List.rev !auteurs
      | r ->(fprintf stderr "gets %s : %s\n%s\n" table (Rc.to_string r) (errmsg db); flush stderr;raise Not_found)


let insert_author db name=
  match exec db (sprintf "INSERT INTO authors(name) VALUES (%S)" name) with
      Rc.OK->
        let id=last_insert_rowid db in
          ignore (exec db (sprintf "UPDATE authors SET author=%Ld WHERE id=%Ld" id id));
          id
    | r ->(fprintf stderr "%s\n%s\n" (Rc.to_string r) (errmsg db); raise (Auteur name))

exception Publisher of string
let insert_publisher db name=
  let id=ref Int64.minus_one in
    match exec db ~cb:(fun row _->match row.(0) with Some a->id:=Int64.of_string a | _->())
      (sprintf "SELECT id FROM publishers WHERE name=%S" name)
    with
        Rc.OK->
          if !id >= Int64.zero then !id else (
            match exec db (sprintf "INSERT INTO publishers(name) VALUES (%S)" name) with
                Rc.OK->last_insert_rowid db
              | r ->(fprintf stderr "%s\n%s\n" (Rc.to_string r) (errmsg db); raise (Publisher name))
          )
      | r ->(fprintf stderr "%s\n%s\n" (Rc.to_string r) (errmsg db); raise (Publisher name))


let inproceedings="inproceedings"
let proceedings="proceedings"
let article="article"
let book="book"

(* let _= *)
(*   let h="biblio" in *)
(*   let db = db_open h in *)
(*     create db; *)
(*     db_close db *)

open Typography
open Document

let rec split f n i0 i=
  if UTF8.out_of_range n i0 then [] else
    if UTF8.out_of_range n i then (
      [String.sub n i0 (String.length n-i0)]
    ) else (
      if f (UTF8.look n i) then
        (String.sub n i0 (i-i0)) :: (split f n (UTF8.next n i) (UTF8.next n i))
      else
        split f n i0 (UTF8.next n i)
    )
let make_name n=
  match List.rev(split (fun x->x=UChar.of_char ',') n 0 0) with
      []->("","")
    | h::s->
        let initiales=List.map
          (fun x->
            let xx=Util.unspace x in
            (String.sub xx 0 (UTF8.next xx 0)) ^ ".") s
        in
        (String.concat "" (List.rev initiales),Util.unspace h)

exception Bib_error of string

let rec intercalate a b=match b with
    []->[]
  | [h]->[h]
  | h0::h1::s->h0::a::(intercalate a (h1::s))


let rec dbCite db subcitation req=
  let results=ref [] in
  let r=sprintf "SELECT %s FROM bibliography %s" (String.concat ", " (List.map fst fields))
    (if String.length (Util.unspace req) > 0 then "WHERE "^req else "")
  in
  match exec db ~cb:(fun row _->results:=row::(!results)) r with
      Rc.OK->List.rev !results
    | r ->(fprintf stderr "%s\n%s\n" (Rc.to_string r) (errmsg db); raise (Bib_error req))


let bibitem bib req=
  let db=db_open bib in
  let res=dbCite db false req in
  ignore (db_close db);
  res

let author_ bib auth=
  let db=db_open bib in
  let r=sprintf "SELECT name FROM authors WHERE %s" auth in
  let auths=ref [] in
  (match exec db ~cb:(fun row _->match row.(0) with
      Some a->auths:=a::(!auths)
    | _->()) r with
      Rc.OK->()
    | rr ->(fprintf stderr "%s\n%s\n" (Rc.to_string rr) (errmsg db); raise (Bib_error r)));
  ignore (db_close db);
  !auths

(* let _=List.iter (fun (a,x)->printf "%s : %s\n" a (Document.string_of_contents x)) *)
(*   (cite "biblio" "title LIKE '%arameterized%'") *)

open Util
open Box
exception No_bib of string
let bib:((int*string option array) IntMap.t) ref=ref IntMap.empty
let revbib:((string option array) IntMap.t) ref=ref IntMap.empty
let citeCounter:unit IntMap.t ref=ref IntMap.empty
let bibfile_=ref (
  if Typography.Config.user_dir="" then None else
    Some (Filename.concat Typography.Config.user_dir "bibi.sqlite3")
)

let bibfile x=bibfile_:=Some x

let no_results x=match Typography.TypoLanguage.lang with
    `FR->Printf.sprintf "La requête n'a pas donné de résultats :\n%s" x
  | _->Printf.sprintf "The request gave no results:\n%s" x
let more_than_one x=match Typography.TypoLanguage.lang with
    `FR->Printf.sprintf "Attention : La requête a donné plus d'un résultat :\n%s" x
  | _->Printf.sprintf "Warning : The request gave more than one result :\n%s" x

module type CitationStyle=sig
  val citation_format:int->string option array->content list
  val compare:(int*string option array)->(int*string option array)->int
end
module type BiblioStyle=sig
  val biblio_format:int->string option array->Document.tree
end

module Biblio (C:CitationStyle) (B:BiblioStyle)=struct
  let citeFile bibfile x=
    try
      let num (b:string option array)=
        match b.(field_num "id") with
            Some bid->(
              try
	        fst (IntMap.find (int_of_string bid) !bib)
              with
	          Not_found->
                    let key=(IntMap.cardinal !bib)+1 in
                    bib:=IntMap.add (int_of_string bid) (key, b) !bib;
                    revbib:=IntMap.add key b !revbib;
                    key
            )
          | None->assert false
      in
      let rec fn l =
        match  l with
	    []-> raise (No_bib (no_results x));
          | (row)::l->
            let a=match row.(field_num "id") with None->assert false | Some a->int_of_string a in
	    citeCounter:=IntMap.add a () !citeCounter;
	    let item =
              bB (fun _->[Marker (BeginLink (Intern (sprintf "_bibi_%d" (num row))))])
              ::(C.citation_format (num row) row)
              @[bB (fun _->[Marker EndLink])]
	    in
	    if l = [] then item@[tT"]"] else
	      item@tT ", "::fn l
      in
      let l = bibitem bibfile x in
      tT"["::fn l
    with
        No_bib s->(Printf.eprintf "%s\n%!" s; [tT "[???]"])
      | _->[]

  let authorCite x y=
    sprintf "%s id IN (SELECT article FROM authors_publications WHERE author IN (SELECT id FROM authors WHERE %s))"
      (if y="" then "" else sprintf "(%s) AND" y) x

  let authorFile bibfile x=match author_ bibfile (sprintf "name LIKE '%%%s%%'" x) with
      []->Printf.eprintf "Unknown author %S\n%!" x;[tT "[???]"]
    | h::_->[tT (snd (make_name h))]

  let cite x=match !bibfile_ with
      None->failwith "Bibi: no bibliographic source defined"
    | Some y->citeFile y x

  let author x=match !bibfile_ with
      None->failwith "Bibi: no bibliographic source defined"
    | Some y->authorFile y x

  open Box

  module TheBibliography (D : DocumentStructure) = struct
    let _ =
      let l=List.sort (C.compare) (IntMap.bindings !revbib) in
      List.iter (fun (i,x)->
        D.structure:=up (newChildAfter !D.structure (B.biblio_format i x));
      ) l
  end
end


let rec default_biblio_format ?follow_crossrefs:(follow_crossrefs=true) row=
  match !bibfile_ with
      None->[]
    | Some bf->(
      let db=db_open bf in
      match row.(field_num "id") with
          None->assert false
        | Some id_->(
          let id=Int64.of_string id_ in
          let auteurs=
            let aut=gets "authors" db id in
            (List.map (fun n->let (x,y)=make_name n in [tT (sprintf "%s%s%s" x (if x<>"" && y<>"" then " " else "") y)]) aut)
          in
          let titre=match row.(field_num "title") with None->[] | Some a->[tT a] in
          let ed=match row.(field_num "edition") with None -> [] | Some a->[tT a] in
          let pub_in=match row.(field_num "crossref") with
              None->[]
            | Some i->(
              match dbCite db true (sprintf "id=%s" i) with
                h::_ when follow_crossrefs->tT "in: "::(default_biblio_format ~follow_crossrefs:false  h)
              | _->[]
            )
          in
          let jour=match row.(field_num "journal") with
            | Some j when pub_in=[] && follow_crossrefs->(
              let jour=ref [] in
              let cb row _=match row.(0) with Some a->jour:=a::(!jour) | None -> () in
              match exec db ~cb:cb (sprintf "SELECT name FROM journals WHERE id=%s" j) with
                  Rc.OK -> [tT "in: ";tT (List.hd !jour)]
                | r ->(fprintf stderr "%s\n%s\n" (Rc.to_string r) (errmsg db); flush stderr;raise Not_found)
            )
            | _->[]
          in
          let booktitle=match row.(field_num "booktitle") with
              Some j when jour=[] && pub_in=[] && follow_crossrefs->(
              [tT "in: ";tT j]
            )
            | _->[]
          in
          let pub=match row.(field_num "publisher") with
              None->[]
            | Some j->(
              let pub=ref [] in
              let cb row _=match row.(0) with Some a->pub:=a::(!pub) | None -> () in
              match exec db ~cb:cb (sprintf "SELECT name FROM publishers WHERE id=%s" j) with
                  Rc.OK -> [tT (List.hd !pub)]
                | r ->(fprintf stderr "%s\n%s\n" (Rc.to_string r) (errmsg db); flush stderr;raise Not_found)
            )
          in
          let volume=if pub_in=[] then (
            let date=match row.(field_num "date") with
                None->[]
              | Some a->[tT (sprintf "(%s)" a)]
            in
            match row.(field_num "volume"),row.(field_num "series") with
                None, None->(
                  match row.(field_num "date") with
                      None->[]
                    | Some a->[tT (sprintf "%s" a)]
                )
              | Some a,Some b->(tT (sprintf "volume %s of %s" a b))::(if date=[] then [] else tT " "::date)
              | Some a,_->(tT (sprintf "volume %s" a))::(if date=[] then [] else tT " "::date)
              | _, Some a->(tT (sprintf "volume %s" a))::(if date=[] then [] else tT " "::date)
          ) else []
          in
          let chap=match row.(field_num "chapter") with None-> [] | Some a->[tT a] in
          let pages=match row.(field_num "pages") with None-> [] | Some a->[tT a] in
          let editors=if pub_in=[] then (
            match gets "editors" db id with
                []->[]
              | _::s as l->
                tT (String.concat ", "
                      (List.map (fun n->
                        let (x,y)=make_name n in
                        sprintf "%s%s%s"
                          x
                          (if x<>"" && y<>"" then " " else "")
                          y) l))::
                  [tT (if s=[] then " (ed.)" else " (eds.)")]
          ) else []
          in
          let doctype=match row.(field_num "type") with
              Some "phdthesis"->[tT "PhD. thesis"]
            | _->[]
          in
          let eprint=match row.(field_num "eprint") with
              Some x when follow_crossrefs->extLink x (verb [tT x])
            | _->[]
          in
          (
            List.concat (intercalate [tT ", "]
                           (List.filter (function []->false | _->true)
                              (auteurs@[titre;ed;booktitle;pub_in]@[chap;volume;pub;pages;editors;doctype;eprint]))))
        )
    )


module MarginBiblio (C:CitationStyle)=struct

  open Box
  let w_mar=2.
  let biblio_format i row=
    let params env a1 a2 a3 a4 a5 a6 line=
      let p=DefaultFormat.parameters env a1 a2 a3 a4 a5 a6 line in
      if line.lineStart=0 then (
        let rec findMark w j=
          if j>=line.lineEnd then 0. else
            if a1.(line.paragraph).(j) = Marker AlignmentMark then w else
              let (_,ww,_)=box_interval a1.(line.paragraph).(j) in
              findMark (w+.ww) (j+1)
        in
        let w=findMark 0. 0 in
        { p with
          left_margin=p.left_margin-.w+.w_mar*.env.size;
          measure=p.measure+.w-.w_mar*.env.size }
      ) else
        {p with
          left_margin=p.left_margin+.w_mar*.env.size;
          measure=p.measure-.w_mar*.env.size}
    in
    let comp mes a1 a2 a3 a4 line a6=
      if line.lineStart>0 then
        Complete.normal {mes with
          normalMeasure=mes.normalMeasure-.w_mar*.mes.size
        } a1 a2 a3 a4 line a6
      else (
        let rec findMark w j=
          if j>=Array.length a1.(line.paragraph) then 0. else
            if a1.(line.paragraph).(j) = Marker AlignmentMark then w else
              let (_,ww,_)=box_interval a1.(line.paragraph).(j) in
              findMark (w+.ww) (j+1)
        in
        Complete.normal { mes with
          normalMeasure=mes.normalMeasure+.findMark 0. 0-.w_mar*.mes.size
        }
          a1 a2 a3 a4 line a6
      )
    in
    Paragraph {
      par_contents=
        [C (fun env->
          try
            (tT"["::C.citation_format i row@
               [tT"] ";
                bB (fun env->let s=env.size/.3. in
                             [glue s s s;Marker (Label (sprintf "_bibi_%d" i));
                              Marker AlignmentMark])]
             @default_biblio_format row)
          with
              _->[]
        )];
      par_env=(fun env->{env with par_indent=[]});
      par_post_env=(fun env1 env2 -> { env1 with names=names env2;
        counters=env2.counters;
        user_positions=user_positions env2 });
      par_parameters=params;
      par_badness=badness;
      par_completeLine=comp;
      par_states=[];
      par_paragraph=(-1) }

end

module DefaultBiblio (C:CitationStyle)=struct

  open Box

  let w=phi
  let params env a1 a2 a3 a4 a5 a6 line=
    let p=DefaultFormat.parameters env a1 a2 a3 a4 a5 a6 line in
    if line.lineStart=0 then (
      p
    ) else
      {p with
        left_margin=p.left_margin+.w*.env.size;
        measure=p.measure-.w*.env.size}
  let comp mes a1 a2 a3 a4 line a6=
    if line.lineStart=0 then
      Complete.normal mes a1 a2 a3 a4 line a6
    else (
      Complete.normal { mes with
        normalMeasure=mes.normalMeasure-.w*.mes.size
      }
        a1 a2 a3 a4 line a6
    )


  let biblio_format i row=
    Paragraph {
      par_contents=
        [C (fun env->
          try
            (tT"["::C.citation_format i row@
               [tT"] ";bB (fun env->
                 let s=env.size/.3. in
                 [glue s s s;Marker (Label (sprintf "_bibi_%d" i));
                  Marker AlignmentMark])]
             @default_biblio_format row)
          with
              _->[]
        )];
      par_env=(fun env->{env with par_indent=[]});
      par_post_env=(fun env1 env2 -> { env1 with names=names env2;
        counters=env2.counters;
        user_positions=user_positions env2 });
      par_parameters=params;
      par_badness=badness;
      par_completeLine=comp;
      par_states=[];
      par_paragraph=(-1) }

end

module CitationInt=struct
  let citation_format i _=[tT (string_of_int i)]
  let compare (a,_) (b,_)=compare a b
end
module CitationNames(M:sig val longCite:Document.content list list->Document.content list end)=struct
  let doublons:(string list, int IntMap.t) Hashtbl.t=Hashtbl.create 200
  let compare (_,a) (_,b)=
    match !bibfile_ with
        None->failwith "Bibi: no bibliographic source defined"
      | Some bf->(
        let db=db_open bf in
        let aut row=
          match row.(field_num "id") with
              None->assert false
            | Some id_->(
              let id=Int64.of_string id_ in
              let aut=gets "authors" db id in
              let auteurs=
                (List.map (fun n->
                  let (_,y)=make_name n in
                  [tT y]
                 ) aut)
              in
              auteurs
            )
        in
        compare (aut a) (aut b)
      )
  let citation_format i row=
    match !bibfile_ with
        None->failwith "Bibi: no bibliographic source defined"
      | Some bf->(
        let db=db_open bf in
        match row.(field_num "id") with
            None->assert false
          | Some id_->(
            let id=Int64.of_string id_ in
            let date=match (row.(field_num "date")) with
                None->""
              | Some x->(
                let year=Str.regexp ".*\\(^\\|[^0-9]\\)\\([0-9][0-9][0-9][0-9]*\\).*" in
                if Str.string_match year x 0 then
                  Str.matched_group 2 x
                else
                  let year'=Str.regexp "\\(^\\|\\(.*[^0-9]\\)\\)\\([0-9]*\\)" in
                  if Str.string_match year' x 0 then
                    Str.matched_group 2 x
                  else
                    x
              )
            in
            let aut=gets "authors" db id in
            let auteurs=
              (List.map (fun n->
                let (_,y)=make_name n in
                [tT y]
               ) aut)
            in

            let h=try Hashtbl.find doublons (date::aut) with _->IntMap.empty in
            if not (IntMap.mem (int_of_string id_) h) then (
              Hashtbl.add doublons (date::aut)
                (IntMap.add (int_of_string id_) (1+IntMap.cardinal h) h)
            );
            let rec subnum_of_int x buf=
              if x=0 then String.concat "" buf else
                let s=String.make 1 (char_of_int (int_of_char 'a'+(x-1) mod 26)) in
                subnum_of_int (x/26) (s::buf)
            in
            [C (fun _->
              let n=try
                      let l=Hashtbl.find doublons (date::aut) in
                      if IntMap.cardinal l<=1 then [] else
                        [tT (subnum_of_int (IntMap.find (int_of_string id_) l) [])]
                with
                    Not_found->[]
              in
              (M.longCite auteurs)
              @ (if date<>"" then [tT (" "^date)] else []) @ n
            )]
          )
      )
end

module AllNames=struct
  let longCite auteurs=List.concat (intercalate [tT ", "] auteurs)
end
module EtAl=struct
  let longCite auteurs=
    if List.length auteurs<=2 then
      List.concat (intercalate [tT ", "] auteurs)
    else
      List.concat (intercalate [tT ", "] (take 1 auteurs)) @ [tT " et al."]
end


module ItemInt=MarginBiblio(CitationInt)
module ItemNames=DefaultBiblio(CitationNames(AllNames))
module ItemEtAl=DefaultBiblio(CitationNames(EtAl))

module BiblioInt=Biblio(CitationInt)(ItemInt)
module BiblioNames=Biblio(CitationNames(AllNames))(ItemNames)
module BiblioEtAl=Biblio(CitationNames(EtAl))(ItemEtAl)
