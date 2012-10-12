open Printf
open Sqlite3
open CamomileLibrary

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
     | r ->fprintf stderr "%s\n%s\n" (Rc.to_string r) (errmsg db); raise Not_found);
  List.iter (fun x->ignore (exec db
                              (sprintf "INSERT INTO authors_publications(author, article) VALUES (%Ld,%Ld)" x art)
                           )
            ) aut

let gets table db art=
  let auteurs=ref [] in
  let cb row _=match row.(0) with Some a->auteurs:=a::(!auteurs) | None -> () in
    match exec db ~cb:cb (sprintf "SELECT name FROM authors WHERE id IN (SELECT author FROM %s_publications WHERE article=%Ld)" table art) with
        Rc.OK -> List.rev !auteurs
      | r ->(fprintf stderr "gets %s : %s\n%s\n" table (Rc.to_string r) (errmsg db); raise Not_found)


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
    if UTF8.out_of_range n i then
      [String.sub n i0 (String.length n-i0)]
    else
      if f (UTF8.look n i) then
        (String.sub n i0 (i-i0)) :: (split f n (UTF8.next n i) (UTF8.next n i))
      else
        split f n i0 (UTF8.next n i)

let make_name n=
  match split (fun x->x=UChar.of_char ',') n 0 0 with
      []->("","")
    | h::s->
        let initiales=List.map
          (fun x->
             let xx=Util.unspace x in
               (String.sub xx 0 (UTF8.next xx 0)) ^ ".") s
        in
          (String.concat "" initiales,h)

exception Bib_error of string

let rec intercalate a b=match b with
    []->[]
  | [h]->[h]
  | h0::h1::s->h0::a::(intercalate a (h1::s))

let rec dbCite db subcitation req=
  let results=ref [] in
  let make_bib row _=
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
            | Some i->
                match dbCite db true (sprintf "id=%s" i) with
                    []->[]
                  | (_,h)::s->[tT "in: "::h]
          in
          let jour=match row.(field_num "journal") with
              None->[]
            | Some j->(
                let jour=ref [] in
                let cb row _=match row.(0) with Some a->jour:=a::(!jour) | None -> () in
                  match exec db ~cb:cb (sprintf "SELECT name FROM journals WHERE id=%s" j) with
                      Rc.OK -> [tT "in: ";tT (List.hd !jour)]
                    | r ->(fprintf stderr "%s\n%s\n" (Rc.to_string r) (errmsg db); raise Not_found)
              )
          in
          let pub=match row.(field_num "publisher") with
              None->[]
            | Some j->(
                let pub=ref [] in
                let cb row _=match row.(0) with Some a->pub:=a::(!pub) | None -> () in
                  match exec db ~cb:cb (sprintf "SELECT name FROM publishers WHERE id=%s" j) with
                      Rc.OK -> [tT (List.hd !pub)]
                    | r ->(fprintf stderr "%s\n%s\n" (Rc.to_string r) (errmsg db); raise Not_found)
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
              | h::s as l->
                  tT (String.concat ", "
                       (List.map (fun n->let (x,y)=make_name n in sprintf "%s %s" x y) l))::
                    [tT (if s=[] then " (ed.)" else " (eds.)")]
          ) else []
          in
          let doctype=match row.(field_num "type") with
              Some "phdthesis"->[tT "PhD. thesis"]
            | _->[]
          in
          results:=(int_of_string id_,
                    List.concat (intercalate [tT ", "]
                                   (List.filter (function []->false | _->true)
                                      (auteurs@[titre;ed;jour]@pub_in@[chap;volume;pub;pages;editors;doctype]))))
            :: (!results)
        )
  in
  let r=sprintf "SELECT %s FROM bibliography %s" (String.concat ", " (List.map fst fields))
    (if String.length (Util.unspace req) > 0 then "WHERE "^req else "")
  in
    match exec db ~cb:(make_bib) r with
        Rc.OK->List.rev !results
      | r ->(fprintf stderr "%s\n%s\n" (Rc.to_string r) (errmsg db); raise (Bib_error req))


let bibitem bib req=
  let db=db_open bib in
  let res=dbCite db false req in
    ignore (db_close db);
    res

let author bib auth=
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
let bib:((int*user content list) IntMap.t) ref=ref IntMap.empty
let revbib:((int*user content list) IntMap.t) ref=ref IntMap.empty
let citeCounter:unit IntMap.t ref=ref IntMap.empty
let bibfile= ref "biblio.bibi"

let no_results x=match Typography.TypoLanguage.lang with
    `FR->Printf.sprintf "La requête n'a pas donné de résultats :\n%s" x
  | _->Printf.sprintf "The request gave no results:\n%s" x
let more_than_one x=match Typography.TypoLanguage.lang with
    `FR->Printf.sprintf "Attention : La requête a donné plus d'un résultat :\n%s" x
  | _->Printf.sprintf "Warning : The request gave more than one result :\n%s" x

let citeFile bibfile x=
  try
    let num a b=
      try
	fst (IntMap.find a !bib)
      with
	Not_found->
          let key=(IntMap.cardinal !bib)+1 in
          bib:=IntMap.add a (key, b) !bib;
          revbib:=IntMap.add key (a, b) !revbib;
          key
    in
    let rec fn l = 
      match  l with
	[]-> raise (No_bib (no_results x));
      | (a,b)::l->
	citeCounter:=IntMap.add a () !citeCounter;
	let item = 
          C (fun _-> [bB (fun _->[User (BeginLink (sprintf "_bibi_%d" (num a b)))]);
           tT (sprintf "%d" (num a b));
           bB (fun _->[User EndLink])])
	in
	if l = [] then item::[tT"]"] else
	  item::tT ","::fn l
    in 
    let l = bibitem bibfile x in
    tT"["::fn l
  with
    No_bib s->(Printf.fprintf stderr "%s\n" s;exit 1)

let authorCite x y=
  sprintf "%s id IN (SELECT article FROM authors_publications WHERE author IN (SELECT id FROM authors WHERE %s))"
    (if y="" then "" else sprintf "(%s) AND" y) x

let authorFile bibfile x=match author bibfile (sprintf "name LIKE '%%%s%%'" x) with
    []->Printf.fprintf stderr "Unknown author %S\n" x;raise Not_found
  | h::_->[tT (snd (make_name h))]

let cite x=citeFile !bibfile x

open Util
open Box
open Line

module type Format=sig
  val parameters:user environment -> user box array array -> drawingBox array -> parameters ->  Break.figurePosition IntMap.t ->line TS.UMap.t -> line -> line -> parameters
end

module TheBibliography (F:Format) (D : DocumentStructure) = struct
  let _ =
    let params env a1 a2 a3 a4 a5 a6 line=
      let p=F.parameters env a1 a2 a3 a4 a5 a6 line in
        if line.lineStart=0 then (
          let rec findMark w j=
            if j>=line.lineEnd then 0. else
              if a1.(line.paragraph).(j) = User AlignmentMark then w else
                let (_,ww,_)=box_interval a1.(line.paragraph).(j) in
                  findMark (w+.ww) (j+1)
          in
          let w=findMark 0. 0 in
            { p with
                left_margin=p.left_margin-.w;
                measure=p.measure+.w }
        ) else
          p
    in
    let comp mes a1 a2 a3 a4 line a6=
      if line.lineStart>0 then Complete.normal mes a1 a2 a3 a4 line a6 else (
        let rec findMark w j=
          if j>=Array.length a1.(line.paragraph) then 0. else
            if a1.(line.paragraph).(j) = User AlignmentMark then w else
              let (_,ww,_)=box_interval a1.(line.paragraph).(j) in
                findMark (w+.ww) (j+1)
        in
          Complete.normal { mes with normalMeasure=mes.normalMeasure+.findMark 0. 0 } a1 a2 a3 a4 line a6
      )
    in
    let make_bibitem i=
      C (fun env->
           try
             let _,c=IntMap.find i !revbib in
               (tT (sprintf "[%d]" i)::bB (fun env->let s=env.size/.3. in
                                           [glue s s s;User (Label (sprintf "_bibi_%d" i));
                                            User AlignmentMark])::c)
           with
               _->[]
        )
    in
      for i=1 to IntMap.cardinal !citeCounter do
        newPar D.structure ~environment:(fun x -> { x with par_indent = [] })
          comp params [make_bibitem i]
      done
end
