(*
  FormatZhBook — a ctexbook-like Chinese book format for Patoline.

  Reproduces the page setup of the reference LaTeX book template
  (ctexbook + geometry width=15cm, twoside, 10pt):

  - A4, 15cm text block, equal side margins;
  - running heads in LaTeX “headings” style (even: folio left /
    chapter mark right; odd: section mark left / folio right),
    plain style (centred folio) on chapter-opening pages, no folio
    on the title page;
  - lowercase roman page numbers until the first numbered chapter,
    then arabic starting from 1;
  - chapter headings “第X章 标题” in Hei (SimHei), centred, with
    forced page breaks before them; sections “1.1 标题” centred,
    subsections left-aligned;
  - table of contents down to subsubsection level with dot leaders,
    第X章 labels and bold chapter rows;
  - equations numbered (章.n) with chapter-scoped reset, plus
    reference helpers for figures/tables/sections;
  - figure captions prefixed with 图.

  All fonts are resolved through the system font tables added to
  [DefaultFormat]: Times New Roman (西文) + SimSun 宋体 (正文汉字),
  SimHei 黑体 (标题). *)
open Typography
open Patoraw
open Patutil
open Typography.Document
open Box
open Extra

module Euler = DefaultFormat.Euler

(** Chinese numerals for chapter numbers (1..99, digits beyond fall
    back to arabic). *)
let cn_number n =
  let digit d = [| "零"; "一"; "二"; "三"; "四"; "五"; "六"; "七"; "八"; "九" |].(d) in
  if n <= 0 then "零"
  else if n < 10 then digit n
  else if n = 10 then "十"
  else if n < 20 then "十" ^ digit (n mod 10)
  else if n < 100 then
    let u = n mod 10 in
    digit (n / 10) ^ "十" ^ (if u = 0 then "" else digit u)
  else string_of_int n

let cn_space = " "

(** Width of a content list in the given environment. *)
let boxes_width env contents =
  let boxes = boxify_scoped env contents in
  let w =
    List.fold_left
      (fun w x -> let _, a, _ = Box.box_interval x in w +. a)
      0. boxes
  in
  boxes, w

module MakeFormat (D : DocumentStructure)
  (Default : module type of DefaultFormat.Format(D)) = struct

  module Default = Default
  include Default

  let caml x = x
  let displayedFormula = Default.displayedFormula

  (** Qualified constructor access for document-side [Caml] blocks:
      the patoline expression printer drops multi-part longidents in
      constructor position (e.g. [Typography.Document.Env] becomes
      [Env], which then resolves to [Maths.Env]).  Documents call this
      wrapper instead. *)
  let envOf f = Typography.Document.Env f

  (* ---------------------------------------------------------------- *)
  (* Page geometry and body typography                                 *)
  (* ---------------------------------------------------------------- *)

  (* Geometry measured from the reference book.pdf (A4, 15cm text
     block): text top at 35mm, bottom at ~246mm, inner margin 24mm,
     outer margin 36mm (twoside); running-head baseline at ~29.6mm
     from the top; plain folio at ~47.4mm from the bottom. *)
  let zh_master =
    { PageLayout.paperWidth = 210.0;
      paperHeight = 297.0;
      (* P4-5: visual text bottom = 297 - marginTop; the reference
         stops at 252.4mm of ink, i.e. a 44.6mm visual margin; the
         measured layout put ours at 262 with marginTop = 35. *)
      marginTop = 42.0;
      marginBottom = 51.0;
      marginLeft = 24.0;
      marginRight = 36.0 }

  (* [lr_new_page] alternates the side margins so that odd (right)
     pages use the master margins and even (left) pages mirror them. *)
  let zh_new_page = PageLayout.lr_new_page zh_master

  (* Body: Latin Modern for western text, FandolSong as per-glyph
     fallback for Han — the exact font pair of the reference book
     (ctex fontset=fandol + XeLaTeX defaults). *)
  let zh_body_family = List.concat [latinModernRoman; fandolSong]

  (* Headings use the same family; bold picks FandolSong-Bold and
     LMRoman10-Bold exactly like the reference. *)
  let zh_heading_family = zh_body_family

  let () =
    DefaultFormat.running_heads := true;
    DefaultFormat.running_head_vpos := 29.6;
    (* P4-4: reference plain folio sits 44.6mm from the page edge. *)
    DefaultFormat.page_number_vpos := 44.6;
    (* P4-3: head text sits at the mirrored text margins (24mm inner,
       36mm outer), like the reference book. *)
    DefaultFormat.head_left_odd := 24.2;
    DefaultFormat.head_left_even := 36.1

  let defaultEnv =
    let f, _subst, _pos = selectFont zh_body_family Regular false in
    (* Reference measurements: body = 9.96pt (3.52mm), baseline pitch
       ≈ 15.6pt (5.5mm), measure 150mm. *)
    let fsize = 3.52 in
    let lead = 5.5 in
    { Default.defaultEnv with
      fontFamily = zh_body_family;
      (* Code blocks switch to the mono family; give it the Song
         fallback so Chinese comments inside listings still render. *)
      fontMonoFamily = List.concat [latinModernMono; fandolSong];
      fontMonoRatio =
        font_size_ratio zh_body_family latinModernMono;
      font = f;
      (* Mixed-family text: [substitutions] and [positioning] are
         computed once from the *western* family and then applied to
         every glyph of the paragraph, including Han glyphs coming
         from the fallback family.  GSUB/GPOS lookups of Latin Modern
         indices against FandolSong glyph ids produce spurious
         substitutions (characters silently disappear from the
         output), so both are neutralised for the Chinese book. *)
      substitutions = (fun glyphs -> glyphs);
      positioning = (fun glyphs -> glyphs);
      (* English hyphenation patterns mangle Han runs (characters are
         silently dropped when the dictionary splits them), and CJK
         text is never hyphenated in the reference book anyway. *)
      hyphenate = (fun _ -> [||]);
      size = fsize;
      lead;
      normalLead = lead;
      normalMeasure = 150.0;
      normalPageFormat = Util.a4;
      new_page = zh_new_page }

  (* ---------------------------------------------------------------- *)
  (* Headings                                                         *)
  (* ---------------------------------------------------------------- *)

  (* Heading sizes measured from the reference: chapter 20.66pt,
     section 14.35pt, subsection ~12pt at a 9.96pt body. *)
  let heading_family level env =
    let env = { env with par_indent = []; fontFamily = zh_heading_family } in
    match level with
    | 1 ->
        { env with fontAlternative = Bold;
                   size = env.size *. 2.07;
                   lead = env.lead *. 2.07 }
    | 2 ->
        { env with fontAlternative = Bold;
                   size = env.size *. 1.44;
                   lead = env.lead *. 1.44 }
    | _ ->
        { env with fontAlternative = Bold;
                   size = env.size *. 1.2;
                   lead = env.lead *. 1.2 }

  let head_title_of env level _path displayname =
    let _, b =
      try StrMap.find "_structure" env.counters with Not_found -> (-1, [0])
    in
    let numstr =
      String.concat "."
        (List.map (fun x -> string_of_int (x + 1))
           (List.rev (List.drop 1 b)))
    in
    if level = 1 && numstr <> "" then
      let chap = try int_of_string numstr with _ -> 0 in
      "第" ^ cn_number chap ^ "章" ^ cn_space
      ^ string_of_contents displayname
    else if numstr <> "" then
      numstr ^ cn_space ^ string_of_contents displayname
    else string_of_contents displayname

  let heading_contents env level path numbered' displayname =
    if numbered' then begin
      let title = head_title_of env level path displayname in
      let head_marker = bB (fun _ -> [Marker (HeadTitle title)]) in
      if level = 1 then begin
        let _, b =
          try StrMap.find "_structure" env.counters with Not_found -> (-1, [0])
        in
        let chap =
          try
            int_of_string
              (String.concat "."
                 (List.map (fun x -> string_of_int (x + 1))
                    (List.rev (List.drop 1 b))))
          with _ -> 0
        in
        [ bB (fun _ -> [Marker (Structure path)]);
          head_marker;
          tT ("第" ^ cn_number chap ^ "章");
          bB (fun env -> let w = env.size in [glue w w w]) ]
        @ displayname
      end else begin
        let _, b =
          try StrMap.find "_structure" env.counters with Not_found -> (-1, [0])
        in
        let numstr =
          String.concat "."
            (List.map (fun x -> string_of_int (x + 1))
               (List.rev (List.drop 1 b)))
        in
        [ bB (fun _ -> [Marker (Structure path)]);
          head_marker;
          tT numstr;
          tT " " ]
        @ displayname
      end
    end else
      [ C (fun _ ->
            bB (fun _ -> [Marker (Structure path)])
            :: bB (fun _ ->
                 [Marker (HeadTitle (string_of_contents displayname))])
            :: displayname) ]

  let heading_par ~level ~numbered' ~path ~displayname =
    Paragraph
      { par_contents =
          [ C (fun env ->
                heading_contents env level path numbered' displayname) ];
        par_env = (fun env -> heading_family level env);
        par_post_env =
          (fun env1 env2 ->
            { env1 with
              names = names env2;
              counters = env2.counters;
              user_positions = user_positions env2 });
        par_parameters =
          (fun a b c d e f g line ->
            let p =
              if level <= 2 then center a b c d e f g line
              else parameters a b c d e f g line
            in
            { p with
              (* P4-1: vertical rhythm measured on the reference book:
                 chapter headings start 26.7mm under the text top and
                 leave 15.5mm to the first body line; section headings
                 keep ~9.7mm above and ~5.5mm below. *)
              min_height_before =
                (if line.lineStart = 0 then
                   if level = 1 then 26.7
                   else if level = 2 then 8.0
                   else 6.5
                 else 0.0);
              min_height_after =
                (if line.lineEnd >= Array.length b.(line.paragraph) then
                   if level = 1 then 11.8
                   else if level = 2 then 3.5
                   else 2.2
                 else 0.0);
              min_lines_after =
                (if line.lineEnd >= Array.length b.(line.paragraph) then
                   if level = 1 then 2 else 1
                 else p.min_lines_after);
              min_page_before =
                (if level = 1 then max p.min_page_before 1
                 else p.min_page_before);
              not_last_line = true });
        par_badness = badness;
        par_completeLine = Complete.normal;
        par_states = [];
        par_paragraph = (-1) }

  let rec sectionize path numbered = function
    | Node n when List.mem_assoc "structural" n.node_tags ->
        let numbered' = numbered && List.mem_assoc "numbered" n.node_tags in
        let level = List.length path + 1 in
        let par =
          heading_par ~level ~numbered' ~path ~displayname:n.displayname
        in
        fst
          (up
             (newChildBefore
                ( Node
                    { n with
                      children =
                        IntMap.mapi
                          (fun k a -> sectionize (k :: path) numbered' a)
                          n.children },
                  [] )
                par))
    | Node n -> Node { n with children = IntMap.map (sectionize path numbered) n.children }
    | a -> a

  let postprocess_tree tree =
    (* The title/author/institute blocks of [DefaultFormat] are not
       used: the title page is typeset by hand in the document. *)
    match tree with
    | Node n ->
        Node
          { n with
            children = IntMap.map (sectionize [] true) n.children }
    | t -> t

  (* ---------------------------------------------------------------- *)
  (* Table of contents with dot leaders                               *)
  (* ---------------------------------------------------------------- *)

  let zh_these parameters str tree max_level =
    let params a b c d e f g line = parameters a b c d e f g line in
    str :=
      newPar !str ~environment:(fun x -> { x with par_indent = [] })
        Complete.normal params
        [ bB (fun env ->
              let spacing = env.size /. phi in
              let rec toc env0 path tree =
                let level = List.length path in
                match tree with
                | Paragraph _ -> []
                | FigureDef _ -> []
                | Node s
                  when level <= max_level && List.mem_assoc "intoc" s.node_tags
                  ->
                    let rec flat_children env1 = function
                      | [] -> []
                      | (_, (FigureDef _)) :: s
                      | (_, (Paragraph _)) :: s -> flat_children env1 s
                      | (k, (Node h as tr)) :: s ->
                          let env' = h.node_env env1 in
                          (toc env' (k :: path) tr)
                          @ flat_children (h.node_post_env env1 env') s
                    in
                    let chi =
                      if List.mem_assoc "numbered" s.node_tags || path = [] then
                        flat_children env0 (IntMap.bindings s.children)
                      else []
                    in
                    let _, b =
                      try StrMap.find "_structure" env0.counters
                      with Not_found -> (-1, [0])
                    in
                    let count = List.rev (List.drop 1 b) in
                    let in_toc = List.mem_assoc "intoc" s.node_tags in
                    let numbered = List.mem_assoc "numbered" s.node_tags in
                    if in_toc && numbered && count <> [] then begin
                      let labl =
                        String.concat "_" ("_" :: List.map string_of_int path)
                      in
                      let page =
                        try
                          1
                          + layout_page
                              (MarkerMap.find (Label labl)
                                 (user_positions env0))
                        with Not_found -> 0
                      in
                      let is_chapter = level = 1 in
                      let env_name =
                        if is_chapter then
                          { env0 with fontAlternative = Bold }
                        else env0
                      in
                      let num_txt =
                        if is_chapter then
                          let chap =
                            match List.rev count with
                            | x :: _ -> x + 1
                            | [] -> 1
                          in
                          "第" ^ cn_number chap ^ "章"
                        else
                          String.concat "."
                            (List.map (fun x -> string_of_int (x + 1)) count)
                      in
                      let num = boxify_scoped env_name [tT num_txt] in
                      let name =
                        boxify_scoped env_name s.displayname
                      in
                      let pg = boxify_scoped env0 [tT (string_of_int page)] in
                      let w l =
                        List.fold_left
                          (fun w b -> let _, w', _ = box_interval b in w +. w')
                          0. l
                      in
                      let wn = w num and wa = w name and wp = w pg in
                      (* P4-2: reference indents 0 / 5.3 / 13.4mm. *)
                      let x_off =
                        match level with
                        | 1 -> 0.0
                        | 2 -> env0.size *. 1.5
                        | _ -> env0.size *. 3.8
                      in
                      let avail = env0.normalMeasure -. x_off in
                      let x_num = 0.0 in
                      let x_name = x_num +. wn +. spacing in
                      let x_pg = avail -. wp in
                      let x_name_end = x_name +. wa in
                      (* dot leaders between the title and the page number *)
                      let dots = ref [] in
                      if x_pg -. x_name_end > env0.size *. 1.5 then begin
                        let r = env0.size *. 0.06 in
                        let step = env0.size *. 0.33 in
                        let x = ref (x_name_end +. env0.size *. 0.45) in
                        let stop = x_pg -. env0.size *. 0.45 in
                        while !x +. r < stop do
                          dots :=
                            RawContent.translate !x (env0.size *. 0.18)
                              (RawContent.Path
                                 ( { RawContent.default_path_param with
                                     RawContent.fillColor = Some Color.black;
                                     RawContent.strokingColor = None },
                                   [ RawContent.circle r ] ))
                            :: !dots;
                          x := !x +. step
                        done
                      end;
                      let cont =
                        List.map (RawContent.translate x_num 0.)
                          (draw_boxes env0 num)
                        @ List.map (RawContent.translate x_name 0.)
                            (draw_boxes env_name name)
                        @ !dots
                        @ List.map (RawContent.translate x_pg 0.)
                            (draw_boxes env0 pg)
                      in
                      let _, bb, _, dd = RawContent.bounding_box cont in
                      (* P4-2: extra air above chapter rows so the
                         groups read like the reference (9.4mm between
                         groups vs 5.5mm inside a group). *)
                      let dd = if is_chapter then dd +. 2.6 else dd in
                      Marker (BeginLink (Intern labl))
                      ::
                      Drawing
                        { drawing_min_width = env0.normalMeasure;
                          drawing_nominal_width = env0.normalMeasure;
                          drawing_max_width = env0.normalMeasure;
                          drawing_width_fixed = true;
                          drawing_adjust_before = false;
                          drawing_y0 = bb;
                          drawing_y1 = dd;
                          drawing_break_badness = 0.;
                          drawing_states = [];
                          drawing_badness = (fun _ -> 0.);
                          drawing_contents =
                            (fun _ ->
                              List.map
                                (RawContent.translate x_off 0.)
                                cont)
                        }
                      :: Marker EndLink
                      :: glue 0. 0. 0. :: chi
                    end else chi
                | Node _ -> []
              in
              toc
                { env with
                  counters = StrMap.add "_structure" (-1, [0]) env.counters }
                []
                (fst (top !tree))) ]

  module TableOfContents = struct
    let do_begin_env () =
      zh_these center D.structure D.structure 3
    let do_end_env () = ()
  end

  (* ---------------------------------------------------------------- *)
  (* Equations: chapter-scoped (章.n) numbering                       *)
  (* ---------------------------------------------------------------- *)

  let chapter_of_counters counters =
    try
      let _, b = StrMap.find "_structure" counters in
      match List.rev b with x :: _ -> x + 1 | [] -> 0
    with Not_found -> 0

  let equation contents =
    let pars a b c d e f g line =
      { (parameters a b c d e f g line) with
        min_height_before = (if line.lineStart = 0 then a.lead else 0.);
        min_height_after =
          (if line.lineEnd >= Array.length b.(line.paragraph) then a.lead
           else 0.) }
    in
    D.structure :=
      newPar ~environment:(fun env -> { env with par_indent = [] })
        !D.structure Complete.normal pars
        [ Env (incr_counter ~level:3 "equation");
          C (fun env ->
               let _, w = boxes_width env contents in
               let chap = chapter_of_counters env.counters in
               let _, x =
                 try StrMap.find "equation" env.counters
                 with Not_found -> (-1, [0])
               in
               let n = 1 + List.hd (if x = [] then [0] else x) in
               let num, w' =
                 boxes_width env
                   (italic
                      [ tT
                          (if chap > 0 then
                             Printf.sprintf "(%d.%d)" chap n
                           else Printf.sprintf "(%d)" n) ])
               in
               let w0 = (env.normalMeasure -. w) /. 2. in
               let w1 = env.normalMeasure -. w' -. w0 -. w in
               bB (fun _ -> [glue w0 w0 w0])
               :: contents
               @ [ bB (fun _ -> glue w1 w1 w1 :: num) ]) ];
    []

  (** Cross-reference an equation by label, producing “(3.1)”. *)
  let ref_equation name =
    [ C (fun env ->
          try
            let counters, _, _ = StrMap.find (make_name name) (names env) in
            let chap = chapter_of_counters counters in
            let _, x =
              try StrMap.find "equation" counters
              with Not_found -> (-1, [0])
            in
            let n = 1 + List.hd (if x = [] then [0] else x) in
            [ tT
                (if chap > 0 then Printf.sprintf "(%d.%d)" chap n
                 else Printf.sprintf "(%d)" n) ]
          with Not_found -> [tT "(?)"]) ]

  (** Numbered caption text for tables: “表 1 caption…” (sequential
      counter, like the reference book).  Wrap in a centred paragraph
      in the document. *)
  let head_counter kind n = tT (Printf.sprintf "%s %d " kind n)

  let ref_counter cnt name =
    [ C (fun env ->
          try
            let counters, _, _ = StrMap.find (make_name name) (names env) in
            let _, x =
              try StrMap.find cnt counters with Not_found -> (-1, [0])
            in
            let n = 1 + List.hd (if x = [] then [0] else x) in
            [ tT (string_of_int n) ]
          with Not_found -> [tT "?"]) ]

  (** Numbered caption text for tables: “表 1 caption…” (sequential
      counter, like the reference book).  Wrap in a centred paragraph
      in the document. *)
  let table_caption ?(kind = "表") contents =
    [ Env (incr_counter "table");
      C (fun env ->
           let _, x =
             try StrMap.find "table" env.counters
             with Not_found -> (-1, [0])
           in
           let n = 1 + List.hd (if x = [] then [0] else x) in
           head_counter kind n :: contents) ]

  (** Cross-reference a table by its label. *)
  let ref_table name = ref_counter "table" name

  (** Cross-reference a figure by its label. *)
  let ref_figure name = ref_counter "figure" name

  (** Numbered display equation from a maths list, e.g.
      [display_equation <$ a = b $>].  Uses display style. *)
  let display_equation m =
    equation [ bB (fun env0 -> Maths.kdraw [env0] (DefaultMacros.displayStyle m)) ]

  (* ---------------------------------------------------------------- *)
  (* Figures with 图 captions                                         *)
  (* ---------------------------------------------------------------- *)

  let figure_drawing ?parameters:(_ = center) ?name:(_ = "")
      ?(caption = []) ?(scale = 1.) drawing env =
    let dr = drawing env in
    let dr = resize_drawing scale dr in
    let _, num =
      try StrMap.find "figure" env.counters with Not_found -> (-1, [])
    in
    let caption, env, _ =
      OutputDrawing.minipage' { env with normalLeftMargin = 0. }
        (paragraph
           ([ tT "图"; tT " " ]
            @ [ tT
                  (String.concat "."
                     (List.map (fun x -> string_of_int (x + 1))
                        (List.rev num))) ]
            @ (if caption = [] then []
               else tT " " :: tT "–" :: tT " " :: caption)))
    in
    let caption =
      try IntMap.find 0 caption with Not_found -> empty_drawing_box
    in
    let fig =
      if caption.drawing_nominal_width <= dr.drawing_nominal_width then
        drawing_blit dr
          ((dr.drawing_nominal_width -. caption.drawing_nominal_width) /. 2.)
          (dr.drawing_y0 -. 2. *. caption.drawing_y1) caption
      else
        drawing_blit caption
          ((caption.drawing_nominal_width -. dr.drawing_nominal_width) /. 2.)
          (2. *. caption.drawing_y1 -. dr.drawing_y0) dr
    in
    { fig with drawing_y0 = fig.drawing_y0 -. env.lead }

  let figure ?(parameters = center) ?(name = "") ?(caption = [])
      ?(scale = 1.) drawing =
    Document.figure ~name:name D.structure center
      (figure_drawing ~parameters ~name ~caption ~scale drawing)

  let figure_here ?(parameters = center) ?(name = "") ?(caption = [])
      ?(scale = 1.) drawing =
    (match !D.structure with
     | Paragraph _, _ -> go_up D.structure
     | _ -> ());
    D.structure :=
      newPar !D.structure
        ~environment:(fun env -> { env with par_indent = [] })
        Complete.normal parameters
        (Env (incr_counter "figure")
         :: bB (fun env ->
              [ Drawing
                  (figure_drawing ~parameters ~name ~caption ~scale drawing
                     env) ])
         :: label name)

  (* ---------------------------------------------------------------- *)
  (* Output: use our postprocess_tree                                 *)
  (* ---------------------------------------------------------------- *)

  module Output (M : Driver.OutputDriver) = struct
    include Default.Output (M)

    let output out_params structure defaultEnv file =
      basic_output out_params (postprocess_tree structure) defaultEnv file
  end
end

module Format (D : DocumentStructure) =
  MakeFormat (D) (DefaultFormat.Format (D))
