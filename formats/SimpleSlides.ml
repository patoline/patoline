open RawContent
open Extra

open Typography
open Typography.Box
open Typography.Document

(* Slide numbering. *)
type numbering_kind =
  | SimpleNumbering
  | RelativeNumbering
  | CustomNumbering of (int -> int -> content list)

let string_numbering : (int -> int -> string) -> numbering_kind = fun f ->
  CustomNumbering (fun i m -> [tT (f i m)])

let slide_numbering = ref (Some SimpleNumbering)

module Format(D:DocumentStructure) =
  struct
    module Default = DefaultFormat.Format(D)
    
    let env_stack = Default.env_stack
    let parameters = Default.parameters

    let verbs_default = Default.verbs_default

    (* Some default environments. *)
    module Env_env         = Default.Env_env
    module Env_center      = Default.Env_center
    module Env_raggedLeft  = Default.Env_raggedLeft
    module Env_raggedRight = Default.Env_raggedRight
    module Env_itemize     = Default.Env_itemize
    module Env_enumerate   = Default.Env_enumerate

    type number_kind =
      Arabic | AlphaLower | AlphaUpper | RomanLower | RomanUpper

    module type Enumerate_Pattern = sig
      val arg1 : number_kind * (string -> content list)
    end

    module Env_genumerate = functor (Pat:Enumerate_Pattern) ->
      Default.Enumerate(struct
        let c, f = Pat.arg1
        let g = match c with
          | Arabic -> string_of_int
          | AlphaLower -> Numerals.alphabetic ~capital:false
          | AlphaUpper -> Numerals.alphabetic ~capital:true
          | RomanLower -> Numerals.roman ~capital:false
          | RomanUpper -> Numerals.roman ~capital:true
        let from_counter x =
          let x = List.hd x + 1 in f (g x)
      end)

    let ragged_left      = Default.ragged_left
    let displayedFormula = Default.displayedFormula

    (* Constants. *)
    let slideh  = 100.0
    let slidew  = phi *. slideh
    let hoffset = 10.0

    (* Default environment. *)
    let measure = (slidew /. 2.0) *. phi

    let defaultEnv =
      let new_page t =
        let zip = Box.make_page (slidew, slideh) (frame_top t) in
        let x0 = (fst zip).frame_x0 +. slidew /. 6.0 in
        let y0 = -. slideh in
        let x1 = (fst zip).frame_x1 -. slidew /. 6.0 in
        let y1 = (fst zip).frame_y1 -. slideh /. 6.0 in
        frame x0 y0 x1 y1 zip
      in
      { Default.defaultEnv
      with normalMeasure = measure
      ; normalLeftMargin = (slidew -. measure) /. 2.0
      ; normalLead       = Default.defaultEnv.normalLead *. 1.2
      ; lead             = Default.defaultEnv.normalLead *. 1.2
      ; normalPageFormat = (slidew, slideh)
      ; hyphenate        = (fun _ -> [||])
      ; par_indent       = []
      ; new_line         = (fun env _ _ _ _ _ height -> height -. env.lead)
      ; new_page }

    (* Slide environments. *)
    module Slide(T : sig val title : content list option end) =
      struct
        let do_begin_env () =
          let title =
            match T.title with
            | None   -> empty.displayname
            | Some t -> t
          in
          let node_tags   = ("slide", "") :: empty.node_tags in
          let node_env    = incr_counter "slide" in
          let displayname = title in
          let name        = string_of_contents title in
          let n = { empty with node_tags ; node_env ; displayname ; name } in
          D.structure := newChildAfter !D.structure (Node n);
          env_stack   := (List.map fst (snd !D.structure)) :: !env_stack;
          D.structure := lastChild !D.structure
  
        let do_end_env () =
          let path = List.rev (List.hd !env_stack) in
          let slide = follow (top !D.structure) path in
          D.structure := up slide;
          env_stack   := List.tl !env_stack
      end

    module Env_raw_slide =
      Slide(struct let title = None end)

    let titleStyle t = size (defaultEnv.size *. 1.2) (bold t)

    module Env_slide(Title : sig val arg1 : content list end) =
      Slide(struct let title = Some (titleStyle Title.arg1) end)

    (* Verbatim environment with a smaller lead. *)
    let verbEnv env =
      { (envFamily env.fontMonoFamily env)
      with size       = env.size *. env.fontMonoRatio
      ; normalMeasure = infinity
      ; par_indent    = []
      ; lead          = env.lead *. env.fontMonoRatio *. 0.75
      ; normalLead    = env.normalLead *. env.fontMonoRatio *. 0.75 }

    (* States environment. *)
    module Env_states(S : sig val arg1 : int list end) =
      struct
        let do_begin_env () =
          D.structure := newChildAfter !D.structure (Node empty);
          env_stack   := (List.map fst (snd !D.structure)) :: !env_stack

        let do_end_env () =
          let filter st = List.filter (fun s -> List.mem s st) in
          let rec restate st = function
            | Paragraph p ->
                let par_states =
                  if p.par_states = [] then st
                  else filter st p.par_states
                in Paragraph { p with par_states }
            | Node n      ->
                let children =
                  let st =
                    if n.node_states = [] then st
                    else filter st n.node_states
                  in IntMap.map (restate st) n.children
                in Node { n with children }
            | t           -> t
          in
          let path = List.rev (List.hd !env_stack) in
          let (slide, path) = follow (top !D.structure) path in
          D.structure := up (restate S.arg1 slide,path);
          env_stack   := List.tl !env_stack
      end

    (* Output module. *)
    module Output(M : Driver.OutputDriver) =
      struct
        open Driver
        open FTypes

        type output = unit
        let outputParams : output = ()

        (* TODO from here. *)
        let output _ tree defaultEnv file =
          let slides = ref [] in

          let typeset_slide path tree layout env0 n =
            let hasTitle = n.displayname <> [] in
            let new_page t =
              let zip = Box.make_page (slidew, slideh) (frame_top t) in
              let x0 = (fst zip).frame_x0 +. slidew /. 6.0 in
              let y0 = -. slideh in
              let x1 = (fst zip).frame_x1 -. slidew /. 6.0 in
              let d  = if hasTitle then 7.0 else 14.0 in
              let y1 = (fst zip).frame_y1 -. slideh /. d in
              frame x0 y0 x1 y1 zip
            in
            let env0 = {env0 with new_page} in

            let rec get_max_state = function
              | Paragraph p -> List.fold_left max 0 p.par_states
              | Node n      -> let f _ a m = max m (get_max_state a) in
                               let ms = List.fold_left max 0 n.node_states in
                               IntMap.fold f n.children ms
              | _           -> 0
            in

            let (env1, fig_params0, params0, new_page0, new_line0
                , compl0, badnesses0, paragraphs0, _, figures0
                , figure_trees0, states0) =
                  flatten ~initial_path:path env0 tree
            in

            let max_state =
              let f = 
                let g maxs box =
                  match box with
                  | Drawing d
                  | Glue d ->
                      max (List.fold_left max maxs d.drawing_states) maxs
                  | _      -> maxs
                in Array.fold_left g
              in Array.fold_left f (get_max_state tree) paragraphs0
            in
            let opts = Array.make (max_state+1) [] in

            let rec typeset_states state layout0 env =
              if state > max_state then (layout0, env) else
              let real_par = ref 0 in
              let par_map = ref IntMap.empty in
              let rec make_paragraphs = function
                | Paragraph p when List.mem state p.par_states || p.par_states = [] ->
                    let n =
                      try fst (IntMap.max_binding !par_map)
                      with Not_found -> (-1)
                    in
                    par_map := IntMap.add (n+1) !real_par !par_map;
                    incr real_par
                | Paragraph p -> incr real_par
                | Node n      ->
                    IntMap.iter (fun _ t -> make_paragraphs t) n.children
                | _           -> ()
              in
              make_paragraphs tree;

              let (params, new_page, new_line, paragraphs, compl, bad, states) =
                if IntMap.is_empty !par_map then
                  ([||], [||], [||], [||], [||], [||], [||])
                else
                  ( Array.make (IntMap.cardinal !par_map) params0.(0)
                  , Array.make (IntMap.cardinal !par_map) new_page0.(0)
                  , Array.make (IntMap.cardinal !par_map) new_line0.(0)
                  , Array.make (IntMap.cardinal !par_map) paragraphs0.(0)
                  , Array.make (IntMap.cardinal !par_map) compl0.(0)
                  , Array.make (IntMap.cardinal !par_map) badnesses0.(0)
                  , Array.make (IntMap.cardinal !par_map) states0.(0) )
              in
              let f k a =
                params.(k)     <- params0.(a);
                paragraphs.(k) <- paragraphs0.(a);
                compl.(k)      <- compl0.(a);
                bad.(k)        <- badnesses0.(a);
                states.(k)     <- states0.(a)
              in IntMap.iter f !par_map;

              let (logs_, opt_pages, figs', user') =
                let layout = Box.frame_top layout0 in
                let initial_line = {uselessLine with layout} in
                TS.typeset ~initial_line ~completeLine:compl
                  ~figure_parameters:[||] ~figures:[||] ~parameters:params
                  ~new_page ~new_line ~badness:bad ~states paragraphs
              in
              let f cont l =
                match l with
                | Placed_line l' ->
                    begin try
                      let paragraph =
                        IntMap.find l'.line.paragraph !par_map
                      in
                      let line = {l'.line with paragraph} in
                      {l' with line} :: cont
                    with Not_found -> cont end
                | _              -> cont
              in
              let c =
                try all_contents (snd (IntMap.max_binding (opt_pages.frame_children)))
                with Not_found -> []
              in
              opts.(state) <- List.fold_left f [] c;

              let next_layout =
                if state = 0 then (opt_pages, []) else
                let (f, l) = frame_down_last (opt_pages, []) in
                let frame_tags = "not_first_state" :: f.frame_tags in
                frame_top ({f with frame_tags}, l)
              in
              typeset_states (state+1) next_layout env1
            in

            let (layout', env') =
              typeset_states 0 (Box.frame_top layout) env1
            in

            (* Position la plus basse de la première ligne de chaque paragraphe *)
            let l = Array.length paragraphs0 in
            let state_start = Array.make_matrix (max_state+1) l infinity in
            let lowest_start = Array.make l infinity in
            let f i l =
              let g ll =
                if ll.line.lineStart = 0 then
                  begin
                    state_start.(i).(ll.line.paragraph) <- ll.line.height;
                    lowest_start.(ll.line.paragraph)    <-
                      min ll.line.height lowest_start.(ll.line.paragraph)
                  end
              in List.iter g l
            in
            Array.iteri f opts;

            (* Premier positionnement de tout le monde ("aligné" en haut) *)
            let max_frame= ref (-.infinity) in
            let min_frame= ref infinity in
            let max_h = ref (-.infinity) in
            let min_h = ref infinity in
            for i = 0 to Array.length opts - 1 do
              let f ll =
                max_frame := max !max_frame (fst ll.line.layout).frame_y1;
                min_frame := min !min_frame (fst ll.line.layout).frame_y0;

                let y0,y1=line_height paragraphs0 [||] ll.line in
                max_h := max !max_h (ll.line.height+.y1);
                min_h := min !min_h (ll.line.height+.y0);
                let height = 
                  ll.line.height -. state_start.(i).(ll.line.paragraph)
                  +. lowest_start.(ll.line.paragraph)
                in
                {ll with line = {ll.line with height}}
              in
              opts.(i) <- List.map f opts.(i)
            done;
            min_frame := max (slideh /. 6.0) !min_frame;

            (* Centrage collectif *)
            for i = 0 to Array.length opts - 1 do
              let place =
                (!max_frame -. !min_frame -.(!max_h-. !min_h)) /. 2.0
              in
              let place =
                match classify_float place with
                | FP_infinite | FP_nan -> 0.0 | _ -> max 0.0 place
              in
              let f ll =
                let height = ll.line.height -. place in
                {ll with line = {ll.line with height}}
              in
              opts.(i) <- List.map f opts.(i)
            done;

            (* Fin du placement vertical *)

            let i =
              try fst (IntMap.max_binding (fst (frame_top layout')).frame_children)
              with Not_found -> (-1)
            in
            slides :=
              (path, tree, paragraphs0, figures0, figure_trees0, env'
              , opts, i) :: !slides;
            (layout', n.node_post_env env0 env')
          in

          let rec typeset_structure path tree layout env0 =
            let is_slide n = List.mem_assoc "slide" n.node_tags in
            match tree with
            | Node n      ->
                if is_slide n then typeset_slide path tree layout env0 n else
                let f k a (l,e) = typeset_structure (k::path) a l e in
                let (l,e) =
                  IntMap.fold f n.children (layout, n.node_env env0)
                in (l, n.node_post_env env0 e)
            | Paragraph p -> (layout, p.par_post_env env0 (p.par_env env0))
            | FigureDef f -> (layout, f.fig_post_env env0 (f.fig_env env0))
          in

          (* Optimization. *)
          let (layout_final, env_final) =
            let env =
              match tree with
              | Node n      -> n.node_env defaultEnv
              | Paragraph p -> p.par_env defaultEnv
              | _           -> defaultEnv
            in
            let t = Sys.time () in
            Printf.fprintf stderr "Optimization starts: %f s\n%!" t;
            let r = typeset_structure [] tree (empty_frame, []) env in
            let t = Sys.time () in
            Printf.fprintf stderr "Optimization ends  : %f s\n%!" t; r
          in

          let draw_slide_number env i=
            match !slide_numbering with
            | None    -> []
            | Some sn ->
                 let i=try List.hd (snd (StrMap.find "slide" env.counters)) with _->0 in
                 let i_fin=try List.hd (snd (StrMap.find "slide" env_final.counters)) with _->0 in
                 let num =
                   match sn with
                   | SimpleNumbering   -> [tT (Printf.sprintf "%d" (i+1))]
                   | RelativeNumbering -> [tT (Printf.sprintf "%d/%d" (i+1) (i_fin+1))]
                   | CustomNumbering f -> f i i_fin
                 in
                 let boxes=boxify_scoped env num in
                 let w=List.fold_left (fun w x->let _,w',_=box_interval x in w+.w') 0. boxes in
                 let x=draw_boxes env boxes in
                 (*
                 List.map (fun y->RawContent.translate (slidew-.w-.2.) 2. (in_order max_int y)) x)
                 *)
                 x
          in

          let draw_slide slide_number (path,tree,paragraphs,figures,figure_trees,env,opts,slide_num)=
            let states = ref [] in
            let destinations = ref StrMap.empty in

            for st = 0 to Array.length opts - 1 do
              let page = empty_page (slidew, slideh) in

              let title =
                match tree with
                | Node n ->
                    let t = Default.paragraph n.displayname in
                    let (mp,_,_) = OutputDrawing.minipage' ~state:st env t in
                    begin
                      try
                        let d = snd (IntMap.min_binding mp) in
                        d.drawing_contents d.drawing_nominal_width
                      with Not_found -> []
                    end
                | _      -> []
              in
              let trans = RawContent.translate 0.0 (slideh -. hoffset) in
              let title = List.map trans title in
              page.contents <- title;

              let pp = Array.of_list opts.(st) in

              let draw_line line=
                let param = line.line_params in
                let line = line.line in
                let y = line.height in

                if line.isFigure then
                  let fig = figures.(line.lastFigure) in
                  let trs = RawContent.translate param.left_margin y in
                  let fig = fig.drawing_contents fig.drawing_nominal_width in
                  page.contents <- (List.map trs fig) @ page.contents
                else if line.paragraph < Array.length paragraphs then
                  let comp = compression paragraphs param line in
                  let dr_in_state dr st =
                    List.mem st dr.drawing_states || dr.drawing_states = []
                  in
                  let rec draw_box x y = function
                    | Kerning kbox ->
                        let fact = box_size kbox.kern_contents /. 1000.0 in
                        let x' = x +. kbox.kern_x0 *. fact in
                        let y' = y +. kbox.kern_y0 *. fact in
                        let w = draw_box x' y' kbox.kern_contents in
                        w +. kbox.advance_width *. fact
                    | Hyphen h     ->
                        let f x' box = x' +. (draw_box (x+.x') y box) in
                        Array.fold_left f 0.0 h.hyphen_normal
                    | GlyphBox a   ->
                        let glyph_x = a.glyph_x +. x in
                        let glyph_y = a.glyph_y +. y in
                        let gl = {a with glyph_x ; glyph_y} in
                        let gl = RawContent.Glyph gl in
                        page.contents <- gl :: page.contents;
                        a.glyph_size *. (Fonts.glyphWidth a.glyph) /. 1000.0
                    | Glue g
                    | Drawing g when dr_in_state g st ->
                        let wd = g.drawing_max_width -. g.drawing_min_width in
                        let w = g.drawing_min_width +. comp *. wd in
                        let cont = g.drawing_contents w in
                        let cont_states =
                          let f = function
                            | States s -> (s.states_states = []) ||
                                          (List.mem st s.states_states)
                            | _        -> true
                          in
                          List.filter f cont
                        in
                        let trs = RawContent.translate x y in
                        let cts = List.map trs cont_states in
                        page.contents <- cts @ page.contents; w
                    | b            -> box_width comp b
                  in
                  ignore (fold_left_line paragraphs
                    (fun x b -> x +. draw_box x y b) param.left_margin line)
              in
              for j = 0 to Array.length pp-1 do draw_line pp.(j) done;

              let rec more_contents f =
                let g = function
                  | Placed_line l -> ()
                  | Raw r         ->
                      page.contents <- (in_state st r) @ page.contents
                in
                List.iter g f.frame_content;
                IntMap.iter (fun k a -> more_contents a) f.frame_children;
              in
              begin
                try
                  let fc = fst (frame_top layout_final) in
                  let f = IntMap.find slide_num fc.frame_children in
                  more_contents f
                with Not_found -> ()
              end;
              let sn = draw_slide_number env slide_number in
              page.contents <- sn @ page.contents;
              states := page :: !states
            done;

            let env =
              let f labl (lm, y0, y1, line) env =
                { env with user_positions =
                  MarkerMap.add (Label labl) line (user_positions env)}
              in
              StrMap.fold f !destinations env
            in
            env,Array.of_list (List.rev !states)
          in

          let slide_num = ref 0 in
          let structPosition = ref (0.,0.) in
          let rec make_structure pages = function
            | Node n when List.mem_assoc "intoc" n.node_tags ->
                  let num = !slide_num in
                  let f _ a m = (make_structure pages a) @ m in
                  let sub = IntMap.fold f n.children [] in
                  [ { name = n.name ; raw_name = [] ; metadata = []
                    ; tags = n.node_tags; page = num
                    ; struct_x = fst !structPosition
                    ; struct_y = snd !structPosition
                    ; children = Array.of_list (List.rev sub) } ]
            | Node n when List.mem_assoc "slide" n.node_tags ->
                begin
                  structPosition := (snd pages.(!slide_num)).(0).size;
                  incr slide_num; []
                end
            | Node n ->
                let f _ a = ignore (make_structure pages a) in
                IntMap.iter f n.children; []
            | _ -> []
          in

          let (pages, structure) =
            match !Driver.input_bin with
            | None    ->
                let slides = Array.of_list (List.rev !slides) in
                let pages = Array.mapi draw_slide slides in
                let str =
                  match make_structure pages tree with
                  | h::_ -> h
                  | []   -> empty_structure
                in (pages, str)
            | Some fn ->
                let ch = open_in fn in
                let b = input_value ch in
                if not b then failwith "Wrong bin for this format";
                let structure = Marshal.from_channel ch in
                let pages = Marshal.from_channel ch in
                close_in ch;
                Printf.fprintf stderr "File %s read.\n" fn;
                (pages, structure)
          in

          M.output' ~structure (Array.map snd pages) file
    end
  end
