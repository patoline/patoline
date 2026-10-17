open Earley_core
open Input
open Earley
open Asttypes
open Parsetree
open Pa_lexing
open Ast_helper
type entry =
  | FromExt 
  | Impl 
  | Intf 
let entry : entry ref = ref FromExt
let file : string option ref = ref None
let print_location ch { Location.loc_start = s; Location.loc_end = e } =
  let open Lexing in
    Printf.fprintf ch "Position %d:%d to %d:%d%!" s.pos_lnum
      (s.pos_cnum - s.pos_bol) e.pos_lnum (e.pos_cnum - e.pos_bol)
let string_location { Location.loc_start = s; Location.loc_end = e } =
  let open Lexing in
    Printf.sprintf "Position %d:%d to %d:%d%!" s.pos_lnum
      (s.pos_cnum - s.pos_bol) e.pos_lnum (e.pos_cnum - e.pos_bol)
let lexing_position str pos =
  let loff = line_offset str in
  let open Lexing in
    {
      pos_fname = (filename str);
      pos_lnum = (line_num str);
      pos_cnum = (loff + pos);
      pos_bol = loff
    }
let locate str pos str' pos' =
  let open Lexing in
    let s = lexing_position str pos in
    let e = lexing_position str' pos' in
    let open Location in { loc_start = s; loc_end = e; loc_ghost = false }
type entry_point =
  | Implementation of Parsetree.structure_item list grammar * blank 
  | Interface of Parsetree.signature_item list grammar * blank 
module Initial =
  struct
    let debug_attach = ref false
    let spec : (Arg.key * Arg.spec * Arg.doc) list =
      [("--impl", (Arg.Unit ((fun () -> entry := Impl))),
         "Treat file as an implementation.");
      ("--intf", (Arg.Unit ((fun () -> entry := Intf))),
        "Treat file as an interface.");
      ("--debug", (Arg.Set_int Earley.debug_lvl),
        "Sets the value of \"Earley.debug_lvl\".");
      ("--debug-attach", (Arg.Set debug_attach),
        "Debug ocamldoc comments attachment.")]
    let before_parse_hook : unit -> unit = fun () -> ()
    type expression_prio =
      | Seq 
      | If 
      | Aff 
      | Tupl 
      | Disj 
      | Conj 
      | Eq 
      | Append 
      | Cons 
      | Sum 
      | Prod 
      | Pow 
      | Opp 
      | App 
      | Dash 
      | Dot 
      | Prefix 
      | Atom 
    let expression_prios =
      [Seq;
      Aff;
      Tupl;
      Disj;
      Conj;
      Eq;
      Append;
      Cons;
      Sum;
      Prod;
      Pow;
      Opp;
      App;
      Dash;
      Dot;
      Prefix;
      Atom]
    let next_exp =
      function
      | Seq -> If
      | If -> Aff
      | Aff -> Tupl
      | Tupl -> Disj
      | Disj -> Conj
      | Conj -> Eq
      | Eq -> Append
      | Append -> Cons
      | Cons -> Sum
      | Sum -> Prod
      | Prod -> Pow
      | Pow -> Opp
      | Opp -> App
      | App -> Dash
      | Dash -> Dot
      | Dot -> Prefix
      | Prefix -> Atom
      | Atom -> Atom
    type alm =
      | NoMatch 
      | Match 
      | Let 
    let allow_match = function | Match -> true | _ -> false
    let allow_let = function | Match|Let -> true | _ -> false
    let string_exp (b, lvl) =
      (match b with | NoMatch -> "" | Match -> "m_" | Let -> "l_") ^
        (match lvl with
         | Seq -> "Seq"
         | If -> "If"
         | Aff -> "Aff"
         | Tupl -> "Tupl"
         | Disj -> "Disj"
         | Conj -> "Conj"
         | Eq -> "Eq"
         | Append -> "Append"
         | Cons -> "Cons"
         | Sum -> "Sum"
         | Prod -> "Prod"
         | Pow -> "Pow"
         | Opp -> "Opp"
         | App -> "App"
         | Dash -> "Dash"
         | Dot -> "Dot"
         | Prefix -> "Prefix"
         | Atom -> "Atom")
    let ((expression_lvl : (alm * expression_prio) -> expression grammar),
         set_expression_lvl)
      = grammar_family ~param_to_string:string_exp "expression_lvl"
    let expression = expression_lvl (Match, Seq)
    let structure_item : structure_item list grammar =
      declare_grammar "structure_item"
    let signature_item : signature_item list grammar =
      declare_grammar "signature_item"
    let ((parameter :
           bool ->
             [ `Arg of (arg_label * expression option * pattern) 
             | `Type of string Location.loc ] grammar),
         set_parameter)
      = grammar_family "parameter"
    let structure = structure_item
    let signature = Earley_core.Earley.declare_grammar "signature"
    let _ =
      Earley_core.Earley.set_grammar signature
        (Earley_core.Earley.fsequence
           (Earley_core.Earley.apply (fun f -> f [])
              (Earley_core.Earley.fixpoint' (fun l -> l) signature_item
                 (fun x -> fun f -> fun l -> f (List.cons x l))))
           (Earley_core.Earley.empty (fun l -> List.flatten l)))
    type type_prio =
      | TopType 
      | As 
      | Arr 
      | ProdType 
      | DashType 
      | AppType 
      | AtomType 
    let type_prios =
      [TopType; As; Arr; ProdType; DashType; AppType; AtomType]
    let type_prio_to_string (_, lvl) =
      match lvl with
      | TopType -> "TopType"
      | As -> "As"
      | Arr -> "Arr"
      | ProdType -> "ProdType"
      | DashType -> "DashType"
      | AppType -> "AppType"
      | AtomType -> "AtomType"
    let next_type_prio =
      function
      | TopType -> As
      | As -> Arr
      | Arr -> ProdType
      | ProdType -> DashType
      | DashType -> AppType
      | AppType -> AtomType
      | AtomType -> AtomType
    let ((typexpr_lvl_raw : (bool * type_prio) -> core_type grammar),
         set_typexpr_lvl)
      = grammar_prio ~param_to_string:type_prio_to_string "typexpr_lvl"
    let typexpr_lvl lvl = typexpr_lvl_raw (true, lvl)
    let typexpr_nopar = typexpr_lvl_raw (false, TopType)
    let typexpr = typexpr_lvl TopType
    type pattern_prio =
      | AltPat 
      | TupPat 
      | ConsPat 
      | ConstrPat 
      | AtomPat 
    let topPat = AltPat
    let pat_prio_to_string (_, lvl) =
      match lvl with
      | AltPat -> "AltPat"
      | TupPat -> "TupPat"
      | ConsPat -> "ConsPat"
      | ConstrPat -> "ConstrPat"
      | AtomPat -> "AtomPat"
    let next_pat_prio =
      function
      | AltPat -> TupPat
      | TupPat -> ConsPat
      | ConsPat -> ConstrPat
      | ConstrPat -> AtomPat
      | AtomPat -> assert false
    let ((pattern_lvl : (bool * pattern_prio) -> pattern grammar),
         set_pattern_lvl)
      = grammar_prio ~param_to_string:pat_prio_to_string "pattern_lvl"
    let pattern = pattern_lvl (true, topPat)
    let let_binding : value_binding list grammar =
      declare_grammar "let_binding"
    let class_body : class_structure grammar = declare_grammar "class_body"
    let class_expr : class_expr grammar = declare_grammar "class_expr"
    let value_path : Longident.t grammar = declare_grammar "value_path"
    let extra_expressions =
      ([] : ((alm * expression_prio) -> expression grammar) list)
    let extra_prefix_expressions = ([] : expression grammar list)
    let extra_types = ([] : (type_prio -> core_type grammar) list)
    let extra_patterns =
      ([] : ((bool * pattern_prio) -> pattern grammar) list)
    let extra_structure = ([] : structure_item list grammar list)
    let extra_signature = ([] : signature_item list grammar list)
    type record_field = (Longident.t Asttypes.loc * Parsetree.expression)
    let constr_decl_list : constructor_declaration list grammar =
      declare_grammar "constr_decl_list"
    let field_decl_list : label_declaration list grammar =
      declare_grammar "field_decl_list"
    let record_list : record_field list grammar =
      declare_grammar "record_list"
    let match_cases : case list grammar = declare_grammar "match_cases"
    let module_expr : module_expr grammar = declare_grammar "module_expr"
    let module_type : module_type grammar = declare_grammar "module_type"
    let parse_string' g e' =
      try parse_string g ocaml_blank e'
      with | e -> (Printf.eprintf "Error in quotation: %s\n%!" e'; raise e)
    let mk_attrib loc txt contents =
      let str = Const.string contents in
      ({ txt; loc = Location.none },
        (PStr [Str.eval ~loc (Exp.constant ~loc str)]))
    let attach_attrib =
      let tbl_s = Hashtbl.create 31 in
      let tbl_e = Hashtbl.create 31 in
      let attach_attrib loc (acc : attribute list) =
        (let open Location in
           let open Lexing in
             if !debug_attach then Printf.eprintf "enter attach\n%!";
             (let rec fn acc res =
                function
                | [] -> res
                | ((start, end_, contents, _) as c)::rest ->
                    let start' = loc.loc_start in
                    let lend = line_num (fst end_) in
                    let loc =
                      locate (fst start) (snd start) (fst end_) (snd end_) in
                    (if !debug_attach
                     then
                       Printf.eprintf "start [%d,%d] [%d,...]\n%!"
                         (line_num (fst start)) lend start'.pos_lnum;
                     if
                       (start'.pos_lnum > lend) &&
                         ((start'.pos_lnum - lend) <= 1)
                     then
                       (if !debug_attach
                        then Printf.eprintf "attach backward %s\n%!" contents;
                        ocamldoc_comments := (List.rev_append acc rest);
                        if contents <> ""
                        then (mk_attrib loc "ocaml.doc" contents) :: res
                        else res)
                     else fn (c :: acc) res rest) in
              let rec gn acc res =
                function
                | [] -> List.rev res
                | ((start, end_, contents, lstart) as c)::rest ->
                    let end' = loc.loc_end in
                    let loc =
                      locate (fst start) (snd start) (fst end_) (snd end_) in
                    (if !debug_attach
                     then
                       Printf.eprintf "end[%d,%d] [...,%d]\n%!" lstart
                         (line_num (fst end_)) end'.pos_lnum;
                     if
                       (lstart >= end'.pos_lnum) &&
                         ((lstart - end'.pos_lnum) <= 1)
                     then
                       (if !debug_attach
                        then Printf.eprintf "attach forward %s\n%!" contents;
                        ocamldoc_comments := (List.rev_append rest acc);
                        if contents <> ""
                        then (mk_attrib loc "ocaml.doc" contents) :: res
                        else res)
                     else gn (c :: acc) res rest) in
              let l2 =
                try Hashtbl.find tbl_e ((loc.loc_start), (loc.loc_end))
                with
                | Not_found ->
                    let res = gn [] [] (List.rev (!ocamldoc_comments)) in
                    (Hashtbl.add tbl_e ((loc.loc_start), (loc.loc_end)) res;
                     res) in
              let l1 =
                try Hashtbl.find tbl_s loc.loc_start
                with
                | Not_found ->
                    let res = fn [] [] (!ocamldoc_comments) in
                    (Hashtbl.add tbl_s loc.loc_start res; res) in
              let l1 = List.map (fun (s, pl) -> Attr.mk s pl) l1 in
              let l2 = List.map (fun (s, pl) -> Attr.mk s pl) l2 in
              l1 @ (acc @ l2)) : attribute list) in
      attach_attrib
    let attach_gen build =
      let tbl = Hashtbl.create 31 in
      fun loc ->
        let open Location in
          let open Lexing in
            let rec fn acc res =
              function
              | [] -> (ocamldoc_comments := (List.rev acc); res)
              | ((start, end_, contents, _) as c)::rest ->
                  let start' = loc.loc_start in
                  let loc =
                    locate (fst start) (snd start) (fst end_) (snd end_) in
                  (if !debug_attach
                   then
                     Printf.eprintf "sig/str [%d,%d] [%d,...]\n%!"
                       (line_num (fst start)) (line_num (fst end_))
                       start'.pos_lnum;
                   if (line_num (fst end_)) < (start'.pos_lnum - 1)
                   then
                     (if !debug_attach
                      then Printf.eprintf "attach ocaml.text %s\n%!" contents;
                      fn acc
                        ((build loc (mk_attrib loc "ocaml.text" contents)) ::
                        res) rest)
                   else fn (c :: acc) res rest) in
            if !debug_attach
            then
              Printf.eprintf "enter attach sig/str [%d,...] %d\n%!"
                (loc.loc_start).pos_lnum (List.length (!ocamldoc_comments));
            (try Hashtbl.find tbl loc.loc_start
             with
             | Not_found ->
                 let res = fn [] [] (!ocamldoc_comments) in
                 (Hashtbl.add tbl loc.loc_start res; res))
    let attach_sig =
      attach_gen
        (fun loc -> fun (str, pl) -> Sig.attribute ~loc (Attr.mk ~loc str pl))
    let attach_str =
      attach_gen
        (fun loc -> fun (str, pl) -> Str.attribute ~loc (Attr.mk ~loc str pl))
    let union_re l =
      let l = List.map (fun s -> "\\(" ^ (s ^ "\\)")) l in
      String.concat "\\|" l
    let arrow_re = Earley_core.Earley.declare_grammar "arrow_re"
    let _ =
      Earley_core.Earley.set_grammar arrow_re
        (Earley_str.regexp ~name:"\\\\(->\\\\)\\\\|\\\\(\\226\\134\\146\\\\)"
           "\\(->\\)\\|\\(\226\134\146\\)" (fun group -> group 0))
    let infix_symb_re prio =
      match prio with
      | Prod ->
          union_re
            ["[/%][!$%&*+./:<=>?@^|~-]*";
            "[*]\\([!$%&+./:<=>?@^|~-][!$%&*+./:<=>?@^|~-]*\\)?";
            "mod\\b";
            "land\\b";
            "lor\\b";
            "lxor\\b"]
      | Sum -> "[+-][!$%&*+./:<=>?@^|~-]*"
      | Append -> "[@^][!$%&*+./:<=>?@^|~-]*"
      | Cons -> "::"
      | Aff ->
          union_re [":=[!$%&*+./:<=>?@^|~-]*"; "<-[!$%&*+./:<=>?@^|~-]*"]
      | Eq ->
          union_re
            ["[<][!$%&*+./:<=>?@^|~]?[!$%&*+./:<=>?@^|~-]*";
            "[&][!$%*+./:<=>?@^|~-][!$%&*+./:<=>?@^|~-]*";
            "|[!$%&*+./:<=>?@^~-][!$%&*+./:<=>?@^|~-]*";
            "[=>$][!$%&*+./:<=>?@^|~-]*";
            "!="]
      | Conj -> union_re ["[&][&][!$%&*+./:<=>?@^|~-]*"; "[&]"]
      | Disj -> union_re ["or\\b"; "||[!$%&*+./:<=>?@^|~-]*"]
      | Pow ->
          union_re
            ["lsl\\b"; "lsr\\b"; "asr\\b"; "[*][*][!$%&*+./:<=>?@^|~-]*"]
      | _ -> assert false
    let infix_prios = [Prod; Sum; Append; Cons; Aff; Eq; Conj; Disj; Pow]
    let prefix_symb_re prio =
      match prio with
      | Opp -> union_re ["-[.]?"; "+[.]?"]
      | Prefix ->
          union_re ["[!][!$%&*+./:<=>?@^|~-]*"; "[~?][!$%&*+./:<=>?@^|~-]+"]
      | _ -> assert false
    let prefix_prios = [Opp; Prefix]
    let (infix_symbol, infix_symbol__set__grammar) =
      Earley_core.Earley.grammar_family "infix_symbol"
    let _ =
      infix_symbol__set__grammar
        (fun prio ->
           Earley_core.Earley.alternatives
             (List.append
                (if prio <> Cons
                 then
                   List.cons
                     (Earley_core.Earley.fsequence
                        (Earley_str.regexp (infix_symb_re prio)
                           (fun group -> group 0))
                        (Earley_core.Earley.fsequence not_special
                           (Earley_core.Earley.empty
                              (fun _default_0 ->
                                 fun sym ->
                                   if is_reserved_symb sym then give_up ();
                                   sym)))) []
                 else [])
                (List.append
                   (if prio = Cons
                    then
                      List.cons
                        (Earley_core.Earley.fsequence_ignore
                           (Earley_core.Earley.string "::" "::")
                           (Earley_core.Earley.empty "::")) []
                    else []) [])))
    let (prefix_symbol, prefix_symbol__set__grammar) =
      Earley_core.Earley.grammar_family "prefix_symbol"
    let _ =
      prefix_symbol__set__grammar
        (fun prio ->
           Earley_core.Earley.fsequence
             (Earley_str.regexp (prefix_symb_re prio) (fun group -> group 0))
             (Earley_core.Earley.fsequence not_special
                (Earley_core.Earley.empty
                   (fun _default_0 ->
                      fun sym ->
                        if (is_reserved_symb sym) || (sym = "!=")
                        then give_up ();
                        sym))))
    let mutable_flag = Earley_core.Earley.declare_grammar "mutable_flag"
    let _ =
      Earley_core.Earley.set_grammar mutable_flag
        (Earley_core.Earley.alternatives
           (List.cons
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.empty ())
                 (Earley_core.Earley.empty Immutable))
              (List.cons
                 (Earley_core.Earley.fsequence mutable_kw
                    (Earley_core.Earley.empty (fun _default_0 -> Mutable)))
                 [])))
    let private_flag = Earley_core.Earley.declare_grammar "private_flag"
    let _ =
      Earley_core.Earley.set_grammar private_flag
        (Earley_core.Earley.alternatives
           (List.cons
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.empty ())
                 (Earley_core.Earley.empty Public))
              (List.cons
                 (Earley_core.Earley.fsequence private_kw
                    (Earley_core.Earley.empty (fun _default_0 -> Private)))
                 [])))
    let virtual_flag = Earley_core.Earley.declare_grammar "virtual_flag"
    let _ =
      Earley_core.Earley.set_grammar virtual_flag
        (Earley_core.Earley.alternatives
           (List.cons
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.empty ())
                 (Earley_core.Earley.empty Concrete))
              (List.cons
                 (Earley_core.Earley.fsequence virtual_kw
                    (Earley_core.Earley.empty (fun _default_0 -> Virtual)))
                 [])))
    let rec_flag = Earley_core.Earley.declare_grammar "rec_flag"
    let _ =
      Earley_core.Earley.set_grammar rec_flag
        (Earley_core.Earley.alternatives
           (List.cons
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.empty ())
                 (Earley_core.Earley.empty Nonrecursive))
              (List.cons
                 (Earley_core.Earley.fsequence rec_kw
                    (Earley_core.Earley.empty (fun _default_0 -> Recursive)))
                 [])))
    let downto_flag = Earley_core.Earley.declare_grammar "downto_flag"
    let _ =
      Earley_core.Earley.set_grammar downto_flag
        (Earley_core.Earley.alternatives
           (List.cons
              (Earley_core.Earley.fsequence downto_kw
                 (Earley_core.Earley.empty (fun _default_0 -> Downto)))
              (List.cons
                 (Earley_core.Earley.fsequence to_kw
                    (Earley_core.Earley.empty (fun _default_0 -> Upto))) [])))
    let entry_points : (string * entry_point) list =
      [(".mli", (Interface (signature, ocaml_blank)));
      (".ml", (Implementation (structure, ocaml_blank)))]
  end
module type Extension  = module type of Initial
module type FExt  = functor (E : Extension) -> Extension
include Initial
let start_pos loc = loc.Location.loc_start
let end_pos loc = loc.Location.loc_end
