open Input
open Glr
open Charset
open Asttypes
open Parsetree
open Longident
include Pa_ocaml_prelude
let _ = ()
module Make(Initial:Extension) =
  struct
    include Initial
    let mk_unary_opp name _loc_name arg _loc_arg =
      let res =
        match (name, (arg.pexp_desc)) with
        | ("-",Pexp_constant (Const_int n)) ->
            Pexp_constant (Const_int (- n))
        | ("-",Pexp_constant (Const_int32 n)) ->
            Pexp_constant (Const_int32 (Int32.neg n))
        | ("-",Pexp_constant (Const_int64 n)) ->
            Pexp_constant (Const_int64 (Int64.neg n))
        | ("-",Pexp_constant (Const_nativeint n)) ->
            Pexp_constant (Const_nativeint (Nativeint.neg n))
        | (("-"|"-."),Pexp_constant (Const_float f)) ->
            Pexp_constant (Const_float ("-" ^ f))
        | ("+",Pexp_constant (Const_int _))
          |("+",Pexp_constant (Const_int32 _))
          |("+",Pexp_constant (Const_int64 _))
          |("+",Pexp_constant (Const_nativeint _))
          |(("+"|"+."),Pexp_constant (Const_float _)) -> arg.pexp_desc
        | (("-"|"-."|"+"|"+."),_) ->
            let p =
              loc_expr _loc_name
                (Pexp_ident (id_loc (Lident ("~" ^ name)) _loc_name)) in
            Pexp_apply (p, [("", arg)])
        | _ ->
            let p =
              loc_expr _loc_name
                (Pexp_ident (id_loc (Lident name) _loc_name)) in
            Pexp_apply (p, [("", arg)]) in
      loc_expr (merge2 _loc_name _loc_arg) res
    let float_lit_dec = "[0-9][0-9_]*[.][0-9_]*\\([eE][+-][0-9][0-9_]*\\)?"
    let float_lit_no_dec = "[0-9][0-9_]*[eE][+-]?[0-9][0-9_]*"
    let float_re = union_re [float_lit_no_dec; float_lit_dec]
    let float_literal =
      Glr.alternatives'
        [Glr.apply (fun f  -> let (_loc_f,f) = f in let _loc = _loc_f in f)
           (locate
              (Glr.regexp ~name:"float" float_re (fun groupe  -> groupe 0)));
        Glr.fsequence (locate (Glr.char '$' '$'))
          (Glr.fsequence (locate (Glr.string "float" "float"))
             (Glr.fsequence (locate (Glr.char ':' ':'))
                (Glr.sequence (locate (expression_lvl (next_exp App)))
                   (locate (Glr.char '$' '$'))
                   (fun e  ->
                      let (_loc_e,e) = e in
                      fun _unnamed_1  ->
                        let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                        fun _unnamed_2  ->
                          let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                          fun _unnamed_3  ->
                            let (_loc__unnamed_3,_unnamed_3) = _unnamed_3 in
                            fun _unnamed_4  ->
                              let (_loc__unnamed_4,_unnamed_4) = _unnamed_4 in
                              let _loc =
                                merge
                                  [_loc__unnamed_4;
                                  _loc__unnamed_3;
                                  _loc__unnamed_2;
                                  _loc_e;
                                  _loc__unnamed_1] in
                              string_of_float (push_pop_float e)))))]
    let char_regular = "[^\\']"
    let string_regular = "[^\\\"]"
    let char_escaped = "[\\\\][\\\\\\\"\\'ntbrs ]"
    let char_dec = "[\\\\][0-9][0-9][0-9]"
    let char_hex = "[\\\\][x][0-9a-fA-F][0-9a-fA-F]"
    exception Illegal_escape of string
    let one_char is_char =
      Glr.alternatives'
        (let y =
           (Glr.apply
              (fun c  -> let (_loc_c,c) = c in let _loc = _loc_c in '\n')
              (locate (Glr.char '\n' '\n')))
           ::
           (let y =
              [Glr.apply
                 (fun c  ->
                    let (_loc_c,c) = c in
                    let _loc = _loc_c in
                    match c.[1] with
                    | 'n' -> '\n'
                    | 't' -> '\t'
                    | 'b' -> '\b'
                    | 'r' -> '\r'
                    | 's' -> ' '
                    | c -> c)
                 (locate
                    (Glr.regexp ~name:"char_escaped" char_escaped
                       (fun groupe  -> groupe 0)));
              Glr.apply
                (fun c  ->
                   let (_loc_c,c) = c in
                   let _loc = _loc_c in
                   let str = String.sub c 1 3 in
                   let i = Scanf.sscanf str "%i" (fun i  -> i) in
                   if i > 255
                   then raise (Illegal_escape str)
                   else char_of_int i)
                (locate
                   (Glr.regexp ~name:"char_dec" char_dec
                      (fun groupe  -> groupe 0)));
              Glr.apply
                (fun c  ->
                   let (_loc_c,c) = c in
                   let _loc = _loc_c in
                   let str = String.sub c 2 2 in
                   let str' = String.concat "" ["0x"; str] in
                   let i = Scanf.sscanf str' "%i" (fun i  -> i) in
                   char_of_int i)
                (locate
                   (Glr.regexp ~name:"char_hex" char_hex
                      (fun groupe  -> groupe 0)))] in
            if not is_char
            then
              (Glr.apply
                 (fun c  -> let (_loc_c,c) = c in let _loc = _loc_c in c.[0])
                 (locate
                    (Glr.regexp ~name:"string_regular" string_regular
                       (fun groupe  -> groupe 0))))
              :: y
            else y) in
         if is_char
         then
           (Glr.apply
              (fun c  -> let (_loc_c,c) = c in let _loc = _loc_c in c.[0])
              (locate
                 (Glr.regexp ~name:"char_regular" char_regular
                    (fun groupe  -> groupe 0))))
           :: y
         else y)
    let char_literal =
      Glr.alternatives'
        [Glr.apply (fun r  -> let (_loc_r,r) = r in let _loc = _loc_r in r)
           (locate
              (change_layout
                 (Glr.fsequence (locate (Glr.char '\'' '\''))
                    (Glr.sequence (locate (one_char true))
                       (locate (Glr.char '\'' '\''))
                       (fun c  ->
                          let (_loc_c,c) = c in
                          fun _unnamed_1  ->
                            let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                            fun _unnamed_2  ->
                              let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                              let _loc =
                                merge
                                  [_loc__unnamed_2; _loc_c; _loc__unnamed_1] in
                              c))) no_blank));
        Glr.fsequence (locate (Glr.char '$' '$'))
          (Glr.fsequence (locate (Glr.string "char" "char"))
             (Glr.fsequence (locate (Glr.char ':' ':'))
                (Glr.sequence (locate (expression_lvl (next_exp App)))
                   (locate (Glr.char '$' '$'))
                   (fun e  ->
                      let (_loc_e,e) = e in
                      fun _unnamed_1  ->
                        let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                        fun _unnamed_2  ->
                          let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                          fun _unnamed_3  ->
                            let (_loc__unnamed_3,_unnamed_3) = _unnamed_3 in
                            fun _unnamed_4  ->
                              let (_loc__unnamed_4,_unnamed_4) = _unnamed_4 in
                              let _loc =
                                merge
                                  [_loc__unnamed_4;
                                  _loc__unnamed_3;
                                  _loc__unnamed_2;
                                  _loc_e;
                                  _loc__unnamed_1] in
                              push_pop_char e))))]
    let interspace = "[ \t]*"
    let string_literal =
      let char_list_to_string lc =
        let len = List.length lc in
        let str = String.create len in
        let ptr = ref lc in
        for i = 0 to len - 1 do
          (match !ptr with
           | [] -> assert false
           | x::l -> (String.unsafe_set str i x; ptr := l))
        done;
        str in
      Glr.alternatives'
        [Glr.apply (fun r  -> let (_loc_r,r) = r in let _loc = _loc_r in r)
           (locate
              (change_layout
                 (Glr.fsequence (locate (Glr.char '"' '"'))
                    (Glr.fsequence
                       (locate
                          (Glr.apply List.rev
                             (Glr.fixpoint []
                                (Glr.apply (fun x  l  -> x :: l)
                                   (one_char false)))))
                       (Glr.sequence
                          (locate
                             (Glr.apply List.rev
                                (Glr.fixpoint []
                                   (Glr.apply (fun x  l  -> x :: l)
                                      (Glr.fsequence
                                         (locate (Glr.char '\\' '\\'))
                                         (Glr.fsequence
                                            (locate (Glr.char '\n' '\n'))
                                            (Glr.sequence
                                               (locate
                                                  (Glr.regexp
                                                     ~name:"interspace"
                                                     interspace
                                                     (fun groupe  -> groupe 0)))
                                               (locate
                                                  (Glr.apply List.rev
                                                     (Glr.fixpoint []
                                                        (Glr.apply
                                                           (fun x  l  -> x ::
                                                              l)
                                                           (one_char false)))))
                                               (fun _unnamed_0  ->
                                                  let (_loc__unnamed_0,_unnamed_0)
                                                    = _unnamed_0 in
                                                  fun lc  ->
                                                    let (_loc_lc,lc) = lc in
                                                    fun _unnamed_2  ->
                                                      let (_loc__unnamed_2,_unnamed_2)
                                                        = _unnamed_2 in
                                                      fun _unnamed_3  ->
                                                        let (_loc__unnamed_3,_unnamed_3)
                                                          = _unnamed_3 in
                                                        let _loc =
                                                          merge
                                                            [_loc__unnamed_3;
                                                            _loc__unnamed_2;
                                                            _loc__unnamed_0;
                                                            _loc_lc] in
                                                        lc))))))))
                          (locate (Glr.char '"' '"'))
                          (fun lcs  ->
                             let (_loc_lcs,lcs) = lcs in
                             fun _unnamed_1  ->
                               let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                               fun lc  ->
                                 let (_loc_lc,lc) = lc in
                                 fun _unnamed_3  ->
                                   let (_loc__unnamed_3,_unnamed_3) =
                                     _unnamed_3 in
                                   let _loc =
                                     merge
                                       [_loc__unnamed_3;
                                       _loc_lc;
                                       _loc_lcs;
                                       _loc__unnamed_1] in
                                   char_list_to_string
                                     (List.flatten (lc :: lcs)))))) no_blank));
        Glr.apply (fun r  -> let (_loc_r,r) = r in let _loc = _loc_r in r)
          (locate
             (change_layout
                (Glr.iter
                   (Glr.fsequence (locate (Glr.char '{' '{'))
                      (Glr.sequence
                         (locate
                            (Glr.regexp "[a-z]*" (fun groupe  -> groupe 0)))
                         (locate (Glr.char '|' '|'))
                         (fun id  ->
                            let (_loc_id,id) = id in
                            fun _unnamed_1  ->
                              let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                              fun _unnamed_2  ->
                                let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                                let _loc =
                                  merge
                                    [_loc__unnamed_2;
                                    _loc_id;
                                    _loc__unnamed_1] in
                                let string_literal_suit =
                                  declare_grammar "string_literal_suit" in
                                let _ =
                                  set_grammar string_literal_suit
                                    (Glr.alternatives'
                                       [Glr.fsequence
                                          (locate (Glr.char '|' '|'))
                                          (Glr.sequence
                                             (locate (Glr.string id id))
                                             (locate (Glr.char '}' '}'))
                                             (fun _unnamed_0  ->
                                                let (_loc__unnamed_0,_unnamed_0)
                                                  = _unnamed_0 in
                                                fun _unnamed_1  ->
                                                  let (_loc__unnamed_1,_unnamed_1)
                                                    = _unnamed_1 in
                                                  fun _unnamed_2  ->
                                                    let (_loc__unnamed_2,_unnamed_2)
                                                      = _unnamed_2 in
                                                    let _loc =
                                                      merge
                                                        [_loc__unnamed_2;
                                                        _loc__unnamed_0;
                                                        _loc__unnamed_1] in
                                                    []));
                                       Glr.sequence (locate Glr.any)
                                         (locate string_literal_suit)
                                         (fun c  ->
                                            let (_loc_c,c) = c in
                                            fun r  ->
                                              let (_loc_r,r) = r in
                                              let _loc =
                                                merge [_loc_c; _loc_r] in
                                              c :: r)]) in
                                Glr.apply
                                  (fun r  ->
                                     let (_loc_r,r) = r in
                                     let _loc = _loc_r in
                                     char_list_to_string r)
                                  (locate string_literal_suit))))) no_blank));
        Glr.fsequence (locate (Glr.char '$' '$'))
          (Glr.fsequence (locate (Glr.string "string" "string"))
             (Glr.fsequence (locate (Glr.char ':' ':'))
                (Glr.sequence (locate (expression_lvl (next_exp App)))
                   (locate (Glr.char '$' '$'))
                   (fun e  ->
                      let (_loc_e,e) = e in
                      fun _unnamed_1  ->
                        let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                        fun _unnamed_2  ->
                          let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                          fun _unnamed_3  ->
                            let (_loc__unnamed_3,_unnamed_3) = _unnamed_3 in
                            fun _unnamed_4  ->
                              let (_loc__unnamed_4,_unnamed_4) = _unnamed_4 in
                              let _loc =
                                merge
                                  [_loc__unnamed_4;
                                  _loc__unnamed_3;
                                  _loc__unnamed_2;
                                  _loc_e;
                                  _loc__unnamed_1] in
                              push_pop_string e))))]
    let quotation = declare_grammar "quotation"
    let _ =
      set_grammar quotation
        (change_layout
           (Glr.alternatives'
              [Glr.fsequence (locate (Glr.string "<:" "<:"))
                 (Glr.sequence (locate quotation) (locate quotation)
                    (fun q  ->
                       let (_loc_q,q) = q in
                       fun q'  ->
                         let (_loc_q',q') = q' in
                         fun _unnamed_2  ->
                           let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                           let _loc =
                             merge [_loc__unnamed_2; _loc_q; _loc_q'] in
                           "<:" ^ (q ^ (">>" ^ q'))));
              Glr.sequence (locate string_literal) (locate quotation)
                (fun s  ->
                   let (_loc_s,s) = s in
                   fun q  ->
                     let (_loc_q,q) = q in
                     let _loc = merge [_loc_s; _loc_q] in
                     (Printf.sprintf "%S" s) ^ q);
              Glr.apply
                (fun _unnamed_0  ->
                   let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                   let _loc = _loc__unnamed_0 in "")
                (locate (Glr.string ">>" ">>"));
              Glr.sequence (locate (one_char false)) (locate quotation)
                (fun c  ->
                   let (_loc_c,c) = c in
                   fun q  ->
                     let (_loc_q,q) = q in
                     let _loc = merge [_loc_c; _loc_q] in
                     (String.make 1 c) ^ q)]) no_blank)
    let label_name = lowercase_ident
    let label =
      Glr.sequence (locate (Glr.string "~" "~")) (locate label_name)
        (fun _unnamed_0  ->
           let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
           fun ln  ->
             let (_loc_ln,ln) = ln in
             let _loc = merge [_loc__unnamed_0; _loc_ln] in ln)
    let opt_label =
      Glr.sequence (locate (Glr.string "?" "?")) (locate label_name)
        (fun _unnamed_0  ->
           let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
           fun ln  ->
             let (_loc_ln,ln) = ln in
             let _loc = merge [_loc__unnamed_0; _loc_ln] in ln)
    let maybe_opt_label =
      Glr.sequence
        (locate
           (Glr.option None
              (Glr.apply (fun x  -> Some x) (Glr.string "?" "?"))))
        (locate label_name)
        (fun o  ->
           let (_loc_o,o) = o in
           fun ln  ->
             let (_loc_ln,ln) = ln in
             let _loc = merge [_loc_o; _loc_ln] in
             if o = None then ln else "?" ^ ln)
    let infix_op =
      Glr.apply
        (fun sym  -> let (_loc_sym,sym) = sym in let _loc = _loc_sym in sym)
        (locate infix_symbol)
    let operator_name =
      Glr.alternatives'
        [Glr.apply
           (fun op  -> let (_loc_op,op) = op in let _loc = _loc_op in op)
           (locate infix_op);
        Glr.apply
          (fun op  -> let (_loc_op,op) = op in let _loc = _loc_op in op)
          (locate prefix_symbol)]
    let value_name =
      Glr.alternatives'
        [Glr.apply
           (fun id  -> let (_loc_id,id) = id in let _loc = _loc_id in id)
           (locate lowercase_ident);
        Glr.fsequence (locate (Glr.string "(" "("))
          (Glr.sequence (locate operator_name) (locate (Glr.string ")" ")"))
             (fun op  ->
                let (_loc_op,op) = op in
                fun _unnamed_1  ->
                  let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                  fun _unnamed_2  ->
                    let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                    let _loc =
                      merge [_loc__unnamed_2; _loc_op; _loc__unnamed_1] in
                    op))]
    let constr_name = capitalized_ident
    let tag_name =
      Glr.sequence (locate (Glr.string "`" "`")) (locate ident)
        (fun _unnamed_0  ->
           let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
           fun c  ->
             let (_loc_c,c) = c in
             let _loc = merge [_loc__unnamed_0; _loc_c] in c)
    let typeconstr_name = lowercase_ident
    let field_name = lowercase_ident
    let module_name = capitalized_ident
    let modtype_name = ident
    let class_name = lowercase_ident
    let inst_var_name = lowercase_ident
    let method_name = lowercase_ident
    let (module_path_gen,set_module_path_gen) =
      grammar_family "module_path_gen"
    let (module_path_suit,set_module_path_suit) =
      grammar_family "module_path_suit"
    let module_path_suit_aux =
      memoize1
        (fun allow_app  ->
           Glr.alternatives'
             (let y =
                [Glr.sequence (locate (Glr.string "." "."))
                   (locate module_name)
                   (fun _unnamed_0  ->
                      let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                      fun m  ->
                        let (_loc_m,m) = m in
                        let _loc = merge [_loc__unnamed_0; _loc_m] in
                        fun acc  -> Ldot (acc, m))] in
              if allow_app
              then
                (Glr.fsequence (locate (Glr.string "(" "("))
                   (Glr.sequence (locate (module_path_gen true))
                      (locate (Glr.string ")" ")"))
                      (fun m'  ->
                         let (_loc_m',m') = m' in
                         fun _unnamed_1  ->
                           let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                           fun _unnamed_2  ->
                             let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                             let _loc =
                               merge
                                 [_loc__unnamed_2; _loc_m'; _loc__unnamed_1] in
                             fun a  -> Lapply (a, m'))))
                :: y
              else y))
    let _ =
      set_module_path_suit
        (fun allow_app  ->
           Glr.alternatives
             [Glr.sequence (locate (module_path_suit_aux allow_app))
                (locate (module_path_suit allow_app))
                (fun f  ->
                   let (_loc_f,f) = f in
                   fun g  ->
                     let (_loc_g,g) = g in
                     let _loc = merge [_loc_f; _loc_g] in
                     fun acc  -> g (f acc));
             Glr.apply
               (fun _unnamed_0  ->
                  let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                  let _loc = _loc__unnamed_0 in fun acc  -> acc)
               (locate (Glr.empty ()))])
    let _ =
      set_module_path_gen
        (fun allow_app  ->
           Glr.sequence (locate module_name)
             (locate (module_path_suit allow_app))
             (fun m  ->
                let (_loc_m,m) = m in
                fun s  ->
                  let (_loc_s,s) = s in
                  let _loc = merge [_loc_m; _loc_s] in s (Lident m)))
    let module_path = module_path_gen false
    let extended_module_path = module_path_gen true
    let value_path =
      Glr.sequence
        (locate
           (Glr.option None
              (Glr.apply (fun x  -> Some x)
                 (Glr.sequence (locate module_path)
                    (locate (Glr.string "." "."))
                    (fun m  ->
                       let (_loc_m,m) = m in
                       fun _unnamed_1  ->
                         let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                         let _loc = merge [_loc_m; _loc__unnamed_1] in m)))))
        (locate value_name)
        (fun mp  ->
           let (_loc_mp,mp) = mp in
           fun vn  ->
             let (_loc_vn,vn) = vn in
             let _loc = merge [_loc_mp; _loc_vn] in
             match mp with | None  -> Lident vn | Some p -> Ldot (p, vn))
    let constr =
      Glr.sequence
        (locate
           (Glr.option None
              (Glr.apply (fun x  -> Some x)
                 (Glr.sequence (locate module_path)
                    (locate (Glr.string "." "."))
                    (fun m  ->
                       let (_loc_m,m) = m in
                       fun _unnamed_1  ->
                         let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                         let _loc = merge [_loc_m; _loc__unnamed_1] in m)))))
        (locate constr_name)
        (fun mp  ->
           let (_loc_mp,mp) = mp in
           fun cn  ->
             let (_loc_cn,cn) = cn in
             let _loc = merge [_loc_mp; _loc_cn] in
             match mp with | None  -> Lident cn | Some p -> Ldot (p, cn))
    let typeconstr =
      Glr.sequence
        (locate
           (Glr.option None
              (Glr.apply (fun x  -> Some x)
                 (Glr.sequence (locate extended_module_path)
                    (locate (Glr.string "." "."))
                    (fun m  ->
                       let (_loc_m,m) = m in
                       fun _unnamed_1  ->
                         let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                         let _loc = merge [_loc_m; _loc__unnamed_1] in m)))))
        (locate typeconstr_name)
        (fun mp  ->
           let (_loc_mp,mp) = mp in
           fun tcn  ->
             let (_loc_tcn,tcn) = tcn in
             let _loc = merge [_loc_mp; _loc_tcn] in
             match mp with | None  -> Lident tcn | Some p -> Ldot (p, tcn))
    let field =
      Glr.sequence
        (locate
           (Glr.option None
              (Glr.apply (fun x  -> Some x)
                 (Glr.sequence (locate module_path)
                    (locate (Glr.string "." "."))
                    (fun m  ->
                       let (_loc_m,m) = m in
                       fun _unnamed_1  ->
                         let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                         let _loc = merge [_loc_m; _loc__unnamed_1] in m)))))
        (locate field_name)
        (fun mp  ->
           let (_loc_mp,mp) = mp in
           fun fn  ->
             let (_loc_fn,fn) = fn in
             let _loc = merge [_loc_mp; _loc_fn] in
             match mp with | None  -> Lident fn | Some p -> Ldot (p, fn))
    let class_path =
      Glr.sequence
        (locate
           (Glr.option None
              (Glr.apply (fun x  -> Some x)
                 (Glr.sequence (locate module_path)
                    (locate (Glr.string "." "."))
                    (fun m  ->
                       let (_loc_m,m) = m in
                       fun _unnamed_1  ->
                         let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                         let _loc = merge [_loc_m; _loc__unnamed_1] in m)))))
        (locate class_name)
        (fun mp  ->
           let (_loc_mp,mp) = mp in
           fun cn  ->
             let (_loc_cn,cn) = cn in
             let _loc = merge [_loc_mp; _loc_cn] in
             match mp with | None  -> Lident cn | Some p -> Ldot (p, cn))
    let modtype_path =
      Glr.sequence
        (locate
           (Glr.option None
              (Glr.apply (fun x  -> Some x)
                 (Glr.sequence (locate extended_module_path)
                    (locate (Glr.string "." "."))
                    (fun m  ->
                       let (_loc_m,m) = m in
                       fun _unnamed_1  ->
                         let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                         let _loc = merge [_loc_m; _loc__unnamed_1] in m)))))
        (locate modtype_name)
        (fun mp  ->
           let (_loc_mp,mp) = mp in
           fun mtn  ->
             let (_loc_mtn,mtn) = mtn in
             let _loc = merge [_loc_mp; _loc_mtn] in
             match mp with | None  -> Lident mtn | Some p -> Ldot (p, mtn))
    let classtype_path =
      Glr.sequence
        (locate
           (Glr.option None
              (Glr.apply (fun x  -> Some x)
                 (Glr.sequence (locate extended_module_path)
                    (locate (Glr.string "." "."))
                    (fun m  ->
                       let (_loc_m,m) = m in
                       fun _unnamed_1  ->
                         let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                         let _loc = merge [_loc_m; _loc__unnamed_1] in m)))))
        (locate class_name)
        (fun mp  ->
           let (_loc_mp,mp) = mp in
           fun cn  ->
             let (_loc_cn,cn) = cn in
             let _loc = merge [_loc_mp; _loc_cn] in
             match mp with | None  -> Lident cn | Some p -> Ldot (p, cn))
    let opt_variance =
      Glr.apply
        (fun v  ->
           let (_loc_v,v) = v in
           let _loc = _loc_v in
           match v with
           | None  -> (false, false)
           | Some "+" -> (true, false)
           | Some "-" -> (false, true)
           | _ -> assert false)
        (locate
           (Glr.option None
              (Glr.apply (fun x  -> Some x)
                 (Glr.regexp "[+-]" (fun groupe  -> groupe 0)))))
    let override_flag =
      Glr.apply
        (fun o  ->
           let (_loc_o,o) = o in
           let _loc = _loc_o in if o <> None then Override else Fresh)
        (locate
           (Glr.option None
              (Glr.apply (fun x  -> Some x) (Glr.string "!" "!"))))
    let attr_id =
      Glr.sequence
        (locate (Glr.regexp ~name:"ident" ident_re (fun groupe  -> groupe 0)))
        (locate
           (Glr.apply List.rev
              (Glr.fixpoint []
                 (Glr.apply (fun x  l  -> x :: l)
                    (Glr.sequence (locate (Glr.char '.' '.'))
                       (locate
                          (Glr.regexp ~name:"ident" ident_re
                             (fun groupe  -> groupe 0)))
                       (fun _unnamed_0  ->
                          let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                          fun id  ->
                            let (_loc_id,id) = id in
                            let _loc = merge [_loc__unnamed_0; _loc_id] in id))))))
        (fun id  ->
           let (_loc_id,id) = id in
           fun l  ->
             let (_loc_l,l) = l in
             let _loc = merge [_loc_id; _loc_l] in
             id_loc (String.concat "." (id :: l)) _loc)
    type attr =  
      | PStr of structure_item list
      | PTyp of core_type
      | PPat of pattern* expression option 
    let payload =
      Glr.alternatives'
        [Glr.apply
           (fun s  -> let (_loc_s,s) = s in let _loc = _loc_s in PStr s)
           (locate structure);
        Glr.sequence (locate (Glr.char ':' ':')) (locate typexpr)
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun t  ->
               let (_loc_t,t) = t in
               let _loc = merge [_loc__unnamed_0; _loc_t] in PTyp t);
        Glr.fsequence (locate (Glr.char '?' '?'))
          (Glr.sequence (locate pattern)
             (locate
                (Glr.option None
                   (Glr.apply (fun x  -> Some x)
                      (Glr.sequence (locate (Glr.string "when" "when"))
                         (locate expression)
                         (fun _unnamed_0  ->
                            let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                            fun e  ->
                              let (_loc_e,e) = e in
                              let _loc = merge [_loc__unnamed_0; _loc_e] in e)))))
             (fun p  ->
                let (_loc_p,p) = p in
                fun e  ->
                  let (_loc_e,e) = e in
                  fun _unnamed_2  ->
                    let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                    let _loc = merge [_loc__unnamed_2; _loc_p; _loc_e] in
                    PPat (p, e)))]
    let attribute =
      Glr.fsequence (locate (Glr.string "[@" "[@"))
        (Glr.sequence (locate attr_id) (locate payload)
           (fun id  ->
              let (_loc_id,id) = id in
              fun p  ->
                let (_loc_p,p) = p in
                fun _unnamed_2  ->
                  let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                  let _loc = merge [_loc__unnamed_2; _loc_id; _loc_p] in
                  (id, p)))
    let attributes =
      Glr.apply
        (fun _unnamed_0  ->
           let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
           let _loc = _loc__unnamed_0 in ())
        (locate
           (Glr.apply List.rev
              (Glr.fixpoint []
                 (Glr.apply (fun x  l  -> x :: l)
                    (Glr.apply
                       (fun a  ->
                          let (_loc_a,a) = a in let _loc = _loc_a in a)
                       (locate attribute))))))
    let ext_attributes =
      Glr.sequence
        (locate
           (Glr.option None
              (Glr.apply (fun x  -> Some x)
                 (Glr.sequence (locate (Glr.char '%' '%')) (locate attribute)
                    (fun _unnamed_0  ->
                       let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                       fun a  ->
                         let (_loc_a,a) = a in
                         let _loc = merge [_loc__unnamed_0; _loc_a] in a)))))
        (locate attributes)
        (fun a  ->
           let (_loc_a,a) = a in
           fun l  ->
             let (_loc_l,l) = l in
             let _loc = merge [_loc_a; _loc_l] in (a, l))
    let post_item_attributes =
      Glr.apply (fun l  -> let (_loc_l,l) = l in let _loc = _loc_l in l)
        (locate
           (Glr.apply List.rev
              (Glr.fixpoint []
                 (Glr.apply (fun x  l  -> x :: l)
                    (Glr.fsequence (locate (Glr.string "[@@" "[@@"))
                       (Glr.fsequence (locate attr_id)
                          (Glr.sequence (locate payload)
                             (locate (Glr.char ']' ']'))
                             (fun p  ->
                                let (_loc_p,p) = p in
                                fun _unnamed_1  ->
                                  let (_loc__unnamed_1,_unnamed_1) =
                                    _unnamed_1 in
                                  fun id  ->
                                    let (_loc_id,id) = id in
                                    fun _unnamed_3  ->
                                      let (_loc__unnamed_3,_unnamed_3) =
                                        _unnamed_3 in
                                      let _loc =
                                        merge
                                          [_loc__unnamed_3;
                                          _loc_id;
                                          _loc_p;
                                          _loc__unnamed_1] in
                                      (id, p)))))))))
    let ext_attributes =
      Glr.apply (fun l  -> let (_loc_l,l) = l in let _loc = _loc_l in l)
        (locate
           (Glr.apply List.rev
              (Glr.fixpoint []
                 (Glr.apply (fun x  l  -> x :: l)
                    (Glr.fsequence (locate (Glr.string "[@@@" "[@@@"))
                       (Glr.fsequence (locate attr_id)
                          (Glr.sequence (locate payload)
                             (locate (Glr.char ']' ']'))
                             (fun p  ->
                                let (_loc_p,p) = p in
                                fun _unnamed_1  ->
                                  let (_loc__unnamed_1,_unnamed_1) =
                                    _unnamed_1 in
                                  fun id  ->
                                    let (_loc_id,id) = id in
                                    fun _unnamed_3  ->
                                      let (_loc__unnamed_3,_unnamed_3) =
                                        _unnamed_3 in
                                      let _loc =
                                        merge
                                          [_loc__unnamed_3;
                                          _loc_id;
                                          _loc_p;
                                          _loc__unnamed_1] in
                                      (id, p)))))))))
    let extension =
      Glr.fsequence (locate (Glr.string "[%" "[%"))
        (Glr.fsequence (locate attr_id)
           (Glr.sequence (locate payload) (locate (Glr.char ']' ']'))
              (fun p  ->
                 let (_loc_p,p) = p in
                 fun _unnamed_1  ->
                   let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                   fun id  ->
                     let (_loc_id,id) = id in
                     fun _unnamed_3  ->
                       let (_loc__unnamed_3,_unnamed_3) = _unnamed_3 in
                       let _loc =
                         merge
                           [_loc__unnamed_3;
                           _loc_id;
                           _loc_p;
                           _loc__unnamed_1] in
                       (id, p))))
    let item_extension =
      Glr.fsequence (locate (Glr.string "[%%" "[%%"))
        (Glr.fsequence (locate attr_id)
           (Glr.sequence (locate payload) (locate (Glr.char ']' ']'))
              (fun p  ->
                 let (_loc_p,p) = p in
                 fun _unnamed_1  ->
                   let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                   fun id  ->
                     let (_loc_id,id) = id in
                     fun _unnamed_3  ->
                       let (_loc__unnamed_3,_unnamed_3) = _unnamed_3 in
                       let _loc =
                         merge
                           [_loc__unnamed_3;
                           _loc_id;
                           _loc_p;
                           _loc__unnamed_1] in
                       (id, p))))
    let poly_typexpr =
      Glr.alternatives'
        [Glr.fsequence
           (locate
              (Glr.sequence
                 (Glr.sequence (locate (Glr.string "'" "'")) (locate ident)
                    (fun _unnamed_0  ->
                       let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                       fun id  ->
                         let (_loc_id,id) = id in
                         let _loc = merge [_loc__unnamed_0; _loc_id] in id))
                 (Glr.fixpoint []
                    (Glr.apply (fun x  l  -> x :: l)
                       (Glr.sequence (locate (Glr.string "'" "'"))
                          (locate ident)
                          (fun _unnamed_0  ->
                             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                             fun id  ->
                               let (_loc_id,id) = id in
                               let _loc = merge [_loc__unnamed_0; _loc_id] in
                               id)))) (fun x  l  -> x :: (List.rev l))))
           (Glr.sequence (locate (Glr.string "." ".")) (locate typexpr)
              (fun _unnamed_0  ->
                 let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                 fun te  ->
                   let (_loc_te,te) = te in
                   fun ids  ->
                     let (_loc_ids,ids) = ids in
                     let _loc = merge [_loc_ids; _loc__unnamed_0; _loc_te] in
                     loc_typ _loc (Ptyp_poly (ids, te))));
        Glr.apply
          (fun te  ->
             let (_loc_te,te) = te in
             let _loc = _loc_te in loc_typ _loc (Ptyp_poly ([], te)))
          (locate typexpr)]
    let poly_syntax_typexpr =
      Glr.fsequence (locate type_kw)
        (Glr.fsequence
           (locate
              (Glr.sequence typeconstr_name
                 (Glr.fixpoint []
                    (Glr.apply (fun x  l  -> x :: l) typeconstr_name))
                 (fun x  l  -> x :: (List.rev l))))
           (Glr.sequence (locate (Glr.string "." ".")) (locate typexpr)
              (fun _unnamed_0  ->
                 let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                 fun te  ->
                   let (_loc_te,te) = te in
                   fun ids  ->
                     let (_loc_ids,ids) = ids in
                     fun _unnamed_3  ->
                       let (_loc__unnamed_3,_unnamed_3) = _unnamed_3 in
                       let _loc =
                         merge
                           [_loc__unnamed_3;
                           _loc_ids;
                           _loc__unnamed_0;
                           _loc_te] in
                       (ids, te))))
    let method_type =
      Glr.fsequence (locate method_name)
        (Glr.sequence (locate (Glr.string ":" ":")) (locate poly_typexpr)
           (fun _unnamed_0  ->
              let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
              fun pte  ->
                let (_loc_pte,pte) = pte in
                fun mn  ->
                  let (_loc_mn,mn) = mn in
                  let _loc = merge [_loc_mn; _loc__unnamed_0; _loc_pte] in
                  { pfield_desc = (Pfield (mn, pte)); pfield_loc = _loc }))
    let tag_spec =
      Glr.alternatives'
        [Glr.sequence (locate tag_name)
           (locate
              (Glr.option None
                 (Glr.apply (fun x  -> Some x)
                    (Glr.fsequence (locate of_kw)
                       (Glr.sequence
                          (locate
                             (Glr.option None
                                (Glr.apply (fun x  -> Some x)
                                   (Glr.char '&' '&')))) (locate typexpr)
                          (fun amp  ->
                             let (_loc_amp,amp) = amp in
                             fun te  ->
                               let (_loc_te,te) = te in
                               fun _unnamed_2  ->
                                 let (_loc__unnamed_2,_unnamed_2) =
                                   _unnamed_2 in
                                 let _loc =
                                   merge [_loc__unnamed_2; _loc_amp; _loc_te] in
                                 (amp, te)))))))
           (fun tn  ->
              let (_loc_tn,tn) = tn in
              fun te  ->
                let (_loc_te,te) = te in
                let _loc = merge [_loc_tn; _loc_te] in
                let (amp,t) =
                  match te with
                  | None  -> (true, [])
                  | Some (amp,l) -> ((amp <> None), [l]) in
                Rtag (tn, amp, t));
        Glr.apply
          (fun te  ->
             let (_loc_te,te) = te in let _loc = _loc_te in Rinherit te)
          (locate typexpr)]
    let tag_spec_first =
      Glr.alternatives'
        [Glr.sequence (locate tag_name)
           (locate
              (Glr.option None
                 (Glr.apply (fun x  -> Some x)
                    (Glr.fsequence (locate of_kw)
                       (Glr.sequence
                          (locate
                             (Glr.option None
                                (Glr.apply (fun x  -> Some x)
                                   (Glr.char '&' '&')))) (locate typexpr)
                          (fun amp  ->
                             let (_loc_amp,amp) = amp in
                             fun te  ->
                               let (_loc_te,te) = te in
                               fun _unnamed_2  ->
                                 let (_loc__unnamed_2,_unnamed_2) =
                                   _unnamed_2 in
                                 let _loc =
                                   merge [_loc__unnamed_2; _loc_amp; _loc_te] in
                                 (amp, te)))))))
           (fun tn  ->
              let (_loc_tn,tn) = tn in
              fun te  ->
                let (_loc_te,te) = te in
                let _loc = merge [_loc_tn; _loc_te] in
                let (amp,t) =
                  match te with
                  | None  -> (true, [])
                  | Some (amp,l) -> ((amp <> None), [l]) in
                [Rtag (tn, amp, t)]);
        Glr.fsequence
          (locate (Glr.option None (Glr.apply (fun x  -> Some x) typexpr)))
          (Glr.sequence (locate (Glr.string "|" "|")) (locate tag_spec)
             (fun _unnamed_0  ->
                let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                fun ts  ->
                  let (_loc_ts,ts) = ts in
                  fun te  ->
                    let (_loc_te,te) = te in
                    let _loc = merge [_loc_te; _loc__unnamed_0; _loc_ts] in
                    match te with
                    | None  -> [ts]
                    | Some te -> [Rinherit te; ts]))]
    let tag_spec_full =
      Glr.alternatives'
        [Glr.sequence (locate tag_name)
           (locate
              (Glr.option (true, [])
                 (Glr.fsequence (locate of_kw)
                    (Glr.fsequence
                       (locate
                          (Glr.option None
                             (Glr.apply (fun x  -> Some x)
                                (Glr.string "&" "&"))))
                       (Glr.sequence (locate typexpr)
                          (locate
                             (Glr.apply List.rev
                                (Glr.fixpoint []
                                   (Glr.apply (fun x  l  -> x :: l)
                                      (Glr.sequence
                                         (locate (Glr.string "&" "&"))
                                         (locate typexpr)
                                         (fun _unnamed_0  ->
                                            let (_loc__unnamed_0,_unnamed_0)
                                              = _unnamed_0 in
                                            fun te  ->
                                              let (_loc_te,te) = te in
                                              let _loc =
                                                merge
                                                  [_loc__unnamed_0; _loc_te] in
                                              te))))))
                          (fun te  ->
                             let (_loc_te,te) = te in
                             fun tes  ->
                               let (_loc_tes,tes) = tes in
                               fun amp  ->
                                 let (_loc_amp,amp) = amp in
                                 fun _unnamed_3  ->
                                   let (_loc__unnamed_3,_unnamed_3) =
                                     _unnamed_3 in
                                   let _loc =
                                     merge
                                       [_loc__unnamed_3;
                                       _loc_amp;
                                       _loc_te;
                                       _loc_tes] in
                                   ((amp <> None), (te :: tes))))))))
           (fun tn  ->
              let (_loc_tn,tn) = tn in
              fun ((_,(amp,tes)) as _unnamed_1)  ->
                let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                let _loc = merge [_loc_tn; _loc__unnamed_1] in
                Rtag (tn, amp, tes));
        Glr.apply
          (fun te  ->
             let (_loc_te,te) = te in let _loc = _loc_te in Rinherit te)
          (locate typexpr)]
    let polymorphic_variant_type: core_type grammar =
      Glr.alternatives'
        [Glr.fsequence (locate (Glr.string "[" "["))
           (Glr.fsequence (locate tag_spec_first)
              (Glr.sequence
                 (locate
                    (Glr.apply List.rev
                       (Glr.fixpoint []
                          (Glr.apply (fun x  l  -> x :: l)
                             (Glr.sequence (locate (Glr.string "|" "|"))
                                (locate tag_spec)
                                (fun _unnamed_0  ->
                                   let (_loc__unnamed_0,_unnamed_0) =
                                     _unnamed_0 in
                                   fun ts  ->
                                     let (_loc_ts,ts) = ts in
                                     let _loc =
                                       merge [_loc__unnamed_0; _loc_ts] in
                                     ts)))))) (locate (Glr.string "]" "]"))
                 (fun tss  ->
                    let (_loc_tss,tss) = tss in
                    fun _unnamed_1  ->
                      let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                      fun tsf  ->
                        let (_loc_tsf,tsf) = tsf in
                        fun _unnamed_3  ->
                          let (_loc__unnamed_3,_unnamed_3) = _unnamed_3 in
                          let _loc =
                            merge
                              [_loc__unnamed_3;
                              _loc_tsf;
                              _loc_tss;
                              _loc__unnamed_1] in
                          let flag = true in
                          loc_typ _loc
                            (Ptyp_variant ((tsf @ tss), flag, None)))));
        Glr.fsequence (locate (Glr.string "[>" "[>"))
          (Glr.fsequence
             (locate
                (Glr.option None (Glr.apply (fun x  -> Some x) tag_spec)))
             (Glr.sequence
                (locate
                   (Glr.apply List.rev
                      (Glr.fixpoint []
                         (Glr.apply (fun x  l  -> x :: l)
                            (Glr.sequence (locate (Glr.string "|" "|"))
                               (locate tag_spec)
                               (fun _unnamed_0  ->
                                  let (_loc__unnamed_0,_unnamed_0) =
                                    _unnamed_0 in
                                  fun ts  ->
                                    let (_loc_ts,ts) = ts in
                                    let _loc =
                                      merge [_loc__unnamed_0; _loc_ts] in
                                    ts)))))) (locate (Glr.string "]" "]"))
                (fun tss  ->
                   let (_loc_tss,tss) = tss in
                   fun _unnamed_1  ->
                     let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                     fun ts  ->
                       let (_loc_ts,ts) = ts in
                       fun _unnamed_3  ->
                         let (_loc__unnamed_3,_unnamed_3) = _unnamed_3 in
                         let _loc =
                           merge
                             [_loc__unnamed_3;
                             _loc_ts;
                             _loc_tss;
                             _loc__unnamed_1] in
                         let tss =
                           match ts with
                           | None  -> tss
                           | Some ts -> ts :: tss in
                         let flag = false in
                         loc_typ _loc (Ptyp_variant (tss, flag, None)))));
        Glr.fsequence (locate (Glr.string "[<" "[<"))
          (Glr.fsequence
             (locate
                (Glr.option None
                   (Glr.apply (fun x  -> Some x) (Glr.string "|" "|"))))
             (Glr.fsequence (locate tag_spec_full)
                (Glr.fsequence
                   (locate
                      (Glr.apply List.rev
                         (Glr.fixpoint []
                            (Glr.apply (fun x  l  -> x :: l)
                               (Glr.sequence (locate (Glr.string "|" "|"))
                                  (locate tag_spec_full)
                                  (fun _unnamed_0  ->
                                     let (_loc__unnamed_0,_unnamed_0) =
                                       _unnamed_0 in
                                     fun tsf  ->
                                       let (_loc_tsf,tsf) = tsf in
                                       let _loc =
                                         merge [_loc__unnamed_0; _loc_tsf] in
                                       tsf))))))
                   (Glr.sequence
                      (locate
                         (Glr.option []
                            (Glr.sequence (locate (Glr.string ">" ">"))
                               (locate
                                  (Glr.sequence tag_name
                                     (Glr.fixpoint []
                                        (Glr.apply (fun x  l  -> x :: l)
                                           tag_name))
                                     (fun x  l  -> x :: (List.rev l))))
                               (fun _unnamed_0  ->
                                  let (_loc__unnamed_0,_unnamed_0) =
                                    _unnamed_0 in
                                  fun tns  ->
                                    let (_loc_tns,tns) = tns in
                                    let _loc =
                                      merge [_loc__unnamed_0; _loc_tns] in
                                    tns)))) (locate (Glr.string "]" "]"))
                      (fun tns  ->
                         let (_loc_tns,tns) = tns in
                         fun _unnamed_1  ->
                           let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                           fun tfss  ->
                             let (_loc_tfss,tfss) = tfss in
                             fun tfs  ->
                               let (_loc_tfs,tfs) = tfs in
                               fun _unnamed_4  ->
                                 let (_loc__unnamed_4,_unnamed_4) =
                                   _unnamed_4 in
                                 fun _unnamed_5  ->
                                   let (_loc__unnamed_5,_unnamed_5) =
                                     _unnamed_5 in
                                   let _loc =
                                     merge
                                       [_loc__unnamed_5;
                                       _loc__unnamed_4;
                                       _loc_tfs;
                                       _loc_tfss;
                                       _loc_tns;
                                       _loc__unnamed_1] in
                                   let flag = true in
                                   loc_typ _loc
                                     (Ptyp_variant
                                        ((tfs :: tfss), flag, (Some tns))))))))]
    let package_constraint =
      Glr.fsequence (locate type_kw)
        (Glr.fsequence (locate capitalized_ident)
           (Glr.sequence (locate (Glr.char '=' '=')) (locate typexpr)
              (fun _unnamed_0  ->
                 let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                 fun te  ->
                   let (_loc_te,te) = te in
                   fun tc  ->
                     let (_loc_tc,tc) = tc in
                     fun _unnamed_3  ->
                       let (_loc__unnamed_3,_unnamed_3) = _unnamed_3 in
                       let _loc =
                         merge
                           [_loc__unnamed_3;
                           _loc_tc;
                           _loc__unnamed_0;
                           _loc_te] in
                       let tc = id_loc tc _loc_tc in (tc, te))))
    let package_type =
      Glr.sequence (locate modtype_path)
        (locate
           (Glr.option []
              (Glr.fsequence (locate with_kw)
                 (Glr.sequence (locate package_constraint)
                    (locate
                       (Glr.apply List.rev
                          (Glr.fixpoint []
                             (Glr.apply (fun x  l  -> x :: l)
                                (Glr.sequence (locate and_kw)
                                   (locate package_constraint)
                                   (fun _unnamed_0  ->
                                      let (_loc__unnamed_0,_unnamed_0) =
                                        _unnamed_0 in
                                      fun pc  ->
                                        let (_loc_pc,pc) = pc in
                                        let _loc =
                                          merge [_loc__unnamed_0; _loc_pc] in
                                        pc))))))
                    (fun pc  ->
                       let (_loc_pc,pc) = pc in
                       fun pcs  ->
                         let (_loc_pcs,pcs) = pcs in
                         fun _unnamed_2  ->
                           let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                           let _loc =
                             merge [_loc__unnamed_2; _loc_pc; _loc_pcs] in
                           pc :: pcs)))))
        (fun mtp  ->
           let (_loc_mtp,mtp) = mtp in
           fun cs  ->
             let (_loc_cs,cs) = cs in
             let _loc = merge [_loc_mtp; _loc_cs] in
             let mtp = id_loc mtp _loc_mtp in Ptyp_package (mtp, cs))
    let opt_present =
      Glr.alternatives'
        [Glr.fsequence (locate (Glr.string "[>" "[>"))
           (Glr.sequence
              (locate
                 (Glr.sequence tag_name
                    (Glr.fixpoint []
                       (Glr.apply (fun x  l  -> x :: l) tag_name))
                    (fun x  l  -> x :: (List.rev l))))
              (locate (Glr.string "]" "]"))
              (fun l  ->
                 let (_loc_l,l) = l in
                 fun _unnamed_1  ->
                   let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                   fun _unnamed_2  ->
                     let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                     let _loc =
                       merge [_loc__unnamed_2; _loc_l; _loc__unnamed_1] in
                     l));
        Glr.apply
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             let _loc = _loc__unnamed_0 in []) (locate (Glr.empty ()))]
    let mkoption loc d =
      let loc = ghost loc in
      loc_typ loc
        (Ptyp_constr
           ((id_loc (Ldot ((Lident "*predef*"), "option")) loc), [d]))
    let typexpr_base: core_type grammar =
      Glr.alternatives'
        [Glr.apply (fun e  -> let (_loc_e,e) = e in let _loc = _loc_e in e)
           (locate (alternatives extra_types));
        Glr.sequence (locate (Glr.string "'" "'")) (locate ident)
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun id  ->
               let (_loc_id,id) = id in
               let _loc = merge [_loc__unnamed_0; _loc_id] in
               loc_typ _loc (Ptyp_var id));
        Glr.apply
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             let _loc = _loc__unnamed_0 in loc_typ _loc Ptyp_any)
          (locate (Glr.string "_" "_"));
        Glr.fsequence (locate (Glr.string "(" "("))
          (Glr.fsequence (locate module_kw)
             (Glr.sequence (locate package_type)
                (locate (Glr.string ")" ")"))
                (fun pt  ->
                   let (_loc_pt,pt) = pt in
                   fun _unnamed_1  ->
                     let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                     fun _unnamed_2  ->
                       let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                       fun _unnamed_3  ->
                         let (_loc__unnamed_3,_unnamed_3) = _unnamed_3 in
                         let _loc =
                           merge
                             [_loc__unnamed_3;
                             _loc__unnamed_2;
                             _loc_pt;
                             _loc__unnamed_1] in
                         loc_typ _loc pt)));
        Glr.fsequence (locate (Glr.string "(" "("))
          (Glr.sequence (locate typexpr) (locate (Glr.string ")" ")"))
             (fun te  ->
                let (_loc_te,te) = te in
                fun _unnamed_1  ->
                  let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                  fun _unnamed_2  ->
                    let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                    let _loc =
                      merge [_loc__unnamed_2; _loc_te; _loc__unnamed_1] in
                    te));
        Glr.fsequence (locate opt_label)
          (Glr.fsequence (locate (Glr.string ":" ":"))
             (Glr.fsequence (locate (typexpr_lvl (next_type_prio Arr)))
                (Glr.sequence (locate (Glr.string "->" "->"))
                   (locate typexpr)
                   (fun _unnamed_0  ->
                      let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                      fun te'  ->
                        let (_loc_te',te') = te' in
                        fun te  ->
                          let (_loc_te,te) = te in
                          fun _unnamed_3  ->
                            let (_loc__unnamed_3,_unnamed_3) = _unnamed_3 in
                            fun ln  ->
                              let (_loc_ln,ln) = ln in
                              let _loc =
                                merge
                                  [_loc_ln;
                                  _loc__unnamed_3;
                                  _loc_te;
                                  _loc__unnamed_0;
                                  _loc_te'] in
                              loc_typ _loc
                                (Ptyp_arrow
                                   (("?" ^ ln), (mkoption _loc_te te), te'))))));
        Glr.fsequence (locate label_name)
          (Glr.fsequence (locate (Glr.string ":" ":"))
             (Glr.fsequence (locate (typexpr_lvl (next_type_prio Arr)))
                (Glr.sequence (locate (Glr.string "->" "->"))
                   (locate typexpr)
                   (fun _unnamed_0  ->
                      let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                      fun te'  ->
                        let (_loc_te',te') = te' in
                        fun te  ->
                          let (_loc_te,te) = te in
                          fun _unnamed_3  ->
                            let (_loc__unnamed_3,_unnamed_3) = _unnamed_3 in
                            fun ln  ->
                              let (_loc_ln,ln) = ln in
                              let _loc =
                                merge
                                  [_loc_ln;
                                  _loc__unnamed_3;
                                  _loc_te;
                                  _loc__unnamed_0;
                                  _loc_te'] in
                              loc_typ _loc (Ptyp_arrow (ln, te, te'))))));
        Glr.apply
          (fun tc  ->
             let (_loc_tc,tc) = tc in
             let _loc = _loc_tc in
             loc_typ _loc (Ptyp_constr ((id_loc tc _loc_tc), [])))
          (locate typeconstr);
        Glr.fsequence (locate (Glr.string "(" "("))
          (Glr.fsequence (locate typexpr)
             (Glr.fsequence
                (locate
                   (Glr.apply List.rev
                      (Glr.fixpoint []
                         (Glr.apply (fun x  l  -> x :: l)
                            (Glr.sequence (locate (Glr.string "," ","))
                               (locate typexpr)
                               (fun _unnamed_0  ->
                                  let (_loc__unnamed_0,_unnamed_0) =
                                    _unnamed_0 in
                                  fun te  ->
                                    let (_loc_te,te) = te in
                                    let _loc =
                                      merge [_loc__unnamed_0; _loc_te] in
                                    te))))))
                (Glr.sequence (locate (Glr.string ")" ")"))
                   (locate typeconstr)
                   (fun _unnamed_0  ->
                      let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                      fun tc  ->
                        let (_loc_tc,tc) = tc in
                        fun tes  ->
                          let (_loc_tes,tes) = tes in
                          fun te  ->
                            let (_loc_te,te) = te in
                            fun _unnamed_4  ->
                              let (_loc__unnamed_4,_unnamed_4) = _unnamed_4 in
                              let _loc =
                                merge
                                  [_loc__unnamed_4;
                                  _loc_te;
                                  _loc_tes;
                                  _loc__unnamed_0;
                                  _loc_tc] in
                              let constr = id_loc tc _loc_tc in
                              loc_typ _loc
                                (Ptyp_constr (constr, (te :: tes)))))));
        Glr.apply
          (fun pvt  -> let (_loc_pvt,pvt) = pvt in let _loc = _loc_pvt in pvt)
          (locate polymorphic_variant_type);
        Glr.fsequence (locate (Glr.string "<" "<"))
          (Glr.sequence
             (locate
                (Glr.option None
                   (Glr.apply (fun x  -> Some x) (Glr.string ".." ".."))))
             (locate (Glr.string ">" ">"))
             (fun rv  ->
                let (_loc_rv,rv) = rv in
                fun _unnamed_1  ->
                  let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                  fun _unnamed_2  ->
                    let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                    let _loc =
                      merge [_loc__unnamed_2; _loc_rv; _loc__unnamed_1] in
                    let ml =
                      if rv = None
                      then []
                      else
                        [{ pfield_desc = Pfield_var; pfield_loc = _loc_rv }] in
                    loc_typ _loc (Ptyp_object ml)));
        Glr.fsequence (locate (Glr.string "<" "<"))
          (Glr.fsequence (locate method_type)
             (Glr.fsequence
                (locate
                   (Glr.apply List.rev
                      (Glr.fixpoint []
                         (Glr.apply (fun x  l  -> x :: l)
                            (Glr.sequence (locate (Glr.string ";" ";"))
                               (locate method_type)
                               (fun _unnamed_0  ->
                                  let (_loc__unnamed_0,_unnamed_0) =
                                    _unnamed_0 in
                                  fun mt  ->
                                    let (_loc_mt,mt) = mt in
                                    let _loc =
                                      merge [_loc__unnamed_0; _loc_mt] in
                                    mt))))))
                (Glr.sequence
                   (locate
                      (Glr.option None
                         (Glr.apply (fun x  -> Some x)
                            (Glr.sequence (locate (Glr.string ";" ";"))
                               (locate
                                  (Glr.option None
                                     (Glr.apply (fun x  -> Some x)
                                        (Glr.string ".." ".."))))
                               (fun _unnamed_0  ->
                                  let (_loc__unnamed_0,_unnamed_0) =
                                    _unnamed_0 in
                                  fun rv  ->
                                    let (_loc_rv,rv) = rv in
                                    let _loc =
                                      merge [_loc__unnamed_0; _loc_rv] in
                                    rv))))) (locate (Glr.string ">" ">"))
                   (fun rv  ->
                      let (_loc_rv,rv) = rv in
                      fun _unnamed_1  ->
                        let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                        fun mts  ->
                          let (_loc_mts,mts) = mts in
                          fun mt  ->
                            let (_loc_mt,mt) = mt in
                            fun _unnamed_4  ->
                              let (_loc__unnamed_4,_unnamed_4) = _unnamed_4 in
                              let _loc =
                                merge
                                  [_loc__unnamed_4;
                                  _loc_mt;
                                  _loc_mts;
                                  _loc_rv;
                                  _loc__unnamed_1] in
                              let ml =
                                if (rv = None) || (rv = (Some None))
                                then []
                                else
                                  [{
                                     pfield_desc = Pfield_var;
                                     pfield_loc = _loc_rv
                                   }] in
                              loc_typ _loc (Ptyp_object ((mt :: mts) @ ml))))));
        Glr.fsequence (locate (Glr.string "#" "#"))
          (Glr.sequence (locate class_path) (locate opt_present)
             (fun cp  ->
                let (_loc_cp,cp) = cp in
                fun o  ->
                  let (_loc_o,o) = o in
                  fun _unnamed_2  ->
                    let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                    let _loc = merge [_loc__unnamed_2; _loc_cp; _loc_o] in
                    let cp = id_loc cp _loc_cp in
                    loc_typ _loc (Ptyp_class (cp, [], o))));
        Glr.fsequence (locate (Glr.string "(" "("))
          (Glr.fsequence (locate typexpr)
             (Glr.fsequence
                (locate
                   (Glr.apply List.rev
                      (Glr.fixpoint []
                         (Glr.apply (fun x  l  -> x :: l)
                            (Glr.sequence (locate (Glr.string "," ","))
                               (locate typexpr)
                               (fun _unnamed_0  ->
                                  let (_loc__unnamed_0,_unnamed_0) =
                                    _unnamed_0 in
                                  fun te  ->
                                    let (_loc_te,te) = te in
                                    let _loc =
                                      merge [_loc__unnamed_0; _loc_te] in
                                    te))))))
                (Glr.fsequence (locate (Glr.string ")" ")"))
                   (Glr.fsequence (locate (Glr.string "#" "#"))
                      (Glr.sequence (locate class_path) (locate opt_present)
                         (fun cp  ->
                            let (_loc_cp,cp) = cp in
                            fun o  ->
                              let (_loc_o,o) = o in
                              fun _unnamed_2  ->
                                let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                                fun _unnamed_3  ->
                                  let (_loc__unnamed_3,_unnamed_3) =
                                    _unnamed_3 in
                                  fun tes  ->
                                    let (_loc_tes,tes) = tes in
                                    fun te  ->
                                      let (_loc_te,te) = te in
                                      fun _unnamed_6  ->
                                        let (_loc__unnamed_6,_unnamed_6) =
                                          _unnamed_6 in
                                        let _loc =
                                          merge
                                            [_loc__unnamed_6;
                                            _loc_te;
                                            _loc_tes;
                                            _loc__unnamed_3;
                                            _loc__unnamed_2;
                                            _loc_cp;
                                            _loc_o] in
                                        let cp = id_loc cp _loc_cp in
                                        loc_typ _loc
                                          (Ptyp_class (cp, (te :: tes), o))))))));
        Glr.fsequence (locate (Glr.char '$' '$'))
          (Glr.fsequence
             (locate
                (Glr.option None
                   (Glr.apply (fun x  -> Some x)
                      (Glr.sequence
                         (locate
                            (Glr.apply
                               (fun _unnamed_0  ->
                                  let (_loc__unnamed_0,_unnamed_0) =
                                    _unnamed_0 in
                                  let _loc = _loc__unnamed_0 in "tuple")
                               (locate (Glr.string "tuple" "tuple"))))
                         (locate (Glr.char ':' ':'))
                         (fun t  ->
                            let (_loc_t,t) = t in
                            fun _unnamed_1  ->
                              let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                              let _loc = merge [_loc_t; _loc__unnamed_1] in t)))))
             (Glr.sequence (locate (expression_lvl (next_exp App)))
                (locate (Glr.char '$' '$'))
                (fun e  ->
                   let (_loc_e,e) = e in
                   fun _unnamed_1  ->
                     let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                     fun t  ->
                       let (_loc_t,t) = t in
                       fun _unnamed_3  ->
                         let (_loc__unnamed_3,_unnamed_3) = _unnamed_3 in
                         let _loc =
                           merge
                             [_loc__unnamed_3;
                             _loc_t;
                             _loc_e;
                             _loc__unnamed_1] in
                         match t with
                         | None  -> push_pop_type e
                         | Some str ->
                             let l = push_pop_type_list e in
                             (match str with
                              | "tuple" -> loc_typ _loc (Ptyp_tuple l)
                              | _ -> raise Give_up))))]
    let typexpr_suit_aux:
      type_prio ->
        type_prio ->
          (type_prio* (core_type -> Location.t -> core_type)) grammar
      =
      memoize1
        (fun lvl'  lvl  ->
           let ln f _loc e _loc_f = loc_typ (merge2 _loc_f _loc) e in
           Glr.alternatives'
             (let y =
                let y =
                  let y =
                    let y =
                      let y = [] in
                      if (lvl' >= DashType) && (lvl <= DashType)
                      then
                        (Glr.fsequence (locate (Glr.string "#" "#"))
                           (Glr.sequence (locate class_path)
                              (locate opt_present)
                              (fun cp  ->
                                 let (_loc_cp,cp) = cp in
                                 fun o  ->
                                   let (_loc_o,o) = o in
                                   fun _unnamed_2  ->
                                     let (_loc__unnamed_2,_unnamed_2) =
                                       _unnamed_2 in
                                     let _loc =
                                       merge
                                         [_loc__unnamed_2; _loc_cp; _loc_o] in
                                     let cp = id_loc cp _loc_cp in
                                     let tex te =
                                       ln te _loc (Ptyp_class (cp, [te], o)) in
                                     (DashType, tex))))
                        :: y
                      else y in
                    if (lvl' >= As) && (lvl <= As)
                    then
                      (Glr.fsequence (locate as_kw)
                         (Glr.sequence (locate (Glr.string "'" "'"))
                            (locate ident)
                            (fun _unnamed_0  ->
                               let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                               fun id  ->
                                 let (_loc_id,id) = id in
                                 fun _unnamed_2  ->
                                   let (_loc__unnamed_2,_unnamed_2) =
                                     _unnamed_2 in
                                   let _loc =
                                     merge
                                       [_loc__unnamed_2;
                                       _loc__unnamed_0;
                                       _loc_id] in
                                   (As,
                                     (fun te  ->
                                        ln te _loc (Ptyp_alias (te, id)))))))
                      :: y
                    else y in
                  if (lvl' >= AppType) && (lvl <= AppType)
                  then
                    (Glr.apply
                       (fun tc  ->
                          let (_loc_tc,tc) = tc in
                          let _loc = _loc_tc in
                          (AppType,
                            (fun te  ->
                               ln te _loc
                                 (Ptyp_constr ((id_loc tc _loc_tc), [te])))))
                       (locate typeconstr))
                    :: y
                  else y in
                if (lvl' > ProdType) && (lvl <= ProdType)
                then
                  (Glr.apply
                     (fun tes  ->
                        let (_loc_tes,tes) = tes in
                        let _loc = _loc_tes in
                        (ProdType,
                          (fun te  -> ln te _loc (Ptyp_tuple (te :: tes)))))
                     (locate
                        (Glr.sequence
                           (Glr.sequence (locate (Glr.string "*" "*"))
                              (locate (typexpr_lvl (next_type_prio ProdType)))
                              (fun _unnamed_0  ->
                                 let (_loc__unnamed_0,_unnamed_0) =
                                   _unnamed_0 in
                                 fun te  ->
                                   let (_loc_te,te) = te in
                                   let _loc =
                                     merge [_loc__unnamed_0; _loc_te] in
                                   te))
                           (Glr.fixpoint []
                              (Glr.apply (fun x  l  -> x :: l)
                                 (Glr.sequence (locate (Glr.string "*" "*"))
                                    (locate
                                       (typexpr_lvl (next_type_prio ProdType)))
                                    (fun _unnamed_0  ->
                                       let (_loc__unnamed_0,_unnamed_0) =
                                         _unnamed_0 in
                                       fun te  ->
                                         let (_loc_te,te) = te in
                                         let _loc =
                                           merge [_loc__unnamed_0; _loc_te] in
                                         te))))
                           (fun x  l  -> x :: (List.rev l)))))
                  :: y
                else y in
              if (lvl' > Arr) && (lvl <= Arr)
              then
                (Glr.sequence (locate (Glr.string "->" "->"))
                   (locate (typexpr_lvl Arr))
                   (fun _unnamed_0  ->
                      let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                      fun te'  ->
                        let (_loc_te',te') = te' in
                        let _loc = merge [_loc__unnamed_0; _loc_te'] in
                        (Arr,
                          (fun te  -> ln te _loc (Ptyp_arrow ("", te, te'))))))
                :: y
              else y))
    let typexpr_suit =
      let f =
        memoize2'
          (fun type_suit  lvl'  lvl  ->
             Glr.alternatives'
               [Glr.iter
                  (Glr.apply
                     (fun ((_,(p1,f1)) as _unnamed_0)  ->
                        let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                        let _loc = _loc__unnamed_0 in
                        Glr.apply
                          (fun ((_,(p2,f2)) as _unnamed_0)  ->
                             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                             let _loc = _loc__unnamed_0 in
                             (p2,
                               (fun f  _loc_f  -> f2 (f1 f _loc_f) _loc_f)))
                          (locate (type_suit p1 lvl)))
                     (locate (typexpr_suit_aux lvl' lvl)));
               Glr.apply
                 (fun _unnamed_0  ->
                    let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                    let _loc = _loc__unnamed_0 in
                    (lvl', (fun f  _loc_f  -> f))) (locate (Glr.empty ()))]) in
      let rec res x y = f res x y in res
    let _ =
      set_typexpr_lvl
        (fun lvl  ->
           Glr.sequence (locate typexpr_base)
             (locate (typexpr_suit AtomType lvl))
             (fun t  ->
                let (_loc_t,t) = t in
                fun ft  ->
                  let (_loc_ft,ft) = ft in
                  let _loc = merge [_loc_t; _loc_ft] in snd ft t _loc_t))
    let type_param =
      Glr.alternatives'
        [Glr.fsequence (locate opt_variance)
           (Glr.sequence (locate (Glr.char '\'' '\'')) (locate ident)
              (fun _unnamed_0  ->
                 let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                 fun id  ->
                   let (_loc_id,id) = id in
                   fun var  ->
                     let (_loc_var,var) = var in
                     let _loc = merge [_loc_var; _loc__unnamed_0; _loc_id] in
                     ((Some (id_loc id _loc_id)), var)));
        Glr.sequence (locate opt_variance) (locate (Glr.char '_' '_'))
          (fun var  ->
             let (_loc_var,var) = var in
             fun _unnamed_1  ->
               let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
               let _loc = merge [_loc_var; _loc__unnamed_1] in (None, var))]
    let type_params =
      Glr.alternatives'
        [Glr.apply
           (fun tp  -> let (_loc_tp,tp) = tp in let _loc = _loc_tp in [tp])
           (locate type_param);
        Glr.fsequence (locate (Glr.string "(" "("))
          (Glr.fsequence (locate type_param)
             (Glr.sequence
                (locate
                   (Glr.apply List.rev
                      (Glr.fixpoint []
                         (Glr.apply (fun x  l  -> x :: l)
                            (Glr.sequence (locate (Glr.string "," ","))
                               (locate type_param)
                               (fun _unnamed_0  ->
                                  let (_loc__unnamed_0,_unnamed_0) =
                                    _unnamed_0 in
                                  fun tp  ->
                                    let (_loc_tp,tp) = tp in
                                    let _loc =
                                      merge [_loc__unnamed_0; _loc_tp] in
                                    tp)))))) (locate (Glr.string ")" ")"))
                (fun tps  ->
                   let (_loc_tps,tps) = tps in
                   fun _unnamed_1  ->
                     let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                     fun tp  ->
                       let (_loc_tp,tp) = tp in
                       fun _unnamed_3  ->
                         let (_loc__unnamed_3,_unnamed_3) = _unnamed_3 in
                         let _loc =
                           merge
                             [_loc__unnamed_3;
                             _loc_tp;
                             _loc_tps;
                             _loc__unnamed_1] in
                         tp :: tps)))]
    let type_equation =
      Glr.fsequence (locate (Glr.char '=' '='))
        (Glr.sequence (locate private_flag) (locate typexpr)
           (fun p  ->
              let (_loc_p,p) = p in
              fun te  ->
                let (_loc_te,te) = te in
                fun _unnamed_2  ->
                  let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                  let _loc = merge [_loc__unnamed_2; _loc_p; _loc_te] in
                  (p, te)))
    let type_constraint =
      Glr.fsequence (locate constraint_kw)
        (Glr.fsequence (locate (Glr.string "'" "'"))
           (Glr.fsequence (locate ident)
              (Glr.sequence (locate (Glr.char '=' '=')) (locate typexpr)
                 (fun _unnamed_0  ->
                    let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                    fun te  ->
                      let (_loc_te,te) = te in
                      fun id  ->
                        let (_loc_id,id) = id in
                        fun _unnamed_3  ->
                          let (_loc__unnamed_3,_unnamed_3) = _unnamed_3 in
                          fun _unnamed_4  ->
                            let (_loc__unnamed_4,_unnamed_4) = _unnamed_4 in
                            let _loc =
                              merge
                                [_loc__unnamed_4;
                                _loc__unnamed_3;
                                _loc_id;
                                _loc__unnamed_0;
                                _loc_te] in
                            ((loc_typ _loc_id (Ptyp_var id)), te, _loc)))))
    let constr_decl =
      let constr_name =
        Glr.alternatives'
          [Glr.apply
             (fun cn  -> let (_loc_cn,cn) = cn in let _loc = _loc_cn in cn)
             (locate constr_name);
          Glr.sequence (locate (Glr.string "(" "("))
            (locate (Glr.string ")" ")"))
            (fun _unnamed_0  ->
               let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
               fun _unnamed_1  ->
                 let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                 let _loc = merge [_loc__unnamed_0; _loc__unnamed_1] in "()")] in
      Glr.sequence (locate constr_name)
        (locate
           (Glr.apply
              (fun te  ->
                 let (_loc_te,te) = te in
                 let _loc = _loc_te in
                 let tes =
                   match te with
                   | None  -> []
                   | Some { ptyp_desc = Ptyp_tuple tes; ptyp_loc = _ } -> tes
                   | Some t -> [t] in
                 (tes, None))
              (locate
                 (Glr.option None
                    (Glr.apply (fun x  -> Some x)
                       (Glr.sequence (locate of_kw) (locate typexpr)
                          (fun _unnamed_0  ->
                             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                             fun te  ->
                               let (_loc_te,te) = te in
                               let _loc = merge [_loc__unnamed_0; _loc_te] in
                               te)))))))
        (fun cn  ->
           let (_loc_cn,cn) = cn in
           fun ((_,(tes,te)) as _unnamed_1)  ->
             let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
             let _loc = merge [_loc_cn; _loc__unnamed_1] in
             let c = id_loc cn _loc_cn in
             constructor_declaration _loc c tes te)
    let field_decl =
      Glr.fsequence (locate mutable_flag)
        (Glr.fsequence (locate field_name)
           (Glr.sequence (locate (Glr.string ":" ":")) (locate poly_typexpr)
              (fun _unnamed_0  ->
                 let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                 fun pte  ->
                   let (_loc_pte,pte) = pte in
                   fun fn  ->
                     let (_loc_fn,fn) = fn in
                     fun m  ->
                       let (_loc_m,m) = m in
                       let _loc =
                         merge [_loc_m; _loc_fn; _loc__unnamed_0; _loc_pte] in
                       label_declaration _loc (id_loc fn _loc_fn) m pte)))
    let type_representation =
      Glr.alternatives'
        [Glr.fsequence
           (locate
              (Glr.option None
                 (Glr.apply (fun x  -> Some x) (Glr.string "|" "|"))))
           (Glr.sequence (locate constr_decl)
              (locate
                 (Glr.apply List.rev
                    (Glr.fixpoint []
                       (Glr.apply (fun x  l  -> x :: l)
                          (Glr.sequence (locate (Glr.string "|" "|"))
                             (locate constr_decl)
                             (fun _unnamed_0  ->
                                let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                                fun cd  ->
                                  let (_loc_cd,cd) = cd in
                                  let _loc = merge [_loc__unnamed_0; _loc_cd] in
                                  cd))))))
              (fun cd  ->
                 let (_loc_cd,cd) = cd in
                 fun cds  ->
                   let (_loc_cds,cds) = cds in
                   fun _unnamed_2  ->
                     let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                     let _loc = merge [_loc__unnamed_2; _loc_cd; _loc_cds] in
                     Ptype_variant (cd :: cds)));
        Glr.fsequence (locate (Glr.string "{" "{"))
          (Glr.fsequence (locate field_decl)
             (Glr.fsequence
                (locate
                   (Glr.apply List.rev
                      (Glr.fixpoint []
                         (Glr.apply (fun x  l  -> x :: l)
                            (Glr.sequence (locate (Glr.string ";" ";"))
                               (locate field_decl)
                               (fun _unnamed_0  ->
                                  let (_loc__unnamed_0,_unnamed_0) =
                                    _unnamed_0 in
                                  fun fd  ->
                                    let (_loc_fd,fd) = fd in
                                    let _loc =
                                      merge [_loc__unnamed_0; _loc_fd] in
                                    fd))))))
                (Glr.sequence
                   (locate
                      (Glr.option None
                         (Glr.apply (fun x  -> Some x) (Glr.string ";" ";"))))
                   (locate (Glr.string "}" "}"))
                   (fun _unnamed_0  ->
                      let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                      fun _unnamed_1  ->
                        let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                        fun fds  ->
                          let (_loc_fds,fds) = fds in
                          fun fd  ->
                            let (_loc_fd,fd) = fd in
                            fun _unnamed_4  ->
                              let (_loc__unnamed_4,_unnamed_4) = _unnamed_4 in
                              let _loc =
                                merge
                                  [_loc__unnamed_4;
                                  _loc_fd;
                                  _loc_fds;
                                  _loc__unnamed_0;
                                  _loc__unnamed_1] in
                              Ptype_record (fd :: fds)))))]
    let type_information =
      Glr.fsequence
        (locate
           (Glr.option None (Glr.apply (fun x  -> Some x) type_equation)))
        (Glr.sequence
           (locate
              (Glr.option None
                 (Glr.apply (fun x  -> Some x)
                    (Glr.fsequence (locate (Glr.char '=' '='))
                       (Glr.sequence (locate private_flag)
                          (locate type_representation)
                          (fun pri  ->
                             let (_loc_pri,pri) = pri in
                             fun tr  ->
                               let (_loc_tr,tr) = tr in
                               fun _unnamed_2  ->
                                 let (_loc__unnamed_2,_unnamed_2) =
                                   _unnamed_2 in
                                 let _loc =
                                   merge [_loc__unnamed_2; _loc_pri; _loc_tr] in
                                 (pri, tr)))))))
           (locate
              (Glr.apply List.rev
                 (Glr.fixpoint []
                    (Glr.apply (fun x  l  -> x :: l) type_constraint))))
           (fun ptr  ->
              let (_loc_ptr,ptr) = ptr in
              fun cstrs  ->
                let (_loc_cstrs,cstrs) = cstrs in
                fun te  ->
                  let (_loc_te,te) = te in
                  let _loc = merge [_loc_te; _loc_ptr; _loc_cstrs] in
                  let (pri,tkind) =
                    match ptr with
                    | None  -> (Public, Ptype_abstract)
                    | Some c -> c in
                  (pri, te, tkind, cstrs)))
    let typedef_gen ?prev_loc  constr filter =
      Glr.fsequence (locate (Glr.option [] type_params))
        (Glr.sequence (locate constr) (locate type_information)
           (fun tcn  ->
              let (_loc_tcn,tcn) = tcn in
              fun ti  ->
                let (_loc_ti,ti) = ti in
                fun tps  ->
                  let (_loc_tps,tps) = tps in
                  let _loc = merge [_loc_tps; _loc_tcn; _loc_ti] in
                  let _loc =
                    match prev_loc with
                    | None  -> _loc
                    | Some l -> merge2 l _loc in
                  let (pri,te,tkind,cstrs) = ti in
                  let (pri,te) =
                    match te with
                    | None  -> (pri, None)
                    | Some (Private ,te) ->
                        (if pri = Private then raise Give_up;
                         (Private, (Some te)))
                    | Some (_,te) -> (pri, (Some te)) in
                  let tps =
                    List.map
                      (function
                       | (Some s,t) -> (s, t)
                       | (None ,_) -> raise Give_up) tps in
                  ((id_loc tcn _loc_tcn),
                    (type_declaration _loc (id_loc (filter tcn) _loc_tcn) tps
                       cstrs tkind pri te))))
    let typedef = typedef_gen typeconstr_name (fun x  -> x)
    let typedef_in_constraint prev_loc =
      typedef_gen ~prev_loc typeconstr Longident.last
    let type_definition =
      Glr.fsequence (locate type_kw)
        (Glr.sequence (locate typedef)
           (locate
              (Glr.apply List.rev
                 (Glr.fixpoint []
                    (Glr.apply (fun x  l  -> x :: l)
                       (Glr.sequence (locate and_kw) (locate typedef)
                          (fun _unnamed_0  ->
                             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                             fun td  ->
                               let (_loc_td,td) = td in
                               let _loc = merge [_loc__unnamed_0; _loc_td] in
                               td))))))
           (fun td  ->
              let (_loc_td,td) = td in
              fun tds  ->
                let (_loc_tds,tds) = tds in
                fun _unnamed_2  ->
                  let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                  let _loc = merge [_loc__unnamed_2; _loc_td; _loc_tds] in td
                    :: tds))
    let exception_declaration =
      Glr.fsequence (locate exception_kw)
        (Glr.sequence (locate constr_name)
           (locate
              (Glr.option None
                 (Glr.apply (fun x  -> Some x)
                    (Glr.sequence (locate of_kw) (locate typexpr)
                       (fun _unnamed_0  ->
                          let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                          fun te  ->
                            let (_loc_te,te) = te in
                            let _loc = merge [_loc__unnamed_0; _loc_te] in te)))))
           (fun cn  ->
              let (_loc_cn,cn) = cn in
              fun te  ->
                let (_loc_te,te) = te in
                fun _unnamed_2  ->
                  let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                  let _loc = merge [_loc__unnamed_2; _loc_cn; _loc_te] in
                  let tes =
                    match te with
                    | None  -> []
                    | Some { ptyp_desc = Ptyp_tuple tes; ptyp_loc = _ } ->
                        tes
                    | Some t -> [t] in
                  ((id_loc cn _loc_cn), tes, (merge2 _loc_cn _loc_te))))
    let exception_definition =
      Glr.alternatives'
        [Glr.fsequence (locate exception_kw)
           (Glr.fsequence (locate constr_name)
              (Glr.sequence (locate (Glr.char '=' '=')) (locate constr)
                 (fun _unnamed_0  ->
                    let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                    fun c  ->
                      let (_loc_c,c) = c in
                      fun cn  ->
                        let (_loc_cn,cn) = cn in
                        fun _unnamed_3  ->
                          let (_loc__unnamed_3,_unnamed_3) = _unnamed_3 in
                          let _loc =
                            merge
                              [_loc__unnamed_3;
                              _loc_cn;
                              _loc__unnamed_0;
                              _loc_c] in
                          let name = id_loc cn _loc_cn in
                          let ex = id_loc c _loc_c in
                          Pstr_exn_rebind (name, ex))));
        Glr.apply
          (fun ((_,(name,ed,_loc')) as _unnamed_0)  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             let _loc = _loc__unnamed_0 in Pstr_exception (name, ed))
          (locate exception_declaration)]
    let class_field_spec = declare_grammar "class_field_spec"
    let class_body_type = declare_grammar "class_body_type"
    let virt_mut =
      Glr.alternatives'
        [Glr.sequence (locate virtual_flag) (locate mutable_flag)
           (fun v  ->
              let (_loc_v,v) = v in
              fun m  ->
                let (_loc_m,m) = m in
                let _loc = merge [_loc_v; _loc_m] in (v, m));
        Glr.sequence (locate mutable_kw) (locate virtual_kw)
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun _unnamed_1  ->
               let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
               let _loc = merge [_loc__unnamed_0; _loc__unnamed_1] in
               (Virtual, Mutable))]
    let virt_priv =
      Glr.alternatives'
        [Glr.sequence (locate virtual_flag) (locate private_flag)
           (fun v  ->
              let (_loc_v,v) = v in
              fun p  ->
                let (_loc_p,p) = p in
                let _loc = merge [_loc_v; _loc_p] in (v, p));
        Glr.sequence (locate private_kw) (locate virtual_kw)
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun _unnamed_1  ->
               let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
               let _loc = merge [_loc__unnamed_0; _loc__unnamed_1] in
               (Virtual, Private))]
    let _ =
      set_grammar class_field_spec
        (Glr.alternatives'
           [Glr.sequence (locate inherit_kw) (locate class_body_type)
              (fun _unnamed_0  ->
                 let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                 fun cbt  ->
                   let (_loc_cbt,cbt) = cbt in
                   let _loc = merge [_loc__unnamed_0; _loc_cbt] in
                   pctf_loc _loc (Pctf_inher cbt));
           Glr.fsequence (locate val_kw)
             (Glr.fsequence (locate virt_mut)
                (Glr.fsequence (locate inst_var_name)
                   (Glr.sequence (locate (Glr.string ":" ":"))
                      (locate typexpr)
                      (fun _unnamed_0  ->
                         let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                         fun te  ->
                           let (_loc_te,te) = te in
                           fun ivn  ->
                             let (_loc_ivn,ivn) = ivn in
                             fun ((_,(vir,mut)) as _unnamed_3)  ->
                               let (_loc__unnamed_3,_unnamed_3) = _unnamed_3 in
                               fun _unnamed_4  ->
                                 let (_loc__unnamed_4,_unnamed_4) =
                                   _unnamed_4 in
                                 let _loc =
                                   merge
                                     [_loc__unnamed_4;
                                     _loc__unnamed_3;
                                     _loc_ivn;
                                     _loc__unnamed_0;
                                     _loc_te] in
                                 Pctf_val (ivn, mut, vir, te, _loc)))));
           Glr.fsequence (locate method_kw)
             (Glr.fsequence (locate virt_priv)
                (Glr.fsequence (locate method_name)
                   (Glr.sequence (locate (Glr.string ":" ":"))
                      (locate poly_typexpr)
                      (fun _unnamed_0  ->
                         let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                         fun te  ->
                           let (_loc_te,te) = te in
                           fun mn  ->
                             let (_loc_mn,mn) = mn in
                             fun ((_,(v,pri)) as _unnamed_3)  ->
                               let (_loc__unnamed_3,_unnamed_3) = _unnamed_3 in
                               fun _unnamed_4  ->
                                 let (_loc__unnamed_4,_unnamed_4) =
                                   _unnamed_4 in
                                 let _loc =
                                   merge
                                     [_loc__unnamed_4;
                                     _loc__unnamed_3;
                                     _loc_mn;
                                     _loc__unnamed_0;
                                     _loc_te] in
                                 if v = Concrete
                                 then Pctf_meth (mn, pri, te, _loc)
                                 else Pctf_virt (mn, pri, te, _loc)))));
           Glr.fsequence (locate constraint_kw)
             (Glr.fsequence (locate typexpr)
                (Glr.sequence (locate (Glr.char '=' '=')) (locate typexpr)
                   (fun _unnamed_0  ->
                      let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                      fun te'  ->
                        let (_loc_te',te') = te' in
                        fun te  ->
                          let (_loc_te,te) = te in
                          fun _unnamed_3  ->
                            let (_loc__unnamed_3,_unnamed_3) = _unnamed_3 in
                            let _loc =
                              merge
                                [_loc__unnamed_3;
                                _loc_te;
                                _loc__unnamed_0;
                                _loc_te'] in
                            Pctf_cstr (te, te', _loc))))])
    let _ =
      set_grammar class_body_type
        (Glr.alternatives'
           [Glr.fsequence (locate object_kw)
              (Glr.fsequence
                 (locate
                    (Glr.option None
                       (Glr.apply (fun x  -> Some x)
                          (Glr.fsequence (locate (Glr.string "(" "("))
                             (Glr.sequence (locate typexpr)
                                (locate (Glr.string ")" ")"))
                                (fun te  ->
                                   let (_loc_te,te) = te in
                                   fun _unnamed_1  ->
                                     let (_loc__unnamed_1,_unnamed_1) =
                                       _unnamed_1 in
                                     fun _unnamed_2  ->
                                       let (_loc__unnamed_2,_unnamed_2) =
                                         _unnamed_2 in
                                       let _loc =
                                         merge
                                           [_loc__unnamed_2;
                                           _loc_te;
                                           _loc__unnamed_1] in
                                       te))))))
                 (Glr.sequence
                    (locate
                       (Glr.apply List.rev
                          (Glr.fixpoint []
                             (Glr.apply (fun x  l  -> x :: l)
                                class_field_spec)))) (locate end_kw)
                    (fun cfs  ->
                       let (_loc_cfs,cfs) = cfs in
                       fun _unnamed_1  ->
                         let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                         fun te  ->
                           let (_loc_te,te) = te in
                           fun _unnamed_3  ->
                             let (_loc__unnamed_3,_unnamed_3) = _unnamed_3 in
                             let _loc =
                               merge
                                 [_loc__unnamed_3;
                                 _loc_te;
                                 _loc_cfs;
                                 _loc__unnamed_1] in
                             let self =
                               match te with
                               | None  -> loc_typ _loc_te Ptyp_any
                               | Some t -> t in
                             let sign = (self, cfs) in
                             pcty_loc _loc (Pcty_signature sign))));
           Glr.sequence
             (locate
                (Glr.option []
                   (Glr.fsequence (locate (Glr.string "[" "["))
                      (Glr.fsequence (locate typexpr)
                         (Glr.sequence
                            (locate
                               (Glr.apply List.rev
                                  (Glr.fixpoint []
                                     (Glr.apply (fun x  l  -> x :: l)
                                        (Glr.sequence
                                           (locate (Glr.string "," ","))
                                           (locate typexpr)
                                           (fun _unnamed_0  ->
                                              let (_loc__unnamed_0,_unnamed_0)
                                                = _unnamed_0 in
                                              fun te  ->
                                                let (_loc_te,te) = te in
                                                let _loc =
                                                  merge
                                                    [_loc__unnamed_0;
                                                    _loc_te] in
                                                te))))))
                            (locate (Glr.string "]" "]"))
                            (fun tes  ->
                               let (_loc_tes,tes) = tes in
                               fun _unnamed_1  ->
                                 let (_loc__unnamed_1,_unnamed_1) =
                                   _unnamed_1 in
                                 fun te  ->
                                   let (_loc_te,te) = te in
                                   fun _unnamed_3  ->
                                     let (_loc__unnamed_3,_unnamed_3) =
                                       _unnamed_3 in
                                     let _loc =
                                       merge
                                         [_loc__unnamed_3;
                                         _loc_te;
                                         _loc_tes;
                                         _loc__unnamed_1] in
                                     te :: tes)))))) (locate classtype_path)
             (fun tes  ->
                let (_loc_tes,tes) = tes in
                fun ctp  ->
                  let (_loc_ctp,ctp) = ctp in
                  let _loc = merge [_loc_tes; _loc_ctp] in
                  let ctp = id_loc ctp _loc_ctp in
                  pcty_loc _loc (Pcty_constr (ctp, tes)))])
    let class_type =
      Glr.sequence
        (locate
           (Glr.apply List.rev
              (Glr.fixpoint []
                 (Glr.apply (fun x  l  -> x :: l)
                    (Glr.fsequence
                       (locate
                          (Glr.option None
                             (Glr.apply (fun x  -> Some x) maybe_opt_label)))
                       (Glr.sequence (locate (Glr.string ":" ":"))
                          (locate typexpr)
                          (fun _unnamed_0  ->
                             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                             fun te  ->
                               let (_loc_te,te) = te in
                               fun l  ->
                                 let (_loc_l,l) = l in
                                 let _loc =
                                   merge [_loc_l; _loc__unnamed_0; _loc_te] in
                                 (l, te)))))))) (locate class_body_type)
        (fun tes  ->
           let (_loc_tes,tes) = tes in
           fun cbt  ->
             let (_loc_cbt,cbt) = cbt in
             let _loc = merge [_loc_tes; _loc_cbt] in
             let app acc (lab,te) =
               match lab with
               | None  -> pcty_loc _loc (Pcty_fun ("", te, acc))
               | Some l ->
                   pcty_loc _loc
                     (Pcty_fun
                        (l,
                          (if (l.[0]) = '?' then mkoption _loc_tes te else te),
                          acc)) in
             List.fold_left app cbt (List.rev tes))
    let type_parameters =
      Glr.sequence (locate type_param)
        (locate
           (Glr.apply List.rev
              (Glr.fixpoint []
                 (Glr.apply (fun x  l  -> x :: l)
                    (Glr.sequence (locate (Glr.string "," ","))
                       (locate type_param)
                       (fun _unnamed_0  ->
                          let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                          fun i2  ->
                            let (_loc_i2,i2) = i2 in
                            let _loc = merge [_loc__unnamed_0; _loc_i2] in i2))))))
        (fun i1  ->
           let (_loc_i1,i1) = i1 in
           fun l  ->
             let (_loc_l,l) = l in
             let _loc = merge [_loc_i1; _loc_l] in i1 :: l)
    let class_spec =
      Glr.fsequence (locate virtual_flag)
        (Glr.fsequence
           (locate
              (Glr.option []
                 (Glr.fsequence (locate (Glr.string "[" "["))
                    (Glr.sequence (locate type_parameters)
                       (locate (Glr.string "]" "]"))
                       (fun params  ->
                          let (_loc_params,params) = params in
                          fun _unnamed_1  ->
                            let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                            fun _unnamed_2  ->
                              let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                              let _loc =
                                merge
                                  [_loc__unnamed_2;
                                  _loc_params;
                                  _loc__unnamed_1] in
                              params)))))
           (Glr.fsequence (locate class_name)
              (Glr.sequence (locate (Glr.string ":" ":")) (locate class_type)
                 (fun _unnamed_0  ->
                    let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                    fun ct  ->
                      let (_loc_ct,ct) = ct in
                      fun cn  ->
                        let (_loc_cn,cn) = cn in
                        fun params  ->
                          let (_loc_params,params) = params in
                          fun v  ->
                            let (_loc_v,v) = v in
                            let _loc =
                              merge
                                [_loc_v;
                                _loc_params;
                                _loc_cn;
                                _loc__unnamed_0;
                                _loc_ct] in
                            class_type_declaration _loc_params _loc
                              (id_loc cn _loc_cn) params v ct))))
    let class_specification =
      Glr.sequence (locate class_spec)
        (locate
           (Glr.apply List.rev
              (Glr.fixpoint []
                 (Glr.apply (fun x  l  -> x :: l)
                    (Glr.sequence (locate and_kw) (locate class_spec)
                       (fun _unnamed_0  ->
                          let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                          fun cd  ->
                            let (_loc_cd,cd) = cd in
                            let _loc = merge [_loc__unnamed_0; _loc_cd] in cd))))))
        (fun cs  ->
           let (_loc_cs,cs) = cs in
           fun css  ->
             let (_loc_css,css) = css in
             let _loc = merge [_loc_cs; _loc_css] in cs :: css)
    let classtype_def =
      Glr.fsequence (locate virtual_flag)
        (Glr.fsequence
           (locate
              (Glr.option []
                 (Glr.fsequence (locate (Glr.string "[" "["))
                    (Glr.sequence (locate type_parameters)
                       (locate (Glr.string "]" "]"))
                       (fun tp  ->
                          let (_loc_tp,tp) = tp in
                          fun _unnamed_1  ->
                            let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                            fun _unnamed_2  ->
                              let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                              let _loc =
                                merge
                                  [_loc__unnamed_2; _loc_tp; _loc__unnamed_1] in
                              tp)))))
           (Glr.fsequence (locate class_name)
              (Glr.sequence (locate (Glr.char '=' '='))
                 (locate class_body_type)
                 (fun _unnamed_0  ->
                    let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                    fun cbt  ->
                      let (_loc_cbt,cbt) = cbt in
                      fun cn  ->
                        let (_loc_cn,cn) = cn in
                        fun params  ->
                          let (_loc_params,params) = params in
                          fun v  ->
                            let (_loc_v,v) = v in
                            let _loc =
                              merge
                                [_loc_v;
                                _loc_params;
                                _loc_cn;
                                _loc__unnamed_0;
                                _loc_cbt] in
                            class_type_declaration _loc_params _loc
                              (id_loc cn _loc_cn) params v cbt))))
    let classtype_definition =
      Glr.fsequence (locate type_kw)
        (Glr.sequence (locate classtype_def)
           (locate
              (Glr.apply List.rev
                 (Glr.fixpoint []
                    (Glr.apply (fun x  l  -> x :: l)
                       (Glr.sequence (locate and_kw) (locate classtype_def)
                          (fun _unnamed_0  ->
                             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                             fun cd  ->
                               let (_loc_cd,cd) = cd in
                               let _loc = merge [_loc__unnamed_0; _loc_cd] in
                               cd))))))
           (fun cd  ->
              let (_loc_cd,cd) = cd in
              fun cds  ->
                let (_loc_cds,cds) = cds in
                fun _unnamed_2  ->
                  let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                  let _loc = merge [_loc__unnamed_2; _loc_cd; _loc_cds] in cd
                    :: cds))
    let constant =
      Glr.alternatives'
        [Glr.apply
           (fun f  ->
              let (_loc_f,f) = f in let _loc = _loc_f in Const_float f)
           (locate float_literal);
        Glr.apply
          (fun c  -> let (_loc_c,c) = c in let _loc = _loc_c in Const_char c)
          (locate char_literal);
        Glr.apply
          (fun s  ->
             let (_loc_s,s) = s in let _loc = _loc_s in const_string s)
          (locate string_literal);
        Glr.apply
          (fun i  -> let (_loc_i,i) = i in let _loc = _loc_i in Const_int32 i)
          (locate int32_lit);
        Glr.apply
          (fun i  -> let (_loc_i,i) = i in let _loc = _loc_i in Const_int64 i)
          (locate int64_lit);
        Glr.apply
          (fun i  ->
             let (_loc_i,i) = i in let _loc = _loc_i in Const_nativeint i)
          (locate nat_int_lit);
        Glr.apply
          (fun i  -> let (_loc_i,i) = i in let _loc = _loc_i in Const_int i)
          (locate integer_literal)]
    let neg_constant =
      Glr.alternatives'
        [Glr.sequence
           (locate
              (Glr.alternatives'
                 [Glr.apply
                    (fun _unnamed_0  ->
                       let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                       let _loc = _loc__unnamed_0 in ())
                    (locate (Glr.char '-' '-'));
                 Glr.apply
                   (fun _unnamed_0  ->
                      let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                      let _loc = _loc__unnamed_0 in ())
                   (locate (Glr.string "-." "-."))])) (locate float_literal)
           (fun _unnamed_0  ->
              let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
              fun f  ->
                let (_loc_f,f) = f in
                let _loc = merge [_loc__unnamed_0; _loc_f] in
                Const_float ("-" ^ f));
        Glr.sequence (locate (Glr.char '-' '-')) (locate int32_lit)
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun i  ->
               let (_loc_i,i) = i in
               let _loc = merge [_loc__unnamed_0; _loc_i] in
               Const_int32 (Int32.neg i));
        Glr.sequence (locate (Glr.char '-' '-')) (locate int64_lit)
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun i  ->
               let (_loc_i,i) = i in
               let _loc = merge [_loc__unnamed_0; _loc_i] in
               Const_int64 (Int64.neg i));
        Glr.sequence (locate (Glr.char '-' '-')) (locate nat_int_lit)
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun i  ->
               let (_loc_i,i) = i in
               let _loc = merge [_loc__unnamed_0; _loc_i] in
               Const_nativeint (Nativeint.neg i));
        Glr.sequence (locate (Glr.char '-' '-')) (locate integer_literal)
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun i  ->
               let (_loc_i,i) = i in
               let _loc = merge [_loc__unnamed_0; _loc_i] in Const_int (- i))]
    let pattern_prios =
      [TopPat; AsPat; AltPat; TupPat; ConsPat; ConstrPat; AtomPat]
    let next_pat_prio =
      function
      | TopPat  -> AsPat
      | AsPat  -> AltPat
      | AltPat  -> TupPat
      | TupPat  -> ConsPat
      | ConsPat  -> ConstrPat
      | ConstrPat  -> AtomPat
      | AtomPat  -> AtomPat
    let ppat_list _loc l =
      let nil = id_loc (Lident "[]") _loc in
      let cons x xs =
        let c = id_loc (Lident "::") _loc in
        let cons =
          ppat_construct (c, (Some (loc_pat _loc (Ppat_tuple [x; xs])))) in
        loc_pat _loc cons in
      List.fold_right cons l (loc_pat _loc (ppat_construct (nil, None)))
    let pattern_base =
      memoize1
        (fun lvl  ->
           Glr.alternatives'
             ((Glr.apply
                 (fun e  -> let (_loc_e,e) = e in let _loc = _loc_e in e)
                 (locate (alternatives extra_patterns))) ::
             (Glr.apply
                (fun vn  ->
                   let (_loc_vn,vn) = vn in
                   let _loc = _loc_vn in
                   (AtomPat, (loc_pat _loc (Ppat_var (id_loc vn _loc_vn)))))
                (locate value_name)) ::
             (Glr.apply
                (fun _unnamed_0  ->
                   let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                   let _loc = _loc__unnamed_0 in
                   (AtomPat, (loc_pat _loc Ppat_any)))
                (locate (Glr.string "_" "_"))) ::
             (Glr.fsequence (locate char_literal)
                (Glr.sequence (locate (Glr.string ".." ".."))
                   (locate char_literal)
                   (fun _unnamed_0  ->
                      let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                      fun c2  ->
                        let (_loc_c2,c2) = c2 in
                        fun c1  ->
                          let (_loc_c1,c1) = c1 in
                          let _loc =
                            merge [_loc_c1; _loc__unnamed_0; _loc_c2] in
                          let (ic1,ic2) = ((Char.code c1), (Char.code c2)) in
                          if ic1 > ic2 then assert false;
                          (let const i =
                             Ppat_constant (Const_char (Char.chr i)) in
                           let rec range acc a b =
                             if a > b
                             then assert false
                             else
                               if a = b
                               then a :: acc
                               else range (a :: acc) (a + 1) b in
                           let opts =
                             List.map (fun i  -> loc_pat _loc (const i))
                               (range [] ic1 ic2) in
                           (AtomPat,
                             (List.fold_left
                                (fun acc  o  ->
                                   loc_pat _loc (Ppat_or (o, acc)))
                                (List.hd opts) (List.tl opts))))))) ::
             (Glr.apply
                (fun c  ->
                   let (_loc_c,c) = c in
                   let _loc = _loc_c in
                   (AtomPat, (loc_pat _loc (Ppat_constant c))))
                (locate
                   (Glr.alternatives'
                      [Glr.apply
                         (fun c  ->
                            let (_loc_c,c) = c in let _loc = _loc_c in c)
                         (locate constant);
                      Glr.apply
                        (fun c  ->
                           let (_loc_c,c) = c in let _loc = _loc_c in c)
                        (locate neg_constant)]))) ::
             (Glr.fsequence (locate (Glr.string "(" "("))
                (Glr.sequence (locate pattern) (locate (Glr.string ")" ")"))
                   (fun p  ->
                      let (_loc_p,p) = p in
                      fun _unnamed_1  ->
                        let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                        fun _unnamed_2  ->
                          let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                          let _loc =
                            merge [_loc__unnamed_2; _loc_p; _loc__unnamed_1] in
                          (AtomPat, p)))) ::
             (let y =
                let y =
                  (Glr.apply
                     (fun c  ->
                        let (_loc_c,c) = c in
                        let _loc = _loc_c in
                        let ast = ppat_construct ((id_loc c _loc_c), None) in
                        (AtomPat, (loc_pat _loc ast))) (locate constr))
                  ::
                  (Glr.apply
                     (fun b  ->
                        let (_loc_b,b) = b in
                        let _loc = _loc_b in
                        let fls = id_loc (Lident b) _loc in
                        (AtomPat,
                          (loc_pat _loc (ppat_construct (fls, None)))))
                     (locate bool_lit))
                  ::
                  (let y =
                     [Glr.apply
                        (fun c  ->
                           let (_loc_c,c) = c in
                           let _loc = _loc_c in
                           (AtomPat, (loc_pat _loc (Ppat_variant (c, None)))))
                        (locate tag_name);
                     Glr.sequence (locate (Glr.string "#" "#"))
                       (locate typeconstr)
                       (fun s  ->
                          let (_loc_s,s) = s in
                          fun t  ->
                            let (_loc_t,t) = t in
                            let _loc = merge [_loc_s; _loc_t] in
                            (AtomPat,
                              (loc_pat _loc (Ppat_type (id_loc t _loc_t)))));
                     Glr.fsequence (locate (Glr.string "{" "{"))
                       (Glr.fsequence (locate field)
                          (Glr.fsequence
                             (locate
                                (Glr.option None
                                   (Glr.apply (fun x  -> Some x)
                                      (Glr.sequence
                                         (locate (Glr.char '=' '='))
                                         (locate pattern)
                                         (fun _unnamed_0  ->
                                            let (_loc__unnamed_0,_unnamed_0)
                                              = _unnamed_0 in
                                            fun p  ->
                                              let (_loc_p,p) = p in
                                              let _loc =
                                                merge
                                                  [_loc__unnamed_0; _loc_p] in
                                              p)))))
                             (Glr.fsequence
                                (locate
                                   (Glr.apply List.rev
                                      (Glr.fixpoint []
                                         (Glr.apply (fun x  l  -> x :: l)
                                            (Glr.fsequence
                                               (locate (Glr.string ";" ";"))
                                               (Glr.sequence (locate field)
                                                  (locate
                                                     (Glr.option None
                                                        (Glr.apply
                                                           (fun x  -> Some x)
                                                           (Glr.sequence
                                                              (locate
                                                                 (Glr.char
                                                                    '=' '='))
                                                              (locate pattern)
                                                              (fun _unnamed_0
                                                                  ->
                                                                 let 
                                                                   (_loc__unnamed_0,_unnamed_0)
                                                                   =
                                                                   _unnamed_0 in
                                                                 fun p  ->
                                                                   let 
                                                                    (_loc_p,p)
                                                                    = p in
                                                                   let _loc =
                                                                    merge
                                                                    [_loc__unnamed_0;
                                                                    _loc_p] in
                                                                   p)))))
                                                  (fun f  ->
                                                     let (_loc_f,f) = f in
                                                     fun p  ->
                                                       let (_loc_p,p) = p in
                                                       fun _unnamed_2  ->
                                                         let (_loc__unnamed_2,_unnamed_2)
                                                           = _unnamed_2 in
                                                         let _loc =
                                                           merge
                                                             [_loc__unnamed_2;
                                                             _loc_f;
                                                             _loc_p] in
                                                         ((id_loc f _loc_f),
                                                           p))))))))
                                (Glr.fsequence
                                   (locate
                                      (Glr.option None
                                         (Glr.apply (fun x  -> Some x)
                                            (Glr.sequence
                                               (locate (Glr.string ";" ";"))
                                               (locate (Glr.string "_" "_"))
                                               (fun _unnamed_0  ->
                                                  let (_loc__unnamed_0,_unnamed_0)
                                                    = _unnamed_0 in
                                                  fun _unnamed_1  ->
                                                    let (_loc__unnamed_1,_unnamed_1)
                                                      = _unnamed_1 in
                                                    let _loc =
                                                      merge
                                                        [_loc__unnamed_0;
                                                        _loc__unnamed_1] in
                                                    ())))))
                                   (Glr.sequence
                                      (locate
                                         (Glr.option None
                                            (Glr.apply (fun x  -> Some x)
                                               (Glr.string ";" ";"))))
                                      (locate (Glr.string "}" "}"))
                                      (fun _unnamed_0  ->
                                         let (_loc__unnamed_0,_unnamed_0) =
                                           _unnamed_0 in
                                         fun _unnamed_1  ->
                                           let (_loc__unnamed_1,_unnamed_1) =
                                             _unnamed_1 in
                                           fun clsd  ->
                                             let (_loc_clsd,clsd) = clsd in
                                             fun fps  ->
                                               let (_loc_fps,fps) = fps in
                                               fun p  ->
                                                 let (_loc_p,p) = p in
                                                 fun f  ->
                                                   let (_loc_f,f) = f in
                                                   fun s  ->
                                                     let (_loc_s,s) = s in
                                                     let _loc =
                                                       merge
                                                         [_loc_s;
                                                         _loc_f;
                                                         _loc_p;
                                                         _loc_fps;
                                                         _loc_clsd;
                                                         _loc__unnamed_0;
                                                         _loc__unnamed_1] in
                                                     let all =
                                                       ((id_loc f _loc_f), p)
                                                       :: fps in
                                                     let f (lab,pat) =
                                                       match pat with
                                                       | Some p -> (lab, p)
                                                       | None  ->
                                                           let slab =
                                                             match lab with
                                                             | Lident s -> s
                                                             | _ ->
                                                                 raise
                                                                   Give_up in
                                                           (lab,
                                                             (loc_pat _loc
                                                                (Ppat_var
                                                                   slab))) in
                                                     let all = List.map f all in
                                                     let cl =
                                                       match clsd with
                                                       | None  -> Closed
                                                       | Some _ -> Open in
                                                     (AtomPat,
                                                       (loc_pat _loc
                                                          (Ppat_record
                                                             (all, cl))))))))));
                     Glr.fsequence (locate (Glr.string "[" "["))
                       (Glr.fsequence (locate pattern)
                          (Glr.fsequence
                             (locate
                                (Glr.apply List.rev
                                   (Glr.fixpoint []
                                      (Glr.apply (fun x  l  -> x :: l)
                                         (Glr.sequence
                                            (locate (Glr.string ";" ";"))
                                            (locate pattern)
                                            (fun _unnamed_0  ->
                                               let (_loc__unnamed_0,_unnamed_0)
                                                 = _unnamed_0 in
                                               fun p  ->
                                                 let (_loc_p,p) = p in
                                                 let _loc =
                                                   merge
                                                     [_loc__unnamed_0;
                                                     _loc_p] in
                                                 p))))))
                             (Glr.sequence
                                (locate
                                   (Glr.option None
                                      (Glr.apply (fun x  -> Some x)
                                         (Glr.string ";" ";"))))
                                (locate (Glr.string "]" "]"))
                                (fun _unnamed_0  ->
                                   let (_loc__unnamed_0,_unnamed_0) =
                                     _unnamed_0 in
                                   fun _unnamed_1  ->
                                     let (_loc__unnamed_1,_unnamed_1) =
                                       _unnamed_1 in
                                     fun ps  ->
                                       let (_loc_ps,ps) = ps in
                                       fun p  ->
                                         let (_loc_p,p) = p in
                                         fun _unnamed_4  ->
                                           let (_loc__unnamed_4,_unnamed_4) =
                                             _unnamed_4 in
                                           let _loc =
                                             merge
                                               [_loc__unnamed_4;
                                               _loc_p;
                                               _loc_ps;
                                               _loc__unnamed_0;
                                               _loc__unnamed_1] in
                                           (AtomPat,
                                             (ppat_list _loc (p :: ps)))))));
                     Glr.sequence (locate (Glr.string "[" "["))
                       (locate (Glr.string "]" "]"))
                       (fun _unnamed_0  ->
                          let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                          fun _unnamed_1  ->
                            let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                            let _loc =
                              merge [_loc__unnamed_0; _loc__unnamed_1] in
                            let nil = id_loc (Lident "[]") _loc in
                            (AtomPat,
                              (loc_pat _loc (ppat_construct (nil, None)))));
                     Glr.fsequence (locate (Glr.string "[|" "[|"))
                       (Glr.fsequence (locate pattern)
                          (Glr.fsequence
                             (locate
                                (Glr.apply List.rev
                                   (Glr.fixpoint []
                                      (Glr.apply (fun x  l  -> x :: l)
                                         (Glr.sequence
                                            (locate (Glr.string ";" ";"))
                                            (locate pattern)
                                            (fun _unnamed_0  ->
                                               let (_loc__unnamed_0,_unnamed_0)
                                                 = _unnamed_0 in
                                               fun p  ->
                                                 let (_loc_p,p) = p in
                                                 let _loc =
                                                   merge
                                                     [_loc__unnamed_0;
                                                     _loc_p] in
                                                 p))))))
                             (Glr.sequence
                                (locate
                                   (Glr.option None
                                      (Glr.apply (fun x  -> Some x)
                                         (Glr.string ";" ";"))))
                                (locate (Glr.string "|]" "|]"))
                                (fun _unnamed_0  ->
                                   let (_loc__unnamed_0,_unnamed_0) =
                                     _unnamed_0 in
                                   fun _unnamed_1  ->
                                     let (_loc__unnamed_1,_unnamed_1) =
                                       _unnamed_1 in
                                     fun ps  ->
                                       let (_loc_ps,ps) = ps in
                                       fun p  ->
                                         let (_loc_p,p) = p in
                                         fun _unnamed_4  ->
                                           let (_loc__unnamed_4,_unnamed_4) =
                                             _unnamed_4 in
                                           let _loc =
                                             merge
                                               [_loc__unnamed_4;
                                               _loc_p;
                                               _loc_ps;
                                               _loc__unnamed_0;
                                               _loc__unnamed_1] in
                                           (AtomPat,
                                             (loc_pat _loc
                                                (Ppat_array (p :: ps))))))));
                     Glr.sequence (locate (Glr.string "[|" "[|"))
                       (locate (Glr.string "|]" "|]"))
                       (fun _unnamed_0  ->
                          let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                          fun _unnamed_1  ->
                            let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                            let _loc =
                              merge [_loc__unnamed_0; _loc__unnamed_1] in
                            (AtomPat, (loc_pat _loc (Ppat_array []))));
                     Glr.sequence (locate (Glr.string "(" "("))
                       (locate (Glr.string ")" ")"))
                       (fun _unnamed_0  ->
                          let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                          fun _unnamed_1  ->
                            let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                            let _loc =
                              merge [_loc__unnamed_0; _loc__unnamed_1] in
                            let unt = id_loc (Lident "()") _loc in
                            (AtomPat,
                              (loc_pat _loc (ppat_construct (unt, None)))));
                     Glr.sequence (locate begin_kw) (locate end_kw)
                       (fun _unnamed_0  ->
                          let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                          fun _unnamed_1  ->
                            let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                            let _loc =
                              merge [_loc__unnamed_0; _loc__unnamed_1] in
                            let unt = id_loc (Lident "()") _loc in
                            (AtomPat,
                              (loc_pat _loc (ppat_construct (unt, None)))));
                     Glr.sequence (locate (Glr.char '$' '$'))
                       (locate capitalized_ident)
                       (fun _unnamed_0  ->
                          let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                          fun c  ->
                            let (_loc_c,c) = c in
                            let _loc = merge [_loc__unnamed_0; _loc_c] in
                            try
                              let str = Sys.getenv c in
                              (AtomPat,
                                (parse_string ~filename:("ENV:" ^ c) pattern
                                   blank str))
                            with | Not_found  -> raise Give_up);
                     Glr.fsequence (locate (Glr.char '$' '$'))
                       (Glr.fsequence
                          (locate
                             (Glr.option None
                                (Glr.apply (fun x  -> Some x)
                                   (Glr.sequence
                                      (locate
                                         (Glr.alternatives'
                                            [Glr.apply
                                               (fun _unnamed_0  ->
                                                  let (_loc__unnamed_0,_unnamed_0)
                                                    = _unnamed_0 in
                                                  let _loc = _loc__unnamed_0 in
                                                  "tuple")
                                               (locate
                                                  (Glr.string "tuple" "tuple"));
                                            Glr.apply
                                              (fun _unnamed_0  ->
                                                 let (_loc__unnamed_0,_unnamed_0)
                                                   = _unnamed_0 in
                                                 let _loc = _loc__unnamed_0 in
                                                 "list")
                                              (locate
                                                 (Glr.string "list" "list"));
                                            Glr.apply
                                              (fun _unnamed_0  ->
                                                 let (_loc__unnamed_0,_unnamed_0)
                                                   = _unnamed_0 in
                                                 let _loc = _loc__unnamed_0 in
                                                 "array")
                                              (locate
                                                 (Glr.string "array" "array"))]))
                                      (locate (Glr.char ':' ':'))
                                      (fun t  ->
                                         let (_loc_t,t) = t in
                                         fun _unnamed_1  ->
                                           let (_loc__unnamed_1,_unnamed_1) =
                                             _unnamed_1 in
                                           let _loc =
                                             merge [_loc_t; _loc__unnamed_1] in
                                           t)))))
                          (Glr.sequence
                             (locate (expression_lvl (next_exp App)))
                             (locate (Glr.char '$' '$'))
                             (fun e  ->
                                let (_loc_e,e) = e in
                                fun _unnamed_1  ->
                                  let (_loc__unnamed_1,_unnamed_1) =
                                    _unnamed_1 in
                                  fun t  ->
                                    let (_loc_t,t) = t in
                                    fun _unnamed_3  ->
                                      let (_loc__unnamed_3,_unnamed_3) =
                                        _unnamed_3 in
                                      let _loc =
                                        merge
                                          [_loc__unnamed_3;
                                          _loc_t;
                                          _loc_e;
                                          _loc__unnamed_1] in
                                      match t with
                                      | None  ->
                                          (AtomPat, (push_pop_pattern e))
                                      | Some str ->
                                          let l = push_pop_pattern_list e in
                                          (match str with
                                           | "tuple" ->
                                               (AtomPat,
                                                 (loc_pat _loc (Ppat_tuple l)))
                                           | "array" ->
                                               (AtomPat,
                                                 (loc_pat _loc (Ppat_array l)))
                                           | "list" ->
                                               (AtomPat, (ppat_list _loc l))
                                           | _ -> raise Give_up))))] in
                   if lvl <= ConstrPat
                   then
                     (Glr.sequence (locate tag_name)
                        (locate (pattern_lvl ConstrPat))
                        (fun c  ->
                           let (_loc_c,c) = c in
                           fun p  ->
                             let (_loc_p,p) = p in
                             let _loc = merge [_loc_c; _loc_p] in
                             (ConstrPat,
                               (loc_pat _loc (Ppat_variant (c, (Some p)))))))
                     :: y
                   else y) in
                if lvl <= ConstrPat
                then
                  (Glr.sequence (locate constr)
                     (locate (pattern_lvl ConstrPat))
                     (fun c  ->
                        let (_loc_c,c) = c in
                        fun p  ->
                          let (_loc_p,p) = p in
                          let _loc = merge [_loc_c; _loc_p] in
                          let ast =
                            ppat_construct ((id_loc c _loc_c), (Some p)) in
                          (ConstrPat, (loc_pat _loc ast))))
                  :: y
                else y in
              if lvl <= ConstrPat
              then
                (Glr.sequence (locate lazy_kw)
                   (locate (pattern_lvl ConstrPat))
                   (fun _unnamed_0  ->
                      let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                      fun p  ->
                        let (_loc_p,p) = p in
                        let _loc = merge [_loc__unnamed_0; _loc_p] in
                        let ast = Ppat_lazy p in
                        (ConstrPat, (loc_pat _loc ast))))
                :: y
              else y)))
    let pattern_suit_aux:
      pattern_prio ->
        pattern_prio -> (pattern_prio* (pattern -> pattern)) grammar
      =
      memoize1
        (fun lvl'  lvl  ->
           let ln f _loc e = loc_pat (merge2 f.ppat_loc _loc) e in
           Glr.alternatives'
             (let y =
                let y =
                  let y =
                    let y =
                      let y =
                        let y = [] in
                        if (lvl' >= TopPat) && (lvl <= TopPat)
                        then
                          (Glr.sequence (locate (Glr.string ":" ":"))
                             (locate typexpr)
                             (fun _unnamed_0  ->
                                let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                                fun ty  ->
                                  let (_loc_ty,ty) = ty in
                                  let _loc = merge [_loc__unnamed_0; _loc_ty] in
                                  (lvl',
                                    (fun p  ->
                                       ln p _loc (Ppat_constraint (p, ty))))))
                          :: y
                        else y in
                      if (lvl' >= AsPat) && (lvl <= AsPat)
                      then
                        (Glr.fsequence (locate (Glr.string ":" ":"))
                           (Glr.fsequence
                              (locate
                                 (Glr.sequence
                                    (Glr.sequence
                                       (locate (Glr.string "'" "'"))
                                       (locate ident)
                                       (fun _unnamed_0  ->
                                          let (_loc__unnamed_0,_unnamed_0) =
                                            _unnamed_0 in
                                          fun id  ->
                                            let (_loc_id,id) = id in
                                            let _loc =
                                              merge
                                                [_loc__unnamed_0; _loc_id] in
                                            id))
                                    (Glr.fixpoint []
                                       (Glr.apply (fun x  l  -> x :: l)
                                          (Glr.sequence
                                             (locate (Glr.string "'" "'"))
                                             (locate ident)
                                             (fun _unnamed_0  ->
                                                let (_loc__unnamed_0,_unnamed_0)
                                                  = _unnamed_0 in
                                                fun id  ->
                                                  let (_loc_id,id) = id in
                                                  let _loc =
                                                    merge
                                                      [_loc__unnamed_0;
                                                      _loc_id] in
                                                  id))))
                                    (fun x  l  -> x :: (List.rev l))))
                              (Glr.sequence (locate (Glr.string "." "."))
                                 (locate typexpr)
                                 (fun _unnamed_0  ->
                                    let (_loc__unnamed_0,_unnamed_0) =
                                      _unnamed_0 in
                                    fun te  ->
                                      let (_loc_te,te) = te in
                                      fun ids  ->
                                        let (_loc_ids,ids) = ids in
                                        fun _unnamed_3  ->
                                          let (_loc__unnamed_3,_unnamed_3) =
                                            _unnamed_3 in
                                          let _loc =
                                            merge
                                              [_loc__unnamed_3;
                                              _loc_ids;
                                              _loc__unnamed_0;
                                              _loc_te] in
                                          (AsPat,
                                            (fun p  ->
                                               ln p _loc
                                                 (Ppat_constraint
                                                    (p,
                                                      (loc_typ _loc
                                                         (Ptyp_poly (ids, te)))))))))))
                        :: y
                      else y in
                    if (lvl' > ConsPat) && (lvl <= ConsPat)
                    then
                      (Glr.sequence (locate (Glr.string "::" "::"))
                         (locate (pattern_lvl ConsPat))
                         (fun c  ->
                            let (_loc_c,c) = c in
                            fun p'  ->
                              let (_loc_p',p') = p' in
                              let _loc = merge [_loc_c; _loc_p'] in
                              (ConsPat,
                                (fun p  ->
                                   let cons = id_loc (Lident "::") _loc_c in
                                   let args =
                                     loc_pat _loc (Ppat_tuple [p; p']) in
                                   ln p _loc
                                     (ppat_construct (cons, (Some args)))))))
                      :: y
                    else y in
                  if (lvl' > TupPat) && (lvl <= TupPat)
                  then
                    (Glr.apply
                       (fun ps  ->
                          let (_loc_ps,ps) = ps in
                          let _loc = _loc_ps in
                          (TupPat,
                            (fun p  -> ln p _loc (Ppat_tuple (p :: ps)))))
                       (locate
                          (Glr.sequence
                             (Glr.sequence (locate (Glr.string "," ","))
                                (locate (pattern_lvl (next_pat_prio TupPat)))
                                (fun _unnamed_0  ->
                                   let (_loc__unnamed_0,_unnamed_0) =
                                     _unnamed_0 in
                                   fun p  ->
                                     let (_loc_p,p) = p in
                                     let _loc =
                                       merge [_loc__unnamed_0; _loc_p] in
                                     p))
                             (Glr.fixpoint []
                                (Glr.apply (fun x  l  -> x :: l)
                                   (Glr.sequence
                                      (locate (Glr.string "," ","))
                                      (locate
                                         (pattern_lvl (next_pat_prio TupPat)))
                                      (fun _unnamed_0  ->
                                         let (_loc__unnamed_0,_unnamed_0) =
                                           _unnamed_0 in
                                         fun p  ->
                                           let (_loc_p,p) = p in
                                           let _loc =
                                             merge [_loc__unnamed_0; _loc_p] in
                                           p))))
                             (fun x  l  -> x :: (List.rev l)))))
                    :: y
                  else y in
                if (lvl' >= AltPat) && (lvl <= AltPat)
                then
                  (Glr.sequence (locate (Glr.string "|" "|"))
                     (locate (pattern_lvl (next_pat_prio AltPat)))
                     (fun _unnamed_0  ->
                        let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                        fun p'  ->
                          let (_loc_p',p') = p' in
                          let _loc = merge [_loc__unnamed_0; _loc_p'] in
                          (AltPat, (fun p  -> ln p _loc (Ppat_or (p, p'))))))
                  :: y
                else y in
              if (lvl' >= AsPat) && (lvl <= AsPat)
              then
                (Glr.sequence (locate as_kw) (locate value_name)
                   (fun _unnamed_0  ->
                      let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                      fun vn  ->
                        let (_loc_vn,vn) = vn in
                        let _loc = merge [_loc__unnamed_0; _loc_vn] in
                        (lvl',
                          (fun p  ->
                             ln p _loc (Ppat_alias (p, (id_loc vn _loc_vn)))))))
                :: y
              else y))
    let pattern_suit =
      let f =
        memoize2'
          (fun pat_suit  lvl'  lvl  ->
             Glr.alternatives'
               [Glr.iter
                  (Glr.apply
                     (fun ((_,(p1,f1)) as _unnamed_0)  ->
                        let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                        let _loc = _loc__unnamed_0 in
                        Glr.apply
                          (fun ((_,(p2,f2)) as _unnamed_0)  ->
                             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                             let _loc = _loc__unnamed_0 in
                             (p2, (fun f  -> f2 (f1 f))))
                          (locate (pat_suit p1 lvl)))
                     (locate (pattern_suit_aux lvl' lvl)));
               Glr.apply
                 (fun _unnamed_0  ->
                    let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                    let _loc = _loc__unnamed_0 in (lvl', (fun f  -> f)))
                 (locate (Glr.empty ()))]) in
      let rec res x y = f res x y in res
    let _ =
      set_pattern_lvl
        (fun lvl  ->
           Glr.iter
             (Glr.apply
                (fun ((_,(lvl',t)) as _unnamed_0)  ->
                   let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                   let _loc = _loc__unnamed_0 in
                   Glr.apply
                     (fun ft  ->
                        let (_loc_ft,ft) = ft in
                        let _loc = _loc_ft in snd ft t)
                     (locate (pattern_suit lvl' lvl)))
                (locate (pattern_base lvl))))
    let expression_lvls =
      [Top;
      Let;
      Seq;
      Coerce;
      If;
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
    let let_prio lvl = if !modern then lvl else Let
    let let_re = if !modern then "\\(let\\)\\|\\(val\\)\\b" else "let\\b"
    type assoc =  
      | NoAssoc
      | Left
      | Right 
    let assoc =
      function
      | Prefix |Dot |Dash |Opp  -> NoAssoc
      | Prod |Sum |Eq  -> Left
      | _ -> Right
    let infix_prio s =
      let s1 = if (String.length s) > 1 then s.[1] else ' ' in
      match ((s.[0]), s1) with
      | _ when List.mem s ["lsl"; "lsr"; "asr"] -> Pow
      | _ when List.mem s ["mod"; "land"; "lor"; "lxor"] -> Prod
      | _ when List.mem s ["&"; "&&"] -> Conj
      | _ when List.mem s ["or"; "||"] -> Disj
      | _ when List.mem s [":="; "<-"] -> Aff
      | ('*','*') -> Pow
      | (('*'|'/'|'%'),_) -> Prod
      | (('+'|'-'),_) -> Sum
      | (':',_) -> Cons
      | (('@'|'^'),_) -> Append
      | (('='|'<'|'>'|'|'|'&'|'$'|'!'),_) -> Eq
      | _ -> (Printf.printf "%s\n%!" s; assert false)
    let prefix_prio s =
      if (s = "-") || ((s = "-.") || ((s = "+") || (s = "+.")))
      then Opp
      else Prefix
    let array_function loc str name =
      let name = if !fast then "unsafe_" ^ name else name in
      loc_expr loc (Pexp_ident (id_loc (Ldot ((Lident str), name)) loc))
    let bigarray_function loc str name =
      let name = if !fast then "unsafe_" ^ name else name in
      let lid = Ldot ((Ldot ((Lident "Bigarray"), str)), name) in
      loc_expr loc (Pexp_ident (id_loc lid loc))
    let untuplify exp =
      match exp.pexp_desc with | Pexp_tuple es -> es | _ -> [exp]
    let bigarray_get loc arr arg =
      let get = if !fast then "unsafe_get" else "get" in
      match untuplify arg with
      | c1::[] ->
          loc_expr loc
            (Pexp_apply
               ((bigarray_function loc "Array1" get), [("", arr); ("", c1)]))
      | c1::c2::[] ->
          loc_expr loc
            (Pexp_apply
               ((bigarray_function loc "Array2" get),
                 [("", arr); ("", c1); ("", c2)]))
      | c1::c2::c3::[] ->
          loc_expr loc
            (Pexp_apply
               ((bigarray_function loc "Array3" get),
                 [("", arr); ("", c1); ("", c2); ("", c3)]))
      | coords ->
          loc_expr loc
            (Pexp_apply
               ((bigarray_function loc "Genarray" "get"),
                 [("", arr); ("", (loc_expr loc (Pexp_array coords)))]))
    let bigarray_set loc arr arg newval =
      let set = if !fast then "unsafe_set" else "set" in
      match untuplify arg with
      | c1::[] ->
          loc_expr loc
            (Pexp_apply
               ((bigarray_function loc "Array1" set),
                 [("", arr); ("", c1); ("", newval)]))
      | c1::c2::[] ->
          loc_expr loc
            (Pexp_apply
               ((bigarray_function loc "Array2" set),
                 [("", arr); ("", c1); ("", c2); ("", newval)]))
      | c1::c2::c3::[] ->
          loc_expr loc
            (Pexp_apply
               ((bigarray_function loc "Array3" set),
                 [("", arr); ("", c1); ("", c2); ("", c3); ("", newval)]))
      | coords ->
          loc_expr loc
            (Pexp_apply
               ((bigarray_function loc "Genarray" "set"),
                 [("", arr);
                 ("", (loc_expr loc (Pexp_array coords)));
                 ("", newval)]))
    let constructor =
      Glr.sequence
        (locate
           (Glr.option None
              (Glr.apply (fun x  -> Some x)
                 (Glr.sequence (locate module_path)
                    (locate (Glr.string "." "."))
                    (fun m  ->
                       let (_loc_m,m) = m in
                       fun _unnamed_1  ->
                         let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                         let _loc = merge [_loc_m; _loc__unnamed_1] in m)))))
        (locate
           (Glr.alternatives'
              [Glr.apply
                 (fun id  ->
                    let (_loc_id,id) = id in let _loc = _loc_id in id)
                 (locate capitalized_ident);
              Glr.apply
                (fun b  -> let (_loc_b,b) = b in let _loc = _loc_b in b)
                (locate bool_lit)]))
        (fun m  ->
           let (_loc_m,m) = m in
           fun id  ->
             let (_loc_id,id) = id in
             let _loc = merge [_loc_m; _loc_id] in
             match m with | None  -> Lident id | Some m -> Ldot (m, id))
    let argument =
      Glr.alternatives'
        [Glr.fsequence (locate label)
           (Glr.sequence (locate (Glr.string ":" ":"))
              (locate (expression_lvl (next_exp App)))
              (fun _unnamed_0  ->
                 let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                 fun e  ->
                   let (_loc_e,e) = e in
                   fun id  ->
                     let (_loc_id,id) = id in
                     let _loc = merge [_loc_id; _loc__unnamed_0; _loc_e] in
                     (id, e)));
        Glr.fsequence (locate opt_label)
          (Glr.sequence (locate (Glr.string ":" ":"))
             (locate (expression_lvl (next_exp App)))
             (fun _unnamed_0  ->
                let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                fun e  ->
                  let (_loc_e,e) = e in
                  fun id  ->
                    let (_loc_id,id) = id in
                    let _loc = merge [_loc_id; _loc__unnamed_0; _loc_e] in
                    (("?" ^ id), e)));
        Glr.apply
          (fun id  ->
             let (_loc_id,id) = id in
             let _loc = _loc_id in
             (id, (loc_expr _loc (Pexp_ident (id_loc (Lident id) _loc)))))
          (locate label);
        Glr.apply
          (fun id  ->
             let (_loc_id,id) = id in
             let _loc = _loc_id in
             (("?" ^ id),
               (loc_expr _loc (Pexp_ident (id_loc (Lident id) _loc)))))
          (locate opt_label);
        Glr.apply
          (fun e  -> let (_loc_e,e) = e in let _loc = _loc_e in ("", e))
          (locate (expression_lvl (next_exp App)))]
    let parameter allow_new_type =
      Glr.alternatives'
        ((Glr.apply
            (fun pat  ->
               let (_loc_pat,pat) = pat in
               let _loc = _loc_pat in `Arg ("", None, pat))
            (locate (pattern_lvl AtomPat))) ::
        (Glr.fsequence (locate (Glr.string "~" "~"))
           (Glr.fsequence (locate (Glr.string "(" "("))
              (Glr.fsequence (locate lowercase_ident)
                 (Glr.sequence
                    (locate
                       (Glr.option None
                          (Glr.apply (fun x  -> Some x)
                             (Glr.sequence (locate (Glr.string ":" ":"))
                                (locate typexpr)
                                (fun _unnamed_0  ->
                                   let (_loc__unnamed_0,_unnamed_0) =
                                     _unnamed_0 in
                                   fun t  ->
                                     let (_loc_t,t) = t in
                                     let _loc =
                                       merge [_loc__unnamed_0; _loc_t] in
                                     t))))) (locate (Glr.string ")" ")"))
                    (fun t  ->
                       let (_loc_t,t) = t in
                       fun _unnamed_1  ->
                         let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                         fun id  ->
                           let (_loc_id,id) = id in
                           fun _unnamed_3  ->
                             let (_loc__unnamed_3,_unnamed_3) = _unnamed_3 in
                             fun _unnamed_4  ->
                               let (_loc__unnamed_4,_unnamed_4) = _unnamed_4 in
                               let _loc =
                                 merge
                                   [_loc__unnamed_4;
                                   _loc__unnamed_3;
                                   _loc_id;
                                   _loc_t;
                                   _loc__unnamed_1] in
                               let pat =
                                 loc_pat _loc_id
                                   (Ppat_var (id_loc id _loc_id)) in
                               let pat =
                                 match t with
                                 | None  -> pat
                                 | Some t ->
                                     loc_pat _loc (Ppat_constraint (pat, t)) in
                               `Arg (id, None, pat)))))) ::
        (Glr.fsequence (locate label)
           (Glr.sequence (locate (Glr.string ":" ":")) (locate pattern)
              (fun _unnamed_0  ->
                 let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                 fun pat  ->
                   let (_loc_pat,pat) = pat in
                   fun id  ->
                     let (_loc_id,id) = id in
                     let _loc = merge [_loc_id; _loc__unnamed_0; _loc_pat] in
                     `Arg (id, None, pat)))) ::
        (Glr.sequence (locate (Glr.char '~' '~')) (locate ident)
           (fun _unnamed_0  ->
              let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
              fun id  ->
                let (_loc_id,id) = id in
                let _loc = merge [_loc__unnamed_0; _loc_id] in
                `Arg
                  (id, None,
                    (loc_pat _loc_id (Ppat_var (id_loc id _loc_id)))))) ::
        (Glr.fsequence (locate (Glr.string "?" "?"))
           (Glr.fsequence (locate (Glr.string "(" "("))
              (Glr.fsequence (locate lowercase_ident)
                 (Glr.fsequence
                    (locate
                       (Glr.option None
                          (Glr.apply (fun x  -> Some x)
                             (Glr.sequence (locate (Glr.string ":" ":"))
                                (locate typexpr)
                                (fun _unnamed_0  ->
                                   let (_loc__unnamed_0,_unnamed_0) =
                                     _unnamed_0 in
                                   fun t  ->
                                     let (_loc_t,t) = t in
                                     let _loc =
                                       merge [_loc__unnamed_0; _loc_t] in
                                     t)))))
                    (Glr.sequence
                       (locate
                          (Glr.option None
                             (Glr.apply (fun x  -> Some x)
                                (Glr.sequence (locate (Glr.string "=" "="))
                                   (locate expression)
                                   (fun _unnamed_0  ->
                                      let (_loc__unnamed_0,_unnamed_0) =
                                        _unnamed_0 in
                                      fun e  ->
                                        let (_loc_e,e) = e in
                                        let _loc =
                                          merge [_loc__unnamed_0; _loc_e] in
                                        e))))) (locate (Glr.string ")" ")"))
                       (fun e  ->
                          let (_loc_e,e) = e in
                          fun _unnamed_1  ->
                            let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                            fun t  ->
                              let (_loc_t,t) = t in
                              fun id  ->
                                let (_loc_id,id) = id in
                                fun _unnamed_4  ->
                                  let (_loc__unnamed_4,_unnamed_4) =
                                    _unnamed_4 in
                                  fun _unnamed_5  ->
                                    let (_loc__unnamed_5,_unnamed_5) =
                                      _unnamed_5 in
                                    let _loc =
                                      merge
                                        [_loc__unnamed_5;
                                        _loc__unnamed_4;
                                        _loc_id;
                                        _loc_t;
                                        _loc_e;
                                        _loc__unnamed_1] in
                                    let pat =
                                      loc_pat _loc_id
                                        (Ppat_var (id_loc id _loc_id)) in
                                    let pat =
                                      match t with
                                      | None  -> pat
                                      | Some t ->
                                          loc_pat (merge2 _loc_id _loc_t)
                                            (Ppat_constraint (pat, t)) in
                                    `Arg (("?" ^ id), e, pat))))))) ::
        (Glr.fsequence (locate opt_label)
           (Glr.fsequence (locate (Glr.string ":" ":"))
              (Glr.fsequence (locate (Glr.string "(" "("))
                 (Glr.fsequence (locate pattern)
                    (Glr.fsequence
                       (locate
                          (Glr.option None
                             (Glr.apply (fun x  -> Some x)
                                (Glr.sequence (locate (Glr.string ":" ":"))
                                   (locate typexpr)
                                   (fun _unnamed_0  ->
                                      let (_loc__unnamed_0,_unnamed_0) =
                                        _unnamed_0 in
                                      fun t  ->
                                        let (_loc_t,t) = t in
                                        let _loc =
                                          merge [_loc__unnamed_0; _loc_t] in
                                        t)))))
                       (Glr.sequence
                          (locate
                             (Glr.option None
                                (Glr.apply (fun x  -> Some x)
                                   (Glr.sequence (locate (Glr.char '=' '='))
                                      (locate expression)
                                      (fun _unnamed_0  ->
                                         let (_loc__unnamed_0,_unnamed_0) =
                                           _unnamed_0 in
                                         fun e  ->
                                           let (_loc_e,e) = e in
                                           let _loc =
                                             merge [_loc__unnamed_0; _loc_e] in
                                           e)))))
                          (locate (Glr.string ")" ")"))
                          (fun e  ->
                             let (_loc_e,e) = e in
                             fun _unnamed_1  ->
                               let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                               fun t  ->
                                 let (_loc_t,t) = t in
                                 fun pat  ->
                                   let (_loc_pat,pat) = pat in
                                   fun _unnamed_4  ->
                                     let (_loc__unnamed_4,_unnamed_4) =
                                       _unnamed_4 in
                                     fun _unnamed_5  ->
                                       let (_loc__unnamed_5,_unnamed_5) =
                                         _unnamed_5 in
                                       fun id  ->
                                         let (_loc_id,id) = id in
                                         let _loc =
                                           merge
                                             [_loc_id;
                                             _loc__unnamed_5;
                                             _loc__unnamed_4;
                                             _loc_pat;
                                             _loc_t;
                                             _loc_e;
                                             _loc__unnamed_1] in
                                         let pat =
                                           match t with
                                           | None  -> pat
                                           | Some t ->
                                               loc_pat
                                                 (merge2 _loc_pat _loc_t)
                                                 (Ppat_constraint (pat, t)) in
                                         `Arg (("?" ^ id), e, pat)))))))) ::
        (Glr.fsequence (locate opt_label)
           (Glr.sequence (locate (Glr.string ":" ":")) (locate pattern)
              (fun _unnamed_0  ->
                 let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                 fun pat  ->
                   let (_loc_pat,pat) = pat in
                   fun id  ->
                     let (_loc_id,id) = id in
                     let _loc = merge [_loc_id; _loc__unnamed_0; _loc_pat] in
                     `Arg (("?" ^ id), None, pat)))) ::
        (Glr.apply
           (fun id  ->
              let (_loc_id,id) = id in
              let _loc = _loc_id in
              `Arg
                (("?" ^ id), None,
                  (loc_pat _loc_id (Ppat_var (id_loc id _loc_id)))))
           (locate opt_label)) ::
        (let y = [] in
         if allow_new_type
         then
           (Glr.fsequence (locate (Glr.char '(' '('))
              (Glr.fsequence (locate type_kw)
                 (Glr.sequence (locate typeconstr_name)
                    (locate (Glr.char ')' ')'))
                    (fun name  ->
                       let (_loc_name,name) = name in
                       fun _unnamed_1  ->
                         let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                         fun _unnamed_2  ->
                           let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                           fun _unnamed_3  ->
                             let (_loc__unnamed_3,_unnamed_3) = _unnamed_3 in
                             let _loc =
                               merge
                                 [_loc__unnamed_3;
                                 _loc__unnamed_2;
                                 _loc_name;
                                 _loc__unnamed_1] in
                             `Type name))))
           :: y
         else y))
    let apply_params params e =
      let f acc =
        function
        | (`Arg (lbl,opt,pat),_loc') ->
            loc_expr (merge2 _loc' e.pexp_loc)
              (pexp_fun (lbl, opt, pat, acc))
        | (`Type name,_loc') ->
            loc_expr (merge2 _loc' e.pexp_loc) (Pexp_newtype (name, acc)) in
      List.fold_left f e (List.rev params)
    let apply_params_cls _loc params e =
      let f acc =
        function
        | `Arg (lbl,opt,pat) -> loc_pcl _loc (Pcl_fun (lbl, opt, pat, acc))
        | `Type name -> assert false in
      List.fold_left f e (List.rev params)
    let right_member =
      Glr.fsequence
        (locate
           (Glr.apply List.rev
              (Glr.fixpoint []
                 (Glr.apply (fun x  l  -> x :: l)
                    (Glr.apply
                       (fun lb  ->
                          let (_loc_lb,lb) = lb in
                          let _loc = _loc_lb in (lb, _loc_lb))
                       (locate (parameter true)))))))
        (Glr.fsequence
           (locate
              (Glr.option None
                 (Glr.apply (fun x  -> Some x)
                    (Glr.sequence (locate (Glr.char ':' ':'))
                       (locate typexpr)
                       (fun _unnamed_0  ->
                          let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                          fun t  ->
                            let (_loc_t,t) = t in
                            let _loc = merge [_loc__unnamed_0; _loc_t] in t)))))
           (Glr.sequence (locate (Glr.char '=' '=')) (locate expression)
              (fun _unnamed_0  ->
                 let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                 fun e  ->
                   let (_loc_e,e) = e in
                   fun ty  ->
                     let (_loc_ty,ty) = ty in
                     fun l  ->
                       let (_loc_l,l) = l in
                       let _loc =
                         merge [_loc_l; _loc_ty; _loc__unnamed_0; _loc_e] in
                       let e =
                         match ty with
                         | None  -> e
                         | Some ty -> loc_expr _loc (pexp_constraint (e, ty)) in
                       apply_params l e)))
    let _ =
      set_grammar let_binding
        (Glr.alternatives'
           [Glr.fsequence (locate (pattern_lvl AsPat))
              (Glr.fsequence (locate right_member)
                 (Glr.sequence (locate post_item_attributes)
                    (locate
                       (Glr.option []
                          (Glr.sequence (locate and_kw) (locate let_binding)
                             (fun _unnamed_0  ->
                                let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                                fun l  ->
                                  let (_loc_l,l) = l in
                                  let _loc = merge [_loc__unnamed_0; _loc_l] in
                                  l))))
                    (fun a  ->
                       let (_loc_a,a) = a in
                       fun l  ->
                         let (_loc_l,l) = l in
                         fun e  ->
                           let (_loc_e,e) = e in
                           fun pat  ->
                             let (_loc_pat,pat) = pat in
                             let _loc =
                               merge [_loc_pat; _loc_e; _loc_a; _loc_l] in
                             (value_binding ~attributes:a
                                (merge2 _loc_pat _loc_e) pat e)
                               :: l)));
           Glr.fsequence (locate lowercase_ident)
             (Glr.fsequence (locate (Glr.char ':' ':'))
                (Glr.fsequence (locate poly_typexpr)
                   (Glr.fsequence (locate right_member)
                      (Glr.sequence (locate post_item_attributes)
                         (locate
                            (Glr.option []
                               (Glr.sequence (locate and_kw)
                                  (locate let_binding)
                                  (fun _unnamed_0  ->
                                     let (_loc__unnamed_0,_unnamed_0) =
                                       _unnamed_0 in
                                     fun l  ->
                                       let (_loc_l,l) = l in
                                       let _loc =
                                         merge [_loc__unnamed_0; _loc_l] in
                                       l))))
                         (fun a  ->
                            let (_loc_a,a) = a in
                            fun l  ->
                              let (_loc_l,l) = l in
                              fun e  ->
                                let (_loc_e,e) = e in
                                fun ty  ->
                                  let (_loc_ty,ty) = ty in
                                  fun _unnamed_4  ->
                                    let (_loc__unnamed_4,_unnamed_4) =
                                      _unnamed_4 in
                                    fun vn  ->
                                      let (_loc_vn,vn) = vn in
                                      let _loc =
                                        merge
                                          [_loc_vn;
                                          _loc__unnamed_4;
                                          _loc_ty;
                                          _loc_e;
                                          _loc_a;
                                          _loc_l] in
                                      let pat =
                                        loc_pat _loc
                                          (Ppat_constraint
                                             ((loc_pat _loc
                                                 (Ppat_var
                                                    (id_loc vn _loc_vn))),
                                               ty)) in
                                      (value_binding ~attributes:a
                                         (merge2 _loc_vn _loc_e) pat e)
                                        :: l)))))])
    let match_cases =
      memoize1
        (fun lvl  ->
           Glr.apply (fun l  -> let (_loc_l,l) = l in let _loc = _loc_l in l)
             (locate
                (Glr.option []
                   (Glr.fsequence
                      (locate
                         (Glr.option None
                            (Glr.apply (fun x  -> Some x)
                               (Glr.string "|" "|"))))
                      (Glr.fsequence (locate pattern)
                         (Glr.fsequence
                            (locate
                               (Glr.option None
                                  (Glr.apply (fun x  -> Some x)
                                     (Glr.sequence (locate when_kw)
                                        (locate expression)
                                        (fun _unnamed_0  ->
                                           let (_loc__unnamed_0,_unnamed_0) =
                                             _unnamed_0 in
                                           fun e  ->
                                             let (_loc_e,e) = e in
                                             let _loc =
                                               merge
                                                 [_loc__unnamed_0; _loc_e] in
                                             e)))))
                            (Glr.fsequence (locate (Glr.string "->" "->"))
                               (Glr.sequence (locate (expression_lvl lvl))
                                  (locate
                                     (Glr.apply List.rev
                                        (Glr.fixpoint []
                                           (Glr.apply (fun x  l  -> x :: l)
                                              (Glr.fsequence
                                                 (locate (Glr.string "|" "|"))
                                                 (Glr.fsequence
                                                    (locate pattern)
                                                    (Glr.fsequence
                                                       (locate
                                                          (Glr.option None
                                                             (Glr.apply
                                                                (fun x  ->
                                                                   Some x)
                                                                (Glr.sequence
                                                                   (locate
                                                                    when_kw)
                                                                   (locate
                                                                    expression)
                                                                   (fun
                                                                    _unnamed_0
                                                                     ->
                                                                    let 
                                                                    (_loc__unnamed_0,_unnamed_0)
                                                                    =
                                                                    _unnamed_0 in
                                                                    fun e  ->
                                                                    let 
                                                                    (_loc_e,e)
                                                                    = e in
                                                                    let _loc
                                                                    =
                                                                    merge
                                                                    [_loc__unnamed_0;
                                                                    _loc_e] in
                                                                    e)))))
                                                       (Glr.sequence
                                                          (locate
                                                             (Glr.string "->"
                                                                "->"))
                                                          (locate
                                                             (expression_lvl
                                                                lvl))
                                                          (fun _unnamed_0  ->
                                                             let (_loc__unnamed_0,_unnamed_0)
                                                               = _unnamed_0 in
                                                             fun e  ->
                                                               let (_loc_e,e)
                                                                 = e in
                                                               fun w  ->
                                                                 let 
                                                                   (_loc_w,w)
                                                                   = w in
                                                                 fun pat  ->
                                                                   let 
                                                                    (_loc_pat,pat)
                                                                    = pat in
                                                                   fun
                                                                    _unnamed_4
                                                                     ->
                                                                    let 
                                                                    (_loc__unnamed_4,_unnamed_4)
                                                                    =
                                                                    _unnamed_4 in
                                                                    let _loc
                                                                    =
                                                                    merge
                                                                    [_loc__unnamed_4;
                                                                    _loc_pat;
                                                                    _loc_w;
                                                                    _loc__unnamed_0;
                                                                    _loc_e] in
                                                                    (pat, e,
                                                                    w))))))))))
                                  (fun e  ->
                                     let (_loc_e,e) = e in
                                     fun l  ->
                                       let (_loc_l,l) = l in
                                       fun _unnamed_2  ->
                                         let (_loc__unnamed_2,_unnamed_2) =
                                           _unnamed_2 in
                                         fun w  ->
                                           let (_loc_w,w) = w in
                                           fun pat  ->
                                             let (_loc_pat,pat) = pat in
                                             fun _unnamed_5  ->
                                               let (_loc__unnamed_5,_unnamed_5)
                                                 = _unnamed_5 in
                                               let _loc =
                                                 merge
                                                   [_loc__unnamed_5;
                                                   _loc_pat;
                                                   _loc_w;
                                                   _loc__unnamed_2;
                                                   _loc_e;
                                                   _loc_l] in
                                               map_cases ((pat, e, w) :: l))))))))))
    let type_coercion =
      Glr.alternatives'
        [Glr.fsequence (locate (Glr.string ":" ":"))
           (Glr.sequence (locate typexpr)
              (locate
                 (Glr.option None
                    (Glr.apply (fun x  -> Some x)
                       (Glr.sequence (locate (Glr.string ":>" ":>"))
                          (locate typexpr)
                          (fun _unnamed_0  ->
                             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                             fun t'  ->
                               let (_loc_t',t') = t' in
                               let _loc = merge [_loc__unnamed_0; _loc_t'] in
                               t')))))
              (fun t  ->
                 let (_loc_t,t) = t in
                 fun t'  ->
                   let (_loc_t',t') = t' in
                   fun _unnamed_2  ->
                     let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                     let _loc = merge [_loc__unnamed_2; _loc_t; _loc_t'] in
                     ((Some t), t')));
        Glr.sequence (locate (Glr.string ":>" ":>")) (locate typexpr)
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun t'  ->
               let (_loc_t',t') = t' in
               let _loc = merge [_loc__unnamed_0; _loc_t'] in
               (None, (Some t')))]
    let expression_list =
      Glr.alternatives'
        [Glr.fsequence (locate (expression_lvl (next_exp Seq)))
           (Glr.sequence
              (locate
                 (Glr.apply List.rev
                    (Glr.fixpoint []
                       (Glr.apply (fun x  l  -> x :: l)
                          (Glr.sequence (locate (Glr.string ";" ";"))
                             (locate (expression_lvl (next_exp Seq)))
                             (fun _unnamed_0  ->
                                let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                                fun e  ->
                                  let (_loc_e,e) = e in
                                  let _loc = merge [_loc__unnamed_0; _loc_e] in
                                  (e, _loc_e)))))))
              (locate
                 (Glr.option None
                    (Glr.apply (fun x  -> Some x) (Glr.string ";" ";"))))
              (fun l  ->
                 let (_loc_l,l) = l in
                 fun _unnamed_1  ->
                   let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                   fun e  ->
                     let (_loc_e,e) = e in
                     let _loc = merge [_loc_e; _loc_l; _loc__unnamed_1] in
                     (e, _loc_e) :: l));
        Glr.apply
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             let _loc = _loc__unnamed_0 in []) (locate (Glr.empty ()))]
    let record_item =
      Glr.alternatives'
        [Glr.fsequence (locate field)
           (Glr.sequence (locate (Glr.char '=' '='))
              (locate (expression_lvl (next_exp Seq)))
              (fun _unnamed_0  ->
                 let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                 fun e  ->
                   let (_loc_e,e) = e in
                   fun f  ->
                     let (_loc_f,f) = f in
                     let _loc = merge [_loc_f; _loc__unnamed_0; _loc_e] in
                     ((id_loc f _loc_f), e)));
        Glr.apply
          (fun f  ->
             let (_loc_f,f) = f in
             let _loc = _loc_f in
             let id = id_loc (Lident f) _loc_f in
             (id, (loc_expr _loc_f (Pexp_ident id))))
          (locate lowercase_ident)]
    let record_list =
      Glr.alternatives'
        [Glr.fsequence (locate record_item)
           (Glr.sequence
              (locate
                 (Glr.apply List.rev
                    (Glr.fixpoint []
                       (Glr.apply (fun x  l  -> x :: l)
                          (Glr.sequence (locate (Glr.string ";" ";"))
                             (locate record_item)
                             (fun _unnamed_0  ->
                                let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                                fun it  ->
                                  let (_loc_it,it) = it in
                                  let _loc = merge [_loc__unnamed_0; _loc_it] in
                                  it))))))
              (locate
                 (Glr.option None
                    (Glr.apply (fun x  -> Some x) (Glr.string ";" ";"))))
              (fun l  ->
                 let (_loc_l,l) = l in
                 fun _unnamed_1  ->
                   let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                   fun it  ->
                     let (_loc_it,it) = it in
                     let _loc = merge [_loc_it; _loc_l; _loc__unnamed_1] in
                     it :: l));
        Glr.apply
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             let _loc = _loc__unnamed_0 in []) (locate (Glr.empty ()))]
    let obj_item =
      Glr.fsequence (locate inst_var_name)
        (Glr.sequence (locate (Glr.char '=' '='))
           (locate (expression_lvl (next_exp Seq)))
           (fun _unnamed_0  ->
              let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
              fun e  ->
                let (_loc_e,e) = e in
                fun v  ->
                  let (_loc_v,v) = v in
                  let _loc = merge [_loc_v; _loc__unnamed_0; _loc_e] in
                  ((id_loc v _loc_v), e)))
    let class_expr_base =
      Glr.alternatives'
        [Glr.apply
           (fun cp  ->
              let (_loc_cp,cp) = cp in
              let _loc = _loc_cp in
              let cp = id_loc cp _loc_cp in
              loc_pcl _loc (Pcl_constr (cp, []))) (locate class_path);
        Glr.fsequence (locate (Glr.char '[' '['))
          (Glr.fsequence (locate typexpr)
             (Glr.fsequence
                (locate
                   (Glr.apply List.rev
                      (Glr.fixpoint []
                         (Glr.apply (fun x  l  -> x :: l)
                            (Glr.sequence (locate (Glr.string "," ","))
                               (locate typexpr)
                               (fun _unnamed_0  ->
                                  let (_loc__unnamed_0,_unnamed_0) =
                                    _unnamed_0 in
                                  fun te  ->
                                    let (_loc_te,te) = te in
                                    let _loc =
                                      merge [_loc__unnamed_0; _loc_te] in
                                    te))))))
                (Glr.sequence (locate (Glr.char ']' ']')) (locate class_path)
                   (fun _unnamed_0  ->
                      let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                      fun cp  ->
                        let (_loc_cp,cp) = cp in
                        fun tes  ->
                          let (_loc_tes,tes) = tes in
                          fun te  ->
                            let (_loc_te,te) = te in
                            fun _unnamed_4  ->
                              let (_loc__unnamed_4,_unnamed_4) = _unnamed_4 in
                              let _loc =
                                merge
                                  [_loc__unnamed_4;
                                  _loc_te;
                                  _loc_tes;
                                  _loc__unnamed_0;
                                  _loc_cp] in
                              let cp = id_loc cp _loc_cp in
                              loc_pcl _loc (Pcl_constr (cp, (te :: tes)))))));
        Glr.fsequence (locate (Glr.string "(" "("))
          (Glr.sequence (locate class_expr) (locate (Glr.string ")" ")"))
             (fun ce  ->
                let (_loc_ce,ce) = ce in
                fun _unnamed_1  ->
                  let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                  fun _unnamed_2  ->
                    let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                    let _loc =
                      merge [_loc__unnamed_2; _loc_ce; _loc__unnamed_1] in
                    loc_pcl _loc ce.pcl_desc));
        Glr.fsequence (locate (Glr.string "(" "("))
          (Glr.fsequence (locate class_expr)
             (Glr.fsequence (locate (Glr.string ":" ":"))
                (Glr.sequence (locate class_type)
                   (locate (Glr.string ")" ")"))
                   (fun ct  ->
                      let (_loc_ct,ct) = ct in
                      fun _unnamed_1  ->
                        let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                        fun _unnamed_2  ->
                          let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                          fun ce  ->
                            let (_loc_ce,ce) = ce in
                            fun _unnamed_4  ->
                              let (_loc__unnamed_4,_unnamed_4) = _unnamed_4 in
                              let _loc =
                                merge
                                  [_loc__unnamed_4;
                                  _loc_ce;
                                  _loc__unnamed_2;
                                  _loc_ct;
                                  _loc__unnamed_1] in
                              loc_pcl _loc (Pcl_constraint (ce, ct))))));
        Glr.fsequence (locate fun_kw)
          (Glr.fsequence
             (locate
                (Glr.sequence (parameter false)
                   (Glr.fixpoint []
                      (Glr.apply (fun x  l  -> x :: l) (parameter false)))
                   (fun x  l  -> x :: (List.rev l))))
             (Glr.sequence (locate (Glr.string "->" "->"))
                (locate class_expr)
                (fun _unnamed_0  ->
                   let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                   fun ce  ->
                     let (_loc_ce,ce) = ce in
                     fun ps  ->
                       let (_loc_ps,ps) = ps in
                       fun _unnamed_3  ->
                         let (_loc__unnamed_3,_unnamed_3) = _unnamed_3 in
                         let _loc =
                           merge
                             [_loc__unnamed_3;
                             _loc_ps;
                             _loc__unnamed_0;
                             _loc_ce] in
                         apply_params_cls _loc ps ce)));
        Glr.fsequence (locate let_kw)
          (Glr.fsequence (locate rec_flag)
             (Glr.fsequence (locate let_binding)
                (Glr.sequence (locate in_kw) (locate class_expr)
                   (fun _unnamed_0  ->
                      let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                      fun ce  ->
                        let (_loc_ce,ce) = ce in
                        fun lbs  ->
                          let (_loc_lbs,lbs) = lbs in
                          fun r  ->
                            let (_loc_r,r) = r in
                            fun _unnamed_4  ->
                              let (_loc__unnamed_4,_unnamed_4) = _unnamed_4 in
                              let _loc =
                                merge
                                  [_loc__unnamed_4;
                                  _loc_r;
                                  _loc_lbs;
                                  _loc__unnamed_0;
                                  _loc_ce] in
                              loc_pcl _loc (Pcl_let (r, lbs, ce))))));
        Glr.fsequence (locate object_kw)
          (Glr.sequence (locate class_body) (locate end_kw)
             (fun cb  ->
                let (_loc_cb,cb) = cb in
                fun _unnamed_1  ->
                  let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                  fun _unnamed_2  ->
                    let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                    let _loc =
                      merge [_loc__unnamed_2; _loc_cb; _loc__unnamed_1] in
                    loc_pcl _loc (Pcl_structure cb)))]
    let _ =
      set_grammar class_expr
        (Glr.sequence (locate class_expr_base)
           (locate
              (Glr.option None
                 (Glr.apply (fun x  -> Some x)
                    (Glr.apply
                       (fun arg  ->
                          let (_loc_arg,arg) = arg in
                          let _loc = _loc_arg in arg)
                       (locate
                          (Glr.sequence argument
                             (Glr.fixpoint []
                                (Glr.apply (fun x  l  -> x :: l) argument))
                             (fun x  l  -> x :: (List.rev l))))))))
           (fun ce  ->
              let (_loc_ce,ce) = ce in
              fun args  ->
                let (_loc_args,args) = args in
                let _loc = merge [_loc_ce; _loc_args] in
                match args with
                | None  -> ce
                | Some l -> loc_pcl _loc (Pcl_apply (ce, l))))
    let class_field =
      Glr.alternatives'
        [Glr.fsequence (locate inherit_kw)
           (Glr.fsequence (locate override_flag)
              (Glr.sequence (locate class_expr)
                 (locate
                    (Glr.option None
                       (Glr.apply (fun x  -> Some x)
                          (Glr.sequence (locate as_kw)
                             (locate lowercase_ident)
                             (fun _unnamed_0  ->
                                let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                                fun id  ->
                                  let (_loc_id,id) = id in
                                  let _loc = merge [_loc__unnamed_0; _loc_id] in
                                  id)))))
                 (fun ce  ->
                    let (_loc_ce,ce) = ce in
                    fun id  ->
                      let (_loc_id,id) = id in
                      fun o  ->
                        let (_loc_o,o) = o in
                        fun _unnamed_3  ->
                          let (_loc__unnamed_3,_unnamed_3) = _unnamed_3 in
                          let _loc =
                            merge [_loc__unnamed_3; _loc_o; _loc_ce; _loc_id] in
                          loc_pcf _loc (Pcf_inher (o, ce, id)))));
        Glr.fsequence (locate val_kw)
          (Glr.fsequence (locate override_flag)
             (Glr.fsequence (locate mutable_flag)
                (Glr.fsequence (locate inst_var_name)
                   (Glr.fsequence
                      (locate
                         (Glr.option None
                            (Glr.apply (fun x  -> Some x)
                               (Glr.sequence (locate (Glr.char ':' ':'))
                                  (locate typexpr)
                                  (fun _unnamed_0  ->
                                     let (_loc__unnamed_0,_unnamed_0) =
                                       _unnamed_0 in
                                     fun t  ->
                                       let (_loc_t,t) = t in
                                       let _loc =
                                         merge [_loc__unnamed_0; _loc_t] in
                                       t)))))
                      (Glr.sequence (locate (Glr.char '=' '=')) (locate expr)
                         (fun _unnamed_0  ->
                            let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                            fun e  ->
                              let (_loc_e,e) = e in
                              fun te  ->
                                let (_loc_te,te) = te in
                                fun ivn  ->
                                  let (_loc_ivn,ivn) = ivn in
                                  fun m  ->
                                    let (_loc_m,m) = m in
                                    fun o  ->
                                      let (_loc_o,o) = o in
                                      fun _unnamed_6  ->
                                        let (_loc__unnamed_6,_unnamed_6) =
                                          _unnamed_6 in
                                        let _loc =
                                          merge
                                            [_loc__unnamed_6;
                                            _loc_o;
                                            _loc_m;
                                            _loc_ivn;
                                            _loc_te;
                                            _loc__unnamed_0;
                                            _loc_e] in
                                        let ivn = id_loc ivn _loc_ivn in
                                        let ex =
                                          match te with
                                          | None  -> e
                                          | Some t ->
                                              loc_expr _loc_te
                                                (pexp_constraint (e, t)) in
                                        Pcf_val (ivn, m, o, ex, _loc)))))));
        Glr.fsequence (locate val_kw)
          (Glr.fsequence (locate mutable_flag)
             (Glr.fsequence (locate virtual_kw)
                (Glr.fsequence (locate inst_var_name)
                   (Glr.sequence (locate (Glr.string ":" ":"))
                      (locate typexpr)
                      (fun _unnamed_0  ->
                         let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                         fun te  ->
                           let (_loc_te,te) = te in
                           fun ivn  ->
                             let (_loc_ivn,ivn) = ivn in
                             fun _unnamed_3  ->
                               let (_loc__unnamed_3,_unnamed_3) = _unnamed_3 in
                               fun m  ->
                                 let (_loc_m,m) = m in
                                 fun _unnamed_5  ->
                                   let (_loc__unnamed_5,_unnamed_5) =
                                     _unnamed_5 in
                                   let _loc =
                                     merge
                                       [_loc__unnamed_5;
                                       _loc_m;
                                       _loc__unnamed_3;
                                       _loc_ivn;
                                       _loc__unnamed_0;
                                       _loc_te] in
                                   let ivn = id_loc ivn _loc_ivn in
                                   Pcf_valvirt (ivn, m, te, _loc))))));
        Glr.fsequence (locate val_kw)
          (Glr.fsequence (locate virtual_kw)
             (Glr.fsequence (locate mutable_kw)
                (Glr.fsequence (locate inst_var_name)
                   (Glr.sequence (locate (Glr.string ":" ":"))
                      (locate typexpr)
                      (fun _unnamed_0  ->
                         let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                         fun te  ->
                           let (_loc_te,te) = te in
                           fun ivn  ->
                             let (_loc_ivn,ivn) = ivn in
                             fun _unnamed_3  ->
                               let (_loc__unnamed_3,_unnamed_3) = _unnamed_3 in
                               fun _unnamed_4  ->
                                 let (_loc__unnamed_4,_unnamed_4) =
                                   _unnamed_4 in
                                 fun _unnamed_5  ->
                                   let (_loc__unnamed_5,_unnamed_5) =
                                     _unnamed_5 in
                                   let _loc =
                                     merge
                                       [_loc__unnamed_5;
                                       _loc__unnamed_4;
                                       _loc__unnamed_3;
                                       _loc_ivn;
                                       _loc__unnamed_0;
                                       _loc_te] in
                                   let ivn = id_loc ivn _loc_ivn in
                                   Pcf_valvirt (ivn, Mutable, te, _loc))))));
        Glr.fsequence (locate method_kw)
          (Glr.fsequence (locate override_flag)
             (Glr.fsequence (locate private_flag)
                (Glr.fsequence (locate method_name)
                   (Glr.fsequence (locate (Glr.string ":" ":"))
                      (Glr.fsequence (locate poly_typexpr)
                         (Glr.sequence (locate (Glr.char '=' '='))
                            (locate expr)
                            (fun _unnamed_0  ->
                               let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                               fun e  ->
                                 let (_loc_e,e) = e in
                                 fun te  ->
                                   let (_loc_te,te) = te in
                                   fun _unnamed_3  ->
                                     let (_loc__unnamed_3,_unnamed_3) =
                                       _unnamed_3 in
                                     fun mn  ->
                                       let (_loc_mn,mn) = mn in
                                       fun p  ->
                                         let (_loc_p,p) = p in
                                         fun o  ->
                                           let (_loc_o,o) = o in
                                           fun _unnamed_7  ->
                                             let (_loc__unnamed_7,_unnamed_7)
                                               = _unnamed_7 in
                                             let _loc =
                                               merge
                                                 [_loc__unnamed_7;
                                                 _loc_o;
                                                 _loc_p;
                                                 _loc_mn;
                                                 _loc__unnamed_3;
                                                 _loc_te;
                                                 _loc__unnamed_0;
                                                 _loc_e] in
                                             let mn = id_loc mn _loc_mn in
                                             let e =
                                               loc_expr _loc
                                                 (Pexp_poly (e, (Some te))) in
                                             Pcf_meth (mn, p, o, e, _loc))))))));
        Glr.fsequence (locate method_kw)
          (Glr.fsequence (locate override_flag)
             (Glr.fsequence (locate private_flag)
                (Glr.fsequence (locate method_name)
                   (Glr.fsequence
                      (locate
                         (Glr.apply List.rev
                            (Glr.fixpoint []
                               (Glr.apply (fun x  l  -> x :: l)
                                  (Glr.apply
                                     (fun p  ->
                                        let (_loc_p,p) = p in
                                        let _loc = _loc_p in (p, _loc_p))
                                     (locate (parameter true)))))))
                      (Glr.fsequence
                         (locate
                            (Glr.option None
                               (Glr.apply (fun x  -> Some x)
                                  (Glr.sequence (locate (Glr.string ":" ":"))
                                     (locate typexpr)
                                     (fun _unnamed_0  ->
                                        let (_loc__unnamed_0,_unnamed_0) =
                                          _unnamed_0 in
                                        fun te  ->
                                          let (_loc_te,te) = te in
                                          let _loc =
                                            merge [_loc__unnamed_0; _loc_te] in
                                          te)))))
                         (Glr.sequence (locate (Glr.char '=' '='))
                            (locate expr)
                            (fun _unnamed_0  ->
                               let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                               fun e  ->
                                 let (_loc_e,e) = e in
                                 fun te  ->
                                   let (_loc_te,te) = te in
                                   fun ps  ->
                                     let (_loc_ps,ps) = ps in
                                     fun mn  ->
                                       let (_loc_mn,mn) = mn in
                                       fun p  ->
                                         let (_loc_p,p) = p in
                                         fun o  ->
                                           let (_loc_o,o) = o in
                                           fun _unnamed_7  ->
                                             let (_loc__unnamed_7,_unnamed_7)
                                               = _unnamed_7 in
                                             let _loc =
                                               merge
                                                 [_loc__unnamed_7;
                                                 _loc_o;
                                                 _loc_p;
                                                 _loc_mn;
                                                 _loc_ps;
                                                 _loc_te;
                                                 _loc__unnamed_0;
                                                 _loc_e] in
                                             let mn = id_loc mn _loc_mn in
                                             let e =
                                               match te with
                                               | None  -> e
                                               | Some te ->
                                                   loc_expr _loc
                                                     (pexp_constraint (e, te)) in
                                             let e: expression =
                                               apply_params ps e in
                                             let e =
                                               loc_expr _loc
                                                 (Pexp_poly (e, None)) in
                                             Pcf_meth (mn, p, o, e, _loc))))))));
        Glr.fsequence (locate method_kw)
          (Glr.fsequence (locate private_flag)
             (Glr.fsequence (locate virtual_kw)
                (Glr.fsequence (locate method_name)
                   (Glr.sequence (locate (Glr.string ":" ":"))
                      (locate poly_typexpr)
                      (fun _unnamed_0  ->
                         let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                         fun pte  ->
                           let (_loc_pte,pte) = pte in
                           fun mn  ->
                             let (_loc_mn,mn) = mn in
                             fun _unnamed_3  ->
                               let (_loc__unnamed_3,_unnamed_3) = _unnamed_3 in
                               fun p  ->
                                 let (_loc_p,p) = p in
                                 fun _unnamed_5  ->
                                   let (_loc__unnamed_5,_unnamed_5) =
                                     _unnamed_5 in
                                   let _loc =
                                     merge
                                       [_loc__unnamed_5;
                                       _loc_p;
                                       _loc__unnamed_3;
                                       _loc_mn;
                                       _loc__unnamed_0;
                                       _loc_pte] in
                                   let mn = id_loc mn _loc_mn in
                                   Pcf_virt (mn, p, pte, _loc))))));
        Glr.fsequence (locate method_kw)
          (Glr.fsequence (locate virtual_kw)
             (Glr.fsequence (locate private_kw)
                (Glr.fsequence (locate method_name)
                   (Glr.sequence (locate (Glr.string ":" ":"))
                      (locate poly_typexpr)
                      (fun _unnamed_0  ->
                         let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                         fun pte  ->
                           let (_loc_pte,pte) = pte in
                           fun mn  ->
                             let (_loc_mn,mn) = mn in
                             fun _unnamed_3  ->
                               let (_loc__unnamed_3,_unnamed_3) = _unnamed_3 in
                               fun _unnamed_4  ->
                                 let (_loc__unnamed_4,_unnamed_4) =
                                   _unnamed_4 in
                                 fun _unnamed_5  ->
                                   let (_loc__unnamed_5,_unnamed_5) =
                                     _unnamed_5 in
                                   let _loc =
                                     merge
                                       [_loc__unnamed_5;
                                       _loc__unnamed_4;
                                       _loc__unnamed_3;
                                       _loc_mn;
                                       _loc__unnamed_0;
                                       _loc_pte] in
                                   let mn = id_loc mn _loc_mn in
                                   Pcf_virt (mn, Private, pte, _loc))))));
        Glr.fsequence (locate constraint_kw)
          (Glr.fsequence (locate typexpr)
             (Glr.sequence (locate (Glr.char '=' '=')) (locate typexpr)
                (fun _unnamed_0  ->
                   let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                   fun te'  ->
                     let (_loc_te',te') = te' in
                     fun te  ->
                       let (_loc_te,te) = te in
                       fun _unnamed_3  ->
                         let (_loc__unnamed_3,_unnamed_3) = _unnamed_3 in
                         let _loc =
                           merge
                             [_loc__unnamed_3;
                             _loc_te;
                             _loc__unnamed_0;
                             _loc_te'] in
                         Pcf_cstr (te, te', _loc))));
        Glr.sequence (locate initializer_kw) (locate expr)
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun e  ->
               let (_loc_e,e) = e in
               let _loc = merge [_loc__unnamed_0; _loc_e] in
               loc_pcf _loc (Pcf_init e))]
    let _ =
      set_grammar class_body
        (Glr.sequence
           (locate (Glr.option None (Glr.apply (fun x  -> Some x) pattern)))
           (locate
              (Glr.apply List.rev
                 (Glr.fixpoint []
                    (Glr.apply (fun x  l  -> x :: l) class_field))))
           (fun p  ->
              let (_loc_p,p) = p in
              fun f  ->
                let (_loc_f,f) = f in
                let _loc = merge [_loc_p; _loc_f] in
                let p =
                  match p with
                  | None  -> loc_pat _loc_p Ppat_any
                  | Some p -> p in
                (p, f)))
    let class_binding =
      Glr.fsequence (locate virtual_flag)
        (Glr.fsequence
           (locate
              (Glr.option []
                 (Glr.fsequence (locate (Glr.string "[" "["))
                    (Glr.sequence (locate type_parameters)
                       (locate (Glr.string "]" "]"))
                       (fun params  ->
                          let (_loc_params,params) = params in
                          fun _unnamed_1  ->
                            let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                            fun _unnamed_2  ->
                              let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                              let _loc =
                                merge
                                  [_loc__unnamed_2;
                                  _loc_params;
                                  _loc__unnamed_1] in
                              params)))))
           (Glr.fsequence (locate class_name)
              (Glr.fsequence
                 (locate
                    (Glr.apply List.rev
                       (Glr.fixpoint []
                          (Glr.apply (fun x  l  -> x :: l) (parameter false)))))
                 (Glr.fsequence
                    (locate
                       (Glr.option None
                          (Glr.apply (fun x  -> Some x)
                             (Glr.sequence (locate (Glr.string ":" ":"))
                                (locate class_type)
                                (fun _unnamed_0  ->
                                   let (_loc__unnamed_0,_unnamed_0) =
                                     _unnamed_0 in
                                   fun ct  ->
                                     let (_loc_ct,ct) = ct in
                                     let _loc =
                                       merge [_loc__unnamed_0; _loc_ct] in
                                     ct)))))
                    (Glr.sequence (locate (Glr.char '=' '='))
                       (locate class_expr)
                       (fun _unnamed_0  ->
                          let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                          fun ce  ->
                            let (_loc_ce,ce) = ce in
                            fun ct  ->
                              let (_loc_ct,ct) = ct in
                              fun ps  ->
                                let (_loc_ps,ps) = ps in
                                fun cn  ->
                                  let (_loc_cn,cn) = cn in
                                  fun params  ->
                                    let (_loc_params,params) = params in
                                    fun v  ->
                                      let (_loc_v,v) = v in
                                      let _loc =
                                        merge
                                          [_loc_v;
                                          _loc_params;
                                          _loc_cn;
                                          _loc_ps;
                                          _loc_ct;
                                          _loc__unnamed_0;
                                          _loc_ce] in
                                      let ce = apply_params_cls _loc ps ce in
                                      let ce =
                                        match ct with
                                        | None  -> ce
                                        | Some ct ->
                                            loc_pcl _loc
                                              (Pcl_constraint (ce, ct)) in
                                      class_type_declaration _loc_params _loc
                                        (id_loc cn _loc_cn) params v ce))))))
    let class_definition =
      Glr.sequence (locate class_binding)
        (locate
           (Glr.apply List.rev
              (Glr.fixpoint []
                 (Glr.apply (fun x  l  -> x :: l)
                    (Glr.sequence (locate and_kw) (locate class_binding)
                       (fun _unnamed_0  ->
                          let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                          fun cb  ->
                            let (_loc_cb,cb) = cb in
                            let _loc = merge [_loc__unnamed_0; _loc_cb] in cb))))))
        (fun cb  ->
           let (_loc_cb,cb) = cb in
           fun cbs  ->
             let (_loc_cbs,cbs) = cbs in
             let _loc = merge [_loc_cb; _loc_cbs] in cb :: cbs)
    let module_expr = declare_grammar "module_expr"
    let module_type = declare_grammar "module_type"
    let pexp_list _loc ?_loc_cl  l =
      if l = []
      then loc_expr _loc (pexp_construct ((id_loc (Lident "[]") _loc), None))
      else
        (let _loc_cl = match _loc_cl with | None  -> _loc | Some pos -> pos in
         List.fold_right
           (fun (x,pos)  acc  ->
              let _loc = merge2 pos _loc_cl in
              loc_expr _loc
                (pexp_construct
                   ((id_loc (Lident "::") _loc),
                     (Some (loc_expr _loc (Pexp_tuple [x; acc])))))) l
           (loc_expr _loc_cl
              (pexp_construct ((id_loc (Lident "[]") _loc_cl), None))))
    let expression_base =
      memoize1
        (fun lvl  ->
           Glr.alternatives'
             ((Glr.apply
                 (fun e  -> let (_loc_e,e) = e in let _loc = _loc_e in e)
                 (locate (alternatives extra_expressions))) ::
             (let y =
                (Glr.apply
                   (fun id  ->
                      let (_loc_id,id) = id in
                      let _loc = _loc_id in
                      (Atom,
                        (loc_expr _loc (Pexp_ident (id_loc id _loc_id)))))
                   (locate value_path))
                ::
                (Glr.apply
                   (fun c  ->
                      let (_loc_c,c) = c in
                      let _loc = _loc_c in
                      (Atom, (loc_expr _loc (Pexp_constant c))))
                   (locate constant))
                ::
                (Glr.fsequence (locate module_path)
                   (Glr.fsequence (locate (Glr.string "." "."))
                      (Glr.fsequence (locate (Glr.string "(" "("))
                         (Glr.sequence (locate expression)
                            (locate (Glr.string ")" ")"))
                            (fun e  ->
                               let (_loc_e,e) = e in
                               fun _unnamed_1  ->
                                 let (_loc__unnamed_1,_unnamed_1) =
                                   _unnamed_1 in
                                 fun _unnamed_2  ->
                                   let (_loc__unnamed_2,_unnamed_2) =
                                     _unnamed_2 in
                                   fun _unnamed_3  ->
                                     let (_loc__unnamed_3,_unnamed_3) =
                                       _unnamed_3 in
                                     fun mp  ->
                                       let (_loc_mp,mp) = mp in
                                       let _loc =
                                         merge
                                           [_loc_mp;
                                           _loc__unnamed_3;
                                           _loc__unnamed_2;
                                           _loc_e;
                                           _loc__unnamed_1] in
                                       let mp = id_loc mp _loc_mp in
                                       (Atom,
                                         (loc_expr _loc (Pexp_open (mp, e)))))))))
                ::
                (Glr.sequence (locate let_kw)
                   (locate
                      (Glr.alternatives'
                         (let y =
                            let y =
                              let y = [] in
                              if lvl < App
                              then
                                (Glr.fsequence (locate open_kw)
                                   (Glr.fsequence (locate override_flag)
                                      (Glr.fsequence (locate module_path)
                                         (Glr.sequence (locate in_kw)
                                            (locate
                                               (expression_lvl (let_prio lvl)))
                                            (fun _unnamed_0  ->
                                               let (_loc__unnamed_0,_unnamed_0)
                                                 = _unnamed_0 in
                                               fun e  ->
                                                 let (_loc_e,e) = e in
                                                 fun mp  ->
                                                   let (_loc_mp,mp) = mp in
                                                   fun o  ->
                                                     let (_loc_o,o) = o in
                                                     fun _unnamed_4  ->
                                                       let (_loc__unnamed_4,_unnamed_4)
                                                         = _unnamed_4 in
                                                       let _loc =
                                                         merge
                                                           [_loc__unnamed_4;
                                                           _loc_o;
                                                           _loc_mp;
                                                           _loc__unnamed_0;
                                                           _loc_e] in
                                                       let mp =
                                                         id_loc mp _loc_mp in
                                                       fun _loc  ->
                                                         (Let,
                                                           (loc_expr _loc
                                                              (Pexp_open
                                                                 (mp, e)))))))))
                                :: y
                              else y in
                            if lvl < App
                            then
                              (Glr.fsequence (locate module_kw)
                                 (Glr.fsequence (locate module_name)
                                    (Glr.fsequence
                                       (locate
                                          (Glr.apply List.rev
                                             (Glr.fixpoint []
                                                (Glr.apply
                                                   (fun x  l  -> x :: l)
                                                   (Glr.fsequence
                                                      (locate
                                                         (Glr.string "(" "("))
                                                      (Glr.fsequence
                                                         (locate module_name)
                                                         (Glr.fsequence
                                                            (locate
                                                               (Glr.string
                                                                  ":" ":"))
                                                            (Glr.sequence
                                                               (locate
                                                                  module_type)
                                                               (locate
                                                                  (Glr.string
                                                                    ")" ")"))
                                                               (fun mt  ->
                                                                  let 
                                                                    (_loc_mt,mt)
                                                                    = mt in
                                                                  fun
                                                                    _unnamed_1
                                                                     ->
                                                                    let 
                                                                    (_loc__unnamed_1,_unnamed_1)
                                                                    =
                                                                    _unnamed_1 in
                                                                    fun
                                                                    _unnamed_2
                                                                     ->
                                                                    let 
                                                                    (_loc__unnamed_2,_unnamed_2)
                                                                    =
                                                                    _unnamed_2 in
                                                                    fun mn 
                                                                    ->
                                                                    let 
                                                                    (_loc_mn,mn)
                                                                    = mn in
                                                                    fun
                                                                    _unnamed_4
                                                                     ->
                                                                    let 
                                                                    (_loc__unnamed_4,_unnamed_4)
                                                                    =
                                                                    _unnamed_4 in
                                                                    let _loc
                                                                    =
                                                                    merge
                                                                    [_loc__unnamed_4;
                                                                    _loc_mn;
                                                                    _loc__unnamed_2;
                                                                    _loc_mt;
                                                                    _loc__unnamed_1] in
                                                                    ((id_loc
                                                                    mn
                                                                    _loc_mn),
                                                                    mt, _loc))))))))))
                                       (Glr.fsequence
                                          (locate
                                             (Glr.option None
                                                (Glr.apply (fun x  -> Some x)
                                                   (Glr.sequence
                                                      (locate
                                                         (Glr.string ":" ":"))
                                                      (locate module_type)
                                                      (fun _unnamed_0  ->
                                                         let (_loc__unnamed_0,_unnamed_0)
                                                           = _unnamed_0 in
                                                         fun mt  ->
                                                           let (_loc_mt,mt) =
                                                             mt in
                                                           let _loc =
                                                             merge
                                                               [_loc__unnamed_0;
                                                               _loc_mt] in
                                                           mt)))))
                                          (Glr.fsequence
                                             (locate (Glr.string "=" "="))
                                             (Glr.fsequence
                                                (locate module_expr)
                                                (Glr.sequence (locate in_kw)
                                                   (locate
                                                      (expression_lvl
                                                         (let_prio lvl)))
                                                   (fun _unnamed_0  ->
                                                      let (_loc__unnamed_0,_unnamed_0)
                                                        = _unnamed_0 in
                                                      fun e  ->
                                                        let (_loc_e,e) = e in
                                                        fun me  ->
                                                          let (_loc_me,me) =
                                                            me in
                                                          fun _unnamed_3  ->
                                                            let (_loc__unnamed_3,_unnamed_3)
                                                              = _unnamed_3 in
                                                            fun mt  ->
                                                              let (_loc_mt,mt)
                                                                = mt in
                                                              fun l  ->
                                                                let (_loc_l,l)
                                                                  = l in
                                                                fun mn  ->
                                                                  let 
                                                                    (_loc_mn,mn)
                                                                    = mn in
                                                                  fun
                                                                    _unnamed_7
                                                                     ->
                                                                    let 
                                                                    (_loc__unnamed_7,_unnamed_7)
                                                                    =
                                                                    _unnamed_7 in
                                                                    let _loc
                                                                    =
                                                                    merge
                                                                    [_loc__unnamed_7;
                                                                    _loc_mn;
                                                                    _loc_l;
                                                                    _loc_mt;
                                                                    _loc__unnamed_3;
                                                                    _loc_me;
                                                                    _loc__unnamed_0;
                                                                    _loc_e] in
                                                                    let me =
                                                                    match mt
                                                                    with
                                                                    | 
                                                                    None  ->
                                                                    me
                                                                    | 
                                                                    Some mt
                                                                    ->
                                                                    mexpr_loc
                                                                    (merge2
                                                                    _loc_mt
                                                                    _loc_me)
                                                                    (Pmod_constraint
                                                                    (me, mt)) in
                                                                    let me =
                                                                    List.fold_left
                                                                    (fun acc 
                                                                    (mn,mt,_loc)
                                                                     ->
                                                                    mexpr_loc
                                                                    (merge2
                                                                    _loc
                                                                    _loc_me)
                                                                    (Pmod_functor
                                                                    (mn, mt,
                                                                    acc))) me
                                                                    (List.rev
                                                                    l) in
                                                                    fun _loc 
                                                                    ->
                                                                    (Let,
                                                                    (loc_expr
                                                                    _loc
                                                                    (Pexp_letmodule
                                                                    ((id_loc
                                                                    mn
                                                                    _loc_mn),
                                                                    me, e))))))))))))
                              :: y
                            else y in
                          if lvl < App
                          then
                            (Glr.fsequence (locate rec_flag)
                               (Glr.fsequence (locate let_binding)
                                  (Glr.sequence (locate in_kw)
                                     (locate (expression_lvl (let_prio lvl)))
                                     (fun _unnamed_0  ->
                                        let (_loc__unnamed_0,_unnamed_0) =
                                          _unnamed_0 in
                                        fun e  ->
                                          let (_loc_e,e) = e in
                                          fun l  ->
                                            let (_loc_l,l) = l in
                                            fun r  ->
                                              let (_loc_r,r) = r in
                                              let _loc =
                                                merge
                                                  [_loc_r;
                                                  _loc_l;
                                                  _loc__unnamed_0;
                                                  _loc_e] in
                                              fun _loc  ->
                                                (Let,
                                                  (loc_expr _loc
                                                     (Pexp_let (r, l, e))))))))
                            :: y
                          else y)))
                   (fun _unnamed_0  ->
                      let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                      fun r  ->
                        let (_loc_r,r) = r in
                        let _loc = merge [_loc__unnamed_0; _loc_r] in r _loc))
                ::
                (let y =
                   let y =
                     let y =
                       let y =
                         let y =
                           (Glr.fsequence (locate (Glr.string "(" "("))
                              (Glr.sequence
                                 (locate
                                    (Glr.option None
                                       (Glr.apply (fun x  -> Some x)
                                          expression)))
                                 (locate (Glr.string ")" ")"))
                                 (fun e  ->
                                    let (_loc_e,e) = e in
                                    fun _unnamed_1  ->
                                      let (_loc__unnamed_1,_unnamed_1) =
                                        _unnamed_1 in
                                      fun _unnamed_2  ->
                                        let (_loc__unnamed_2,_unnamed_2) =
                                          _unnamed_2 in
                                        let _loc =
                                          merge
                                            [_loc__unnamed_2;
                                            _loc_e;
                                            _loc__unnamed_1] in
                                        (Atom,
                                          (match e with
                                           | Some e ->
                                               loc_expr _loc e.pexp_desc
                                           | None  ->
                                               let cunit =
                                                 id_loc (Lident "()") _loc in
                                               loc_expr _loc
                                                 (pexp_construct
                                                    (cunit, None)))))))
                           ::
                           (Glr.fsequence (locate begin_kw)
                              (Glr.sequence
                                 (locate
                                    (Glr.option None
                                       (Glr.apply (fun x  -> Some x)
                                          expression))) (locate end_kw)
                                 (fun e  ->
                                    let (_loc_e,e) = e in
                                    fun _unnamed_1  ->
                                      let (_loc__unnamed_1,_unnamed_1) =
                                        _unnamed_1 in
                                      fun _unnamed_2  ->
                                        let (_loc__unnamed_2,_unnamed_2) =
                                          _unnamed_2 in
                                        let _loc =
                                          merge
                                            [_loc__unnamed_2;
                                            _loc_e;
                                            _loc__unnamed_1] in
                                        (Atom,
                                          (match e with
                                           | Some e -> e
                                           | None  ->
                                               let cunit =
                                                 id_loc (Lident "()") _loc in
                                               loc_expr _loc
                                                 (pexp_construct
                                                    (cunit, None)))))))
                           ::
                           (Glr.sequence (locate constructor)
                              (locate
                                 (Glr.option None
                                    (Glr.apply (fun x  -> Some x)
                                       (if lvl <= App
                                        then
                                          Glr.apply
                                            (fun e  ->
                                               let (_loc_e,e) = e in
                                               let _loc = _loc_e in e)
                                            (locate
                                               (expression_lvl (next_exp App)))
                                        else Glr.fail ""))))
                              (fun c  ->
                                 let (_loc_c,c) = c in
                                 fun e  ->
                                   let (_loc_e,e) = e in
                                   let _loc = merge [_loc_c; _loc_e] in
                                   (App,
                                     (loc_expr _loc
                                        (pexp_construct
                                           ((id_loc c _loc_c), e))))))
                           ::
                           (let y =
                              let y =
                                let y =
                                  [Glr.apply
                                     (fun l  ->
                                        let (_loc_l,l) = l in
                                        let _loc = _loc_l in
                                        (Atom,
                                          (loc_expr _loc
                                             (Pexp_variant (l, None)))))
                                     (locate tag_name);
                                  Glr.fsequence
                                    (locate (Glr.string "[|" "[|"))
                                    (Glr.sequence (locate expression_list)
                                       (locate (Glr.string "|]" "|]"))
                                       (fun l  ->
                                          let (_loc_l,l) = l in
                                          fun _unnamed_1  ->
                                            let (_loc__unnamed_1,_unnamed_1)
                                              = _unnamed_1 in
                                            fun _unnamed_2  ->
                                              let (_loc__unnamed_2,_unnamed_2)
                                                = _unnamed_2 in
                                              let _loc =
                                                merge
                                                  [_loc__unnamed_2;
                                                  _loc_l;
                                                  _loc__unnamed_1] in
                                              (Atom,
                                                (loc_expr _loc
                                                   (Pexp_array
                                                      (List.map fst l))))));
                                  Glr.fsequence (locate (Glr.string "[" "["))
                                    (Glr.sequence (locate expression_list)
                                       (locate (Glr.string "]" "]"))
                                       (fun l  ->
                                          let (_loc_l,l) = l in
                                          fun cl  ->
                                            let (_loc_cl,cl) = cl in
                                            fun _unnamed_2  ->
                                              let (_loc__unnamed_2,_unnamed_2)
                                                = _unnamed_2 in
                                              let _loc =
                                                merge
                                                  [_loc__unnamed_2;
                                                  _loc_l;
                                                  _loc_cl] in
                                              (Atom,
                                                (loc_expr _loc
                                                   (pexp_list _loc ~_loc_cl l).pexp_desc))));
                                  Glr.fsequence (locate (Glr.string "{" "{"))
                                    (Glr.fsequence
                                       (locate
                                          (Glr.option None
                                             (Glr.apply (fun x  -> Some x)
                                                (Glr.sequence
                                                   (locate
                                                      (expression_lvl
                                                         (next_exp Seq)))
                                                   (locate with_kw)
                                                   (fun e  ->
                                                      let (_loc_e,e) = e in
                                                      fun _unnamed_1  ->
                                                        let (_loc__unnamed_1,_unnamed_1)
                                                          = _unnamed_1 in
                                                        let _loc =
                                                          merge
                                                            [_loc_e;
                                                            _loc__unnamed_1] in
                                                        e)))))
                                       (Glr.sequence (locate record_list)
                                          (locate (Glr.string "}" "}"))
                                          (fun l  ->
                                             let (_loc_l,l) = l in
                                             fun _unnamed_1  ->
                                               let (_loc__unnamed_1,_unnamed_1)
                                                 = _unnamed_1 in
                                               fun e  ->
                                                 let (_loc_e,e) = e in
                                                 fun _unnamed_3  ->
                                                   let (_loc__unnamed_3,_unnamed_3)
                                                     = _unnamed_3 in
                                                   let _loc =
                                                     merge
                                                       [_loc__unnamed_3;
                                                       _loc_e;
                                                       _loc_l;
                                                       _loc__unnamed_1] in
                                                   (Atom,
                                                     (loc_expr _loc
                                                        (Pexp_record (l, e)))))));
                                  Glr.fsequence (locate while_kw)
                                    (Glr.fsequence (locate expression)
                                       (Glr.fsequence (locate do_kw)
                                          (Glr.sequence (locate expression)
                                             (locate done_kw)
                                             (fun e'  ->
                                                let (_loc_e',e') = e' in
                                                fun _unnamed_1  ->
                                                  let (_loc__unnamed_1,_unnamed_1)
                                                    = _unnamed_1 in
                                                  fun _unnamed_2  ->
                                                    let (_loc__unnamed_2,_unnamed_2)
                                                      = _unnamed_2 in
                                                    fun e  ->
                                                      let (_loc_e,e) = e in
                                                      fun _unnamed_4  ->
                                                        let (_loc__unnamed_4,_unnamed_4)
                                                          = _unnamed_4 in
                                                        let _loc =
                                                          merge
                                                            [_loc__unnamed_4;
                                                            _loc_e;
                                                            _loc__unnamed_2;
                                                            _loc_e';
                                                            _loc__unnamed_1] in
                                                        (Atom,
                                                          (loc_expr _loc
                                                             (Pexp_while
                                                                (e, e'))))))));
                                  Glr.fsequence (locate for_kw)
                                    (Glr.fsequence (locate lowercase_ident)
                                       (Glr.fsequence
                                          (locate (Glr.char '=' '='))
                                          (Glr.fsequence (locate expression)
                                             (Glr.fsequence
                                                (locate downto_flag)
                                                (Glr.fsequence
                                                   (locate expression)
                                                   (Glr.fsequence
                                                      (locate do_kw)
                                                      (Glr.sequence
                                                         (locate expression)
                                                         (locate done_kw)
                                                         (fun e''  ->
                                                            let (_loc_e'',e'')
                                                              = e'' in
                                                            fun _unnamed_1 
                                                              ->
                                                              let (_loc__unnamed_1,_unnamed_1)
                                                                = _unnamed_1 in
                                                              fun _unnamed_2 
                                                                ->
                                                                let (_loc__unnamed_2,_unnamed_2)
                                                                  =
                                                                  _unnamed_2 in
                                                                fun e'  ->
                                                                  let 
                                                                    (_loc_e',e')
                                                                    = e' in
                                                                  fun d  ->
                                                                    let 
                                                                    (_loc_d,d)
                                                                    = d in
                                                                    fun e  ->
                                                                    let 
                                                                    (_loc_e,e)
                                                                    = e in
                                                                    fun
                                                                    _unnamed_6
                                                                     ->
                                                                    let 
                                                                    (_loc__unnamed_6,_unnamed_6)
                                                                    =
                                                                    _unnamed_6 in
                                                                    fun id 
                                                                    ->
                                                                    let 
                                                                    (_loc_id,id)
                                                                    = id in
                                                                    fun
                                                                    _unnamed_8
                                                                     ->
                                                                    let 
                                                                    (_loc__unnamed_8,_unnamed_8)
                                                                    =
                                                                    _unnamed_8 in
                                                                    let _loc
                                                                    =
                                                                    merge
                                                                    [_loc__unnamed_8;
                                                                    _loc_id;
                                                                    _loc__unnamed_6;
                                                                    _loc_e;
                                                                    _loc_d;
                                                                    _loc_e';
                                                                    _loc__unnamed_2;
                                                                    _loc_e'';
                                                                    _loc__unnamed_1] in
                                                                    (Atom,
                                                                    (loc_expr
                                                                    _loc
                                                                    (Pexp_for
                                                                    ((id_loc
                                                                    id
                                                                    _loc_id),
                                                                    e, e', d,
                                                                    e''))))))))))));
                                  Glr.sequence (locate new_kw)
                                    (locate class_path)
                                    (fun _unnamed_0  ->
                                       let (_loc__unnamed_0,_unnamed_0) =
                                         _unnamed_0 in
                                       fun p  ->
                                         let (_loc_p,p) = p in
                                         let _loc =
                                           merge [_loc__unnamed_0; _loc_p] in
                                         (Atom,
                                           (loc_expr _loc
                                              (Pexp_new (id_loc p _loc_p)))));
                                  Glr.fsequence (locate object_kw)
                                    (Glr.sequence (locate class_body)
                                       (locate end_kw)
                                       (fun o  ->
                                          let (_loc_o,o) = o in
                                          fun _unnamed_1  ->
                                            let (_loc__unnamed_1,_unnamed_1)
                                              = _unnamed_1 in
                                            fun _unnamed_2  ->
                                              let (_loc__unnamed_2,_unnamed_2)
                                                = _unnamed_2 in
                                              let _loc =
                                                merge
                                                  [_loc__unnamed_2;
                                                  _loc_o;
                                                  _loc__unnamed_1] in
                                              (Atom,
                                                (loc_expr _loc
                                                   (Pexp_object o)))));
                                  Glr.fsequence
                                    (locate (Glr.string "{<" "{<"))
                                    (Glr.sequence
                                       (locate
                                          (Glr.option []
                                             (Glr.fsequence (locate obj_item)
                                                (Glr.sequence
                                                   (locate
                                                      (Glr.apply List.rev
                                                         (Glr.fixpoint []
                                                            (Glr.apply
                                                               (fun x  l  ->
                                                                  x :: l)
                                                               (Glr.sequence
                                                                  (locate
                                                                    (Glr.string
                                                                    ";" ";"))
                                                                  (locate
                                                                    obj_item)
                                                                  (fun
                                                                    _unnamed_0
                                                                     ->
                                                                    let 
                                                                    (_loc__unnamed_0,_unnamed_0)
                                                                    =
                                                                    _unnamed_0 in
                                                                    fun o  ->
                                                                    let 
                                                                    (_loc_o,o)
                                                                    = o in
                                                                    let _loc
                                                                    =
                                                                    merge
                                                                    [_loc__unnamed_0;
                                                                    _loc_o] in
                                                                    o))))))
                                                   (locate
                                                      (Glr.option None
                                                         (Glr.apply
                                                            (fun x  -> Some x)
                                                            (Glr.string ";"
                                                               ";"))))
                                                   (fun l  ->
                                                      let (_loc_l,l) = l in
                                                      fun _unnamed_1  ->
                                                        let (_loc__unnamed_1,_unnamed_1)
                                                          = _unnamed_1 in
                                                        fun o  ->
                                                          let (_loc_o,o) = o in
                                                          let _loc =
                                                            merge
                                                              [_loc_o;
                                                              _loc_l;
                                                              _loc__unnamed_1] in
                                                          o :: l)))))
                                       (locate (Glr.string ">}" ">}"))
                                       (fun l  ->
                                          let (_loc_l,l) = l in
                                          fun _unnamed_1  ->
                                            let (_loc__unnamed_1,_unnamed_1)
                                              = _unnamed_1 in
                                            fun _unnamed_2  ->
                                              let (_loc__unnamed_2,_unnamed_2)
                                                = _unnamed_2 in
                                              let _loc =
                                                merge
                                                  [_loc__unnamed_2;
                                                  _loc_l;
                                                  _loc__unnamed_1] in
                                              (Atom,
                                                (loc_expr _loc
                                                   (Pexp_override l)))));
                                  Glr.fsequence (locate (Glr.string "(" "("))
                                    (Glr.fsequence (locate module_kw)
                                       (Glr.fsequence (locate module_expr)
                                          (Glr.sequence
                                             (locate
                                                (Glr.option None
                                                   (Glr.apply
                                                      (fun x  -> Some x)
                                                      (Glr.sequence
                                                         (locate
                                                            (Glr.string ":"
                                                               ":"))
                                                         (locate package_type)
                                                         (fun _unnamed_0  ->
                                                            let (_loc__unnamed_0,_unnamed_0)
                                                              = _unnamed_0 in
                                                            fun pt  ->
                                                              let (_loc_pt,pt)
                                                                = pt in
                                                              let _loc =
                                                                merge
                                                                  [_loc__unnamed_0;
                                                                  _loc_pt] in
                                                              pt)))))
                                             (locate (Glr.string ")" ")"))
                                             (fun pt  ->
                                                let (_loc_pt,pt) = pt in
                                                fun _unnamed_1  ->
                                                  let (_loc__unnamed_1,_unnamed_1)
                                                    = _unnamed_1 in
                                                  fun me  ->
                                                    let (_loc_me,me) = me in
                                                    fun _unnamed_3  ->
                                                      let (_loc__unnamed_3,_unnamed_3)
                                                        = _unnamed_3 in
                                                      fun _unnamed_4  ->
                                                        let (_loc__unnamed_4,_unnamed_4)
                                                          = _unnamed_4 in
                                                        let _loc =
                                                          merge
                                                            [_loc__unnamed_4;
                                                            _loc__unnamed_3;
                                                            _loc_me;
                                                            _loc_pt;
                                                            _loc__unnamed_1] in
                                                        let desc =
                                                          match pt with
                                                          | Some
                                                              (Ptyp_package
                                                              (n,l)) ->
                                                              Pexp_pack
                                                                (me, (n, l))
                                                          | _ ->
                                                              raise Give_up in
                                                        (Atom,
                                                          (loc_expr _loc desc))))));
                                  Glr.fsequence
                                    (locate (Glr.string "<:" "<:"))
                                    (Glr.fsequence
                                       (locate
                                          (Glr.alternatives'
                                             [Glr.apply
                                                (fun _unnamed_0  ->
                                                   let (_loc__unnamed_0,_unnamed_0)
                                                     = _unnamed_0 in
                                                   let _loc = _loc__unnamed_0 in
                                                   "expression")
                                                (locate
                                                   (Glr.string "expr" "expr"));
                                             Glr.apply
                                               (fun _unnamed_0  ->
                                                  let (_loc__unnamed_0,_unnamed_0)
                                                    = _unnamed_0 in
                                                  let _loc = _loc__unnamed_0 in
                                                  "type")
                                               (locate
                                                  (Glr.string "type" "type"));
                                             Glr.apply
                                               (fun _unnamed_0  ->
                                                  let (_loc__unnamed_0,_unnamed_0)
                                                    = _unnamed_0 in
                                                  let _loc = _loc__unnamed_0 in
                                                  "pattern")
                                               (locate
                                                  (Glr.string "pat" "pat"));
                                             Glr.apply
                                               (fun _unnamed_0  ->
                                                  let (_loc__unnamed_0,_unnamed_0)
                                                    = _unnamed_0 in
                                                  let _loc = _loc__unnamed_0 in
                                                  "structure")
                                               (locate
                                                  (Glr.string "structure"
                                                     "structure"));
                                             Glr.apply
                                               (fun _unnamed_0  ->
                                                  let (_loc__unnamed_0,_unnamed_0)
                                                    = _unnamed_0 in
                                                  let _loc = _loc__unnamed_0 in
                                                  "signature")
                                               (locate
                                                  (Glr.string "signature"
                                                     "signature"))]))
                                       (Glr.fsequence
                                          (locate
                                             (Glr.option None
                                                (Glr.apply (fun x  -> Some x)
                                                   (Glr.sequence
                                                      (locate
                                                         (Glr.char '@' '@'))
                                                      (locate
                                                         (expression_lvl
                                                            (next_exp App)))
                                                      (fun _unnamed_0  ->
                                                         let (_loc__unnamed_0,_unnamed_0)
                                                           = _unnamed_0 in
                                                         fun e  ->
                                                           let (_loc_e,e) = e in
                                                           let _loc =
                                                             merge
                                                               [_loc__unnamed_0;
                                                               _loc_e] in
                                                           e)))))
                                          (Glr.sequence
                                             (locate (Glr.char '<' '<'))
                                             (locate quotation)
                                             (fun _unnamed_0  ->
                                                let (_loc__unnamed_0,_unnamed_0)
                                                  = _unnamed_0 in
                                                fun q  ->
                                                  let (_loc_q,q) = q in
                                                  fun loc  ->
                                                    let (_loc_loc,loc) = loc in
                                                    fun name  ->
                                                      let (_loc_name,name) =
                                                        name in
                                                      fun _unnamed_4  ->
                                                        let (_loc__unnamed_4,_unnamed_4)
                                                          = _unnamed_4 in
                                                        let _loc =
                                                          merge
                                                            [_loc__unnamed_4;
                                                            _loc_name;
                                                            _loc_loc;
                                                            _loc__unnamed_0;
                                                            _loc_q] in
                                                        (Atom,
                                                          (quote_expression
                                                             _loc_q loc q
                                                             name))))));
                                  Glr.sequence (locate (Glr.char '$' '$'))
                                    (locate capitalized_ident)
                                    (fun _unnamed_0  ->
                                       let (_loc__unnamed_0,_unnamed_0) =
                                         _unnamed_0 in
                                       fun c  ->
                                         let (_loc_c,c) = c in
                                         let _loc =
                                           merge [_loc__unnamed_0; _loc_c] in
                                         (Atom,
                                           (match c with
                                            | "FILE" ->
                                                loc_expr _loc
                                                  (Pexp_constant
                                                     (const_string
                                                        (start_pos _loc).Lexing.pos_fname))
                                            | "LINE" ->
                                                loc_expr _loc
                                                  (Pexp_constant
                                                     (Const_int
                                                        ((start_pos _loc).Lexing.pos_lnum)))
                                            | _ ->
                                                (try
                                                   let str = Sys.getenv c in
                                                   parse_string
                                                     ~filename:("ENV:" ^ c)
                                                     expression blank str
                                                 with
                                                 | Not_found  ->
                                                     raise Give_up))));
                                  Glr.fsequence (locate (Glr.char '$' '$'))
                                    (Glr.fsequence
                                       (locate
                                          (Glr.option None
                                             (Glr.apply (fun x  -> Some x)
                                                (Glr.sequence
                                                   (locate
                                                      (Glr.alternatives'
                                                         [Glr.apply
                                                            (fun _unnamed_0 
                                                               ->
                                                               let (_loc__unnamed_0,_unnamed_0)
                                                                 = _unnamed_0 in
                                                               let _loc =
                                                                 _loc__unnamed_0 in
                                                               "tuple")
                                                            (locate
                                                               (Glr.string
                                                                  "tuple"
                                                                  "tuple"));
                                                         Glr.apply
                                                           (fun _unnamed_0 
                                                              ->
                                                              let (_loc__unnamed_0,_unnamed_0)
                                                                = _unnamed_0 in
                                                              let _loc =
                                                                _loc__unnamed_0 in
                                                              "list")
                                                           (locate
                                                              (Glr.string
                                                                 "list"
                                                                 "list"));
                                                         Glr.apply
                                                           (fun _unnamed_0 
                                                              ->
                                                              let (_loc__unnamed_0,_unnamed_0)
                                                                = _unnamed_0 in
                                                              let _loc =
                                                                _loc__unnamed_0 in
                                                              "array")
                                                           (locate
                                                              (Glr.string
                                                                 "array"
                                                                 "array"))]))
                                                   (locate (Glr.char ':' ':'))
                                                   (fun t  ->
                                                      let (_loc_t,t) = t in
                                                      fun _unnamed_1  ->
                                                        let (_loc__unnamed_1,_unnamed_1)
                                                          = _unnamed_1 in
                                                        let _loc =
                                                          merge
                                                            [_loc_t;
                                                            _loc__unnamed_1] in
                                                        t)))))
                                       (Glr.sequence
                                          (locate
                                             (expression_lvl (next_exp App)))
                                          (locate (Glr.char '$' '$'))
                                          (fun e  ->
                                             let (_loc_e,e) = e in
                                             fun _unnamed_1  ->
                                               let (_loc__unnamed_1,_unnamed_1)
                                                 = _unnamed_1 in
                                               fun t  ->
                                                 let (_loc_t,t) = t in
                                                 fun _unnamed_3  ->
                                                   let (_loc__unnamed_3,_unnamed_3)
                                                     = _unnamed_3 in
                                                   let _loc =
                                                     merge
                                                       [_loc__unnamed_3;
                                                       _loc_t;
                                                       _loc_e;
                                                       _loc__unnamed_1] in
                                                   match t with
                                                   | None  ->
                                                       (Atom,
                                                         (push_pop_expression
                                                            e))
                                                   | Some str ->
                                                       let l =
                                                         push_pop_expression_list
                                                           e in
                                                       (match str with
                                                        | "tuple" ->
                                                            (Atom,
                                                              (loc_expr _loc
                                                                 (Pexp_tuple
                                                                    l)))
                                                        | "array" ->
                                                            (Atom,
                                                              (loc_expr _loc
                                                                 (Pexp_array
                                                                    l)))
                                                        | "list" ->
                                                            let l =
                                                              List.map
                                                                (fun x  ->
                                                                   (x, _loc))
                                                                l in
                                                            (Atom,
                                                              (loc_expr _loc
                                                                 (pexp_list
                                                                    _loc l).pexp_desc))
                                                        | _ -> raise Give_up))));
                                  Glr.iter
                                    (Glr.apply
                                       (fun p  ->
                                          let (_loc_p,p) = p in
                                          let _loc = _loc_p in
                                          let lvl' = prefix_prio p in
                                          if lvl <= lvl'
                                          then
                                            Glr.apply
                                              (fun e  ->
                                                 let (_loc_e,e) = e in
                                                 let _loc = _loc_e in
                                                 (lvl',
                                                   (mk_unary_opp p _loc_p e
                                                      _loc_e)))
                                              (locate (expression_lvl lvl'))
                                          else Glr.fail "")
                                       (locate prefix_symbol))] in
                                if lvl <= App
                                then
                                  (Glr.sequence (locate tag_name)
                                     (locate (expression_lvl (next_exp App)))
                                     (fun l  ->
                                        let (_loc_l,l) = l in
                                        fun e  ->
                                          let (_loc_e,e) = e in
                                          let _loc = merge [_loc_l; _loc_e] in
                                          (App,
                                            (loc_expr _loc
                                               (Pexp_variant (l, (Some e)))))))
                                  :: y
                                else y in
                              if lvl <= App
                              then
                                (Glr.sequence (locate lazy_kw)
                                   (locate (expression_lvl App))
                                   (fun _unnamed_0  ->
                                      let (_loc__unnamed_0,_unnamed_0) =
                                        _unnamed_0 in
                                      fun e  ->
                                        let (_loc_e,e) = e in
                                        let _loc =
                                          merge [_loc__unnamed_0; _loc_e] in
                                        (App, (loc_expr _loc (Pexp_lazy e)))))
                                :: y
                              else y in
                            if lvl <= App
                            then
                              (Glr.sequence (locate assert_kw)
                                 (locate
                                    (Glr.alternatives'
                                       [Glr.apply
                                          (fun _unnamed_0  ->
                                             let (_loc__unnamed_0,_unnamed_0)
                                               = _unnamed_0 in
                                             let _loc = _loc__unnamed_0 in
                                             pexp_assertfalse _loc)
                                          (locate false_kw);
                                       Glr.apply
                                         (fun e  ->
                                            let (_loc_e,e) = e in
                                            let _loc = _loc_e in
                                            Pexp_assert e)
                                         (locate (expression_lvl App))]))
                                 (fun _unnamed_0  ->
                                    let (_loc__unnamed_0,_unnamed_0) =
                                      _unnamed_0 in
                                    fun e  ->
                                      let (_loc_e,e) = e in
                                      let _loc =
                                        merge [_loc__unnamed_0; _loc_e] in
                                      (App, (loc_expr _loc e))))
                              :: y
                            else y) in
                         if lvl < App
                         then
                           (Glr.fsequence (locate if_kw)
                              (Glr.fsequence (locate expression)
                                 (Glr.fsequence (locate then_kw)
                                    (Glr.sequence
                                       (locate (expression_lvl If))
                                       (locate
                                          (Glr.option None
                                             (Glr.apply (fun x  -> Some x)
                                                (Glr.sequence
                                                   (locate else_kw)
                                                   (locate
                                                      (expression_lvl If))
                                                   (fun _unnamed_0  ->
                                                      let (_loc__unnamed_0,_unnamed_0)
                                                        = _unnamed_0 in
                                                      fun e  ->
                                                        let (_loc_e,e) = e in
                                                        let _loc =
                                                          merge
                                                            [_loc__unnamed_0;
                                                            _loc_e] in
                                                        e)))))
                                       (fun e  ->
                                          let (_loc_e,e) = e in
                                          fun e'  ->
                                            let (_loc_e',e') = e' in
                                            fun _unnamed_2  ->
                                              let (_loc__unnamed_2,_unnamed_2)
                                                = _unnamed_2 in
                                              fun c  ->
                                                let (_loc_c,c) = c in
                                                fun _unnamed_4  ->
                                                  let (_loc__unnamed_4,_unnamed_4)
                                                    = _unnamed_4 in
                                                  let _loc =
                                                    merge
                                                      [_loc__unnamed_4;
                                                      _loc_c;
                                                      _loc__unnamed_2;
                                                      _loc_e;
                                                      _loc_e'] in
                                                  (If,
                                                    (loc_expr _loc
                                                       (Pexp_ifthenelse
                                                          (c, e, e')))))))))
                           :: y
                         else y in
                       if lvl < App
                       then
                         (Glr.fsequence (locate try_kw)
                            (Glr.fsequence (locate expression)
                               (Glr.sequence (locate with_kw)
                                  (locate (match_cases (let_prio lvl)))
                                  (fun _unnamed_0  ->
                                     let (_loc__unnamed_0,_unnamed_0) =
                                       _unnamed_0 in
                                     fun l  ->
                                       let (_loc_l,l) = l in
                                       fun e  ->
                                         let (_loc_e,e) = e in
                                         fun _unnamed_3  ->
                                           let (_loc__unnamed_3,_unnamed_3) =
                                             _unnamed_3 in
                                           let _loc =
                                             merge
                                               [_loc__unnamed_3;
                                               _loc_e;
                                               _loc__unnamed_0;
                                               _loc_l] in
                                           (Let,
                                             (loc_expr _loc (Pexp_try (e, l))))))))
                         :: y
                       else y in
                     if lvl < App
                     then
                       (Glr.fsequence (locate match_kw)
                          (Glr.fsequence (locate expression)
                             (Glr.sequence (locate with_kw)
                                (locate (match_cases (let_prio lvl)))
                                (fun _unnamed_0  ->
                                   let (_loc__unnamed_0,_unnamed_0) =
                                     _unnamed_0 in
                                   fun l  ->
                                     let (_loc_l,l) = l in
                                     fun e  ->
                                       let (_loc_e,e) = e in
                                       fun _unnamed_3  ->
                                         let (_loc__unnamed_3,_unnamed_3) =
                                           _unnamed_3 in
                                         let _loc =
                                           merge
                                             [_loc__unnamed_3;
                                             _loc_e;
                                             _loc__unnamed_0;
                                             _loc_l] in
                                         (Let,
                                           (loc_expr _loc (Pexp_match (e, l))))))))
                       :: y
                     else y in
                   if lvl < App
                   then
                     (Glr.fsequence (locate fun_kw)
                        (Glr.fsequence
                           (locate
                              (Glr.apply List.rev
                                 (Glr.fixpoint []
                                    (Glr.apply (fun x  l  -> x :: l)
                                       (Glr.apply
                                          (fun lbl  ->
                                             let (_loc_lbl,lbl) = lbl in
                                             let _loc = _loc_lbl in
                                             (lbl, _loc_lbl))
                                          (locate (parameter true)))))))
                           (Glr.sequence (locate (Glr.string "->" "->"))
                              (locate (expression_lvl (let_prio lvl)))
                              (fun _unnamed_0  ->
                                 let (_loc__unnamed_0,_unnamed_0) =
                                   _unnamed_0 in
                                 fun e  ->
                                   let (_loc_e,e) = e in
                                   fun l  ->
                                     let (_loc_l,l) = l in
                                     fun _unnamed_3  ->
                                       let (_loc__unnamed_3,_unnamed_3) =
                                         _unnamed_3 in
                                       let _loc =
                                         merge
                                           [_loc__unnamed_3;
                                           _loc_l;
                                           _loc__unnamed_0;
                                           _loc_e] in
                                       (Let,
                                         (loc_expr _loc
                                            (apply_params l e).pexp_desc))))))
                     :: y
                   else y in
                 if lvl < App
                 then
                   (Glr.sequence (locate function_kw)
                      (locate (match_cases (let_prio lvl)))
                      (fun _unnamed_0  ->
                         let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                         fun l  ->
                           let (_loc_l,l) = l in
                           let _loc = merge [_loc__unnamed_0; _loc_l] in
                           (Let, (loc_expr _loc (pexp_function l)))))
                   :: y
                 else y) in
              if lvl <= Aff
              then
                (Glr.fsequence (locate inst_var_name)
                   (Glr.sequence (locate (Glr.string "<-" "<-"))
                      (locate (expression_lvl (next_exp Aff)))
                      (fun _unnamed_0  ->
                         let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                         fun e  ->
                           let (_loc_e,e) = e in
                           fun v  ->
                             let (_loc_v,v) = v in
                             let _loc =
                               merge [_loc_v; _loc__unnamed_0; _loc_e] in
                             (Aff,
                               (loc_expr _loc
                                  (Pexp_setinstvar ((id_loc v _loc_v), e)))))))
                :: y
              else y)))
    let apply_lbl _loc (lbl,e) =
      let e =
        match e with
        | None  -> loc_expr _loc (Pexp_ident (id_loc (Lident lbl) _loc))
        | Some e -> e in
      (lbl, e)
    let rec mk_seq =
      function
      | [] -> assert false
      | e::[] -> e
      | x::l ->
          let res = mk_seq l in
          loc_expr (merge2 x.pexp_loc res.pexp_loc) (Pexp_sequence (x, res))
    let semi_col =
      black_box
        (fun str  pos  ->
           let (c,str',pos') = read str pos in
           if c = ';'
           then
             let (c',_,_) = read str' pos' in
             (if c' = ';' then raise Give_up else ((), str', pos'))
           else raise Give_up) (Charset.singleton ';') false ";"
    let double_semi_col =
      black_box
        (fun str  pos  ->
           let (c,str',pos') = read str pos in
           if c = ';'
           then
             let (c',_,_) = read str' pos' in
             (if c' <> ';' then raise Give_up else ((), str', pos'))
           else raise Give_up) (Charset.singleton ';') false ";;"
    let expression_suit_aux =
      memoize2
        (fun lvl'  lvl  ->
           let ln f _loc e = loc_expr (merge2 f.pexp_loc _loc) e in
           Glr.alternatives'
             (let y =
                let y =
                  let y =
                    let y =
                      (Glr.sequence (locate (Glr.string "." "."))
                         (locate
                            (Glr.alternatives'
                               (let y =
                                  let y =
                                    let y =
                                      let y =
                                        let y =
                                          let y =
                                            let y =
                                              let y = [] in
                                              if
                                                (lvl' >= Dot) && (lvl <= Dot)
                                              then
                                                (Glr.apply
                                                   (fun f  ->
                                                      let (_loc_f,f) = f in
                                                      let _loc = _loc_f in
                                                      (Dot,
                                                        (fun e'  ->
                                                           let f =
                                                             id_loc f _loc_f in
                                                           loc_expr _loc
                                                             (Pexp_field
                                                                (e', f)))))
                                                   (locate field))
                                                :: y
                                              else y in
                                            if (lvl' >= Aff) && (lvl <= Aff)
                                            then
                                              (Glr.fsequence (locate field)
                                                 (Glr.sequence
                                                    (locate
                                                       (Glr.string "<-" "<-"))
                                                    (locate
                                                       (expression_lvl
                                                          (next_exp Aff)))
                                                    (fun _unnamed_0  ->
                                                       let (_loc__unnamed_0,_unnamed_0)
                                                         = _unnamed_0 in
                                                       fun e  ->
                                                         let (_loc_e,e) = e in
                                                         fun f  ->
                                                           let (_loc_f,f) = f in
                                                           let _loc =
                                                             merge
                                                               [_loc_f;
                                                               _loc__unnamed_0;
                                                               _loc_e] in
                                                           (Aff,
                                                             (fun e'  ->
                                                                let f =
                                                                  id_loc f
                                                                    _loc_f in
                                                                loc_expr _loc
                                                                  (Pexp_setfield
                                                                    (e', f,
                                                                    e)))))))
                                              :: y
                                            else y in
                                          if (lvl' >= Dot) && (lvl <= Dot)
                                          then
                                            (Glr.fsequence
                                               (locate (Glr.string "{" "{"))
                                               (Glr.sequence
                                                  (locate expression)
                                                  (locate
                                                     (Glr.string "}" "}"))
                                                  (fun f  ->
                                                     let (_loc_f,f) = f in
                                                     fun _unnamed_1  ->
                                                       let (_loc__unnamed_1,_unnamed_1)
                                                         = _unnamed_1 in
                                                       fun _unnamed_2  ->
                                                         let (_loc__unnamed_2,_unnamed_2)
                                                           = _unnamed_2 in
                                                         let _loc =
                                                           merge
                                                             [_loc__unnamed_2;
                                                             _loc_f;
                                                             _loc__unnamed_1] in
                                                         (Dot,
                                                           (fun e'  ->
                                                              bigarray_get
                                                                (merge2
                                                                   e'.pexp_loc
                                                                   _loc) e' f)))))
                                            :: y
                                          else y in
                                        if (lvl' >= Aff) && (lvl <= Aff)
                                        then
                                          (Glr.fsequence
                                             (locate (Glr.string "{" "{"))
                                             (Glr.fsequence
                                                (locate expression)
                                                (Glr.fsequence
                                                   (locate
                                                      (Glr.string "}" "}"))
                                                   (Glr.sequence
                                                      (locate
                                                         (Glr.string "<-"
                                                            "<-"))
                                                      (locate
                                                         (expression_lvl
                                                            (next_exp Aff)))
                                                      (fun _unnamed_0  ->
                                                         let (_loc__unnamed_0,_unnamed_0)
                                                           = _unnamed_0 in
                                                         fun e  ->
                                                           let (_loc_e,e) = e in
                                                           fun _unnamed_2  ->
                                                             let (_loc__unnamed_2,_unnamed_2)
                                                               = _unnamed_2 in
                                                             fun f  ->
                                                               let (_loc_f,f)
                                                                 = f in
                                                               fun _unnamed_4
                                                                  ->
                                                                 let 
                                                                   (_loc__unnamed_4,_unnamed_4)
                                                                   =
                                                                   _unnamed_4 in
                                                                 let _loc =
                                                                   merge
                                                                    [_loc__unnamed_4;
                                                                    _loc_f;
                                                                    _loc__unnamed_2;
                                                                    _loc__unnamed_0;
                                                                    _loc_e] in
                                                                 (Aff,
                                                                   (fun e' 
                                                                    ->
                                                                    bigarray_set
                                                                    (merge2
                                                                    e'.pexp_loc
                                                                    _loc) e'
                                                                    f e)))))))
                                          :: y
                                        else y in
                                      if (lvl' >= Dot) && (lvl <= Dot)
                                      then
                                        (Glr.fsequence
                                           (locate (Glr.string "[" "["))
                                           (Glr.sequence (locate expression)
                                              (locate (Glr.string "]" "]"))
                                              (fun f  ->
                                                 let (_loc_f,f) = f in
                                                 fun _unnamed_1  ->
                                                   let (_loc__unnamed_1,_unnamed_1)
                                                     = _unnamed_1 in
                                                   fun _unnamed_2  ->
                                                     let (_loc__unnamed_2,_unnamed_2)
                                                       = _unnamed_2 in
                                                     let _loc =
                                                       merge
                                                         [_loc__unnamed_2;
                                                         _loc_f;
                                                         _loc__unnamed_1] in
                                                     (Dot,
                                                       (fun e'  ->
                                                          ln e' _loc
                                                            (Pexp_apply
                                                               ((array_function
                                                                   (merge2
                                                                    e'.pexp_loc
                                                                    _loc)
                                                                   "String"
                                                                   "get"),
                                                                 [("", e');
                                                                 ("", f)])))))))
                                        :: y
                                      else y in
                                    if (lvl' >= Aff) && (lvl <= Aff)
                                    then
                                      (Glr.fsequence
                                         (locate (Glr.string "[" "["))
                                         (Glr.fsequence (locate expression)
                                            (Glr.fsequence
                                               (locate (Glr.string "]" "]"))
                                               (Glr.sequence
                                                  (locate
                                                     (Glr.string "<-" "<-"))
                                                  (locate
                                                     (expression_lvl
                                                        (next_exp Aff)))
                                                  (fun _unnamed_0  ->
                                                     let (_loc__unnamed_0,_unnamed_0)
                                                       = _unnamed_0 in
                                                     fun e  ->
                                                       let (_loc_e,e) = e in
                                                       fun _unnamed_2  ->
                                                         let (_loc__unnamed_2,_unnamed_2)
                                                           = _unnamed_2 in
                                                         fun f  ->
                                                           let (_loc_f,f) = f in
                                                           fun _unnamed_4  ->
                                                             let (_loc__unnamed_4,_unnamed_4)
                                                               = _unnamed_4 in
                                                             let _loc =
                                                               merge
                                                                 [_loc__unnamed_4;
                                                                 _loc_f;
                                                                 _loc__unnamed_2;
                                                                 _loc__unnamed_0;
                                                                 _loc_e] in
                                                             (Aff,
                                                               (fun e'  ->
                                                                  ln e' _loc
                                                                    (
                                                                    Pexp_apply
                                                                    ((array_function
                                                                    (merge2
                                                                    e'.pexp_loc
                                                                    _loc)
                                                                    "String"
                                                                    "set"),
                                                                    [
                                                                    ("", e');
                                                                    ("", f);
                                                                    ("", e)])))))))))
                                      :: y
                                    else y in
                                  if (lvl' >= Dot) && (lvl <= Dot)
                                  then
                                    (Glr.fsequence
                                       (locate (Glr.string "(" "("))
                                       (Glr.sequence (locate expression)
                                          (locate (Glr.string ")" ")"))
                                          (fun f  ->
                                             let (_loc_f,f) = f in
                                             fun _unnamed_1  ->
                                               let (_loc__unnamed_1,_unnamed_1)
                                                 = _unnamed_1 in
                                               fun _unnamed_2  ->
                                                 let (_loc__unnamed_2,_unnamed_2)
                                                   = _unnamed_2 in
                                                 let _loc =
                                                   merge
                                                     [_loc__unnamed_2;
                                                     _loc_f;
                                                     _loc__unnamed_1] in
                                                 (Dot,
                                                   (fun e'  ->
                                                      ln e' _loc
                                                        (Pexp_apply
                                                           ((array_function
                                                               (merge2
                                                                  e'.pexp_loc
                                                                  _loc)
                                                               "Array" "get"),
                                                             [("", e');
                                                             ("", f)])))))))
                                    :: y
                                  else y in
                                if (lvl' > Aff) && (lvl <= Aff)
                                then
                                  (Glr.fsequence
                                     (locate (Glr.string "(" "("))
                                     (Glr.fsequence (locate expression)
                                        (Glr.fsequence
                                           (locate (Glr.string ")" ")"))
                                           (Glr.sequence
                                              (locate (Glr.string "<-" "<-"))
                                              (locate
                                                 (expression_lvl
                                                    (next_exp Aff)))
                                              (fun _unnamed_0  ->
                                                 let (_loc__unnamed_0,_unnamed_0)
                                                   = _unnamed_0 in
                                                 fun e  ->
                                                   let (_loc_e,e) = e in
                                                   fun _unnamed_2  ->
                                                     let (_loc__unnamed_2,_unnamed_2)
                                                       = _unnamed_2 in
                                                     fun f  ->
                                                       let (_loc_f,f) = f in
                                                       fun _unnamed_4  ->
                                                         let (_loc__unnamed_4,_unnamed_4)
                                                           = _unnamed_4 in
                                                         let _loc =
                                                           merge
                                                             [_loc__unnamed_4;
                                                             _loc_f;
                                                             _loc__unnamed_2;
                                                             _loc__unnamed_0;
                                                             _loc_e] in
                                                         (Aff,
                                                           (fun e'  ->
                                                              ln e' _loc
                                                                (Pexp_apply
                                                                   ((array_function
                                                                    (merge2
                                                                    e'.pexp_loc
                                                                    _loc)
                                                                    "Array"
                                                                    "set"),
                                                                    [
                                                                    ("", e');
                                                                    ("", f);
                                                                    ("", e)])))))))))
                                  :: y
                                else y)))
                         (fun _unnamed_0  ->
                            let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                            fun r  ->
                              let (_loc_r,r) = r in
                              let _loc = merge [_loc__unnamed_0; _loc_r] in r))
                      ::
                      (let y =
                         let y =
                           [Glr.iter
                              (Glr.apply
                                 (fun op  ->
                                    let (_loc_op,op) = op in
                                    let _loc = _loc_op in
                                    let p = infix_prio op in
                                    let a = assoc p in
                                    if
                                      (lvl <= p) &&
                                        ((lvl' > p) ||
                                           ((a = Left) && (lvl' = p)))
                                    then
                                      Glr.apply
                                        (fun e  ->
                                           let (_loc_e,e) = e in
                                           let _loc = _loc_e in
                                           (p,
                                             (fun e'  ->
                                                ln e' e.pexp_loc
                                                  (if op = "::"
                                                   then
                                                     pexp_construct
                                                       ((id_loc (Lident "::")
                                                           _loc_op),
                                                         (Some
                                                            (ln e' _loc
                                                               (Pexp_tuple
                                                                  [e'; e]))))
                                                   else
                                                     Pexp_apply
                                                       ((loc_expr _loc_op
                                                           (Pexp_ident
                                                              (id_loc
                                                                 (Lident op)
                                                                 _loc_op))),
                                                         [("", e'); ("", e)])))))
                                        (locate
                                           (expression_lvl
                                              (if a = Right
                                               then p
                                               else next_exp p)))
                                    else Glr.fail "") (locate infix_op))] in
                         if (lvl' > App) && (lvl <= App)
                         then
                           (Glr.apply
                              (fun l  ->
                                 let (_loc_l,l) = l in
                                 let _loc = _loc_l in
                                 (App,
                                   (fun f  -> ln f _loc (Pexp_apply (f, l)))))
                              (locate
                                 (Glr.sequence
                                    (Glr.apply
                                       (fun a  ->
                                          let (_loc_a,a) = a in
                                          let _loc = _loc_a in a)
                                       (locate argument))
                                    (Glr.fixpoint []
                                       (Glr.apply (fun x  l  -> x :: l)
                                          (Glr.apply
                                             (fun a  ->
                                                let (_loc_a,a) = a in
                                                let _loc = _loc_a in a)
                                             (locate argument))))
                                    (fun x  l  -> x :: (List.rev l)))))
                           :: y
                         else y in
                       if (lvl' >= Dash) && (lvl <= Dash)
                       then
                         (Glr.sequence (locate (Glr.string "#" "#"))
                            (locate method_name)
                            (fun _unnamed_0  ->
                               let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                               fun f  ->
                                 let (_loc_f,f) = f in
                                 let _loc = merge [_loc__unnamed_0; _loc_f] in
                                 (Dash,
                                   (fun e'  -> ln e' _loc (Pexp_send (e', f))))))
                         :: y
                       else y) in
                    if (lvl' >= Seq) && (lvl <= Seq)
                    then
                      (Glr.apply
                         (fun _unnamed_0  ->
                            let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                            let _loc = _loc__unnamed_0 in
                            (Seq, (fun e  -> e))) (locate semi_col))
                      :: y
                    else y in
                  if (lvl' > Seq) && (lvl <= Seq)
                  then
                    (Glr.apply
                       (fun l  ->
                          let (_loc_l,l) = l in
                          let _loc = _loc_l in
                          (Seq, (fun f  -> mk_seq (f :: l))))
                       (locate
                          (Glr.sequence
                             (Glr.sequence (locate semi_col)
                                (locate (expression_lvl (next_exp Seq)))
                                (fun _unnamed_0  ->
                                   let (_loc__unnamed_0,_unnamed_0) =
                                     _unnamed_0 in
                                   fun e  ->
                                     let (_loc_e,e) = e in
                                     let _loc =
                                       merge [_loc__unnamed_0; _loc_e] in
                                     e))
                             (Glr.fixpoint []
                                (Glr.apply (fun x  l  -> x :: l)
                                   (Glr.sequence (locate semi_col)
                                      (locate (expression_lvl (next_exp Seq)))
                                      (fun _unnamed_0  ->
                                         let (_loc__unnamed_0,_unnamed_0) =
                                           _unnamed_0 in
                                         fun e  ->
                                           let (_loc_e,e) = e in
                                           let _loc =
                                             merge [_loc__unnamed_0; _loc_e] in
                                           e))))
                             (fun x  l  -> x :: (List.rev l)))))
                    :: y
                  else y in
                if (lvl' > Coerce) && (lvl <= Coerce)
                then
                  (Glr.apply
                     (fun t  ->
                        let (_loc_t,t) = t in
                        let _loc = _loc_t in
                        (Seq,
                          (fun e'  ->
                             ln e' _loc
                               (match t with
                                | (Some t1,None ) -> pexp_constraint (e', t1)
                                | (t1,Some t2) -> pexp_coerce (e', t1, t2)
                                | (None ,None ) -> assert false))))
                     (locate type_coercion))
                  :: y
                else y in
              if (lvl' > Tupl) && (lvl <= Tupl)
              then
                (Glr.apply
                   (fun l  ->
                      let (_loc_l,l) = l in
                      let _loc = _loc_l in
                      (Tupl, (fun f  -> ln f _loc (Pexp_tuple (f :: l)))))
                   (locate
                      (Glr.sequence
                         (Glr.sequence (locate (Glr.string "," ","))
                            (locate (expression_lvl (next_exp Tupl)))
                            (fun _unnamed_0  ->
                               let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                               fun e  ->
                                 let (_loc_e,e) = e in
                                 let _loc = merge [_loc__unnamed_0; _loc_e] in
                                 e))
                         (Glr.fixpoint []
                            (Glr.apply (fun x  l  -> x :: l)
                               (Glr.sequence (locate (Glr.string "," ","))
                                  (locate (expression_lvl (next_exp Tupl)))
                                  (fun _unnamed_0  ->
                                     let (_loc__unnamed_0,_unnamed_0) =
                                       _unnamed_0 in
                                     fun e  ->
                                       let (_loc_e,e) = e in
                                       let _loc =
                                         merge [_loc__unnamed_0; _loc_e] in
                                       e)))) (fun x  l  -> x :: (List.rev l)))))
                :: y
              else y))
    let expression_suit =
      let f =
        memoize2'
          (fun expression_suit  lvl'  lvl  ->
             Glr.alternatives'
               [Glr.iter
                  (Glr.apply
                     (fun ((_,(p1,f1)) as _unnamed_0)  ->
                        let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                        let _loc = _loc__unnamed_0 in
                        Glr.apply
                          (fun ((_,(p2,f2)) as _unnamed_0)  ->
                             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                             let _loc = _loc__unnamed_0 in
                             (p2, (fun f  -> f2 (f1 f))))
                          (locate (expression_suit p1 lvl)))
                     (locate (expression_suit_aux lvl' lvl)));
               Glr.apply
                 (fun _unnamed_0  ->
                    let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                    let _loc = _loc__unnamed_0 in (lvl', (fun f  -> f)))
                 (locate (Glr.empty ()))]) in
      let rec res x y = f res x y in res
    let _ =
      set_expression_lvl
        (fun lvl  ->
           Glr.iter
             (Glr.apply
                (fun ((_,(lvl',e)) as _unnamed_0)  ->
                   let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                   let _loc = _loc__unnamed_0 in
                   Glr.apply
                     (fun ((_,(_,f)) as _unnamed_0)  ->
                        let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                        let _loc = _loc__unnamed_0 in f e)
                     (locate (expression_suit lvl' lvl)))
                (locate (expression_base lvl))))
    let module_expr_base =
      Glr.alternatives'
        [Glr.apply
           (fun mp  ->
              let (_loc_mp,mp) = mp in
              let _loc = _loc_mp in
              let mid = id_loc mp _loc in mexpr_loc _loc (Pmod_ident mid))
           (locate module_path);
        Glr.fsequence (locate struct_kw)
          (Glr.sequence (locate structure) (locate end_kw)
             (fun ms  ->
                let (_loc_ms,ms) = ms in
                fun _unnamed_1  ->
                  let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                  fun _unnamed_2  ->
                    let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                    let _loc =
                      merge [_loc__unnamed_2; _loc_ms; _loc__unnamed_1] in
                    mexpr_loc _loc (Pmod_structure ms)));
        Glr.fsequence (locate functor_kw)
          (Glr.fsequence (locate (Glr.string "(" "("))
             (Glr.fsequence (locate module_name)
                (Glr.fsequence (locate (Glr.string ":" ":"))
                   (Glr.fsequence (locate module_type)
                      (Glr.fsequence (locate (Glr.string ")" ")"))
                         (Glr.sequence (locate (Glr.string "->" "->"))
                            (locate module_expr)
                            (fun _unnamed_0  ->
                               let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                               fun me  ->
                                 let (_loc_me,me) = me in
                                 fun _unnamed_2  ->
                                   let (_loc__unnamed_2,_unnamed_2) =
                                     _unnamed_2 in
                                   fun mt  ->
                                     let (_loc_mt,mt) = mt in
                                     fun _unnamed_4  ->
                                       let (_loc__unnamed_4,_unnamed_4) =
                                         _unnamed_4 in
                                       fun mn  ->
                                         let (_loc_mn,mn) = mn in
                                         fun _unnamed_6  ->
                                           let (_loc__unnamed_6,_unnamed_6) =
                                             _unnamed_6 in
                                           fun _unnamed_7  ->
                                             let (_loc__unnamed_7,_unnamed_7)
                                               = _unnamed_7 in
                                             let _loc =
                                               merge
                                                 [_loc__unnamed_7;
                                                 _loc__unnamed_6;
                                                 _loc_mn;
                                                 _loc__unnamed_4;
                                                 _loc_mt;
                                                 _loc__unnamed_2;
                                                 _loc__unnamed_0;
                                                 _loc_me] in
                                             mexpr_loc _loc
                                               (Pmod_functor
                                                  ((id_loc mn _loc_mn), mt,
                                                    me)))))))));
        Glr.fsequence (locate (Glr.string "(" "("))
          (Glr.fsequence (locate module_expr)
             (Glr.sequence
                (locate
                   (Glr.option None
                      (Glr.apply (fun x  -> Some x)
                         (Glr.sequence (locate (Glr.string ":" ":"))
                            (locate module_type)
                            (fun _unnamed_0  ->
                               let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                               fun mt  ->
                                 let (_loc_mt,mt) = mt in
                                 let _loc = merge [_loc__unnamed_0; _loc_mt] in
                                 mt))))) (locate (Glr.string ")" ")"))
                (fun mt  ->
                   let (_loc_mt,mt) = mt in
                   fun _unnamed_1  ->
                     let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                     fun me  ->
                       let (_loc_me,me) = me in
                       fun _unnamed_3  ->
                         let (_loc__unnamed_3,_unnamed_3) = _unnamed_3 in
                         let _loc =
                           merge
                             [_loc__unnamed_3;
                             _loc_me;
                             _loc_mt;
                             _loc__unnamed_1] in
                         match mt with
                         | None  -> me
                         | Some mt ->
                             mexpr_loc _loc (Pmod_constraint (me, mt)))));
        Glr.fsequence (locate (Glr.string "(" "("))
          (Glr.fsequence (locate val_kw)
             (Glr.fsequence (locate expr)
                (Glr.sequence
                   (locate
                      (Glr.option None
                         (Glr.apply (fun x  -> Some x)
                            (Glr.sequence (locate (Glr.string ":" ":"))
                               (locate package_type)
                               (fun _unnamed_0  ->
                                  let (_loc__unnamed_0,_unnamed_0) =
                                    _unnamed_0 in
                                  fun pt  ->
                                    let (_loc_pt,pt) = pt in
                                    let _loc =
                                      merge [_loc__unnamed_0; _loc_pt] in
                                    pt))))) (locate (Glr.string ")" ")"))
                   (fun pt  ->
                      let (_loc_pt,pt) = pt in
                      fun _unnamed_1  ->
                        let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                        fun e  ->
                          let (_loc_e,e) = e in
                          fun _unnamed_3  ->
                            let (_loc__unnamed_3,_unnamed_3) = _unnamed_3 in
                            fun _unnamed_4  ->
                              let (_loc__unnamed_4,_unnamed_4) = _unnamed_4 in
                              let _loc =
                                merge
                                  [_loc__unnamed_4;
                                  _loc__unnamed_3;
                                  _loc_e;
                                  _loc_pt;
                                  _loc__unnamed_1] in
                              let e =
                                match pt with
                                | Some (Ptyp_package (n,l)) ->
                                    Pmod_unpack (e, (n, l))
                                | _ -> raise Give_up in
                              mexpr_loc _loc e))))]
    let _ =
      set_grammar module_expr
        (Glr.sequence (locate module_expr_base)
           (locate
              (Glr.apply List.rev
                 (Glr.fixpoint []
                    (Glr.apply (fun x  l  -> x :: l)
                       (Glr.fsequence (locate (Glr.string "(" "("))
                          (Glr.sequence (locate module_expr)
                             (locate (Glr.string ")" ")"))
                             (fun m  ->
                                let (_loc_m,m) = m in
                                fun _unnamed_1  ->
                                  let (_loc__unnamed_1,_unnamed_1) =
                                    _unnamed_1 in
                                  fun _unnamed_2  ->
                                    let (_loc__unnamed_2,_unnamed_2) =
                                      _unnamed_2 in
                                    let _loc =
                                      merge
                                        [_loc__unnamed_2;
                                        _loc_m;
                                        _loc__unnamed_1] in
                                    (_loc, m))))))))
           (fun m  ->
              let (_loc_m,m) = m in
              fun l  ->
                let (_loc_l,l) = l in
                let _loc = merge [_loc_m; _loc_l] in
                List.fold_left
                  (fun acc  (_loc_n,n)  ->
                     mexpr_loc (merge2 _loc_m _loc_n) (Pmod_apply (acc, n)))
                  m l))
    let module_type_base =
      Glr.alternatives'
        [Glr.apply
           (fun mp  ->
              let (_loc_mp,mp) = mp in
              let _loc = _loc_mp in
              let mid = id_loc mp _loc in mtyp_loc _loc (Pmty_ident mid))
           (locate modtype_path);
        Glr.fsequence (locate sig_kw)
          (Glr.sequence (locate signature) (locate end_kw)
             (fun ms  ->
                let (_loc_ms,ms) = ms in
                fun _unnamed_1  ->
                  let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                  fun _unnamed_2  ->
                    let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                    let _loc =
                      merge [_loc__unnamed_2; _loc_ms; _loc__unnamed_1] in
                    mtyp_loc _loc (Pmty_signature ms)));
        Glr.fsequence (locate functor_kw)
          (Glr.fsequence (locate (Glr.string "(" "("))
             (Glr.fsequence (locate module_name)
                (Glr.fsequence (locate (Glr.string ":" ":"))
                   (Glr.fsequence (locate module_type)
                      (Glr.fsequence (locate (Glr.string ")" ")"))
                         (Glr.sequence (locate (Glr.string "->" "->"))
                            (locate module_type)
                            (fun _unnamed_0  ->
                               let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                               fun me  ->
                                 let (_loc_me,me) = me in
                                 fun _unnamed_2  ->
                                   let (_loc__unnamed_2,_unnamed_2) =
                                     _unnamed_2 in
                                   fun mt  ->
                                     let (_loc_mt,mt) = mt in
                                     fun _unnamed_4  ->
                                       let (_loc__unnamed_4,_unnamed_4) =
                                         _unnamed_4 in
                                       fun mn  ->
                                         let (_loc_mn,mn) = mn in
                                         fun _unnamed_6  ->
                                           let (_loc__unnamed_6,_unnamed_6) =
                                             _unnamed_6 in
                                           fun _unnamed_7  ->
                                             let (_loc__unnamed_7,_unnamed_7)
                                               = _unnamed_7 in
                                             let _loc =
                                               merge
                                                 [_loc__unnamed_7;
                                                 _loc__unnamed_6;
                                                 _loc_mn;
                                                 _loc__unnamed_4;
                                                 _loc_mt;
                                                 _loc__unnamed_2;
                                                 _loc__unnamed_0;
                                                 _loc_me] in
                                             mtyp_loc _loc
                                               (Pmty_functor
                                                  ((id_loc mn _loc_mn), mt,
                                                    me)))))))));
        Glr.fsequence (locate (Glr.string "(" "("))
          (Glr.sequence (locate module_type) (locate (Glr.string ")" ")"))
             (fun mt  ->
                let (_loc_mt,mt) = mt in
                fun _unnamed_1  ->
                  let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                  fun _unnamed_2  ->
                    let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                    let _loc =
                      merge [_loc__unnamed_2; _loc_mt; _loc__unnamed_1] in
                    mt));
        Glr.fsequence (locate module_kw)
          (Glr.fsequence (locate type_kw)
             (Glr.sequence (locate of_kw) (locate module_expr)
                (fun _unnamed_0  ->
                   let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                   fun me  ->
                     let (_loc_me,me) = me in
                     fun _unnamed_2  ->
                       let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                       fun _unnamed_3  ->
                         let (_loc__unnamed_3,_unnamed_3) = _unnamed_3 in
                         let _loc =
                           merge
                             [_loc__unnamed_3;
                             _loc__unnamed_2;
                             _loc__unnamed_0;
                             _loc_me] in
                         mtyp_loc _loc (Pmty_typeof me))))]
    let mod_constraint =
      Glr.alternatives'
        [Glr.iter
           (Glr.apply
              (fun t  ->
                 let (_loc_t,t) = t in
                 let _loc = _loc_t in
                 Glr.apply
                   (fun ((_,(tn,ty)) as _unnamed_0)  ->
                      let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                      let _loc = _loc__unnamed_0 in (tn, (Pwith_type ty)))
                   (locate (typedef_in_constraint _loc_t))) (locate type_kw));
        Glr.fsequence (locate module_kw)
          (Glr.fsequence (locate module_path)
             (Glr.sequence (locate (Glr.char '=' '='))
                (locate extended_module_path)
                (fun _unnamed_0  ->
                   let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                   fun m2  ->
                     let (_loc_m2,m2) = m2 in
                     fun m1  ->
                       let (_loc_m1,m1) = m1 in
                       fun _unnamed_3  ->
                         let (_loc__unnamed_3,_unnamed_3) = _unnamed_3 in
                         let _loc =
                           merge
                             [_loc__unnamed_3;
                             _loc_m1;
                             _loc__unnamed_0;
                             _loc_m2] in
                         let name = id_loc m1 _loc_m1 in
                         (name, (Pwith_module (id_loc m2 _loc_m2))))));
        Glr.fsequence (locate type_kw)
          (Glr.fsequence (locate (Glr.option [] type_params))
             (Glr.fsequence (locate typeconstr_name)
                (Glr.sequence (locate (Glr.string ":=" ":="))
                   (locate typexpr)
                   (fun _unnamed_0  ->
                      let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                      fun te  ->
                        let (_loc_te,te) = te in
                        fun tcn  ->
                          let (_loc_tcn,tcn) = tcn in
                          fun tps  ->
                            let (_loc_tps,tps) = tps in
                            fun _unnamed_4  ->
                              let (_loc__unnamed_4,_unnamed_4) = _unnamed_4 in
                              let _loc =
                                merge
                                  [_loc__unnamed_4;
                                  _loc_tps;
                                  _loc_tcn;
                                  _loc__unnamed_0;
                                  _loc_te] in
                              let tps =
                                List.map
                                  (function
                                   | (Some s,t) -> (s, t)
                                   | (None ,_) -> raise Give_up) tps in
                              let td =
                                type_declaration _loc (id_loc tcn _loc_tcn)
                                  tps [] Ptype_abstract Public (Some te) in
                              ((id_loc (Lident tcn) _loc_tcn),
                                (Pwith_typesubst td))))));
        Glr.fsequence (locate module_kw)
          (Glr.fsequence (locate module_name)
             (Glr.sequence (locate (Glr.string ":=" ":="))
                (locate extended_module_path)
                (fun _unnamed_0  ->
                   let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                   fun emp  ->
                     let (_loc_emp,emp) = emp in
                     fun mn  ->
                       let (_loc_mn,mn) = mn in
                       fun _unnamed_3  ->
                         let (_loc__unnamed_3,_unnamed_3) = _unnamed_3 in
                         let _loc =
                           merge
                             [_loc__unnamed_3;
                             _loc_mn;
                             _loc__unnamed_0;
                             _loc_emp] in
                         ((id_loc (Lident mn) _loc_mn),
                           (Pwith_modsubst (id_loc emp _loc_emp))))))]
    let _ =
      set_grammar module_type
        (Glr.sequence (locate module_type_base)
           (locate
              (Glr.option None
                 (Glr.apply (fun x  -> Some x)
                    (Glr.fsequence (locate with_kw)
                       (Glr.sequence (locate mod_constraint)
                          (locate
                             (Glr.apply List.rev
                                (Glr.fixpoint []
                                   (Glr.apply (fun x  l  -> x :: l)
                                      (Glr.sequence (locate and_kw)
                                         (locate mod_constraint)
                                         (fun _unnamed_0  ->
                                            let (_loc__unnamed_0,_unnamed_0)
                                              = _unnamed_0 in
                                            fun m  ->
                                              let (_loc_m,m) = m in
                                              let _loc =
                                                merge
                                                  [_loc__unnamed_0; _loc_m] in
                                              m))))))
                          (fun m  ->
                             let (_loc_m,m) = m in
                             fun l  ->
                               let (_loc_l,l) = l in
                               fun _unnamed_2  ->
                                 let (_loc__unnamed_2,_unnamed_2) =
                                   _unnamed_2 in
                                 let _loc =
                                   merge [_loc__unnamed_2; _loc_m; _loc_l] in
                                 m :: l))))))
           (fun m  ->
              let (_loc_m,m) = m in
              fun l  ->
                let (_loc_l,l) = l in
                let _loc = merge [_loc_m; _loc_l] in
                match l with
                | None  -> m
                | Some l -> mtyp_loc _loc (Pmty_with (m, l))))
    let structure_item_base =
      Glr.alternatives'
        [Glr.fsequence
           (locate (Glr.regexp ~name:"let" let_re (fun groupe  -> groupe 0)))
           (Glr.sequence (locate rec_flag) (locate let_binding)
              (fun r  ->
                 let (_loc_r,r) = r in
                 fun l  ->
                   let (_loc_l,l) = l in
                   fun _unnamed_2  ->
                     let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                     let _loc = merge [_loc__unnamed_2; _loc_r; _loc_l] in
                     match l with
                     | ({ ppat_desc = Ppat_any ; ppat_loc = _ },e)::[] ->
                         pstr_eval e
                     | _ -> Pstr_value (r, l)));
        Glr.fsequence (locate external_kw)
          (Glr.fsequence (locate value_name)
             (Glr.fsequence (locate (Glr.string ":" ":"))
                (Glr.fsequence (locate typexpr)
                   (Glr.sequence (locate (Glr.string "=" "="))
                      (locate
                         (Glr.apply List.rev
                            (Glr.fixpoint []
                               (Glr.apply (fun x  l  -> x :: l)
                                  string_literal))))
                      (fun _unnamed_0  ->
                         let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                         fun ls  ->
                           let (_loc_ls,ls) = ls in
                           fun ty  ->
                             let (_loc_ty,ty) = ty in
                             fun _unnamed_3  ->
                               let (_loc__unnamed_3,_unnamed_3) = _unnamed_3 in
                               fun n  ->
                                 let (_loc_n,n) = n in
                                 fun _unnamed_5  ->
                                   let (_loc__unnamed_5,_unnamed_5) =
                                     _unnamed_5 in
                                   let _loc =
                                     merge
                                       [_loc__unnamed_5;
                                       _loc_n;
                                       _loc__unnamed_3;
                                       _loc_ty;
                                       _loc__unnamed_0;
                                       _loc_ls] in
                                   let l = List.length ls in
                                   if (l < 1) || (l > 3) then raise Give_up;
                                   Pstr_primitive
                                     ((id_loc n _loc_n),
                                       { pval_type = ty; pval_prim = ls }))))));
        Glr.apply
          (fun td  ->
             let (_loc_td,td) = td in let _loc = _loc_td in Pstr_type td)
          (locate type_definition);
        Glr.apply
          (fun ex  -> let (_loc_ex,ex) = ex in let _loc = _loc_ex in ex)
          (locate exception_definition);
        Glr.sequence (locate module_kw)
          (locate
             (Glr.alternatives'
                [Glr.fsequence (locate rec_kw)
                   (Glr.fsequence (locate module_name)
                      (Glr.fsequence (locate (Glr.string ":" ":"))
                         (Glr.fsequence (locate module_type)
                            (Glr.fsequence (locate (Glr.char '=' '='))
                               (Glr.sequence (locate module_expr)
                                  (locate
                                     (Glr.apply List.rev
                                        (Glr.fixpoint []
                                           (Glr.apply (fun x  l  -> x :: l)
                                              (Glr.fsequence (locate and_kw)
                                                 (Glr.fsequence
                                                    (locate module_name)
                                                    (Glr.fsequence
                                                       (locate
                                                          (Glr.string ":" ":"))
                                                       (Glr.fsequence
                                                          (locate module_type)
                                                          (Glr.sequence
                                                             (locate
                                                                (Glr.char '='
                                                                   '='))
                                                             (locate
                                                                module_expr)
                                                             (fun _unnamed_0 
                                                                ->
                                                                let (_loc__unnamed_0,_unnamed_0)
                                                                  =
                                                                  _unnamed_0 in
                                                                fun me  ->
                                                                  let 
                                                                    (_loc_me,me)
                                                                    = me in
                                                                  fun mt  ->
                                                                    let 
                                                                    (_loc_mt,mt)
                                                                    = mt in
                                                                    fun
                                                                    _unnamed_3
                                                                     ->
                                                                    let 
                                                                    (_loc__unnamed_3,_unnamed_3)
                                                                    =
                                                                    _unnamed_3 in
                                                                    fun mn 
                                                                    ->
                                                                    let 
                                                                    (_loc_mn,mn)
                                                                    = mn in
                                                                    fun
                                                                    _unnamed_5
                                                                     ->
                                                                    let 
                                                                    (_loc__unnamed_5,_unnamed_5)
                                                                    =
                                                                    _unnamed_5 in
                                                                    let _loc
                                                                    =
                                                                    merge
                                                                    [_loc__unnamed_5;
                                                                    _loc_mn;
                                                                    _loc__unnamed_3;
                                                                    _loc_mt;
                                                                    _loc__unnamed_0;
                                                                    _loc_me] in
                                                                    module_binding
                                                                    _loc
                                                                    (id_loc
                                                                    mn
                                                                    _loc_mn)
                                                                    mt me))))))))))
                                  (fun me  ->
                                     let (_loc_me,me) = me in
                                     fun ms  ->
                                       let (_loc_ms,ms) = ms in
                                       fun _unnamed_2  ->
                                         let (_loc__unnamed_2,_unnamed_2) =
                                           _unnamed_2 in
                                         fun mt  ->
                                           let (_loc_mt,mt) = mt in
                                           fun _unnamed_4  ->
                                             let (_loc__unnamed_4,_unnamed_4)
                                               = _unnamed_4 in
                                             fun mn  ->
                                               let (_loc_mn,mn) = mn in
                                               fun _unnamed_6  ->
                                                 let (_loc__unnamed_6,_unnamed_6)
                                                   = _unnamed_6 in
                                                 let _loc =
                                                   merge
                                                     [_loc__unnamed_6;
                                                     _loc_mn;
                                                     _loc__unnamed_4;
                                                     _loc_mt;
                                                     _loc__unnamed_2;
                                                     _loc_me;
                                                     _loc_ms] in
                                                 let m =
                                                   module_binding _loc
                                                     (id_loc mn _loc_mn) mt
                                                     me in
                                                 Pstr_recmodule (m :: ms)))))));
                Glr.fsequence (locate module_name)
                  (Glr.fsequence
                     (locate
                        (Glr.apply List.rev
                           (Glr.fixpoint []
                              (Glr.apply (fun x  l  -> x :: l)
                                 (Glr.fsequence (locate (Glr.string "(" "("))
                                    (Glr.fsequence (locate module_name)
                                       (Glr.fsequence
                                          (locate (Glr.string ":" ":"))
                                          (Glr.sequence (locate module_type)
                                             (locate (Glr.string ")" ")"))
                                             (fun mt  ->
                                                let (_loc_mt,mt) = mt in
                                                fun _unnamed_1  ->
                                                  let (_loc__unnamed_1,_unnamed_1)
                                                    = _unnamed_1 in
                                                  fun _unnamed_2  ->
                                                    let (_loc__unnamed_2,_unnamed_2)
                                                      = _unnamed_2 in
                                                    fun mn  ->
                                                      let (_loc_mn,mn) = mn in
                                                      fun _unnamed_4  ->
                                                        let (_loc__unnamed_4,_unnamed_4)
                                                          = _unnamed_4 in
                                                        let _loc =
                                                          merge
                                                            [_loc__unnamed_4;
                                                            _loc_mn;
                                                            _loc__unnamed_2;
                                                            _loc_mt;
                                                            _loc__unnamed_1] in
                                                        ((id_loc mn _loc_mn),
                                                          mt, _loc))))))))))
                     (Glr.fsequence
                        (locate
                           (Glr.option None
                              (Glr.apply (fun x  -> Some x)
                                 (Glr.sequence (locate (Glr.string ":" ":"))
                                    (locate module_type)
                                    (fun _unnamed_0  ->
                                       let (_loc__unnamed_0,_unnamed_0) =
                                         _unnamed_0 in
                                       fun mt  ->
                                         let (_loc_mt,mt) = mt in
                                         let _loc =
                                           merge [_loc__unnamed_0; _loc_mt] in
                                         mt)))))
                        (Glr.sequence (locate (Glr.string "=" "="))
                           (locate module_expr)
                           (fun _unnamed_0  ->
                              let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                              fun me  ->
                                let (_loc_me,me) = me in
                                fun mt  ->
                                  let (_loc_mt,mt) = mt in
                                  fun l  ->
                                    let (_loc_l,l) = l in
                                    fun mn  ->
                                      let (_loc_mn,mn) = mn in
                                      let _loc =
                                        merge
                                          [_loc_mn;
                                          _loc_l;
                                          _loc_mt;
                                          _loc__unnamed_0;
                                          _loc_me] in
                                      let me =
                                        match mt with
                                        | None  -> me
                                        | Some mt ->
                                            mexpr_loc
                                              (merge2 _loc_mt _loc_me)
                                              (Pmod_constraint (me, mt)) in
                                      let me =
                                        List.fold_left
                                          (fun acc  (mn,mt,_loc)  ->
                                             mexpr_loc (merge2 _loc _loc_me)
                                               (Pmod_functor (mn, mt, acc)))
                                          me (List.rev l) in
                                      let (name,_,me) =
                                        module_binding _loc
                                          (id_loc mn _loc_mn) None me in
                                      Pstr_module (name, me)))));
                Glr.fsequence (locate type_kw)
                  (Glr.fsequence (locate modtype_name)
                     (Glr.sequence (locate (Glr.string "=" "="))
                        (locate module_type)
                        (fun _unnamed_0  ->
                           let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                           fun mt  ->
                             let (_loc_mt,mt) = mt in
                             fun mn  ->
                               let (_loc_mn,mn) = mn in
                               fun _unnamed_3  ->
                                 let (_loc__unnamed_3,_unnamed_3) =
                                   _unnamed_3 in
                                 let _loc =
                                   merge
                                     [_loc__unnamed_3;
                                     _loc_mn;
                                     _loc__unnamed_0;
                                     _loc_mt] in
                                 Pstr_modtype ((id_loc mn _loc_mn), mt))))]))
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun r  ->
               let (_loc_r,r) = r in
               let _loc = merge [_loc__unnamed_0; _loc_r] in r);
        Glr.fsequence (locate open_kw)
          (Glr.sequence (locate override_flag) (locate module_path)
             (fun o  ->
                let (_loc_o,o) = o in
                fun m  ->
                  let (_loc_m,m) = m in
                  fun _unnamed_2  ->
                    let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                    let _loc = merge [_loc__unnamed_2; _loc_o; _loc_m] in
                    Pstr_open (id_loc m _loc_m)));
        Glr.sequence (locate include_kw) (locate module_expr)
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun me  ->
               let (_loc_me,me) = me in
               let _loc = merge [_loc__unnamed_0; _loc_me] in Pstr_include me);
        Glr.sequence (locate class_kw)
          (locate
             (Glr.alternatives'
                [Glr.apply
                   (fun ctd  ->
                      let (_loc_ctd,ctd) = ctd in
                      let _loc = _loc_ctd in Pstr_class_type ctd)
                   (locate classtype_definition);
                Glr.apply
                  (fun cds  ->
                     let (_loc_cds,cds) = cds in
                     let _loc = _loc_cds in Pstr_class cds)
                  (locate class_definition)]))
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun r  ->
               let (_loc_r,r) = r in
               let _loc = merge [_loc__unnamed_0; _loc_r] in r);
        Glr.apply
          (fun e  -> let (_loc_e,e) = e in let _loc = _loc_e in pstr_eval e)
          (locate expression)]
    let _ =
      set_grammar structure_item
        (Glr.alternatives'
           [Glr.apply
              (fun e  -> let (_loc_e,e) = e in let _loc = _loc_e in e)
              (locate (alternatives extra_structure));
           Glr.fsequence (locate (Glr.char '$' '$'))
             (Glr.fsequence (locate (expression_lvl (next_exp App)))
                (Glr.sequence (locate (Glr.char '$' '$'))
                   (locate
                      (Glr.option None
                         (Glr.apply (fun x  -> Some x) (Glr.string ";;" ";;"))))
                   (fun _unnamed_0  ->
                      let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                      fun _unnamed_1  ->
                        let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                        fun e  ->
                          let (_loc_e,e) = e in
                          fun _unnamed_3  ->
                            let (_loc__unnamed_3,_unnamed_3) = _unnamed_3 in
                            let _loc =
                              merge
                                [_loc__unnamed_3;
                                _loc_e;
                                _loc__unnamed_0;
                                _loc__unnamed_1] in
                            push_pop_structure e)));
           Glr.sequence (locate structure_item_base)
             (locate
                (Glr.option None
                   (Glr.apply (fun x  -> Some x) (Glr.string ";;" ";;"))))
             (fun s  ->
                let (_loc_s,s) = s in
                fun _unnamed_1  ->
                  let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                  let _loc = merge [_loc_s; _loc__unnamed_1] in
                  [loc_str _loc_s s])])
    let signature_item_base =
      Glr.alternatives'
        [Glr.fsequence (locate val_kw)
           (Glr.fsequence (locate value_name)
              (Glr.fsequence (locate (Glr.string ":" ":"))
                 (Glr.sequence (locate typexpr) (locate post_item_attributes)
                    (fun ty  ->
                       let (_loc_ty,ty) = ty in
                       fun a  ->
                         let (_loc_a,a) = a in
                         fun _unnamed_2  ->
                           let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                           fun n  ->
                             let (_loc_n,n) = n in
                             fun _unnamed_4  ->
                               let (_loc__unnamed_4,_unnamed_4) = _unnamed_4 in
                               let _loc =
                                 merge
                                   [_loc__unnamed_4;
                                   _loc_n;
                                   _loc__unnamed_2;
                                   _loc_ty;
                                   _loc_a] in
                               psig_value ~attributes:a _loc
                                 (id_loc n _loc_n) ty []))));
        Glr.fsequence (locate external_kw)
          (Glr.fsequence (locate value_name)
             (Glr.fsequence (locate (Glr.string ":" ":"))
                (Glr.fsequence (locate typexpr)
                   (Glr.fsequence (locate (Glr.string "=" "="))
                      (Glr.sequence
                         (locate
                            (Glr.apply List.rev
                               (Glr.fixpoint []
                                  (Glr.apply (fun x  l  -> x :: l)
                                     string_literal))))
                         (locate post_item_attributes)
                         (fun ls  ->
                            let (_loc_ls,ls) = ls in
                            fun a  ->
                              let (_loc_a,a) = a in
                              fun _unnamed_2  ->
                                let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                                fun ty  ->
                                  let (_loc_ty,ty) = ty in
                                  fun _unnamed_4  ->
                                    let (_loc__unnamed_4,_unnamed_4) =
                                      _unnamed_4 in
                                    fun n  ->
                                      let (_loc_n,n) = n in
                                      fun _unnamed_6  ->
                                        let (_loc__unnamed_6,_unnamed_6) =
                                          _unnamed_6 in
                                        let _loc =
                                          merge
                                            [_loc__unnamed_6;
                                            _loc_n;
                                            _loc__unnamed_4;
                                            _loc_ty;
                                            _loc__unnamed_2;
                                            _loc_ls;
                                            _loc_a] in
                                        let l = List.length ls in
                                        if (l < 1) || (l > 3)
                                        then raise Give_up;
                                        psig_value ~attributes:a _loc
                                          (id_loc n _loc_n) ty ls))))));
        Glr.apply
          (fun td  ->
             let (_loc_td,td) = td in let _loc = _loc_td in Psig_type td)
          (locate type_definition);
        Glr.apply
          (fun ((_,(name,ed,_loc')) as _unnamed_0)  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             let _loc = _loc__unnamed_0 in Psig_exception (name, ed))
          (locate exception_declaration);
        Glr.fsequence (locate module_kw)
          (Glr.fsequence (locate rec_kw)
             (Glr.fsequence (locate module_name)
                (Glr.fsequence (locate (Glr.string ":" ":"))
                   (Glr.sequence (locate module_type)
                      (locate
                         (Glr.apply List.rev
                            (Glr.fixpoint []
                               (Glr.apply (fun x  l  -> x :: l)
                                  (Glr.fsequence (locate and_kw)
                                     (Glr.fsequence (locate module_name)
                                        (Glr.sequence
                                           (locate (Glr.string ":" ":"))
                                           (locate module_type)
                                           (fun _unnamed_0  ->
                                              let (_loc__unnamed_0,_unnamed_0)
                                                = _unnamed_0 in
                                              fun mt  ->
                                                let (_loc_mt,mt) = mt in
                                                fun mn  ->
                                                  let (_loc_mn,mn) = mn in
                                                  fun _unnamed_3  ->
                                                    let (_loc__unnamed_3,_unnamed_3)
                                                      = _unnamed_3 in
                                                    let _loc =
                                                      merge
                                                        [_loc__unnamed_3;
                                                        _loc_mn;
                                                        _loc__unnamed_0;
                                                        _loc_mt] in
                                                    module_declaration _loc
                                                      (id_loc mn _loc_mn) mt))))))))
                      (fun mt  ->
                         let (_loc_mt,mt) = mt in
                         fun ms  ->
                           let (_loc_ms,ms) = ms in
                           fun _unnamed_2  ->
                             let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                             fun mn  ->
                               let (_loc_mn,mn) = mn in
                               fun _unnamed_4  ->
                                 let (_loc__unnamed_4,_unnamed_4) =
                                   _unnamed_4 in
                                 fun _unnamed_5  ->
                                   let (_loc__unnamed_5,_unnamed_5) =
                                     _unnamed_5 in
                                   let _loc =
                                     merge
                                       [_loc__unnamed_5;
                                       _loc__unnamed_4;
                                       _loc_mn;
                                       _loc__unnamed_2;
                                       _loc_mt;
                                       _loc_ms] in
                                   let m =
                                     module_declaration _loc
                                       (id_loc mn _loc_mn) mt in
                                   Psig_recmodule (m :: ms))))));
        Glr.sequence (locate module_kw)
          (locate
             (Glr.alternatives'
                [Glr.fsequence (locate module_name)
                   (Glr.fsequence
                      (locate
                         (Glr.apply List.rev
                            (Glr.fixpoint []
                               (Glr.apply (fun x  l  -> x :: l)
                                  (Glr.fsequence
                                     (locate (Glr.string "(" "("))
                                     (Glr.fsequence (locate module_name)
                                        (Glr.fsequence
                                           (locate (Glr.string ":" ":"))
                                           (Glr.sequence (locate module_type)
                                              (locate (Glr.string ")" ")"))
                                              (fun mt  ->
                                                 let (_loc_mt,mt) = mt in
                                                 fun _unnamed_1  ->
                                                   let (_loc__unnamed_1,_unnamed_1)
                                                     = _unnamed_1 in
                                                   fun _unnamed_2  ->
                                                     let (_loc__unnamed_2,_unnamed_2)
                                                       = _unnamed_2 in
                                                     fun mn  ->
                                                       let (_loc_mn,mn) = mn in
                                                       fun _unnamed_4  ->
                                                         let (_loc__unnamed_4,_unnamed_4)
                                                           = _unnamed_4 in
                                                         let _loc =
                                                           merge
                                                             [_loc__unnamed_4;
                                                             _loc_mn;
                                                             _loc__unnamed_2;
                                                             _loc_mt;
                                                             _loc__unnamed_1] in
                                                         ((id_loc mn _loc_mn),
                                                           mt, _loc))))))))))
                      (Glr.sequence (locate (Glr.string ":" ":"))
                         (locate module_type)
                         (fun _unnamed_0  ->
                            let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                            fun mt  ->
                              let (_loc_mt,mt) = mt in
                              fun l  ->
                                let (_loc_l,l) = l in
                                fun mn  ->
                                  let (_loc_mn,mn) = mn in
                                  let _loc =
                                    merge
                                      [_loc_mn;
                                      _loc_l;
                                      _loc__unnamed_0;
                                      _loc_mt] in
                                  let mt =
                                    List.fold_left
                                      (fun acc  (mn,mt,_loc)  ->
                                         mtyp_loc (merge2 _loc _loc_mt)
                                           (Pmty_functor (mn, mt, acc))) mt
                                      (List.rev l) in
                                  let (a,b) =
                                    module_declaration _loc
                                      (id_loc mn _loc_mn) mt in
                                  Psig_module (a, b))));
                Glr.fsequence (locate type_kw)
                  (Glr.sequence (locate modtype_name)
                     (locate
                        (Glr.option None
                           (Glr.apply (fun x  -> Some x)
                              (Glr.sequence (locate (Glr.string "=" "="))
                                 (locate module_type)
                                 (fun _unnamed_0  ->
                                    let (_loc__unnamed_0,_unnamed_0) =
                                      _unnamed_0 in
                                    fun mt  ->
                                      let (_loc_mt,mt) = mt in
                                      let _loc =
                                        merge [_loc__unnamed_0; _loc_mt] in
                                      mt)))))
                     (fun mn  ->
                        let (_loc_mn,mn) = mn in
                        fun mt  ->
                          let (_loc_mt,mt) = mt in
                          fun _unnamed_2  ->
                            let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                            let _loc =
                              merge [_loc__unnamed_2; _loc_mn; _loc_mt] in
                            let mt =
                              match mt with
                              | None  -> Pmodtype_abstract
                              | Some mt -> Pmodtype_manifest mt in
                            Psig_modtype ((id_loc mn _loc_mn), mt)))]))
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun r  ->
               let (_loc_r,r) = r in
               let _loc = merge [_loc__unnamed_0; _loc_r] in r);
        Glr.fsequence (locate open_kw)
          (Glr.sequence (locate override_flag) (locate module_path)
             (fun o  ->
                let (_loc_o,o) = o in
                fun m  ->
                  let (_loc_m,m) = m in
                  fun _unnamed_2  ->
                    let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                    let _loc = merge [_loc__unnamed_2; _loc_o; _loc_m] in
                    Psig_open (id_loc m _loc_m)));
        Glr.sequence (locate include_kw) (locate module_type)
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun me  ->
               let (_loc_me,me) = me in
               let _loc = merge [_loc__unnamed_0; _loc_me] in Psig_include me);
        Glr.sequence (locate class_kw)
          (locate
             (Glr.alternatives'
                [Glr.apply
                   (fun ctd  ->
                      let (_loc_ctd,ctd) = ctd in
                      let _loc = _loc_ctd in Psig_class_type ctd)
                   (locate classtype_definition);
                Glr.apply
                  (fun cs  ->
                     let (_loc_cs,cs) = cs in
                     let _loc = _loc_cs in Psig_class cs)
                  (locate class_specification)]))
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun r  ->
               let (_loc_r,r) = r in
               let _loc = merge [_loc__unnamed_0; _loc_r] in r)]
    let _ =
      set_grammar signature_item
        (Glr.alternatives'
           [Glr.apply
              (fun e  -> let (_loc_e,e) = e in let _loc = _loc_e in e)
              (locate (alternatives extra_signature));
           Glr.fsequence (locate (Glr.char '$' '$'))
             (Glr.sequence (locate (expression_lvl (next_exp App)))
                (locate (Glr.char '$' '$'))
                (fun e  ->
                   let (_loc_e,e) = e in
                   fun _unnamed_1  ->
                     let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                     fun _unnamed_2  ->
                       let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                       let _loc =
                         merge [_loc__unnamed_2; _loc_e; _loc__unnamed_1] in
                       push_pop_signature e));
           Glr.sequence (locate signature_item_base)
             (locate
                (Glr.option None
                   (Glr.apply (fun x  -> Some x) (Glr.string ";;" ";;"))))
             (fun s  ->
                let (_loc_s,s) = s in
                fun _unnamed_1  ->
                  let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                  let _loc = merge [_loc_s; _loc__unnamed_1] in
                  [loc_sig _loc s])])
    exception Top_Exit
    let top_phrase =
      Glr.alternatives'
        [Glr.fsequence
           (locate
              (Glr.option None
                 (Glr.apply (fun x  -> Some x) (Glr.char ';' ';'))))
           (Glr.sequence
              (locate
                 (Glr.sequence
                    (Glr.apply
                       (fun s  ->
                          let (_loc_s,s) = s in
                          let _loc = _loc_s in loc_str _loc s)
                       (locate structure_item_base))
                    (Glr.fixpoint []
                       (Glr.apply (fun x  l  -> x :: l)
                          (Glr.apply
                             (fun s  ->
                                let (_loc_s,s) = s in
                                let _loc = _loc_s in loc_str _loc s)
                             (locate structure_item_base))))
                    (fun x  l  -> x :: (List.rev l))))
              (locate double_semi_col)
              (fun l  ->
                 let (_loc_l,l) = l in
                 fun _unnamed_1  ->
                   let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                   fun _unnamed_2  ->
                     let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                     let _loc =
                       merge [_loc__unnamed_2; _loc_l; _loc__unnamed_1] in
                     Ptop_def l));
        Glr.sequence
          (locate
             (Glr.option None
                (Glr.apply (fun x  -> Some x) (Glr.char ';' ';'))))
          (locate (Glr.eof ()))
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun _unnamed_1  ->
               let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
               let _loc = merge [_loc__unnamed_0; _loc__unnamed_1] in
               raise Top_Exit)]
  end
