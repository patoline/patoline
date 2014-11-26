== Extending the OCaml parser ==

The BNF-like syntax described in the previous sections is implemented as an
OCaml extension written using the tool in itself. This means that Decap comes
with an extensible OCaml parser similar to what Camlp4 provides. Our parser
aims at being (almost) fully compatibility with OCaml syntax together, with
the added BNF extension. Here is a list of some known (and deliberate)
differences with the original OCaml parser:
\begin{itemize}
\item ##val## may be use in place of ##let## for struture fields,
\item type coercion are allowed in expression and pattern without parenthesis.
      The priority level used is between sequences and conditionals, which
      means that ##a ; if b then c else d : t## is parsed as
      ##a ; if b then c else (d : t)##.
      For patterns, type coercion is at the lowest priority level:
      ##a , b : t## means ##(a, b) : t##.
\item A preprocessor is available. This may break some code if a "#" symbol
      occurs at a beginning of a line.
\end{itemize} 

The supported preprocessor directives, are listed bellow. Note that the "#"
character needs to be the first character on the line.

\begin{itemize}
\item The line number indication directive as in OCaml.
\item ``#define macro value`` defining a macro with no parameter
\item ``#ifdef macro`` and ``#ifundef macro`` which are classical
\item ``#ifversion [<|>|=|<=|>=] XX.XX`` to test the ocaml version.
\item ``#endif``, ``#else`` and ``#elif ...`` to close the conditional directive.
\end{itemize}

Moreover, OCaml can access the environment variables ##VAR## with the syntax
##$VAR##. In an expression or a pattern, it is replaced by the value of the
given environment variable (there should be no space between the dollar sign
and the first letter of the variable name (which must be a capital letter).
The value of the variable is not considered as a string, it is parsed as an
OCaml expression or pattern. Here is an exemple:

###

 #define TRUE 1
 #define FALSE 0

 let f x = match x with
   | $TRUE -> true
   | $FALSE -> false
###

== Extending OCaml grammar ==

To extend OCaml's grammar, one should create a functor, taking as input some
structure containing the entry points of the OCaml grammar and some other
usefull functions, and extends the OCaml grammar by calling the provided
function. After the definition of the function, the extension needs to be
registered.

Here is an example adding the ##do ... where ...## and ##let ... try ... ##
constructs to OCaml. Note that quotation are used, as with Camlp4.

//Warning: quotation are still being developped and are not complete yet. For
instance, quotation patterns are not yet available and the constructors of
OCaml's AST have to be used directly to match and transform expressions. This
implies that the following example depends upon OCaml's version.//

###

###
### OCaml "pa_do_try.ml"
  open Pa_ocaml_prelude

  #define LOCATE locate

  module Ext = functor(In:Extension) -> 
  struct
    include In

    let extension lvl = parser
    | STR("do") e:(expr) STR("where") r:STR("rec")? b:let_binding ->
        (Let, if r<>None then
                <:expr<let rec $bindings:b$ in $e$>>
              else 
                <:expr<let $bindings:b$ in $e$>>)

    | STR("let") STR("try") b:let_binding 
      STR("in") e:(expr) STR("with") c:(match_cases Top) ->

      (* missing quotation in pattern yet *)
  #ifversion >= 4.02
         let c = List.map 
             Parsetree.(fun ({ pc_rhs = e; _ } as b) -> 
                { b with pc_rhs = <:expr< fun () -> $e$ >> }) c
  #else
         let c = List.map 
            (fun (pat, e) -> (pat, <:expr< fun () -> $e$ >>)) c
  #endif
         in
         (Let, 
          <:expr<(try let $bindings:b$ in fun () -> $e$ with $cases:c$) ()>>)

    let extra_expressions = extension::extra_expressions
    let _ = add_reserved_id "where"

  end

  module M = Pa_main.Start(Pa_ocaml.Make(Ext(Initial)))
### 
###

###

This file can be compiled with the following line:

###

ocamlfind ocamlopt -pp ../pa_ocaml -o pa_do_try -linkall -package decap
  -I +compiler-libs unix.cmxa str.cmxa ocamlcommon.cmxa
  decap.cmxa pa_do_try.ml decap_ocaml.cmxa

###

The module ##Pa_ocaml_prelude.Initial : Extension##, which is included inside
##decap.cmxa##, contains entry points of the OCaml grammar, data types related
to priorities, ...

The idea is to write a functor taking as input a module of type ##Extension##,
and incude it so that the resulting module is of the same type. Then, the list
##extra_expressions## can be extended with one parser, in order to add parsing
rule for expressions. We may also use ##add_reserved_id## to add new keyword.

Finally, the extension is registered and the module inside ##decap_ocaml.cmxa##
will build the ocaml grammar together with all register extension.

== Non terminals, quotation and anti-quotations ==

**We now list rapidely the available, non terminal entry points (extensible or not) available to write extensions, all quotations and anti-quotations. This documentation is very succinct and will be augmented when our software will be completed.**

Here are the entry points available in the module ##Pa_ocaml_prelude.Initial : Extension## (this will be extended in the near future, to allow for instance adding new infix construction in expression of pattern):

\begin{itemize}
\item ##expression_lvl : expression_prio -> expression grammar##
  and ##expression = expression_lvl Top##, extensible using the list ##extra_expression : (expression_lvl * expression) grammar list##
\item ##structure : structure_item list grammar## extensible using ##extra_structure = structure_item list grammar list##
\item ##signature : signature_item list grammar## extensible using ##extra_signature = ([] : signature_item list grammar list##
\item ##typexpr_lvl : type_prio -> core_type grammar##
  and ##typexpr = typexpr_lvl TopType##  extensible using the list ##extra_types : core_type grammar list##
\item ##pattern_lvl : pattern_prio -> pattern grammar##
  and ##pattern = pattern_lvl : TopPat##  extensible using the list ##extra_patterns = (pattern_prio * pattern) grammar list##
\item ##let_binding : value_binding list grammar##
\item ##class_body : class_structure grammar##
\item ##class_expr : class_expr grammar##

\end{itemize}

The priority levels for expressions, types and patterns are:

### OCaml 

type expression_prio =
  | Top | Let | Seq | Coerce | If | Aff | Tupl | Disj 
  | Conj | Eq | Append | Cons | Sum | Prod | Pow | Opp 
  | App | Dash | Dot | Prefix | Atom

type type_prio = TopType | As | Arr | ProdType | DashType
  | AppType | AtomType

type pattern_prio = TopPat | AsPat | AltPat | TupPat 
  | ConsPat | ConstrPat | AtomPat

###

Quotations with the syntax ##<:id< ... >>## are currently available with the following value for ##id## (this will be extended):
##expr## (expressions), ##type##, ##pat## (pattern), ##structure##, ##signature##, ##constructors## (definition of one or more constructors
of a variant type), ##fields## (definition of one or more fields of a record type), ##bindings## (let bindings, not including the ##rec## keyword), ##cases## (one of more match cases), ##module## (module expression) and ##module type##.

Anti-quotations are available with the syntax ##$...$## (with no space after the opening ##$## and no space before the closing ##$## inside the following context: type, constructor declarations, record field declarations, pattern, module expression, module type expression, structure and signature.

Moreover, named quotation ##$id:...$## are available with the following value of ##id## and the corresponding type for the ellipsed expression:
\begin{itemize}
\item inside expressions and patterns:
   ##lid:string##, ##uid:string##, ##ident:string##, ##int:int##, ##int32:int32##, ##int64:int64##, ##natint:nativeint##, ##bool:bool##, ##float:float##, ##char:char##, ##string:string##, ##tuple:pattern/expression list##, ##list:pattern/expression list##, ##array:pattern/expression list##.
\item inside type: ##tuple:type list##,
\item inside let bindings: ##bindings: value_binding list## (named to avoid parsing ambiguïties),
\item inside match cases: ##cases: case list## (named to avoid parsing ambiguïties).
\end{itemize}

