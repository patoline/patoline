== Extending the OCaml parser ==

The syntax for BNF described in the previous sections is an OCaml extension written using
this tool in itself. This means that Decap comes with an extensible OCaml parser 
similar to what camlp4 provides.

First our parser aims at a (almost) full compatibility with OCaml syntax together with the BNF extension.
Here are some known differences with the original OCaml parser:
\begin{itemize}
\item ##val## may be use in place of ##let## for struture fields.
\item type coercion are allowed in expression and pattern without parenthesis. For expression the priority
is between sequences and conditionals ##a ; if b then c else d : t## means ##a ; if b then c else (d : t)##.
And for pattern, type coercion is at the lowest priority level: ##a , b : t## means ##(a, b) : t##.
\item A preprocessor is available. This may break some code if a "#" symbol occurs at a beginning of a line.
\end{itemize} 

Here are the preprocessor directive, which all shoud have a "#" caractere as first character on the line:

\begin{itemize}
\item The line number indication directive as in OCaml.
\item ``#define macro value`` defining a macro with no parameter
\item ``#ifdef macro`` and ``#ifundef macro`` which are classical
\item ``#ifversion [<|>|=|<=|>=] XX.XX`` to test the ocaml version.
\item ``#endif``, ``#else`` and ``#elif ...`` to close the conditional directive.
\end{itemize}

Moreover, OCaml can access environment variables: ##$VARIABLE##, in an expression or a pattern, is
replaced by the value of the given environment variable (there should
be no space between the dollar sign and the first letter of the
variable name (which must be a capital letter). The value of the variable is not considered as a string, it is parsed as an OCaml expression.or pattern. Exemple:

###

 #define TRUE 1
 #define FALSE 0

 let f x = match x with
   | $TRUE -> true
   | $FALSE -> false

###

== Extending OCaml grammar ==

To extend OCaml's grammar, you must write a functor, that 
takes as input some a structure containing some entry points of the OCaml grammar and some other usefull functions and extends the OCaml grammar by calling provided function. After the definition of the function, you must register the extension.

Here as an exemple adding the ##do ... where ...## and ##let ... try ... ## constructs to OCaml. Note: this exemple use quotation as in Camlp4 (remark, quotation are not yet available for patterns, this is why this code depends upon OCaml's version:

### OCaml "pa_do_try.ml"

 open Pa_ocaml_prelude

 #define LOCATE locate

 module Ext = functor(In:Extension) -> 
 struct
   include In

   let extention = parser
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

 let _ = register_extension (module Ext)

### 

