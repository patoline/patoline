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

(** P2-4: MathML input backdoor.

    Translates a small but useful subset of MathML into Patoline's
    native math syntax.  The goal is to allow pasting the output of
    MathJax / KaTeX as a starting point.  Recognised tags:

    - [math], [mrow], [mi], [mn], [mo], [mtext]: identity passthrough
    - [mfrac]:               [(a / b)]
    - [msup] / [msub]:       [(a ^ b)] / [(a _ b)]
    - [msqrt]:               [(sqrt a)]
    - [mover] / [munder]:    stretchy=[overline/underline], else [over/under]
    - [mfenced]:             [open ... close]

    Anything we do not understand is rendered as the concatenation of
    its children.

    Each direct child of a "split tag" (mfrac, msup, msub, mover,
    munder, msubsup, munderover, mroot) is wrapped in parentheses so
    the downstream splitter can recover the operand boundaries even
    when operands contain spaces or operators. *)

(** Split [s] on top-level whitespace, treating balanced parens as a
    single atom.  Adjacent atoms separated only by `)(` are still
    treated as distinct atoms. *)
let split_top_level s =
  let len = String.length s in
  let atoms = ref [] in
  let cur = Buffer.create 32 in
  let depth = ref 0 in
  let i = ref 0 in
  let flush () =
    if Buffer.length cur > 0 then (
      atoms := Buffer.contents cur :: !atoms;
      Buffer.clear cur)
  in
  while !i < len do
    let c = s.[!i] in
    if c = '(' then (Buffer.add_char cur c; depth := !depth + 1; i := !i + 1)
    else if c = ')' then begin
      Buffer.add_char cur c; depth := !depth - 1; i := !i + 1;
      if !i < len && !depth = 0 &&
         s.[!i] <> ' ' && s.[!i] <> '\t' && s.[!i] <> '\n' && s.[!i] <> '\r' &&
         s.[!i] <> ')' then flush ()
    end
    else if (c = ' ' || c = '\t' || c = '\n' || c = '\r') && !depth = 0 then begin
      flush ();
      i := !i + 1
    end
    else (Buffer.add_char cur c; i := !i + 1)
  done;
  flush ();
  List.rev !atoms

let split_two s = match split_top_level s with
  | [a; b] -> (a, b)
  | [a] -> (a, "")
  | l ->
      let n = List.length l in
      let last = List.nth l (n - 1) in
      let head = String.concat " " (List.filteri (fun i _ -> i < n - 1) l) in
      (head, last)

let split_three s = match split_top_level s with
  | [a; b; c] -> (a, b, c)
  | [a; b] -> (a, b, "")
  | [a] -> (a, "", "")
  | l ->
      let n = List.length l in
      let last = List.nth l (n - 1) in
      let middle = List.nth l (n - 2) in
      let head = String.concat " " (List.filteri (fun i _ -> i < n - 2) l) in
      (head, middle, last)

let unescape_entities s =
  let len = String.length s in
  let buf = Buffer.create len in
  let i = ref 0 in
  while !i < len do
    if s.[!i] = '&' then (
      let semi = ref (!i + 1) in
      while !semi < len && s.[!semi] <> ';' do incr semi done;
      if !semi < len then (
        let entity = String.sub s (!i + 1) (!semi - !i - 1) in
        let replacement =
          match entity with
          | "amp" -> "&" | "lt" -> "<" | "gt" -> ">" | "quot" -> "\""
          | "apos" -> "'" | "nbsp" -> " "
          | "times" -> "*" | "divide" -> "/" | "plus" -> "+" | "minus" -> "-"
          | "le" -> "<=" | "ge" -> ">=" | "ne" -> "!="
          | "infin" -> "infty"
          | "alpha" -> "alpha" | "beta" -> "beta" | "gamma" -> "gamma"
          | "delta" -> "delta" | "epsilon" -> "epsilon"
          | "zeta" -> "zeta" | "eta" -> "eta" | "theta" -> "theta"
          | "iota" -> "iota" | "kappa" -> "kappa" | "lambda" -> "lambda"
          | "mu" -> "mu" | "nu" -> "nu" | "xi" -> "xi" | "pi" -> "pi"
          | "rho" -> "rho" | "sigma" -> "sigma" | "tau" -> "tau"
          | "phi" -> "phi" | "chi" -> "chi" | "psi" -> "psi"
          | "omega" -> "omega"
          | "Gamma" -> "Gamma" | "Delta" -> "Delta" | "Theta" -> "Theta"
          | "Lambda" -> "Lambda" | "Xi" -> "Xi" | "Pi" -> "Pi"
          | "Sigma" -> "Sigma" | "Phi" -> "Phi" | "Psi" -> "Psi"
          | "Omega" -> "Omega"
          | _ -> ""
        in
        Buffer.add_string buf replacement;
        i := !semi + 1)
      else (Buffer.add_char buf s.[!i]; i := !i + 1))
    else (Buffer.add_char buf s.[!i]; i := !i + 1)
  done;
  Buffer.contents buf

(** Render a single tag with attribute table [attrs] and rendered
    children [body].  For tags that split into discrete operands
    ([mfrac], [msup], ...) [body] should contain its operands
    separated by spaces and each operand already wrapped in parens. *)
let render_tag tag attrs body =
  let open_ = try Hashtbl.find attrs "open" with Not_found -> "(" in
  let close_ = try Hashtbl.find attrs "close" with Not_found -> ")" in
  let seps = try Hashtbl.find attrs "separators" with Not_found -> "," in
  let sep = if seps = "" then "," else String.make 1 seps.[0] in
  let stretchy = try Hashtbl.find attrs "stretchy" = "true"
                 with Not_found -> false in
  match tag with
  | "math" | "mrow" | "mi" | "mn" | "mo" | "mtext"
  | "mstyle" | "mpadded" | "mphantom" -> body
  | "mfrac" ->
      let a, b = split_two body in "(" ^ a ^ " / " ^ b ^ ")"
  | "msup" ->
      let a, b = split_two body in "(" ^ a ^ " ^ " ^ b ^ ")"
  | "msub" ->
      let a, b = split_two body in "(" ^ a ^ " _ " ^ b ^ ")"
  | "msubsup" ->
      let a, b, c = split_three body in
      "(" ^ a ^ " _ " ^ b ^ " ^ " ^ c ^ ")"
  | "msqrt" -> "(sqrt " ^ body ^ ")"
  | "mroot" ->
      let a, b = split_two body in "(root " ^ b ^ " " ^ a ^ ")"
  | "mover" ->
      let a, b = split_two body in
      let cmd = if stretchy then "overline" else "over" in
      "(" ^ cmd ^ " " ^ a ^ " " ^ b ^ ")"
  | "munder" ->
      let a, b = split_two body in
      let cmd = if stretchy then "underline" else "under" in
      "(" ^ cmd ^ " " ^ a ^ " " ^ b ^ ")"
  | "munderover" ->
      let a, b, c = split_three body in
      let cmd1 = if stretchy then "underline" else "under" in
      let cmd2 = if stretchy then "overline" else "over" in
      "(" ^ cmd1 ^ " " ^ a ^ " " ^ b ^ ") (" ^
      cmd2 ^ " " ^ a ^ " " ^ c ^ ")"
  | "mfenced" ->
      open_ ^ String.concat sep (split_top_level body) ^ close_
  | "mtable" -> String.concat " \\\\ " (split_top_level body)
  | "mtr" | "mtd" -> body
  | "mspace" | "mspace_*" -> ""
  | _ -> body

(** Tags whose direct children must be split into discrete operands
    for downstream rendering. *)
let split_tags = [ "mfrac"; "msup"; "msub"; "msubsup";
                   "mover"; "munder"; "munderover"; "mroot" ]

let is_split_tag tag = List.mem tag split_tags

(** ---- tag scanning ---- *)

exception Close of string

(** Scan a start tag at position [pos]; returns
    (tag, attrs, kind, new_pos) where [new_pos] points just past the
    opening tag. *)
let scan_open_tag s pos =
  let len = String.length s in
  if pos >= len || s.[pos] <> '<' then
    raise (Invalid_argument "Mathml: expected '<'");
  let i = ref (pos + 1) in
  let name_start = !i in
  while !i < len &&
        let c = s.[!i] in
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
        (c >= '0' && c <= '9') || c = '-' || c = '_' || c = ':'
  do i := !i + 1 done;
  if !i = name_start then
    raise (Invalid_argument "Mathml: empty tag name");
  let tag = String.sub s name_start (!i - name_start) in
  let attrs = Hashtbl.create 4 in
  let rec parse_attrs () =
    while !i < len &&
          (s.[!i] = ' ' || s.[!i] = '\t' || s.[!i] = '\n' || s.[!i] = '\r')
    do i := !i + 1 done;
    if !i >= len then
      raise (Invalid_argument "Mathml: unexpected EOF in tag");
    let c = s.[!i] in
    if c = '>' then (i := !i + 1; `Open)
    else if c = '/' then (
      if !i + 1 < len && s.[!i + 1] = '>' then (
        i := !i + 2; `SelfClose)
      else raise (Invalid_argument "Mathml: malformed / in tag"))
    else (
      let nstart = !i in
      while !i < len &&
            let c = s.[!i] in
            (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
            (c >= '0' && c <= '9') || c = '-' || c = '_' || c = ':'
      do i := !i + 1 done;
      let aname = String.sub s nstart (!i - nstart) in
      while !i < len &&
            (s.[!i] = ' ' || s.[!i] = '\t' || s.[!i] = '\n' || s.[!i] = '\r')
      do i := !i + 1 done;
      let value =
        if !i < len && s.[!i] = '=' then (
          i := !i + 1;
          while !i < len &&
                (s.[!i] = ' ' || s.[!i] = '\t' || s.[!i] = '\n' || s.[!i] = '\r')
          do i := !i + 1 done;
          if !i >= len then
            raise (Invalid_argument "Mathml: missing attribute value");
          if s.[!i] = '"' || s.[!i] = '\'' then (
            let q = s.[!i] in
            i := !i + 1;
            let vstart = !i in
            while !i < len && s.[!i] <> q do
              if s.[!i] = '&' then
                while !i < len && s.[!i] <> ';' do i := !i + 1 done;
              if !i < len then i := !i + 1
            done;
            let v = String.sub s vstart (!i - vstart) in
            if !i < len then i := !i + 1;
            v)
          else raise (Invalid_argument "Mathml: expected quoted value"))
        else ""
      in
      Hashtbl.replace attrs aname value;
      parse_attrs ())
  in
  let kind = parse_attrs () in
  (tag, attrs, kind, !i)

let scan_close_tag s pos =
  let len = String.length s in
  if pos + 1 >= len || s.[pos] <> '<' || s.[pos + 1] <> '/' then None
  else begin
    let i = ref (pos + 2) in
    let nstart = !i in
    while !i < len &&
          let c = s.[!i] in
          (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
          (c >= '0' && c <= '9') || c = '-' || c = '_' || c = ':'
    do i := !i + 1 done;
    let name = String.sub s nstart (!i - nstart) in
    while !i < len && s.[!i] <> '>' do incr i done;
    if !i < len then incr i;
    Some (name, !i)
  end

(** Parse a sequence of siblings into [out_buf].  Terminates on a close
    tag.  When [wrap_children] is true, each direct child is rendered
    wrapped in parens so [split_top_level] in [render_tag] can recover
    operand boundaries. *)
let rec parse_siblings s i len out_buf wrap_children =
  let rec loop () =
    if !i >= len then ()
    else if s.[!i] = '<' then begin
      match scan_close_tag s !i with
      | Some (name, next_i) ->
          i := next_i;
          raise (Close name)
      | None ->
          let tag, attrs, kind, after_open = scan_open_tag s !i in
          i := after_open;
          if kind = `SelfClose then begin
            Buffer.add_string out_buf (render_tag tag attrs "")
          end
          else begin
            let child_buf = Buffer.create 32 in
            (try parse_siblings s i len child_buf (is_split_tag tag)
             with Close _ -> ());
            let body = String.trim (Buffer.contents child_buf) in
            let rendered = render_tag tag attrs body in
            let emitted =
              if wrap_children then "(" ^ rendered ^ ")"
              else rendered
            in
            Buffer.add_string out_buf emitted;
          end;
          (if !i < len && s.[!i] = '<' && s.[!i + 1] <> '/' then
             Buffer.add_char out_buf ' ');
          loop ()
    end
    else begin
      let start = !i in
      while !i < len && s.[!i] <> '<' do incr i done;
      let text = String.sub s start (!i - start) in
      let trimmed = unescape_entities (String.trim text) in
      if trimmed <> "" then begin
        Buffer.add_string out_buf trimmed;
        if !i < len && s.[!i] = '<' then
          Buffer.add_char out_buf ' '
      end;
      loop ()
    end
  in
  loop ()

let parse_node s pos out_buf =
  let i = ref pos in
  let len = String.length s in
  try parse_siblings s i len out_buf false
  with Close _ -> ()

let to_patoline ?(wrap = true) src =
  let out = Buffer.create (String.length src) in
  parse_node src 0 out;
  let body = String.trim (Buffer.contents out) in
  if wrap && body <> "" then "(math " ^ body ^ ")" else body