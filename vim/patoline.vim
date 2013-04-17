" Vim syntax file
" Language:        Patoline
" Maintainer:      Rodolphe Lepigre <rodolphe.lepigre@univ-savoie.fr>
" Last Change:     15/04/2013
" Version:         1.0
" Original Author: Rodolphe Lepigre <rodolphe.lepigre@univ-savoie.fr>

if exists("b:current_syntax")
  finish
endif

" Document title
syn match patoHeader    "=\{10}=*\s*$"
syn match patoHeaderSep "-\{10}-*\s*$"
hi link patoHeader     Constant
hi link patoHeaderSep  Constant

" Comments
syn region  patoComment start="(\*" end="\*)" contains=patoComment,patoCommentTags,patoPreproc
syn keyword patoCommentTags contained TODO FIXME NOTE
syn match   patoPreproc contained "\v#\w*\s\w*"
hi link patoComment     Comment
hi link patoCommentTags Todo
hi link patoPreproc     SpecialComment

" Math environment
syn region patoMathEnv start="\$"   end="\$" contains=patoComment
syn region patoMathEnv start="\$\$" end="\$\$" contains=patoComment
hi link patoMathEnv Identifier

" Section titles
syn match patoTitle "^\s*=\{2}[^=]*[^=]=\{2}\s*$"
syn match patoTitle "^\s*=\{3}[^=]*[^=]=\{3}\s*$"
syn match patoTitle "^\s*=\{4}[^=]*[^=]=\{4}\s*$"
syn match patoTitle "^\s*=\{5}[^=]*[^=]=\{5}\s*$"
syn match patoTitle "^\s*=\{6}[^=]*[^=]=\{6}\s*$"
syn match patoTitle "^\s*=\{7}[^=]*[^=]=\{7}\s*$"
syn match patoTitle "^\s*=\{8}[^=]*[^=]=\{8}\s*$"
syn match patoTitle "^\s*=\{9}[^=]*[^=]=\{9}\s*$"
syn match patoTitle "^\s*-\{2}[^-]*[^-]-\{2}\s*$"
syn match patoTitle "^\s*-\{3}[^-]*[^-]-\{3}\s*$"
syn match patoTitle "^\s*-\{4}[^-]*[^-]-\{4}\s*$"
syn match patoTitle "^\s*-\{5}[^-]*[^-]-\{5}\s*$"
syn match patoTitle "^\s*-\{6}[^-]*[^-]-\{6}\s*$"
syn match patoTitle "^\s*-\{7}[^-]*[^-]-\{7}\s*$"
syn match patoTitle "^\s*-\{8}[^-]*[^-]-\{8}\s*$"
syn match patoTitle "^\s*-\{9}[^-]*[^-]-\{9}\s*$"
hi link patoTitle Constant

" Environments
syn match patoEnv "^\s*\\begin{\w*}\s*$"
syn match patoEnv "^\s*\\end{\w*}\s*$"
hi link patoEnv Constant

syn match patoItem "^\s*\\item"
hi link patoItem Constant

" OCaml
syn include @ocaml syntax/ocaml.vim
syn region ocamlCode matchgroup=Snip start="\\Caml(" end=")\n" contains=@ocaml
hi link Snip SpecialComment

let b:current_syntax = "patoline"
