" Vim syntax file
" Language:        Patoline
" Maintainer:      Rodolphe Lepigre <rodolphe.lepigre@univ-savoie.fr>
" Last Change:     26/04/2013
" Version:         1.0
" Original Author: Rodolphe Lepigre <rodolphe.lepigre@univ-savoie.fr>
"
" Copyright Florian Hatat, Tom Hirschowitz, Pierre Hyvernat,
" Pierre-Etienne Meunier, Christophe Raffalli, Guillaume Theyssier,
" Rodolphe Lepigre 2013.
"
" This file is part of Patoline.
"
" Patoline is free software: you can redistribute it and/or modify
" it under the terms of the GNU General Public License as published by
" the Free Software Foundation, either version 3 of the License, or
" (at your option) any later version.
"
" Patoline is distributed in the hope that it will be useful,
" but WITHOUT ANY WARRANTY; without even the implied warranty of
" MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
" GNU General Public License for more details.
"
" You should have received a copy of the GNU General Public License
" along with Patoline.  If not, see <http://www.gnu.org/licenses/>.
"

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
syn match patoTitle "^\s*=\{2}[^=].*[^=]=\{2}\s*$"
syn match patoTitle "^\s*=\{3}[^=].*[^=]=\{3}\s*$"
syn match patoTitle "^\s*=\{4}[^=].*[^=]=\{4}\s*$"
syn match patoTitle "^\s*=\{5}[^=].*[^=]=\{5}\s*$"
syn match patoTitle "^\s*=\{6}[^=].*[^=]=\{6}\s*$"
syn match patoTitle "^\s*=\{7}[^=].*[^=]=\{7}\s*$"
syn match patoTitle "^\s*=\{8}[^=].*[^=]=\{8}\s*$"
syn match patoTitle "^\s*=\{9}[^=].*[^=]=\{9}\s*$"
syn match patoTitle "^\s*-\{2}[^-].*[^-]-\{2}\s*$"
syn match patoTitle "^\s*-\{3}[^-].*[^-]-\{3}\s*$"
syn match patoTitle "^\s*-\{4}[^-].*[^-]-\{4}\s*$"
syn match patoTitle "^\s*-\{5}[^-].*[^-]-\{5}\s*$"
syn match patoTitle "^\s*-\{6}[^-].*[^-]-\{6}\s*$"
syn match patoTitle "^\s*-\{7}[^-].*[^-]-\{7}\s*$"
syn match patoTitle "^\s*-\{8}[^-].*[^-]-\{8}\s*$"
syn match patoTitle "^\s*-\{9}[^-].*[^-]-\{9}\s*$"
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
