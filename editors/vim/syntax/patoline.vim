" Vim syntax file
" Language:        Patoline
" Maintainer:      Rodolphe Lepigre <rodolphe.lepigre@inria.fr>
" Last Change:     22/11/2018
" Version:         1.0
" Original Author: Rodolphe Lepigre <rodolphe.lepigre@inria.fr>
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

syn spell toplevel

syn match newstring "{\([a-z_]*\)|.*|\1}"
hi link newstring String

" Comments
syn region  patoComment start="(\*" end="\*)" contains=patoComment,patoCommentTags,string,newstring
syn keyword patoCommentTags contained TODO FIXME NOTE
syn region string start=+"+ skip=+\\\\\|\\"+ end=+"+ oneline
hi link patoComment     Comment
hi link patoCommentTags Todo

" Preproc
syn match   patoPreproc "#\w*\s\w*"
hi link patoPreproc     SpecialComment

" Call to macros and environments
syn match patoEnv "\\\w*"
syn match patoEnv "\\\w*{\_[^}]*}"
syn match patoEnv "\\\w*{\_[^}]*}{\_[^}]*}"

" OCaml
syn include @ocaml syntax/ocaml.vim
syn region ocamlCode matchgroup=camlEnv start="^\s*\\Caml(" end=")\s*$" contains=@ocaml
syn region ocamlCode matchgroup=camlEnv start="\\\w*(" end=")" contains=@ocaml
hi link camlEnv         SpecialComment

syn region ocamlCode matchgroup=patoEnv start="\\\w*{\_[^}]*}(" end=")" contains=@ocaml

syn region environment matchgroup=patoEnv start="^\s*\\begin{\w*}\s*$" end="^\s*\\end{\w*}\s*$" contains=ALL

syn region italicText start="//" end="//"
syn region boldText   start="\*\*" end="\*\*"
syn region patoEnv    start="||" end="||"
syn region patoEnv    start="__" end="__"
syn region patoEnv    start="--" end="--"

hi italicText  gui=underline cterm=underline
hi boldText    gui=italic    cterm=italic
hi link patoEnv Type

" Document title, sections, itemize
syn match patoTitle "=\{10}=*\s*$"
syn match patoTitle "-\{10}-*\s*$"

syn match patoTitle "^\s*=\{2}[^=].*[^=]=\{2}\s*$" contains=ocamlCode
syn match patoTitle "^\s*=\{3}[^=].*[^=]=\{3}\s*$" contains=ocamlCode
syn match patoTitle "^\s*=\{4}[^=].*[^=]=\{4}\s*$" contains=ocamlCode
syn match patoTitle "^\s*=\{5}[^=].*[^=]=\{5}\s*$" contains=ocamlCode
syn match patoTitle "^\s*=\{6}[^=].*[^=]=\{6}\s*$" contains=ocamlCode
syn match patoTitle "^\s*=\{7}[^=].*[^=]=\{7}\s*$" contains=ocamlCode
syn match patoTitle "^\s*=\{8}[^=].*[^=]=\{8}\s*$" contains=ocamlCode
syn match patoTitle "^\s*=\{9}[^=].*[^=]=\{9}\s*$" contains=ocamlCode

syn match patoTitle "^\s*-\{2}[^-].*[^-]-\{2}\s*$" contains=ocamlCode
syn match patoTitle "^\s*-\{3}[^-].*[^-]-\{3}\s*$" contains=ocamlCode
syn match patoTitle "^\s*-\{4}[^-].*[^-]-\{4}\s*$" contains=ocamlCode
syn match patoTitle "^\s*-\{5}[^-].*[^-]-\{5}\s*$" contains=ocamlCode
syn match patoTitle "^\s*-\{6}[^-].*[^-]-\{6}\s*$" contains=ocamlCode
syn match patoTitle "^\s*-\{7}[^-].*[^-]-\{7}\s*$" contains=ocamlCode
syn match patoTitle "^\s*-\{8}[^-].*[^-]-\{8}\s*$" contains=ocamlCode
syn match patoTitle "^\s*-\{9}[^-].*[^-]-\{9}\s*$" contains=ocamlCode

syn match patoTitle "^\s*->.*$"   contains=ocamlCode
syn match patoTitle "^\s*-<\s*$"  contains=ocamlCode

syn match patoTitle "^\s*=>.*$"   contains=ocamlCode
syn match patoTitle "^\s*=<\s*$"  contains=ocamlCode

syn match patoTitle "^\s*\.>.*$"  contains=ocamlCode
syn match patoTitle "^\s*\.<\s*$" contains=ocamlCode

syn region patoItemize matchgroup=patoTitle start="^\s*\\begin{enumerate}\s*$" end="^\s*\\end{enumerate}\s*$" contains=ALL
syn region patoItemize matchgroup=patoTitle start="^\s*\\begin{itemize}\s*$" end="^\s*\\end{itemize}\s*$" contains=ALL
syn match patoTitle "^\s*\\item\s"

hi link patoTitle Constant

" Math environment
syn region patoMathEnv start="\$"   end="\$"   contains=patoComment
syn region patoMathEnv start="\$\$" end="\$\$" contains=patoComment
"hi link patoMathEnv Identifier
hi patoMathEnv ctermfg=Cyan

" Verbatim environment (monospaced)
" syn region patoVerb start="``"  end="``"
syn match patoVerb "##.*##"
syn region patoVerb start="^###" end="^###"
syn region patoVerb start="\\verb{" end="}"
syn match patoVerb "\\[\$\\\*&|/\"]"
hi link patoVerb Keyword

let b:current_syntax = "patoline"
