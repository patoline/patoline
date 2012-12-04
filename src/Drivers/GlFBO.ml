(*
  Copyright Tom Hirschowitz, Florian Hatat, Pierre-Etienne Meunier,
  Christophe Raffalli and others, 2012.

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


type fbo

external create_fbo_texture : int -> int -> bool -> fbo = "gl_create_fbo_texture"
external bind_fbo: fbo -> unit = "gl_bind_fbo"
external bind_texture : fbo -> unit = "gl_bind_texture"
external unbind_fbo : fbo -> unit = "gl_unbind_fbo"

external merge_blend : unit -> unit = "gl_merge_blend"
external merge_blend2 : unit -> unit = "gl_merge_blend2"


external init_shader : unit -> unit = "gl_init_shader"
external use_shader : unit -> unit = "gl_use_shader"
external no_shader : unit -> unit = "gl_no_shader"

