

type fbo

external create_fbo_texture : int -> int -> fbo = "gl_create_fbo_texture"
external bind_fbo: fbo -> unit = "gl_bind_fbo"
external bind_texture : fbo -> unit = "gl_bind_texture"
external unbind_fbo : fbo -> unit = "gl_unbind_fbo"

external merge_blend : unit -> unit = "gl_merge_blend"
external merge_blend2 : unit -> unit = "gl_merge_blend2"
