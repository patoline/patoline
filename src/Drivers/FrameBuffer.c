#include<GL/gl.h>
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/custom.h>
#include <caml/memory.h>


typedef struct _fbo_texture {
  GLuint textureId;
  GLuint rboId;
  GLuint fboId;
} *fbo_texture;

void delete_fbo(value fbo) {
  fbo_texture cfbo = Data_custom_val(fbo);
  glDeleteTextures(1,&(cfbo->textureId));
  glDeleteRenderbuffers(1,&(cfbo->rboId));
  glDeleteFramebuffers(1,&(cfbo->fboId));
}


static struct custom_operations objst_custom_ops = {
    identifier: "obj_st handling",
    finalize:    delete_fbo,
    compare:     custom_compare_default,
    hash:        custom_hash_default,
    serialize:   custom_serialize_default,
    deserialize: custom_deserialize_default
};

CAMLprim gl_create_fbo_texture(value ml_width, value ml_height)
{ CAMLparam0();
  int height = Int_val(ml_height);
  int width = Int_val(ml_width);
  CAMLlocal1(fbo);

  fbo = caml_alloc_custom( &objst_custom_ops, sizeof(struct _fbo_texture), 0, 1);
  fbo_texture cfbo = Data_custom_val(fbo);


  glGenTextures(1, &(cfbo->textureId));
  glBindTexture(GL_TEXTURE_2D, cfbo->textureId);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_TRUE); // automatic mipmap
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, width, height, 0,
	       GL_RGBA, GL_UNSIGNED_BYTE, 0);
  glBindTexture(GL_TEXTURE_2D, 0);

  // create a renderbuffer object to store depth info
  glGenRenderbuffers(1, &(cfbo->rboId));
  glBindRenderbuffer(GL_RENDERBUFFER, cfbo->rboId);
  glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT,
			width, height);
  glBindRenderbuffer(GL_RENDERBUFFER, 0);

  // create a framebuffer object
  glGenFramebuffers(1, &(cfbo->fboId));
  glBindFramebuffer(GL_FRAMEBUFFER,cfbo-> fboId);

  // attach the texture to FBO color attachment point
  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0,
			 GL_TEXTURE_2D, cfbo->textureId, 0);

  // attach the renderbuffer to depth attachment point
  glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT,
			    GL_RENDERBUFFER, cfbo->rboId);

  // check FBO status
  GLenum status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
  if(status != GL_FRAMEBUFFER_COMPLETE)    
    caml_failwith("Createing FBO texture failed");

  // switch back to window-system-provided framebuffer
  glBindFramebuffer(GL_FRAMEBUFFER, 0);

  CAMLreturn(fbo);
}

CAMLprim gl_bind_fbo(value fbo)
{ CAMLparam0();
  fbo_texture cfbo = Data_custom_val(fbo);
  glBindFramebuffer(GL_FRAMEBUFFER,cfbo-> fboId);
  CAMLreturn(Val_unit);
}

CAMLprim gl_bind_texture(value fbo)
{ CAMLparam0();
  fbo_texture cfbo = Data_custom_val(fbo);
  glBindTexture(GL_TEXTURE_2D, cfbo->textureId);
  CAMLreturn(Val_unit);
}

CAMLprim gl_unbind_fbo(value fbo)
{ CAMLparam0();
  fbo_texture cfbo = Data_custom_val(fbo);
  glBindTexture(GL_TEXTURE_2D, cfbo->textureId);
  glGenerateMipmap(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, 0);
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
  CAMLreturn(Val_unit);
}

CAMLprim gl_merge_blend(value u)
{ CAMLparam0();
  glBlendFuncSeparate(GL_SRC_ALPHA, GL_DST_ALPHA, GL_ONE, GL_ONE);
  CAMLreturn(Val_unit);
}
CAMLprim gl_merge_blend2(value u)
{ CAMLparam0();
  glBlendFuncSeparate(GL_ONE_MINUS_DST_ALPHA, GL_DST_ALPHA, GL_ZERO, GL_ZERO);
  CAMLreturn(Val_unit);
}

