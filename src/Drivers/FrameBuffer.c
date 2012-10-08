#ifdef __APPLE__
#include <OpenGL/gl.h>
#include <OpenGL/glu.h>
#define GL_GLEXT_PROTOTYPES
#include <OpenGL/glext.h>
#else
#include <GL/gl.h>
#include <GL/glu.h>
#define GL_GLEXT_PROTOTYPES
#include <GL/glext.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/custom.h>
#include <caml/memory.h>

#define Base_raw(raw) (Field(raw,1))
#define Offset_raw(raw) (Field(raw,2))

#define Addr_raw(raw) (Base_raw(raw)+Long_val(Offset_raw(raw)))

#define Void_raw(raw) ((void *) Addr_raw(raw))

typedef struct _fbo_texture {
  GLuint textureId;
  GLuint rboId;
  GLuint fboId;
  int is_texture;
} *fbo_texture;

void testGL(msg) {
  GLenum errCode;
  const GLubyte* errString;

  while((errCode = glGetError()) != GL_NO_ERROR)
    {
      errString = gluErrorString(errCode);
      fprintf(stderr,"GL_ERROR: %s in %s\n", errString, msg); 
    }

}

void delete_fbo(value fbo) {
  fbo_texture cfbo = Data_custom_val(fbo);
  if (cfbo->is_texture)
    glDeleteTextures(1,&(cfbo->textureId));
  else 
    glDeleteRenderbuffers(1,&(cfbo->textureId));
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

CAMLprim gl_create_fbo_texture(value ml_width, value ml_height, value ml_texture)
{ CAMLparam0();
  int height = Int_val(ml_height);
  int width = Int_val(ml_width);
  int texture = Bool_val(ml_texture);
  CAMLlocal1(fbo);

  fbo = caml_alloc_custom( &objst_custom_ops, sizeof(struct _fbo_texture), 0, 1);
  fbo_texture cfbo = Data_custom_val(fbo);
  cfbo->is_texture = texture;
  
  if (texture) {
    glGenTextures(1, &(cfbo->textureId));
    testGL("glGenTextures");
    glBindTexture(GL_TEXTURE_2D, cfbo->textureId);
    testGL("glBindTexture");
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    testGL("glParameterf 1");
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    testGL("glParameterf 2");
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    testGL("glParameterf 3");
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    testGL("glParameterf 4");
    glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_TRUE); // automatic mipmap
    testGL("glParameterf 5");
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0,
		 GL_RGBA, GL_UNSIGNED_BYTE, 0);
    testGL("glTexImage2D");
    glBindTexture(GL_TEXTURE_2D, 0);
    testGL("glUnBindTexture");
  } else {
    glGenRenderbuffers(1, &(cfbo->textureId));
    glBindRenderbuffer(GL_RENDERBUFFER, cfbo->textureId);
    glRenderbufferStorage(GL_RENDERBUFFER, GL_RGBA,
			  width, height);
    glBindRenderbuffer(GL_RENDERBUFFER, 0);
  }

  // create a renderbuffer object to store depth info
  glGenRenderbuffers(1, &(cfbo->rboId));
  glBindRenderbuffer(GL_RENDERBUFFER, cfbo->rboId);
  glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT,
			width, height);
  glBindRenderbuffer(GL_RENDERBUFFER, 0);

  // create a framebuffer object
  glGenFramebuffers(1, &(cfbo->fboId));
  testGL("glGenFramebuffers");
  glBindFramebuffer(GL_FRAMEBUFFER,cfbo-> fboId);
  testGL("glBindFrameBuffer");

  // attach the texture to FBO color attachment point
  if (texture)
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0,
			   GL_TEXTURE_2D, cfbo->textureId, 0);
  else
    glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0,
			      GL_RENDERBUFFER, cfbo->textureId);
  testGL("Attach color");


  // attach the renderbuffer to depth attachment point
  glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT,
			    GL_RENDERBUFFER, cfbo->rboId);
  testGL("Attach depth");

  // check FBO status
  GLenum status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
  if(status != GL_FRAMEBUFFER_COMPLETE)    
    caml_failwith("Createing FBO texture failed");

  // switch back to window-system-provided framebuffer
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
  testGL("UnbindFrameBuffer");

  CAMLreturn(fbo);
}

CAMLprim gl_bind_fbo(value fbo)
{ CAMLparam0();
  fbo_texture cfbo = Data_custom_val(fbo);
  glBindFramebuffer(GL_FRAMEBUFFER,cfbo-> fboId);
  testGL("glBindFrameBuffer");
  glDrawBuffer(GL_COLOR_ATTACHMENT0);
  testGL("glDrawBuffer");
  glReadBuffer(GL_COLOR_ATTACHMENT0);
  testGL("glReadBuffer");
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
  if (cfbo->is_texture) {
    glBindTexture(GL_TEXTURE_2D, cfbo->textureId);
    glGenerateMipmap(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, 0);
  }
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
  CAMLreturn(Val_unit);
}

CAMLprim gl_merge_blend(value u)
{ CAMLparam0();
  glBlendFuncSeparate(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA, GL_ONE, GL_ONE);
  /*  glBlendEquation(GL_MAX);*/
  CAMLreturn(Val_unit);
}
CAMLprim gl_merge_blend2(value u)
{ CAMLparam0();
  glBlendFuncSeparate(GL_ONE_MINUS_DST_ALPHA, GL_DST_ALPHA, GL_ZERO, GL_ZERO);
  /*  glBlendEquation(GL_FUNC_ADD);*/
  CAMLreturn(Val_unit);
}


const char* vertex_shader_source = "\
varying vec4 color;\
void main()\
{\
        gl_Position = ftransform();\
        color = gl_Color;\
}";

const char* pixel_shader_source = "\
varying vec4 color;\
const float pi = 3.1415926535897932384626;\
attribute float x_factor = 1.0;\
attribute float y_factor = 1.0;\
\
void main()\
{\
        vec4 alpha = color[3];\
        float t = 2.0 - alpha * 3.0;\
        float a = 0.0;\
        if (t <= 0.0) {\
           t = - t;\
           a = 1.0 - (acos(t) - t * sqrt(1.0 - t * t))/pi;\
        } else if (t <= 1.0) {\
           a = (acos(t) - t * sqrt(1.0 - t * t))/pi;\
        } else a = 0;\
        gl_FragColor = color;\
        gl_FragColor[3] = a;\
}";

GLuint vertex_shader = 0;
GLuint pixel_shader = 0;
GLuint program_shader = 0;

#define MSGSIZE 10000
static char msg[MSGSIZE];

void compile_shader(GLenum type, GLuint *id, const char* source) { 
  GLint result;

  *id = glCreateShader(type);
  glShaderSource(*id, 1, &source, NULL);
  glCompileShader(*id);
  glGetShaderiv(*id, GL_COMPILE_STATUS, &result);
  glGetShaderInfoLog(*id, MSGSIZE, 0, msg); 
  if (!result) printf("shader compilation: %s\n", msg);
  if (!result) exit(1);
}

CAMLprim gl_init_shader(value u) {
  CAMLparam0();
  GLint result;

  compile_shader(GL_VERTEX_SHADER,&vertex_shader, vertex_shader_source);
  compile_shader(GL_FRAGMENT_SHADER, &pixel_shader, pixel_shader_source);

  if (program_shader) glDeleteProgram(program_shader);
  program_shader = glCreateProgram();
  glAttachShader(program_shader, vertex_shader);
  glAttachShader(program_shader, pixel_shader);
  glLinkProgram(program_shader);
  glGetProgramiv(program_shader, GL_LINK_STATUS, &result);
  glGetProgramInfoLog(program_shader, MSGSIZE, 0, msg); 
  if (!result) printf("link: %s\n", msg);
  if (!result) exit(1);

  CAMLreturn(Val_unit);  
}

CAMLprim gl_use_shader(value u) {
  CAMLparam0();
  glUseProgram(program_shader);
  CAMLreturn(Val_unit);
}

CAMLprim gl_no_shader(value u) {
  CAMLparam0();
  glUseProgram(0);
  CAMLreturn(Val_unit);
}

