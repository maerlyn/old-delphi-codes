// XOpenGL
{: "Alternate" OpenGL functions to handle multi-texturing.<p>

   Using this functions allows specifying none/one/multiple ARB multi-texture
   coordinates with standard texture specification call.<p>

   Before using any of the xglTexCoordXxxx fonctions, call one of the
   xglMapTexCoordToXxxx functions to establish the redirectors.<p>

   This unit is Open-Source under MPL<br>
   Copyright 2001 - Eric Grange (egrange@glscene.org)<br>
   http://glscene.org<p>

   <b>History :</b><ul>
      <li>21/02/01 - EG - Added TexGen and vertex arrays mappings
   </ul>
}
unit XOpenGL;

interface

uses OpenGL12;

type
   TMapTexCoordMode = (mtcmNull, mtcmMain, mtcmDual);

var
   xglMapTextCoordMode : TMapTexCoordMode;

{: xglTexCoord functions will be ignored. }
procedure xglMapTexCoordToNull;
{: xglTexCoord functions will define the main texture coordinates. }
procedure xglMapTexCoordToMain;
{: xglTexCoord functions will define the two first texture coordinates. }
procedure xglMapTexCoordToDual;

var
   // Explicit texture coordinates specification
   xglTexCoord2f: procedure(s, t: TGLfloat); stdcall;
   xglTexCoord2fv: procedure(v: PGLfloat); stdcall;
   xglTexCoord3f: procedure(s, t, r: TGLfloat); stdcall;
   xglTexCoord3fv: procedure(v: PGLfloat); stdcall;
   xglTexCoord4f: procedure(s, t, r, q: TGLfloat); stdcall;
   xglTexCoord4fv: procedure(v: PGLfloat); stdcall;

   // TexGen texture coordinates specification
   xglTexGenf: procedure(coord, pname: TGLEnum; param: TGLfloat); stdcall;
   xglTexGenfv: procedure(coord, pname: TGLEnum; params: PGLfloat); stdcall;
   xglTexGeni: procedure(coord, pname: TGLEnum; param: TGLint); stdcall;
   xglTexGeniv: procedure(coord, pname: TGLEnum; params: PGLint); stdcall;

   // Vertex Arrays texture coordinates specification
   xglTexCoordPointer: procedure(size: TGLint; atype: TGLEnum; stride: TGLsizei; data: pointer); stdcall;

   // Misc
   xglEnable: procedure(cap: TGLEnum); stdcall;
   xglDisable: procedure(cap: TGLEnum); stdcall;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------------------------------------------------------
// Multitexturing coordinates duplication functions
// ------------------------------------------------------------------

// --------- Dual Texturing

procedure glTexCoord2f_Dual(s, t: TGLfloat); stdcall;
begin
   glTexCoord2f(s, t);
   glMultiTexCoord2fARB(GL_TEXTURE1_ARB, s, t);
end;

procedure glTexCoord2fv_Dual(v: PGLfloat); stdcall;
begin
   glTexCoord2fv(v);
   glMultiTexCoord2fvARB(GL_TEXTURE1_ARB, v);
end;

procedure glTexCoord3f_Dual(s, t, r: TGLfloat); stdcall;
begin
   glTexCoord3f(s, t, r);
   glMultiTexCoord3fARB(GL_TEXTURE1_ARB, s, t, r);
end;

procedure glTexCoord3fv_Dual(v: PGLfloat); stdcall;
begin
   glTexCoord3fv(v);
   glMultiTexCoord3fvARB(GL_TEXTURE1_ARB, v);
end;

procedure glTexCoord4f_Dual(s, t, r, q: TGLfloat); stdcall;
begin
   glTexCoord4f(s, t, r, q);
   glMultiTexCoord4fARB(GL_TEXTURE1_ARB, s, t, r, q);
end;

procedure glTexCoord4fv_Dual(v: PGLfloat); stdcall;
begin
   glTexCoord4fv(v);
   glMultiTexCoord4fvARB(GL_TEXTURE1_ARB, v);
end;

procedure glTexGenf_Dual(coord, pname: TGLEnum; param: TGLfloat); stdcall;
begin
   glTexGenf(coord, pname, param);
   glActiveTextureARB(GL_TEXTURE1_ARB);
   glTexGenf(coord, pname, param);
   glActiveTextureARB(GL_TEXTURE0_ARB);
end;

procedure glTexGenfv_Dual(coord, pname: TGLEnum; params: PGLfloat); stdcall;
begin
   glTexGenfv(coord, pname, params);
   glActiveTextureARB(GL_TEXTURE1_ARB);
   glTexGenfv(coord, pname, params);
   glActiveTextureARB(GL_TEXTURE0_ARB);
end;

procedure glTexGeni_Dual(coord, pname: TGLEnum; param: TGLint); stdcall;
begin
   glTexGeni(coord, pname, param);
   glActiveTextureARB(GL_TEXTURE1_ARB);
   glTexGeni(coord, pname, param);
   glActiveTextureARB(GL_TEXTURE0_ARB);
end;

procedure glTexGeniv_Dual(coord, pname: TGLEnum; params: PGLint); stdcall;
begin
   glTexGeniv(coord, pname, params);
   glActiveTextureARB(GL_TEXTURE1_ARB);
   glTexGeniv(coord, pname, params);
   glActiveTextureARB(GL_TEXTURE0_ARB);
end;

procedure glEnable_Dual(cap: TGLEnum); stdcall;
begin
   glEnable(cap);
   glActiveTextureARB(GL_TEXTURE1_ARB);
   glEnable(cap);
   glActiveTextureARB(GL_TEXTURE0_ARB);
end;

procedure glDisable_Dual(cap: TGLEnum); stdcall;
begin
   glDisable(cap);
   glActiveTextureARB(GL_TEXTURE1_ARB);
   glDisable(cap);
   glActiveTextureARB(GL_TEXTURE0_ARB);
end;

procedure xglTexCoordPointer_Dual(size: TGLint; atype: TGLEnum; stride: TGLsizei; data: pointer); stdcall;
begin
   glTexCoordPointer(size, atype, stride, data);
   glActiveTextureARB(GL_TEXTURE1_ARB);
   glTexCoordPointer(size, atype, stride, data);
   glActiveTextureARB(GL_TEXTURE0_ARB);
end;

// --------- Null Texturing

procedure glTexCoord2f_Null(s, t: TGLfloat); stdcall;
begin end;

procedure glTexCoord2fv_Null(v: PGLfloat); stdcall;
begin end;

procedure glTexCoord3f_Null(s, t, r: TGLfloat); stdcall;
begin end;

procedure glTexCoord3fv_Null(v: PGLfloat); stdcall;
begin end;

procedure glTexCoord4f_Null(s, t, r, q: TGLfloat); stdcall;
begin end;

procedure glTexCoord4fv_Null(v: PGLfloat); stdcall;
begin end;

procedure glTexGenf_Null(coord, pname: TGLEnum; param: TGLfloat); stdcall;
begin end;

procedure glTexGenfv_Null(coord, pname: TGLEnum; params: PGLfloat); stdcall;
begin end;

procedure glTexGeni_Null(coord, pname: TGLEnum; param: TGLint); stdcall;
begin end;

procedure glTexGeniv_Null(coord, pname: TGLEnum; params: PGLint); stdcall;
begin end;

procedure glEnable_Null(cap: TGLEnum); stdcall;
begin end;

procedure glDisable_Null(cap: TGLEnum); stdcall;
begin end;

procedure xglTexCoordPointer_Null(size: TGLint; atype: TGLEnum; stride: TGLsizei; data: pointer); stdcall;
begin end;

// ------------------------------------------------------------------
// Redirections management functions
// ------------------------------------------------------------------

// xglMapTexCoordToNull
//
procedure xglMapTexCoordToNull;
begin
   if xglMapTextCoordMode<>mtcmNull then begin
      xglMapTextCoordMode:=mtcmNull;

      xglTexCoord2f:=glTexCoord2f_Null;
      xglTexCoord2fv:=glTexCoord2fv_Null;
      xglTexCoord3f:=glTexCoord3f_Null;
      xglTexCoord3fv:=glTexCoord3fv_Null;
      xglTexCoord4f:=glTexCoord4f_Null;
      xglTexCoord4fv:=glTexCoord4fv_Null;

      xglTexGenf:=glTexGenf_Null;
      xglTexGenfv:=glTexGenfv_Null;
      xglTexGeni:=glTexGeni_Null;
      xglTexGeniv:=glTexGeniv_Null;

      xglTexCoordPointer:=xglTexCoordPointer_Null;

      xglEnable:=glEnable_Null;
      xglDisable:=glDisable_Null;
   end;
end;

// xglTexCoordMapToMain
//
procedure xglMapTexCoordToMain;
begin
   if xglMapTextCoordMode<>mtcmMain then begin
      xglMapTextCoordMode:=mtcmMain;

      xglTexCoord2f:=glTexCoord2f;
      xglTexCoord2fv:=glTexCoord2fv;
      xglTexCoord3f:=glTexCoord3f;
      xglTexCoord3fv:=glTexCoord3fv;
      xglTexCoord4f:=glTexCoord4f;
      xglTexCoord4fv:=glTexCoord4fv;

      xglTexGenf:=glTexGenf;
      xglTexGenfv:=glTexGenfv;
      xglTexGeni:=glTexGeni;
      xglTexGeniv:=glTexGeniv;

      xglTexCoordPointer:=glTexCoordPointer;

      xglEnable:=glEnable;
      xglDisable:=glDisable;
   end;
end;

// xglTexCoordMapToDual
//
procedure xglMapTexCoordToDual;
begin
   if xglMapTextCoordMode<>mtcmDual then begin
      xglMapTextCoordMode:=mtcmDual;
      Assert(GL_ARB_multitexture);

      xglTexCoord2f:=glTexCoord2f_Dual;
      xglTexCoord2fv:=glTexCoord2fv_Dual;
      xglTexCoord3f:=glTexCoord3f_Dual;
      xglTexCoord3fv:=glTexCoord3fv_Dual;
      xglTexCoord4f:=glTexCoord4f_Dual;
      xglTexCoord4fv:=glTexCoord4fv_Dual;

      xglTexGenf:=glTexGenf_Dual;
      xglTexGenfv:=glTexGenfv_Dual;
      xglTexGeni:=glTexGeni_Dual;
      xglTexGeniv:=glTexGeniv_Dual;

      xglTexCoordPointer:=xglTexCoordPointer_Dual;

      xglEnable:=glEnable_Dual;
      xglDisable:=glDisable_Dual;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   xglMapTexCoordToNull;

end.
