{: FileMD2<p>

	MD2 file loader<p>

	<b>Historique : </b><font size=-1><ul>
      <li>21/07/00 - Egg - Added frame names (Roger Cao/Carlos A. Rivero)
      <li>07/06/00 - Egg - Added Header, reduced dependencies,
                           LoadFromFile replaced with LoadFromStream,
                           some cleanup & optimizations
	</ul></font>
}
unit FileMD2;

interface

{$R-}

uses Classes, TypesMD2;

type

   // TFileMD2
   //
   TFileMD2 = class
      private
         fm_iFrames, fm_iVertices, fm_iTriangles: LongInt;
      public
         m_index_list: PMake_index_list;
         m_frame_list: PMake_frame_list;

         frameNames : TStrings;

         constructor Create; virtual;
         destructor Destroy; override;

         procedure LoadFromStream(aStream : TStream);

         property m_iFrames: LongInt read fm_iFrames;
         property m_iVertices: LongInt read fm_iVertices;
         property m_iTriangles: LongInt read fm_iTriangles;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, Geometry;

// ------------------
// ------------------ TFileMD2 ------------------
// ------------------

// Create
//
constructor TFileMD2.Create;
begin
   inherited;
   m_index_list := nil;
   m_frame_list := nil;
   fm_iFrames := 0;
   fm_iVertices := 0;
   fm_iTriangles := 0;
   frameNames:=TStringList.Create;
end;

// Destroy
//
destructor TFileMD2.Destroy;
var
   i : Integer;
begin
   if Assigned(m_frame_list) then begin
      for i:=0 to fm_iFrames-1 do
         Dispose(FrameList(m_frame_list)[i].vertex);
      Dispose(m_frame_list);
      if Assigned(m_index_list) then
         Dispose(m_index_list);
   end;
   frameNames.Free;
   inherited;
end;

// LoadFromStream
//
procedure TFileMD2.LoadFromStream(aStream : TStream);
var
   g_skins: array[0..MAX_MD2SKINS-1, 0..63] of Char;
   base_st: array[0..MAX_VERTS-1] of Tdstvert_t;
   buffer: array[0..MAX_VERTS*4+128-1] of Byte;
   modelheader: tdmdl_t;
   tri: tdtriangle_t;
   out_t: pdaliasframe_t;
   i, j: Integer;
   frameName : String;
begin
   if Assigned(m_frame_list) then begin
      for i:=0 to fm_iFrames-1 do
        Dispose(FrameList(m_frame_list)[i].vertex);
      Dispose(m_frame_list);
      if Assigned(m_index_list) then
         Dispose(m_index_list);
   end;

   aStream.Read(ModelHeader, sizeof(ModelHeader));

   ////////////////////////////////////////////////////

   fm_iFrames := modelheader.num_frames;
   fm_iVertices := modelheader.num_xyz;
   fm_iTriangles := modelheader.num_tris;

   m_index_list := AllocMem(sizeof(tmake_index_list) * modelheader.num_tris);
   m_frame_list := AllocMem(Sizeof(tmake_frame_list) * modelheader.num_frames);

   for i:=0 to modelheader.num_frames-1 do
      frameList(m_frame_list)[i].vertex := AllocMem(sizeof(tmake_vertex_list) * modelheader.num_xyz);

   ////////////////////////////////////////////////////

   aStream.Read(g_Skins, modelheader.num_skins * MAX_SKINNAME);
   aStream.Read(base_st, modelheader.num_st * sizeof(base_st[0]));

   for i:=0 to modelheader.num_tris-1 do begin
      aStream.Read(Tri, sizeof(tdtriangle_t));
      with IndexList(m_index_list)[i] do begin
         a := tri.index_xyz[2];
         b := tri.index_xyz[1];
         c := tri.index_xyz[0];
         a_s := base_st[tri.index_st[2]].s/ModelHeader.skinwidth;
         a_t := base_st[tri.index_st[2]].t/ModelHeader.skinHeight;
         b_s := base_st[tri.index_st[1]].s/ModelHeader.skinwidth;
         b_t := base_st[tri.index_st[1]].t/ModelHeader.skinHeight;
         c_s := base_st[tri.index_st[0]].s/ModelHeader.skinwidth;
         c_t := base_st[tri.index_st[0]].t/ModelHeader.skinHeight;
      end;
   end;

   for i:=0 to modelheader.num_frames-1 do begin
      out_t := pdaliasframe_t(@buffer);
      aStream.Read(out_t^, modelheader.framesize);
      frameName:=Trim(out_t^.name);
      if Copy(frameName, Length(frameName)-1, 1)[1] in ['0'..'9'] then
         frameName:=Copy(frameName, 1, Length(frameName)-2)
      else frameName:=Copy(frameName, 1, Length(frameName)-1);
      if frameNames.IndexOf(frameName)<0 then
         frameNames.AddObject(frameName, Pointer(i));
      for j:=0 to modelheader.num_xyz-1 do begin
         with VertList(FrameList(m_frame_list)[i].vertex)[j] do begin
            x := out_t^.verts[j].v[0] * out_t^.scale[0] + out_t^.translate[0];
            y := out_t^.verts[j].v[1] * out_t^.scale[1] + out_t^.translate[1];
            z := out_t^.verts[j].v[2] * out_t^.scale[2] + out_t^.translate[2];
         end;
      end;
   end;
end;

end.

