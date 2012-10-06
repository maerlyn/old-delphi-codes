{: GLFileOBJ<p>

    Support-Code to load Wavefront OBJ Files into TFreeForm-Components
    in GLScene.<p>
    Note that you must manually add this unit to one of ypur project's uses
    to enable support for OBJ & OBJF at run-time.<p>

	<b>Historique : </b><font size=-1><ul>
      <li>03/10/00 - Egg - Fixed TGLOBJVectorFile.LoadFromStream.ReadFace
      <li>08/10/00 - Egg - Added standard header, basic htmlification of old header,
                           removed specific trim funcs,
                           renamed TMBAGLOBJVectorFile to TGLOBJVectorFile
   </ul><p>

   (c) 2000 Marian Aldenhövel<br>
       Hainstraße 8<br>
       53121 Bonn<br>
       info@MBA-Software.de<p>

  License:<br>

    Contributed to Eric Grange and GLScene,
    same licensing, batteries not included.<p>

  History:<p>

    26.9.2000:  - OBJF-Support (t- and q-lines) see
                  http://www.cs.sunysb.edu/~stripe/<br>
    18.9.2000:  - Bugfixing.
                - SaveTo-Methods.<br>
    14.9.2000:  - Calculate normals where missing in the file.
                - Collect Facegroups by Name and do not start a new
      				  one for every 'g'-line in the file.
                - Relative indexing in 'f'-lines.<br>
    13.9.2000:  - Start of this project as an exercise to get familiar with<br>
                    a) the OBJ-Format and<br>
                    b) GLScene internals<br>
                  Midterm-goal: Import what Poser 4 exports and display it
                                correctly in an GLScene.<br>
}
unit GLFileOBJ;

{.$DEFINE STATS} { Define to display statistics after loading. }

interface

uses Windows{$IFDEF STATS}, Dialogs{$ENDIF}, Classes, SysUtils, GLScene,
     Geometry, OpenGL12, GLMisc, GLVectorFileObjects, VectorLists, GLTexture;

const BufSize=10240; { Load input data in chunks of BufSize Bytes. }
      LineLen=100;   { Allocate memory for the current line in chunks
                       of LineLen Bytes. }

type { OBJ File reader. }
     TGLOBJVectorFile=class(TVectorFile)
       private
         FSourceStream:TStream;       { Load from this stream }
         FBuffer,FLine:string;        { Buffer and current line }
         FLineNo:integer;             { current Line number - for error messages }
         FEof:boolean;                { Stream done? }
         FBufPos:integer;             { Position in the buffer }
         procedure ReadLine;          { Read next line }
         procedure Error(Msg:string); { Raise a class-specific exception }
       public
         procedure LoadFromStream(aStream:TStream); override;
         procedure Clear;

         procedure SaveToStream(aStream:TStream); override;
       end;

     EGLOBJFileError=class(Exception)
       private
         FLineNo:integer;
       public
         property LineNo:integer read FLineNo;
       end;

implementation

{ Utility functions ********************************************************** }

function StreamEOF(S:TStream):boolean;
{ Is the stream at its end? }
begin Result:=(S.Position>=S.Size); end;

function Rest(const s:string;Count:integer):string;
{ Return the right part of s including s[Count]. }
begin
  Result:=copy(s,Count,Length(s)-Count+1);
end;

function NextToken(var s:string;Delimiter:char):string;
{ Return the next Delimiter-delimited Token from the string s and
  remove it from s }
var p:integer;
begin
  p:=pos(Delimiter,s);
  if p=0
    then
      begin
        Result:=s;
        s:='';
      end
    else
      begin
        Result:=copy(s,1,p-1);
        s:=TrimLeft(Rest(s,p+1));
      end;
end;

{ ** TOBJFGVertexNormalTexIndexList ****************************************** }
{ - based on TFGVertexNormalTexIndexList (GLVectorFileObjects.pas)
  - adds support for polygons and for "missing" normals and
    texture-coordinates. Pass -1 to Add for the index of a missing object.
  - Polygons are defined by counting off the number of vertices added to the
    PolygonVertices-property. So a PolygonVertices-List of

      [3,4,6]

    says "Vertex indices 0,1 and 2 make up a triangle, 3,4,5 and 6 a quad and
    7,8,9,10,11 and 12 a hexagon".

}
type
   TOBJFGMode=(objfgmmPolygons,objfgmmTriangleStrip);

   TOBJFGVertexNormalTexIndexList = class (TFGVertexNormalTexIndexList)
       private
         FMode:TOBJFGMode;
         FName:string;
         FPolygonVertices:TIntegerList;
         FCurrentVertexCount:integer;
         FShowNormals:boolean;
         procedure PolygonComplete; { Current polygon completed. Adds FCurrentVertexCount
                                      to FPolygonVertices and sets the variable to 0 }

         procedure SetMode(aMode:TOBJFGMode);
       public
         constructor Create(aOwner:TFaceGroups); override;
         destructor Destroy; override;

         procedure Add(VertexIdx,NormalIdx,TexCoordIdx:Integer);
         procedure BuildList(var mrci : TRenderContextInfo); override;

         property Mode:TOBJFGMode read FMode write SetMode;
         property Name:string read FName write FName;
         property PolygonVertices:TIntegerList read FPolygonVertices;
         property ShowNormals:boolean read FShowNormals write FShowNormals;
       end;

constructor TOBJFGVertexNormalTexIndexList.Create(aOwner:TFaceGroups);
begin
  inherited Create(aOwner);
  FMode:=objfgmmTriangleStrip;
  //FShowNormals:=True;
end;

destructor TOBJFGVertexNormalTexIndexList.Destroy;
begin
  FPolygonVertices.Free;
  inherited Destroy;
end;

procedure TOBJFGVertexNormalTexIndexList.Add(VertexIdx,NormalIdx,TexCoordIdx:Integer);
begin
  inherited Add(VertexIdx,NormalIdx,TexCoordIdx);
  inc(FCurrentVertexCount);
end;

procedure TOBJFGVertexNormalTexIndexList.PolygonComplete;
begin
  Assert(FMode=objfgmmPolygons,'PolygonComplete may only be called for Facegroups with Mode=objfgmmPolygons.');
  FPolygonVertices.Add(FCurrentVertexCount);
  FCurrentVertexCount:=0;
end;

procedure TOBJFGVertexNormalTexIndexList.SetMode(aMode:TOBJFGMode);
begin
  if aMode=FMode then exit;
  Assert(VertexIndices.Count=0,'Decide on the mode before adding vertices.');
  FMode:=aMode;
  if FMode=objfgmmPolygons
    then FPolygonVertices:=TIntegerList.Create
    else
      begin
        FPolygonVertices.Free;
        FPolygonVertices:=NIL;
      end;
end;

procedure TOBJFGVertexNormalTexIndexList.BuildList(var mrci : TRenderContextInfo);
var
   VertexPool:PAffineVectorArray;
   NormalPool:PAffineVectorArray;
   TexCoordPool:PAffineVectorArray;

  procedure BuildPolygons;
  var Polygon,Index,j,idx:Integer;
      N:TAffineVector;
  begin
    { Build it. Ignoring texture-coordinates and normals that are missing. }
    Index:=0; { Current index into the Index-Lists. }
    { For every Polygon }
    for Polygon:=0 to FPolygonVertices.Count-1 do
      begin
        glBegin(GL_POLYGON);
        try
          { For every Vertex in the current Polygon }
          for j:=0 to FPolygonVertices[Polygon]-1 do
            begin
              idx:=NormalIndices.List[Index];
              if idx<>-1 then glNormal3fv(@NormalPool[idx]);

              idx:=TexCoordIndices.List[Index];
              if idx<>-1 then glTexCoord2fv(@TexCoordPool[idx]);

              glVertex3fv(@VertexPool[VertexIndices.List[Index]]);
              inc(Index);
            end;
        finally
          glEnd;
        end;
      end;

    { Visible normals, rather moronic and mainly for debugging. }
    if FShowNormals then
      begin
        Index:=0;
        for Polygon:=0 to FPolygonVertices.Count-1 do
          begin
            { For every Vertex in the current Polygon }
            for j:=0 to FPolygonVertices[Polygon]-1 do
              begin
                idx:=NormalIndices.List[Index];
                if idx<>-1 then
                  begin
                    glBegin(GL_LINES);
                    try
                      glVertex3fv(@VertexPool[VertexIndices.List[Index]]);
                      N:=VectorAdd(VertexPool[VertexIndices.List[Index]],VectorScale(NormalPool[idx],0.1));
                      glVertex3fv(@N);
                    finally
                      glEnd;
                    end;
                  end;
                inc(Index);
              end;
          end;
      end;
  end;

  procedure BuildTriangleStrip;
  (*
  begin
    Owner.Owner.DeclareArraysToOpenGL(False);
    glDrawElements(GL_TRIANGLE_STRIP,VertexIndices.Count,
                   GL_UNSIGNED_INT,VertexIndices.List);
  end;
  *)
  var Index,idx:Integer;
  begin
    { Build it. Ignoring texture-coordinates and normals that are missing. }
    glBegin(GL_TRIANGLE_STRIP);
    try
      for Index:=0 to VertexIndices.Count-1 do
        begin
          idx:=NormalIndices.List[Index];
          if idx<>-1 then glNormal3fv(@NormalPool[idx]);

          idx:=TexCoordIndices.List[Index];
          if idx<>-1 then glTexCoord2fv(@TexCoordPool[idx]);

          glVertex3fv(@VertexPool[VertexIndices.List[Index]]);
        end;
    finally
      glEnd;
    end;
  end;

begin
  Assert(    (VertexIndices.Count=TexCoordIndices.Count)
         and (VertexIndices.Count=NormalIndices.Count),
         'Number of Vertices does not match number of Normals or Texture coordinates.');

  { Shorthand notations. }
  VertexPool:=Owner.Owner.Vertices.List;
  NormalPool:=Owner.Owner.Normals.List;
  TexCoordPool:=Owner.Owner.TexCoords.List;

  case FMode of
    objfgmmPolygons:     BuildPolygons;
    objfgmmTriangleStrip:BuildTriangleStrip;
  end;
end;

{ ** TGLOBJVectorFile ******************************************************** }

procedure TGLOBJVectorFile.ReadLine;
{ Read a single line of text from the source stream, set FEof to true when done. }
var j:integer;

  procedure FillBuffer;
  var L:integer;
  begin
    L:=(FSourceStream.Size-FSourceStream.Position);
    if L>BufSize then L:=BufSize;
    SetLength(FBuffer,L);
    FSourceStream.Read(FBuffer[1],L);
    FBufPos:=1;
  end;

begin
  inc(FLineNo);

  if (FBufPos<1) then FillBuffer;

  j:=1;
  while True do
    if FBufPos>Length(FBuffer)
      then
        if StreamEof(FSourceStream)
          then
            begin
              FEof:=True;
              break
            end
          else FillBuffer
      else
        begin
          if (FBuffer[FBufPos]=#10) or (FBuffer[FBufPos]=#13)
            then
              begin
                inc(FBufPos);
                if FBufPos>Length(FBuffer)
                  then
                    if StreamEof(FSourceStream)
                      then break
                      else FillBuffer;
                if (FBuffer[FBufPos]=#10) or (FBuffer[FBufPos]=#13) then inc(FBufPos);
                break;
              end
            else
              begin
                if j>Length(FLine) then SetLength(FLine,Length(FLine)+LineLen);
                FLine[j]:=FBuffer[FBufPos];
                inc(FBufPos);
                inc(j);
              end;
        end;

  SetLength(FLine,j-1);
end;

procedure TGLOBJVectorFile.Error(Msg:string);
var E:EGLOBJFileError;
begin
  E:=EGLOBJFileError.Create(Msg);
  E.FLineNo:=FLineNo;
  raise E;
end;

procedure TGLOBJVectorFile.Clear;
begin
  { Humph... }
end;

procedure TGLOBJVectorFile.LoadFromStream(aStream:TStream);
var hv:THomogeneousVector;
    av:TAffineVector;
    Mesh:TMeshObject;
    FaceGroup:TOBJFGVertexNormalTexIndexList;
    FaceGroupNames:TStringList;

  procedure ReadHomogeneousVector;
  { Read a vector with a maximum of 4 elements from the current line. }
  var i,c:integer;
      f:string;
  begin
    FillChar(hv,SizeOf(hv),0);
    i:=0;
    while (FLine<>'') and (i<4) do
      try
        f:=NextToken(FLine,' ');
        val(f,hv[i],c);
        if c<>0 then Error(Format('''%s'' is not a valid floating-point constant.',[f]));
        inc(i);
      except
        On E:Exception do Error(Format('%s:''%s''',[E.ClassName,E.Message]));
        else Raise;
      end;
  end;

  procedure ReadAffineVector;
  { Read a vector with a maximum of 3 elements from the current line. }
  var i,c:integer;
      f:string;
  begin
    FillChar(av,SizeOf(av),0);
    i:=0;
    while (FLine<>'') and (i<3) do
      try
        f:=NextToken(FLine,' ');
        val(f,av[i],c);
        if c<>0 then Error(Format('''%s'' is not a valid floating-point constant.',[f]));
        inc(i);
      except
        On E:Exception do Error(Format('%s:''%s''',[E.ClassName,E.Message]));
        else Raise;
      end;
  end;

  procedure SetCurrentFaceGroup(aName:string);
  var i:integer;
  begin
    i:=FaceGroupNames.IndexOf(aName);
    if i=-1
      then
        begin
          FaceGroup:=TOBJFGVertexNormalTexIndexList.Create(Mesh.FaceGroups);
          FaceGroup.FName:=aName;
          FaceGroup.Mode:=objfgmmPolygons;
          FaceGroupNames.AddObject(aName,FaceGroup);
        end
      else FaceGroup:=TOBJFGVertexNormalTexIndexList(FaceGroupNames.Objects[i]);
  end;

  procedure AddFaceVertex(FaceVertices:String);
  var s:string;
      vIdx,tIdx,nIdx:Integer;

    function GetIndex(Count:integer):integer;
    begin
      s:=NextToken(FaceVertices,'/');
      Result:=StrToIntDef(s,0);

      if Result=0
        then Result:=-1 // Missing
        else
          if Result<0
            then
              { Relative, make absolute. "-1" means last, "-2" second last. }
              Result:=Mesh.Vertices.Count+Result
            else
              { Absolute, correct for zero-base. }
              dec(Result);
    end;

  begin
    vIdx:=GetIndex(Mesh.Vertices.Count);
    tIdx:=GetIndex(Mesh.TexCoords.Count);
    nIdx:=GetIndex(Mesh.Normals.Count);

    FaceGroup.Add(vIdx,nIdx,tIdx);
  end;

   procedure ReadFace;
   var
      faceVertices : String;
   begin
      if FLine<>'' then begin
         if not Assigned(FaceGroup) then
            SetCurrentFaceGroup('');
         try
            while FLine<>'' do begin
               faceVertices:=NextToken(FLine, ' ');
               AddFaceVertex(faceVertices);
            end;
         finally
            FaceGroup.PolygonComplete;
         end;
      end;
   end;

  procedure CalcMissingNormals;
  { Calculate all the missing normals }
  var VertexPool:PAffineVectorArray;
      n:TAffineVector;
      p:array[1..3] of PAffineVector;
      Face,Index:integer;
      FG:TOBJFGVertexNormalTexIndexList;

    procedure DoCalcNormal;
    var idx:integer;
    begin
      idx:=TOBJFGVertexNormalTexIndexList(Mesh.FaceGroups[Face]).NormalIndices.List[Index];
      if idx=-1 then
        begin
          n:=CalcPlaneNormal(p[1]^,p[2]^,p[3]^);
          Mesh.Normals.Add(n[0],n[1],n[2]);
          TOBJFGVertexNormalTexIndexList(Mesh.FaceGroups[Face]).NormalIndices.List[Index]:=Mesh.Normals.Count-1;
        end;
    end;

    procedure CalcForPolygons;
    var Polygon,FirstVertexIndex,j:Integer;
    begin
      with FG do
        begin
          { Walk the polygons and calculate normals for those vertices that
            are missing. }
          Index:=0; { Current index into the Index-List of this Facegroup. }

          { For every Polygon }
          for Polygon:=0 to FPolygonVertices.Count-1 do
            begin
              { Init }
              FirstVertexIndex:=Index;
              FillChar(p,SizeOf(p),0);
              { Last Vertex in this polygon }
              p[2]:=@VertexPool[VertexIndices.List[Index+FPolygonVertices[Polygon]-1]];
              { First Vertex in this polygon }
              p[3]:=@VertexPool[VertexIndices.List[Index]];
              { For every Vertex in the current Polygon but the last. }
              for j:=0 to FPolygonVertices[Polygon]-2 do
                begin
                  Move(p[2],p[1],2*SizeOf(PAffineVector));
                  p[3]:=@VertexPool[VertexIndices.List[Index+1]];
                  DoCalcNormal;
                  inc(Index);
                end;

              { For the last vertex use the first as partner to span the plane. }
              Move(p[2],p[1],2*SizeOf(PAffineVector));
              p[3]:=@VertexPool[VertexIndices.List[FirstVertexIndex]];
              DoCalcNormal;
              inc(Index);
            end; { of for FPolygonVertices }
        end; { of with Facegroup }
    end;

    procedure CalcForTriangleStrip;
    begin
    end;

  begin
    { Shorthand notations. }
    VertexPool:=Mesh.Vertices.List;

    for Face:=0 to Mesh.FaceGroups.Count-1 do
      begin
        FG:=TOBJFGVertexNormalTexIndexList(Mesh.FaceGroups[Face]);
        case FG.Mode of
          objfgmmPolygons:     CalcForPolygons;
          objfgmmTriangleStrip:CalcForTriangleStrip;
        end;
      end;
  end;

  procedure ReadTriangleStripContinued;
  var FaceVertices:string;
  begin
    if FaceGroup=NIL then Error('q-line without preceding t-line.');
    if FLine<>'' then
      while FLine<>'' do
        begin
          FaceVertices:=NextToken(FLine,' ');
          AddFaceVertex(FaceVertices);
        end;
  end;

  procedure ReadTriangleStrip;
  { Start a new Facegroup, mode=triangle strip }
  begin
    FaceGroup:=TOBJFGVertexNormalTexIndexList.Create(Mesh.FaceGroups);
    FaceGroup.Mode:=objfgmmTriangleStrip;
    { The rest is the same as for continuation of a strip. }
    ReadTriangleStripContinued;
  end;

var Command:string;
    {$IFDEF STATS}t0,t1,t2:integer;{$ENDIF}
begin
  {$IFDEF STATS} t0:=GetTickCount; {$ENDIF}

  Clear;
  FEof:=False;
  FSourceStream:=aStream;
  FLineNo:=0;

  Mesh:=TMeshObject.Create(Owner.MeshObjects);
  Mesh.Mode:=momFaceGroups;

  FaceGroupNames:=TStringList.Create;
  FaceGroupNames.Duplicates:=dupError;
  FaceGroupNames.Sorted:=True;
  FaceGroup:=NIL;

  while not FEof do
    begin
      ReadLine;
      if FLine=''     then continue; { Skip blank line }
      if FLine[1]='#' then continue; { Skip comment }

      Command:=AnsiUpperCase(NextToken(FLine,' '));

           if Command='V'      then
                                 begin
                                   ReadHomogeneousVector;
                                   Mesh.Vertices.Add(hv[0],hv[1],hv[2]);
                                 end
      else if Command='VT'     then
                                 begin
                                   ReadAffineVector;
                                   Mesh.TexCoords.Add(av[0],av[1],0);
                                 end
      else if Command='VN'     then
                                 begin
                                   ReadAffineVector;
                                   Mesh.Normals.Add(av[0],av[1],av[2]);
                                 end
      else if Command='VP'     then { Parameter Space Vertex: Ignore }
      else if Command='G'      then
                                 { Only the first name on the line, multiple groups not supported. }
                                 SetCurrentFaceGroup(NextToken(FLine,' '))
      else if Command='F'      then ReadFace
      else if Command='O'      then { Object Name:  Ignore }
      else if Command='MTLLIB' then { Material Library: Ignore }
      else if Command='USEMTL' then { Use Material: Ignore }
      else if Command='S'      then { Smooth Group: Ignore }
      else if Command='T'      then ReadTriangleStrip
      else if Command='Q'      then ReadTriangleStripContinued
      else Error('Unsupported Command '''+Command+'''');
    end;

  {$IFDEF STATS} t1:=GetTickCount; {$ENDIF}

  CalcMissingNormals;

  {$IFDEF STATS}
    t2:=GetTickCount;
    ShowMessage(Format('TGLOBJVectorFile Loaded in %dms'#13+
                       #13+
                       '    %dms spent reading'#13+
                       '    %dms spent calculating normals'#13+
                       '    %d Geometric Vertices'#13+
                       '    %d Texture Vertices'#13+
                       '    %d Normals'#13+
                       '    %d FaceGroups/Strips',
                       [t2-t0,
                        t1-t0,
                        t2-t1,
                        Mesh.Vertices.Count,
                        Mesh.TexCoords.Count,
                        Mesh.Normals.Count,
                        Mesh.FaceGroups.Count]));
  {$ENDIF}
end;

procedure TGLOBJVectorFile.SaveToStream(aStream:TStream);
var Mesh:TMeshObject;
    OldDecimalSeparator:char;
    
  procedure Write(s:string);
  begin
    if s<>'' then aStream.Write(s[1],Length(s));
  end;

  procedure WriteLn(s:string);
  begin
    Write(s);
    Write(#13#10);
  end;

  procedure WriteHeader;
  begin
    WriteLn('# OBJ-File exported by GLScene');
    WriteLn('');
  end;

  procedure WriteVertices;
  var s:string;
      i:integer;
  begin
    WriteLn(Format('# %d Vertices:',[Mesh.Vertices.Count]));
    with Mesh.Vertices do
      for i:=0 to Count-1 do
        begin
          s:=Format('v %g %g %g',[List[i][0],List[i][1],List[i][2]]);
          Writeln(s);
        end;
    WriteLn('');
  end;

  procedure WriteNormals;
  var s:string;
      i:integer;
  begin
    WriteLn(Format('# %d Normals:',[Mesh.Normals.Count]));
    with Mesh.Normals do
      for i:=0 to Count-1 do
        begin
          s:=Format('vn %g %g %g',[List[i][0],List[i][1],List[i][2]]);
          Writeln(s);
        end;
    WriteLn('');
  end;

  procedure WriteTexCoords;
  var s:string;
      i:integer;
  begin
    WriteLn(Format('# %d Texture-Coordinates:',[Mesh.TexCoords.Count]));
    with Mesh.TexCoords do
      for i:=0 to Count-1 do
        begin
          s:=Format('vt %g %g',[List[i][0],List[i][1]]);
          Writeln(s);
        end;
    WriteLn('');
  end;

  procedure WriteFaceGroup(aFaceGroup:TOBJFGVertexNormalTexIndexList);
  var vIdx,nIdx,tIdx:integer;
      i,Index,Polygon:integer;
      Line,t:string;
  begin
    with aFaceGroup do
      begin
        if Name=''
          then Writeln('g')
          else WriteLn('g '+Name);
        Index:=0;
        for Polygon:=0 to PolygonVertices.Count-1 do
          begin
            Line:='f ';
            for i:=1 to PolygonVertices[Polygon] do
              begin
                vIdx:=VertexIndices[Index]+1;
                nIdx:=NormalIndices[Index]+1;
                tIdx:=TexCoordIndices[Index]+1;
                t:=IntToStr(vIdx)+'/';
                if tIdx=-1 then t:=t+'/' else t:=t+IntToStr(tIdx)+'/';
                if nIdx=-1 then t:=t+'/' else t:=t+IntToStr(nIdx)+'/';
                Line:=Line+Copy(t,1,length(t)-1)+' ';
                inc(Index);
              end;
            Writeln(Line);
          end;
      end;
    Writeln('');
  end;

  procedure WriteFaceGroups;
  var i:integer;
  begin
    for i:=0 to Mesh.FaceGroups.Count-1 do
      if Mesh.FaceGroups[i] is TOBJFGVertexNormalTexIndexList then
        WriteFaceGroup(TOBJFGVertexNormalTexIndexList(Mesh.FaceGroups[i]));
  end;

begin
  Assert(Owner is TFreeForm,'Can only save FreeForms.');
  Assert(Owner.MeshObjects.Count<=1,'Only single meshes supported.');

  OldDecimalSeparator:=DecimalSeparator;
  DecimalSeparator:='.';
  { Better not call anything that wants the system-locale intact
    from this block }
  try
    WriteHeader;

    if Owner.MeshObjects.Count>0 then
      begin
        Mesh:=Owner.MeshObjects[0];

        WriteVertices;
        WriteNormals;
        WriteTexCoords;
        WriteFaceGroups;
      end;
  finally
    DecimalSeparator:=OldDecimalSeparator;
  end;
end;

initialization

  { Register this Fileformat-Handler with GLScene }
  RegisterVectorFileFormat('obj','WaveFront model file',TGLOBJVectorFile);
  RegisterVectorFileFormat('objf','Stripe model file',  TGLOBJVectorFile);

finalization

  { Unregister, why bother? :-) }
  UnregisterVectorFileClass(TGLOBJVectorFile);

end.

