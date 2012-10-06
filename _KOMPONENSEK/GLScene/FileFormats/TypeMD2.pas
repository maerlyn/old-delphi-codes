unit TypeMD2;

interface

const
  MAX_TRIANGLES = 4096;
  MAX_VERTS = 2048;
  MAX_FRAMES = 512;
  MAX_MD2SKINS = 32;
  MAX_SKINNAME = 64;

type
  PMake_Index_List = ^TMake_Index_List;
  TMake_Index_List = record
    a, b, c: Integer;
    a_s, a_t,
    b_s, b_t,
    c_s, c_t: Single;
  end;

  PMake_Vertex_List = ^TMake_Vertex_List;
  TMake_Vertex_List = record
    x, y, z: Single;
  end;

  PMake_Frame_List = ^TMake_Frame_List;
  TMake_Frame_List = record
    Vertex: PMake_vertex_list;
  end;

  TVec3_T = record
    V: array[0..2] of Single;
  end;

  TDstVert_T = record
    s: SmallInt;
    t: SmallInt;
  end;

  TDtriangle_t = record
    index_xyz: array[0..2] of SmallInt;
    index_st: array[0..2] of SmallInt;
  end;

  TDtrivertx_t = record
    v: array[0..2] of Byte;
    lightnormalindex: byte;
  end;

  PDaliasframe_t = ^Tdaliasframe_t;
  Tdaliasframe_t = record
    scale: array[0..2] of Single;
    translate: array[0..2] of Single;
    name: array[0..15] of Char;
    verts: array[0..0] of Tdtrivertx_t;
  end;

  TDmdl_T = record
    ident: Integer;
    version: Integer;

    skinwidth: Integer;
    skinheight: Integer;
    framesize: Integer;

    num_skins: Integer;
    num_xyz: Integer;
    num_st: Integer;
    num_tris: Integer;
    num_glcmds: Integer;
    num_frames: Integer;

    ofs_skins: Integer;
    ofs_st: Integer;
    ofs_tris: Integer;
    ofs_frames: Integer;
    ofs_glcmds: Integer;
    ofs_end: Integer;
  end;

  TTrivert_t = record
    V: Tvec3_t;
    lightnormalindex: Integer;
  end;

  TFrame_t = record
    mins, maxs: TVec3_t;
    name: array[0..15] of Char;
    v: array[0..MAX_VERTS-1] of TTrivert_t;
  end;

  frameList = array of TMake_Frame_List;
  indexList = array of TMake_Index_List;
  VertList = array of TMake_Vertex_List;
implementation

end.
