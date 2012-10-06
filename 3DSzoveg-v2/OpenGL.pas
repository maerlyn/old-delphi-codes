unit OpenGL;

interface

uses Windows;

type TRCOptions = set of (opDoubleBuffered,opGDI,opStereo);

     GLenum      = UINT;      PGLenum     = ^GLenum;
     GLboolean   = UCHAR;     PGLboolean  = ^GLboolean;
     GLbitfield  = UINT;      PGLbitfield = ^GLbitfield;
     GLbyte      = ShortInt;  PGLbyte     = ^GLbyte;
     GLshort     = SHORT;     PGLshort    = ^GLshort;
     GLint       = Integer;   PGLint      = ^GLint;
     GLsizei     = Integer;   PGLsizei    = ^GLsizei;
     GLubyte     = UCHAR;     PGLubyte    = ^GLubyte;
     GLushort    = Word;      PGLushort   = ^GLushort;
     GLuint      = UINT;      PGLuint     = ^GLuint;
     GLfloat     = Single;    PGLfloat    = ^GLfloat;
     GLclampf    = Single;    PGLclampf   = ^GLclampf;
     GLdouble    = Double;    PGLdouble   = ^GLdouble;
     GLclampd    = Double;    PGLclampd   = ^GLclampd;

//------------------------------------------------------------------------------

var GL_VERSION_1_0,
    GL_VERSION_1_1,
    GL_VERSION_1_2,
    GLU_VERSION_1_1,
    GLU_VERSION_1_2 : Boolean;

    // Extensions (gl)
    GL_EXT_abgr,
    GL_EXT_bgra,
    GL_EXT_packed_pixels,
    GL_EXT_paletted_texture,
    GL_EXT_vertex_array,
    GL_SGI_compiled_vertex_array,
    GL_SGI_cull_vertex,
    GL_SGI_index_array_formats,
    GL_SGI_index_func,
    GL_SGI_index_material,
    GL_SGI_index_texture,
    GL_WIN_swap_hint,
    GL_EXT_blend_color,
    GL_EXT_blend_logic_op,
    GL_EXT_blend_minmax,
    GL_EXT_blend_subtract,
    GL_EXT_convolution,
    GL_EXT_copy_texture,
    GL_EXT_histogram,
    GL_EXT_polygon_offset,
    GL_EXT_subtexture,
    GL_EXT_texture,
    GL_EXT_texture_object,
    GL_EXT_texture3D,
    GL_EXT_cmyka,
    GL_EXT_rescale_normal,
    GL_SGI_color_matrix,
    GL_SGI_texture_color_table,
    GL_SGI_color_table,
    GL_EXT_clip_volume_hint,
    GL_EXT_misc_attribute,
    GL_EXT_scene_marker,
    GL_EXT_shared_texture_palette,

    // Extensions (glu)
    GLU_EXT_TEXTURE,
    GLU_EXT_object_space_tess,
    GLU_EXT_nurbs_tessellator     : Boolean;

//------------------------------------------------------------------------------

const // ********** GL generic constants **********

      // AttribMask
      GL_CURRENT_BIT                             = $00000001;
      GL_POINT_BIT                               = $00000002;
      GL_LINE_BIT                                = $00000004;
      GL_POLYGON_BIT                             = $00000008;
      GL_POLYGON_STIPPLE_BIT                     = $00000010;
      GL_PIXEL_MODE_BIT                          = $00000020;
      GL_LIGHTING_BIT                            = $00000040;
      GL_FOG_BIT                                 = $00000080;
      GL_DEPTH_BUFFER_BIT                        = $00000100;
      GL_ACCUM_BUFFER_BIT                        = $00000200;
      GL_STENCIL_BUFFER_BIT                      = $00000400;
      GL_VIEWPORT_BIT                            = $00000800;
      GL_TRANSFORM_BIT                           = $00001000;
      GL_ENABLE_BIT                              = $00002000;
      GL_COLOR_BUFFER_BIT                        = $00004000;
      GL_HINT_BIT                                = $00008000;
      GL_EVAL_BIT                                = $00010000;
      GL_LIST_BIT                                = $00020000;
      GL_TEXTURE_BIT                             = $00040000;
      GL_SCISSOR_BIT                             = $00080000;
      GL_ALL_ATTRIB_BITS                         = $000FFFFF;

      // ClientAttribMask
      GL_CLIENT_PIXEL_STORE_BIT                  = $00000001;
      GL_CLIENT_VERTEX_ARRAY_BIT                 = $00000002;
      GL_CLIENT_ALL_ATTRIB_BITS                  = $FFFFFFFF;

      // Boolean
      GL_FALSE                                   = 0;
      GL_TRUE                                    = 1;

      // BeginMode
      GL_POINTS                                  = $0000;
      GL_LINES                                   = $0001;
      GL_LINE_LOOP                               = $0002;
      GL_LINE_STRIP                              = $0003;
      GL_TRIANGLES                               = $0004;
      GL_TRIANGLE_STRIP                          = $0005;
      GL_TRIANGLE_FAN                            = $0006;
      GL_QUADS                                   = $0007;
      GL_QUAD_STRIP                              = $0008;
      GL_POLYGON                                 = $0009;

      // AccumOp
      GL_ACCUM                                   = $0100;
      GL_LOAD                                    = $0101;
      GL_RETURN                                  = $0102;
      GL_MULT                                    = $0103;
      GL_ADD                                     = $0104;

      // AlphaFunction
      GL_NEVER                                   = $0200;
      GL_LESS                                    = $0201;
      GL_EQUAL                                   = $0202;
      GL_LEQUAL                                  = $0203;
      GL_GREATER                                 = $0204;
      GL_NOTEQUAL                                = $0205;
      GL_GEQUAL                                  = $0206;
      GL_ALWAYS                                  = $0207;

      // BlendingFactorDest
      GL_ZERO                                    = 0;
      GL_ONE                                     = 1;
      GL_SRC_COLOR                               = $0300;
      GL_ONE_MINUS_SRC_COLOR                     = $0301;
      GL_SRC_ALPHA                               = $0302;
      GL_ONE_MINUS_SRC_ALPHA                     = $0303;
      GL_DST_ALPHA                               = $0304;
      GL_ONE_MINUS_DST_ALPHA                     = $0305;

      // BlendingFactorSrc
      GL_DST_COLOR                               = $0306;
      GL_ONE_MINUS_DST_COLOR                     = $0307;
      GL_SRC_ALPHA_SATURATE                      = $0308;

      // BlendingFactor (Src and Dest) GL 1.2 ARB imaging
      GL_CONSTANT_COLOR                          = $8001;
      GL_ONE_MINUS_CONSTANT_COLOR                = $8002;
      GL_CONSTANT_ALPHA                          = $8003;
      GL_ONE_MINUS_CONSTANT_ALPHA                = $8004;

      // BlendEquation GL 1.2 ARB imaging
      GL_FUNC_ADD                                = $8006;
      GL_MIN                                     = $8007;
      GL_MAX                                     = $8008;
      GL_FUNC_SUBTRACT                           = $800A;
      GL_FUNC_REVERSE_SUBTRACT                   = $800B;

      // color table GL 1.2 ARB imaging
      GL_COLOR_TABLE                             = $80D0;
      GL_POST_CONVOLUTION_COLOR_TABLE            = $80D1;
      GL_POST_COLOR_MATRIX_COLOR_TABLE           = $80D2;
      GL_PROXY_COLOR_TABLE                       = $80D3;
      GL_PROXY_POST_CONVOLUTION_COLOR_TABLE      = $80D4;
      GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE     = $80D5;
      GL_COLOR_TABLE_SCALE                       = $80D6;
      GL_COLOR_TABLE_BIAS                        = $80D7;
      GL_COLOR_TABLE_FORMAT                      = $80D8;
      GL_COLOR_TABLE_WIDTH                       = $80D9;
      GL_COLOR_TABLE_RED_SIZE                    = $80DA;
      GL_COLOR_TABLE_GREEN_SIZE                  = $80DB;
      GL_COLOR_TABLE_BLUE_SIZE                   = $80DC;
      GL_COLOR_TABLE_ALPHA_SIZE                  = $80DD;
      GL_COLOR_TABLE_LUMINANCE_SIZE              = $80DE;
      GL_COLOR_TABLE_INTENSITY_SIZE              = $80DF;

      // convolutions GL 1.2 ARB imaging
      GL_CONVOLUTION_1D                          = $8010;
      GL_CONVOLUTION_2D                          = $8011;
      GL_SEPARABLE_2D                            = $8012;
      GL_CONVOLUTION_BORDER_MODE                 = $8013;
      GL_CONVOLUTION_FILTER_SCALE                = $8014;
      GL_CONVOLUTION_FILTER_BIAS                 = $8015;
      GL_REDUCE                                  = $8016;
      GL_CONVOLUTION_FORMAT                      = $8017;
      GL_CONVOLUTION_WIDTH                       = $8018;
      GL_CONVOLUTION_HEIGHT                      = $8019;
      GL_MAX_CONVOLUTION_WIDTH                   = $801A;
      GL_MAX_CONVOLUTION_HEIGHT                  = $801B;
      GL_POST_CONVOLUTION_RED_SCALE              = $801C;
      GL_POST_CONVOLUTION_GREEN_SCALE            = $801D;
      GL_POST_CONVOLUTION_BLUE_SCALE             = $801E;
      GL_POST_CONVOLUTION_ALPHA_SCALE            = $801F;
      GL_POST_CONVOLUTION_RED_BIAS               = $8020;
      GL_POST_CONVOLUTION_GREEN_BIAS             = $8021;
      GL_POST_CONVOLUTION_BLUE_BIAS              = $8022;
      GL_POST_CONVOLUTION_ALPHA_BIAS             = $8023;

      // histogram GL 1.2 ARB imaging
      GL_HISTOGRAM                               = $8024;
      GL_PROXY_HISTOGRAM                         = $8025;
      GL_HISTOGRAM_WIDTH                         = $8026;
      GL_HISTOGRAM_FORMAT                        = $8027;
      GL_HISTOGRAM_RED_SIZE                      = $8028;
      GL_HISTOGRAM_GREEN_SIZE                    = $8029;
      GL_HISTOGRAM_BLUE_SIZE                     = $802A;
      GL_HISTOGRAM_ALPHA_SIZE                    = $802B;
      GL_HISTOGRAM_LUMINANCE_SIZE                = $802C;
      GL_HISTOGRAM_SINK                          = $802D;
      GL_MINMAX                                  = $802E;
      GL_MINMAX_FORMAT                           = $802F;
      GL_MINMAX_SINK                             = $8030;

      // DrawBufferMode
      GL_NONE                                    = 0;
      GL_FRONT_LEFT                              = $0400;
      GL_FRONT_RIGHT                             = $0401;
      GL_BACK_LEFT                               = $0402;
      GL_BACK_RIGHT                              = $0403;
      GL_FRONT                                   = $0404;
      GL_BACK                                    = $0405;
      GL_LEFT                                    = $0406;
      GL_RIGHT                                   = $0407;
      GL_FRONT_AND_BACK                          = $0408;
      GL_AUX0                                    = $0409;
      GL_AUX1                                    = $040A;
      GL_AUX2                                    = $040B;
      GL_AUX3                                    = $040C;

      // ErrorCode
      GL_NO_ERROR                                = 0;
      GL_INVALID_ENUM                            = $0500;
      GL_INVALID_VALUE                           = $0501;
      GL_INVALID_OPERATION                       = $0502;
      GL_STACK_OVERFLOW                          = $0503;
      GL_STACK_UNDERFLOW                         = $0504;
      GL_OUT_OF_MEMORY                           = $0505;

      // FeedbackType
      GL_2D                                      = $0600;
      GL_3D                                      = $0601;
      GL_3D_COLOR                                = $0602;
      GL_3D_COLOR_TEXTURE                        = $0603;
      GL_4D_COLOR_TEXTURE                        = $0604;

      // FeedBackToken
      GL_PASS_THROUGH_TOKEN                      = $0700;
      GL_POINT_TOKEN                             = $0701;
      GL_LINE_TOKEN                              = $0702;
      GL_POLYGON_TOKEN                           = $0703;
      GL_BITMAP_TOKEN                            = $0704;
      GL_DRAW_PIXEL_TOKEN                        = $0705;
      GL_COPY_PIXEL_TOKEN                        = $0706;
      GL_LINE_RESET_TOKEN                        = $0707;

      // FogMode
      GL_EXP                                     = $0800;
      GL_EXP2                                    = $0801;

      // FrontFaceDirection
      GL_CW                                      = $0900;
      GL_CCW                                     = $0901;

      // GetMapQuery
      GL_COEFF                                   = $0A00;
      GL_ORDER                                   = $0A01;
      GL_DOMAIN                                  = $0A02;

      // GetPixelMap
      GL_PIXEL_MAP_I_TO_I                        = $0C70;
      GL_PIXEL_MAP_S_TO_S                        = $0C71;
      GL_PIXEL_MAP_I_TO_R                        = $0C72;
      GL_PIXEL_MAP_I_TO_G                        = $0C73;
      GL_PIXEL_MAP_I_TO_B                        = $0C74;
      GL_PIXEL_MAP_I_TO_A                        = $0C75;
      GL_PIXEL_MAP_R_TO_R                        = $0C76;
      GL_PIXEL_MAP_G_TO_G                        = $0C77;
      GL_PIXEL_MAP_B_TO_B                        = $0C78;
      GL_PIXEL_MAP_A_TO_A                        = $0C79;

      // GetPointervPName
      GL_VERTEX_ARRAY_POINTER                    = $808E;
      GL_NORMAL_ARRAY_POINTER                    = $808F;
      GL_COLOR_ARRAY_POINTER                     = $8090;
      GL_INDEX_ARRAY_POINTER                     = $8091;
      GL_TEXTURE_COORD_ARRAY_POINTER             = $8092;
      GL_EDGE_FLAG_ARRAY_POINTER                 = $8093;

      // GetPName
      GL_CURRENT_COLOR                           = $0B00;
      GL_CURRENT_INDEX                           = $0B01;
      GL_CURRENT_NORMAL                          = $0B02;
      GL_CURRENT_TEXTURE_COORDS                  = $0B03;
      GL_CURRENT_RASTER_COLOR                    = $0B04;
      GL_CURRENT_RASTER_INDEX                    = $0B05;
      GL_CURRENT_RASTER_TEXTURE_COORDS           = $0B06;
      GL_CURRENT_RASTER_POSITION                 = $0B07;
      GL_CURRENT_RASTER_POSITION_VALID           = $0B08;
      GL_CURRENT_RASTER_DISTANCE                 = $0B09;
      GL_POINT_SMOOTH                            = $0B10;
      GL_POINT_SIZE                              = $0B11;
      GL_POINT_SIZE_RANGE                        = $0B12;
      GL_POINT_SIZE_GRANULARITY                  = $0B13;
      GL_LINE_SMOOTH                             = $0B20;
      GL_LINE_WIDTH                              = $0B21;
      GL_LINE_WIDTH_RANGE                        = $0B22;
      GL_LINE_WIDTH_GRANULARITY                  = $0B23;
      GL_LINE_STIPPLE                            = $0B24;
      GL_LINE_STIPPLE_PATTERN                    = $0B25;
      GL_LINE_STIPPLE_REPEAT                     = $0B26;
      GL_LIST_MODE                               = $0B30;
      GL_MAX_LIST_NESTING                        = $0B31;
      GL_LIST_BASE                               = $0B32;
      GL_LIST_INDEX                              = $0B33;
      GL_POLYGON_MODE                            = $0B40;
      GL_POLYGON_SMOOTH                          = $0B41;
      GL_POLYGON_STIPPLE                         = $0B42;
      GL_EDGE_FLAG                               = $0B43;
      GL_CULL_FACE                               = $0B44;
      GL_CULL_FACE_MODE                          = $0B45;
      GL_FRONT_FACE                              = $0B46;
      GL_LIGHTING                                = $0B50;
      GL_LIGHT_MODEL_LOCAL_VIEWER                = $0B51;
      GL_LIGHT_MODEL_TWO_SIDE                    = $0B52;
      GL_LIGHT_MODEL_AMBIENT                     = $0B53;
      GL_LIGHT_MODEL_COLOR_CONTROL               = $81F8; // GL 1.2
      GL_SINGLE_COLOR                            = $81F9; // GL 1.2
      GL_SEPARATE_SPECULAR_COLOR                 = $81FA; // GL 1.2
      GL_SHADE_MODEL                             = $0B54;
      GL_COLOR_MATERIAL_FACE                     = $0B55;
      GL_COLOR_MATERIAL_PARAMETER                = $0B56;
      GL_COLOR_MATERIAL                          = $0B57;
      GL_FOG                                     = $0B60;
      GL_FOG_INDEX                               = $0B61;
      GL_FOG_DENSITY                             = $0B62;
      GL_FOG_START                               = $0B63;
      GL_FOG_END                                 = $0B64;
      GL_FOG_MODE                                = $0B65;
      GL_FOG_COLOR                               = $0B66;
      GL_DEPTH_RANGE                             = $0B70;
      GL_DEPTH_TEST                              = $0B71;
      GL_DEPTH_WRITEMASK                         = $0B72;
      GL_DEPTH_CLEAR_VALUE                       = $0B73;
      GL_DEPTH_FUNC                              = $0B74;
      GL_ACCUM_CLEAR_VALUE                       = $0B80;
      GL_STENCIL_TEST                            = $0B90;
      GL_STENCIL_CLEAR_VALUE                     = $0B91;
      GL_STENCIL_FUNC                            = $0B92;
      GL_STENCIL_VALUE_MASK                      = $0B93;
      GL_STENCIL_FAIL                            = $0B94;
      GL_STENCIL_PASS_DEPTH_FAIL                 = $0B95;
      GL_STENCIL_PASS_DEPTH_PASS                 = $0B96;
      GL_STENCIL_REF                             = $0B97;
      GL_STENCIL_WRITEMASK                       = $0B98;
      GL_MATRIX_MODE                             = $0BA0;
      GL_NORMALIZE                               = $0BA1;
      GL_VIEWPORT                                = $0BA2;
      GL_MODELVIEW_STACK_DEPTH                   = $0BA3;
      GL_PROJECTION_STACK_DEPTH                  = $0BA4;
      GL_TEXTURE_STACK_DEPTH                     = $0BA5;
      GL_MODELVIEW_MATRIX                        = $0BA6;
      GL_PROJECTION_MATRIX                       = $0BA7;
      GL_TEXTURE_MATRIX                          = $0BA8;
      GL_ATTRIB_STACK_DEPTH                      = $0BB0;
      GL_CLIENT_ATTRIB_STACK_DEPTH               = $0BB1;
      GL_ALPHA_TEST                              = $0BC0;
      GL_ALPHA_TEST_FUNC                         = $0BC1;
      GL_ALPHA_TEST_REF                          = $0BC2;
      GL_DITHER                                  = $0BD0;
      GL_BLEND_DST                               = $0BE0;
      GL_BLEND_SRC                               = $0BE1;
      GL_BLEND                                   = $0BE2;
      GL_BLEND_COLOR                             = $8005; // GL 1.2 ARB imaging
      GL_LOGIC_OP_MODE                           = $0BF0;
      GL_INDEX_LOGIC_OP                          = $0BF1;
      GL_LOGIC_OP                                = $0BF1;
      GL_COLOR_LOGIC_OP                          = $0BF2;
      GL_AUX_BUFFERS                             = $0C00;
      GL_DRAW_BUFFER                             = $0C01;
      GL_READ_BUFFER                             = $0C02;
      GL_SCISSOR_BOX                             = $0C10;
      GL_SCISSOR_TEST                            = $0C11;
      GL_INDEX_CLEAR_VALUE                       = $0C20;
      GL_INDEX_WRITEMASK                         = $0C21;
      GL_COLOR_CLEAR_VALUE                       = $0C22;
      GL_COLOR_WRITEMASK                         = $0C23;
      GL_INDEX_MODE                              = $0C30;
      GL_RGBA_MODE                               = $0C31;
      GL_DOUBLEBUFFER                            = $0C32;
      GL_STEREO                                  = $0C33;
      GL_RENDER_MODE                             = $0C40;
      GL_PERSPECTIVE_CORRECTION_HINT             = $0C50;
      GL_POINT_SMOOTH_HINT                       = $0C51;
      GL_LINE_SMOOTH_HINT                        = $0C52;
      GL_POLYGON_SMOOTH_HINT                     = $0C53;
      GL_FOG_HINT                                = $0C54;
      GL_TEXTURE_GEN_S                           = $0C60;
      GL_TEXTURE_GEN_T                           = $0C61;
      GL_TEXTURE_GEN_R                           = $0C62;
      GL_TEXTURE_GEN_Q                           = $0C63;
      GL_PIXEL_MAP_I_TO_I_SIZE                   = $0CB0;
      GL_PIXEL_MAP_S_TO_S_SIZE                   = $0CB1;
      GL_PIXEL_MAP_I_TO_R_SIZE                   = $0CB2;
      GL_PIXEL_MAP_I_TO_G_SIZE                   = $0CB3;
      GL_PIXEL_MAP_I_TO_B_SIZE                   = $0CB4;
      GL_PIXEL_MAP_I_TO_A_SIZE                   = $0CB5;
      GL_PIXEL_MAP_R_TO_R_SIZE                   = $0CB6;
      GL_PIXEL_MAP_G_TO_G_SIZE                   = $0CB7;
      GL_PIXEL_MAP_B_TO_B_SIZE                   = $0CB8;
      GL_PIXEL_MAP_A_TO_A_SIZE                   = $0CB9;
      GL_UNPACK_SWAP_BYTES                       = $0CF0;
      GL_UNPACK_LSB_FIRST                        = $0CF1;
      GL_UNPACK_ROW_LENGTH                       = $0CF2;
      GL_UNPACK_SKIP_ROWS                        = $0CF3;
      GL_UNPACK_SKIP_PIXELS                      = $0CF4;
      GL_UNPACK_ALIGNMENT                        = $0CF5;
      GL_PACK_SWAP_BYTES                         = $0D00;
      GL_PACK_LSB_FIRST                          = $0D01;
      GL_PACK_ROW_LENGTH                         = $0D02;
      GL_PACK_SKIP_ROWS                          = $0D03;
      GL_PACK_SKIP_PIXELS                        = $0D04;
      GL_PACK_ALIGNMENT                          = $0D05;
      GL_PACK_SKIP_IMAGES                        = $806B; // GL 1.2
      GL_PACK_IMAGE_HEIGHT                       = $806C; // GL 1.2
      GL_UNPACK_SKIP_IMAGES                      = $806D; // GL 1.2
      GL_UNPACK_IMAGE_HEIGHT                     = $806E; // GL 1.2
      GL_MAP_COLOR                               = $0D10;
      GL_MAP_STENCIL                             = $0D11;
      GL_INDEX_SHIFT                             = $0D12;
      GL_INDEX_OFFSET                            = $0D13;
      GL_RED_SCALE                               = $0D14;
      GL_RED_BIAS                                = $0D15;
      GL_ZOOM_X                                  = $0D16;
      GL_ZOOM_Y                                  = $0D17;
      GL_GREEN_SCALE                             = $0D18;
      GL_GREEN_BIAS                              = $0D19;
      GL_BLUE_SCALE                              = $0D1A;
      GL_BLUE_BIAS                               = $0D1B;
      GL_ALPHA_SCALE                             = $0D1C;
      GL_ALPHA_BIAS                              = $0D1D;
      GL_DEPTH_SCALE                             = $0D1E;
      GL_DEPTH_BIAS                              = $0D1F;
      GL_MAX_EVAL_ORDER                          = $0D30;
      GL_MAX_LIGHTS                              = $0D31;
      GL_MAX_CLIP_PLANES                         = $0D32;
      GL_MAX_TEXTURE_SIZE                        = $0D33;
      GL_MAX_3D_TEXTURE_SIZE                     = $8073; // GL 1.2
      GL_MAX_PIXEL_MAP_TABLE                     = $0D34;
      GL_MAX_ATTRIB_STACK_DEPTH                  = $0D35;
      GL_MAX_MODELVIEW_STACK_DEPTH               = $0D36;
      GL_MAX_NAME_STACK_DEPTH                    = $0D37;
      GL_MAX_PROJECTION_STACK_DEPTH              = $0D38;
      GL_MAX_TEXTURE_STACK_DEPTH                 = $0D39;
      GL_MAX_VIEWPORT_DIMS                       = $0D3A;
      GL_MAX_CLIENT_ATTRIB_STACK_DEPTH           = $0D3B;
      GL_MAX_ELEMENTS_VERTICES                   = $F0E8; // GL 1.2
      GL_MAX_ELEMENTS_INDICES                    = $F0E9; // GL 1.2
      GL_RESCALE_NORMAL                          = $803A; // GL 1.2
      GL_SUBPIXEL_BITS                           = $0D50;
      GL_INDEX_BITS                              = $0D51;
      GL_RED_BITS                                = $0D52;
      GL_GREEN_BITS                              = $0D53;
      GL_BLUE_BITS                               = $0D54;
      GL_ALPHA_BITS                              = $0D55;
      GL_DEPTH_BITS                              = $0D56;
      GL_STENCIL_BITS                            = $0D57;
      GL_ACCUM_RED_BITS                          = $0D58;
      GL_ACCUM_GREEN_BITS                        = $0D59;
      GL_ACCUM_BLUE_BITS                         = $0D5A;
      GL_ACCUM_ALPHA_BITS                        = $0D5B;
      GL_NAME_STACK_DEPTH                        = $0D70;
      GL_AUTO_NORMAL                             = $0D80;
      GL_MAP1_COLOR_4                            = $0D90;
      GL_MAP1_INDEX                              = $0D91;
      GL_MAP1_NORMAL                             = $0D92;
      GL_MAP1_TEXTURE_COORD_1                    = $0D93;
      GL_MAP1_TEXTURE_COORD_2                    = $0D94;
      GL_MAP1_TEXTURE_COORD_3                    = $0D95;
      GL_MAP1_TEXTURE_COORD_4                    = $0D96;
      GL_MAP1_VERTEX_3                           = $0D97;
      GL_MAP1_VERTEX_4                           = $0D98;
      GL_MAP2_COLOR_4                            = $0DB0;
      GL_MAP2_INDEX                              = $0DB1;
      GL_MAP2_NORMAL                             = $0DB2;
      GL_MAP2_TEXTURE_COORD_1                    = $0DB3;
      GL_MAP2_TEXTURE_COORD_2                    = $0DB4;
      GL_MAP2_TEXTURE_COORD_3                    = $0DB5;
      GL_MAP2_TEXTURE_COORD_4                    = $0DB6;
      GL_MAP2_VERTEX_3                           = $0DB7;
      GL_MAP2_VERTEX_4                           = $0DB8;
      GL_MAP1_GRID_DOMAIN                        = $0DD0;
      GL_MAP1_GRID_SEGMENTS                      = $0DD1;
      GL_MAP2_GRID_DOMAIN                        = $0DD2;
      GL_MAP2_GRID_SEGMENTS                      = $0DD3;
      GL_TEXTURE_1D                              = $0DE0;
      GL_TEXTURE_2D                              = $0DE1;
      GL_TEXTURE_3D                              = $806F; // GL 1.2
      GL_FEEDBACK_BUFFER_POINTER                 = $0DF0;
      GL_FEEDBACK_BUFFER_SIZE                    = $0DF1;
      GL_FEEDBACK_BUFFER_TYPE                    = $0DF2;
      GL_SELECTION_BUFFER_POINTER                = $0DF3;
      GL_SELECTION_BUFFER_SIZE                   = $0DF4;
      GL_POLYGON_OFFSET_UNITS                    = $2A00;
      GL_POLYGON_OFFSET_POINT                    = $2A01;
      GL_POLYGON_OFFSET_LINE                     = $2A02;
      GL_POLYGON_OFFSET_FILL                     = $8037;
      GL_POLYGON_OFFSET_FACTOR                   = $8038;
      GL_TEXTURE_BINDING_1D                      = $8068;
      GL_TEXTURE_BINDING_2D                      = $8069;
      GL_VERTEX_ARRAY                            = $8074;
      GL_NORMAL_ARRAY                            = $8075;
      GL_COLOR_ARRAY                             = $8076;
      GL_INDEX_ARRAY                             = $8077;
      GL_TEXTURE_COORD_ARRAY                     = $8078;
      GL_EDGE_FLAG_ARRAY                         = $8079;
      GL_VERTEX_ARRAY_SIZE                       = $807A;
      GL_VERTEX_ARRAY_TYPE                       = $807B;
      GL_VERTEX_ARRAY_STRIDE                     = $807C;
      GL_NORMAL_ARRAY_TYPE                       = $807E;
      GL_NORMAL_ARRAY_STRIDE                     = $807F;
      GL_COLOR_ARRAY_SIZE                        = $8081;
      GL_COLOR_ARRAY_TYPE                        = $8082;
      GL_COLOR_ARRAY_STRIDE                      = $8083;
      GL_INDEX_ARRAY_TYPE                        = $8085;
      GL_INDEX_ARRAY_STRIDE                      = $8086;
      GL_TEXTURE_COORD_ARRAY_SIZE                = $8088;
      GL_TEXTURE_COORD_ARRAY_TYPE                = $8089;
      GL_TEXTURE_COORD_ARRAY_STRIDE              = $808A;
      GL_EDGE_FLAG_ARRAY_STRIDE                  = $808C;
      GL_COLOR_MATRIX                            = $80B1; // GL 1.2 ARB imaging
      GL_COLOR_MATRIX_STACK_DEPTH                = $80B2; // GL 1.2 ARB imaging
      GL_MAX_COLOR_MATRIX_STACK_DEPTH            = $80B3; // GL 1.2 ARB imaging
      GL_POST_COLOR_MATRIX_RED_SCALE             = $80B4; // GL 1.2 ARB imaging
      GL_POST_COLOR_MATRIX_GREEN_SCALE           = $80B5; // GL 1.2 ARB imaging
      GL_POST_COLOR_MATRIX_BLUE_SCALE            = $80B6; // GL 1.2 ARB imaging
      GL_POST_COLOR_MATRIX_ALPHA_SCALE           = $80B7; // GL 1.2 ARB imaging
      GL_POST_COLOR_MATRIX_RED_BIAS              = $80B8; // GL 1.2 ARB imaging
      GL_POST_COLOR_MATRIX_GREEN_BIAS            = $80B9; // GL 1.2 ARB imaging
      GL_POST_COLOR_MATRIX_BLUE_BIAS             = $80BA; // GL 1.2 ARB imaging
      GL_POST_COLOR_MATRIX_ALPHA_BIAS            = $80BB; // GL 1.2 ARB imaging

      // GetTextureParameter
      GL_TEXTURE_WIDTH                           = $1000;
      GL_TEXTURE_HEIGHT                          = $1001;
      GL_TEXTURE_INTERNAL_FORMAT                 = $1003;
      GL_TEXTURE_COMPONENTS                      = $1003;
      GL_TEXTURE_BORDER_COLOR                    = $1004;
      GL_TEXTURE_BORDER                          = $1005;
      GL_TEXTURE_RED_SIZE                        = $805C;
      GL_TEXTURE_GREEN_SIZE                      = $805D;
      GL_TEXTURE_BLUE_SIZE                       = $805E;
      GL_TEXTURE_ALPHA_SIZE                      = $805F;
      GL_TEXTURE_LUMINANCE_SIZE                  = $8060;
      GL_TEXTURE_INTENSITY_SIZE                  = $8061;
      GL_TEXTURE_PRIORITY                        = $8066;
      GL_TEXTURE_RESIDENT                        = $8067;
      GL_BGR                                     = $80E0; // v 1.2
      GL_BGRA                                    = $80E1; // v 1.2

      // HintMode
      GL_DONT_CARE                               = $1100;
      GL_FASTEST                                 = $1101;
      GL_NICEST                                  = $1102;

      // LightParameter
      GL_AMBIENT                                 = $1200;
      GL_DIFFUSE                                 = $1201;
      GL_SPECULAR                                = $1202;
      GL_POSITION                                = $1203;
      GL_SPOT_DIRECTION                          = $1204;
      GL_SPOT_EXPONENT                           = $1205;
      GL_SPOT_CUTOFF                             = $1206;
      GL_CONSTANT_ATTENUATION                    = $1207;
      GL_LINEAR_ATTENUATION                      = $1208;
      GL_QUADRATIC_ATTENUATION                   = $1209;

      // ListMode
      GL_COMPILE                                 = $1300;
      GL_COMPILE_AND_EXECUTE                     = $1301;

      // DataType
      GL_BYTE                                    = $1400;
      GL_UNSIGNED_BYTE                           = $1401;
      GL_SHORT                                   = $1402;
      GL_UNSIGNED_SHORT                          = $1403;
      GL_INT                                     = $1404;
      GL_UNSIGNED_INT                            = $1405;
      GL_FLOAT                                   = $1406;
      GL_2_BYTES                                 = $1407;
      GL_3_BYTES                                 = $1408;
      GL_4_BYTES                                 = $1409;
      GL_DOUBLE                                  = $140A;
      GL_DOUBLE_EXT                              = $140A;

      // LogicOp
      GL_CLEAR                                   = $1500;
      GL_AND                                     = $1501;
      GL_AND_REVERSE                             = $1502;
      GL_COPY                                    = $1503;
      GL_AND_INVERTED                            = $1504;
      GL_NOOP                                    = $1505;
      GL_XOR                                     = $1506;
      GL_OR                                      = $1507;
      GL_NOR                                     = $1508;
      GL_EQUIV                                   = $1509;
      GL_INVERT                                  = $150A;
      GL_OR_REVERSE                              = $150B;
      GL_COPY_INVERTED                           = $150C;
      GL_OR_INVERTED                             = $150D;
      GL_NAND                                    = $150E;
      GL_SET                                     = $150F;

      // MaterialParameter
      GL_EMISSION                                = $1600;
      GL_SHININESS                               = $1601;
      GL_AMBIENT_AND_DIFFUSE                     = $1602;
      GL_COLOR_INDEXES                           = $1603;

      // MatrixMode
      GL_MODELVIEW                               = $1700;
      GL_PROJECTION                              = $1701;
      GL_TEXTURE                                 = $1702;

      // PixelCopyType
      GL_COLOR                                   = $1800;
      GL_DEPTH                                   = $1801;
      GL_STENCIL                                 = $1802;

      // PixelFormat
      GL_COLOR_INDEX                             = $1900;
      GL_STENCIL_INDEX                           = $1901;
      GL_DEPTH_COMPONENT                         = $1902;
      GL_RED                                     = $1903;
      GL_GREEN                                   = $1904;
      GL_BLUE                                    = $1905;
      GL_ALPHA                                   = $1906;
      GL_RGB                                     = $1907;
      GL_RGBA                                    = $1908;
      GL_LUMINANCE                               = $1909;
      GL_LUMINANCE_ALPHA                         = $190A;

      // PixelType
      GL_BITMAP                                  = $1A00;

      // PolygonMode
      GL_POINT                                   = $1B00;
      GL_LINE                                    = $1B01;
      GL_FILL                                    = $1B02;

      // RenderingMode
      GL_RENDER                                  = $1C00;
      GL_FEEDBACK                                = $1C01;
      GL_SELECT                                  = $1C02;

      // ShadingModel
      GL_FLAT                                    = $1D00;
      GL_SMOOTH                                  = $1D01;

      // StencilOp
      GL_KEEP                                    = $1E00;
      GL_REPLACE                                 = $1E01;
      GL_INCR                                    = $1E02;
      GL_DECR                                    = $1E03;

      // StringName
      GL_VENDOR                                  = $1F00;
      GL_RENDERER                                = $1F01;
      GL_VERSION                                 = $1F02;
      GL_EXTENSIONS                              = $1F03;

      // TextureCoordName
      GL_S                                       = $2000;
      GL_T                                       = $2001;
      GL_R                                       = $2002;
      GL_Q                                       = $2003;

      // TextureEnvMode
      GL_MODULATE                                = $2100;
      GL_DECAL                                   = $2101;

      // TextureEnvParameter
      GL_TEXTURE_ENV_MODE                        = $2200;
      GL_TEXTURE_ENV_COLOR                       = $2201;

      // TextureEnvTarget
      GL_TEXTURE_ENV                             = $2300;

      // TextureGenMode
      GL_EYE_LINEAR                              = $2400;
      GL_OBJECT_LINEAR                           = $2401;
      GL_SPHERE_MAP                              = $2402;

      // TextureGenParameter
      GL_TEXTURE_GEN_MODE                        = $2500;
      GL_OBJECT_PLANE                            = $2501;
      GL_EYE_PLANE                               = $2502;

      // TextureMagFilter
      GL_NEAREST                                 = $2600;
      GL_LINEAR                                  = $2601;

      // TextureMinFilter
      GL_NEAREST_MIPMAP_NEAREST                  = $2700;
      GL_LINEAR_MIPMAP_NEAREST                   = $2701;
      GL_NEAREST_MIPMAP_LINEAR                   = $2702;
      GL_LINEAR_MIPMAP_LINEAR                    = $2703;

      // TextureParameterName
      GL_TEXTURE_MAG_FILTER                      = $2800;
      GL_TEXTURE_MIN_FILTER                      = $2801;
      GL_TEXTURE_WRAP_R                          = $8072; // GL 1.2
      GL_TEXTURE_WRAP_S                          = $2802;
      GL_TEXTURE_WRAP_T                          = $2803;
      GL_CLAMP_TO_EDGE                           = $812F; // GL 1.2
      GL_TEXTURE_MIN_LOD                         = $813A; // GL 1.2
      GL_TEXTURE_MAX_LOD                         = $813B; // GL 1.2
      GL_TEXTURE_BASE_LEVEL                      = $813C; // GL 1.2
      GL_TEXTURE_MAX_LEVEL                       = $813D; // GL 1.2
      GL_TEXTURE_DEPTH                           = $8071; // GL 1.2

      // TextureTarget
      GL_PROXY_TEXTURE_1D                        = $8063;
      GL_PROXY_TEXTURE_2D                        = $8064;
      GL_PROXY_TEXTURE_3D                        = $8070; // GL 1.2

      // TextureWrapMode
      GL_CLAMP                                   = $2900;
      GL_REPEAT                                  = $2901;

      // PixelInternalFormat
      GL_R3_G3_B2                                = $2A10;
      GL_ALPHA4                                  = $803B;
      GL_ALPHA8                                  = $803C;
      GL_ALPHA12                                 = $803D;
      GL_ALPHA16                                 = $803E;
      GL_LUMINANCE4                              = $803F;
      GL_LUMINANCE8                              = $8040;
      GL_LUMINANCE12                             = $8041;
      GL_LUMINANCE16                             = $8042;
      GL_LUMINANCE4_ALPHA4                       = $8043;
      GL_LUMINANCE6_ALPHA2                       = $8044;
      GL_LUMINANCE8_ALPHA8                       = $8045;
      GL_LUMINANCE12_ALPHA4                      = $8046;
      GL_LUMINANCE12_ALPHA12                     = $8047;
      GL_LUMINANCE16_ALPHA16                     = $8048;
      GL_INTENSITY                               = $8049;
      GL_INTENSITY4                              = $804A;
      GL_INTENSITY8                              = $804B;
      GL_INTENSITY12                             = $804C;
      GL_INTENSITY16                             = $804D;
      GL_RGB4                                    = $804F;
      GL_RGB5                                    = $8050;
      GL_RGB8                                    = $8051;
      GL_RGB10                                   = $8052;
      GL_RGB12                                   = $8053;
      GL_RGB16                                   = $8054;
      GL_RGBA2                                   = $8055;
      GL_RGBA4                                   = $8056;
      GL_RGB5_A1                                 = $8057;
      GL_RGBA8                                   = $8058;
      GL_RGB10_A2                                = $8059;
      GL_RGBA12                                  = $805A;
      GL_RGBA16                                  = $805B;
      UNSIGNED_BYTE_3_3_2                        = $8032; // GL 1.2
      UNSIGNED_BYTE_2_3_3_REV                    = $8362; // GL 1.2
      UNSIGNED_SHORT_5_6_5                       = $8363; // GL 1.2
      UNSIGNED_SHORT_5_6_5_REV                   = $8364; // GL 1.2
      UNSIGNED_SHORT_4_4_4_4                     = $8033; // GL 1.2
      UNSIGNED_SHORT_4_4_4_4_REV                 = $8365; // GL 1.2
      UNSIGNED_SHORT_5_5_5_1                     = $8034; // GL 1.2
      UNSIGNED_SHORT_1_5_5_5_REV                 = $8366; // GL 1.2
      UNSIGNED_INT_8_8_8_8                       = $8035; // GL 1.2
      UNSIGNED_INT_8_8_8_8_REV                   = $8367; // GL 1.2
      UNSIGNED_INT_10_10_10_2                    = $8036; // GL 1.2
      UNSIGNED_INT_2_10_10_10_REV                = $8368; // GL 1.2

      // InterleavedArrayFormat
      GL_V2F                                     = $2A20;
      GL_V3F                                     = $2A21;
      GL_C4UB_V2F                                = $2A22;
      GL_C4UB_V3F                                = $2A23;
      GL_C3F_V3F                                 = $2A24;
      GL_N3F_V3F                                 = $2A25;
      GL_C4F_N3F_V3F                             = $2A26;
      GL_T2F_V3F                                 = $2A27;
      GL_T4F_V4F                                 = $2A28;
      GL_T2F_C4UB_V3F                            = $2A29;
      GL_T2F_C3F_V3F                             = $2A2A;
      GL_T2F_N3F_V3F                             = $2A2B;
      GL_T2F_C4F_N3F_V3F                         = $2A2C;
      GL_T4F_C4F_N3F_V4F                         = $2A2D;

      // ClipPlaneName
      GL_CLIP_PLANE0                             = $3000;
      GL_CLIP_PLANE1                             = $3001;
      GL_CLIP_PLANE2                             = $3002;
      GL_CLIP_PLANE3                             = $3003;
      GL_CLIP_PLANE4                             = $3004;
      GL_CLIP_PLANE5                             = $3005;

      // LightName
      GL_LIGHT0                                  = $4000;
      GL_LIGHT1                                  = $4001;
      GL_LIGHT2                                  = $4002;
      GL_LIGHT3                                  = $4003;
      GL_LIGHT4                                  = $4004;
      GL_LIGHT5                                  = $4005;
      GL_LIGHT6                                  = $4006;
      GL_LIGHT7                                  = $4007;

      // ----- extensions enumerants -----
      // EXT_abgr
      GL_ABGR_EXT                                = $8000;

      // EXT_packed_pixels
      GL_UNSIGNED_BYTE_3_3_2_EXT                 = $8032;
      GL_UNSIGNED_SHORT_4_4_4_4_EXT              = $8033;
      GL_UNSIGNED_SHORT_5_5_5_1_EXT              = $8034;
      GL_UNSIGNED_INT_8_8_8_8_EXT                = $8035;
      GL_UNSIGNED_INT_10_10_10_2_EXT             = $8036;

      // EXT_vertex_array
      GL_VERTEX_ARRAY_EXT                        = $8074;
      GL_NORMAL_ARRAY_EXT                        = $8075;
      GL_COLOR_ARRAY_EXT                         = $8076;
      GL_INDEX_ARRAY_EXT                         = $8077;
      GL_TEXTURE_COORD_ARRAY_EXT                 = $8078;
      GL_EDGE_FLAG_ARRAY_EXT                     = $8079;
      GL_VERTEX_ARRAY_SIZE_EXT                   = $807A;
      GL_VERTEX_ARRAY_TYPE_EXT                   = $807B;
      GL_VERTEX_ARRAY_STRIDE_EXT                 = $807C;
      GL_VERTEX_ARRAY_COUNT_EXT                  = $807D;
      GL_NORMAL_ARRAY_TYPE_EXT                   = $807E;
      GL_NORMAL_ARRAY_STRIDE_EXT                 = $807F;
      GL_NORMAL_ARRAY_COUNT_EXT                  = $8080;
      GL_COLOR_ARRAY_SIZE_EXT                    = $8081;
      GL_COLOR_ARRAY_TYPE_EXT                    = $8082;
      GL_COLOR_ARRAY_STRIDE_EXT                  = $8083;
      GL_COLOR_ARRAY_COUNT_EXT                   = $8084;
      GL_INDEX_ARRAY_TYPE_EXT                    = $8085;
      GL_INDEX_ARRAY_STRIDE_EXT                  = $8086;
      GL_INDEX_ARRAY_COUNT_EXT                   = $8087;
      GL_TEXTURE_COORD_ARRAY_SIZE_EXT            = $8088;
      GL_TEXTURE_COORD_ARRAY_TYPE_EXT            = $8089;
      GL_TEXTURE_COORD_ARRAY_STRIDE_EXT          = $808A;
      GL_TEXTURE_COORD_ARRAY_COUNT_EXT           = $808B;
      GL_EDGE_FLAG_ARRAY_STRIDE_EXT              = $808C;
      GL_EDGE_FLAG_ARRAY_COUNT_EXT               = $808D;
      GL_VERTEX_ARRAY_POINTER_EXT                = $808E;
      GL_NORMAL_ARRAY_POINTER_EXT                = $808F;
      GL_COLOR_ARRAY_POINTER_EXT                 = $8090;
      GL_INDEX_ARRAY_POINTER_EXT                 = $8091;
      GL_TEXTURE_COORD_ARRAY_POINTER_EXT         = $8092;
      GL_EDGE_FLAG_ARRAY_POINTER_EXT             = $8093;

      // EXT_color_table
      GL_TABLE_TOO_LARGE_EXT                     = $8031;
      GL_COLOR_TABLE_FORMAT_EXT                  = $80D8;
      GL_COLOR_TABLE_WIDTH_EXT                   = $80D9;
      GL_COLOR_TABLE_RED_SIZE_EXT                = $80DA;
      GL_COLOR_TABLE_GREEN_SIZE_EXT              = $80DB;
      GL_COLOR_TABLE_BLUE_SIZE_EXT               = $80DC;
      GL_COLOR_TABLE_ALPHA_SIZE_EXT              = $80DD;
      GL_COLOR_TABLE_LUMINANCE_SIZE_EXT          = $80DE;
      GL_COLOR_TABLE_INTENSITY_SIZE_EXT          = $80DF;

      // EXT_bgra
      GL_BGR_EXT                                 = $80E0;
      GL_BGRA_EXT                                = $80E1;

      // EXT_paletted_texture
      GL_COLOR_INDEX1_EXT                        = $80E2;
      GL_COLOR_INDEX2_EXT                        = $80E3;
      GL_COLOR_INDEX4_EXT                        = $80E4;
      GL_COLOR_INDEX8_EXT                        = $80E5;
      GL_COLOR_INDEX12_EXT                       = $80E6;
      GL_COLOR_INDEX16_EXT                       = $80E7;

      // SGI_compiled_vertex_array
      GL_ARRAY_ELEMENT_LOCK_FIRST_SGI            = $81A8;
      GL_ARRAY_ELEMENT_LOCK_COUNT_SGI            = $81A9;

      // SGI_cull_vertex
      GL_CULL_VERTEX_SGI                         = $81AA;
      GL_CULL_VERTEX_EYE_POSITION_SGI            = $81AB;
      GL_CULL_VERTEX_OBJECT_POSITION_SGI         = $81AC;

      // SGI_index_array_formats
      GL_IUI_V2F_SGI                             = $81AD;
      GL_IUI_V3F_SGI                             = $81AE;
      GL_IUI_N3F_V2F_SGI                         = $81AF;
      GL_IUI_N3F_V3F_SGI                         = $81B0;
      GL_T2F_IUI_V2F_SGI                         = $81B1;
      GL_T2F_IUI_V3F_SGI                         = $81B2;
      GL_T2F_IUI_N3F_V2F_SGI                     = $81B3;
      GL_T2F_IUI_N3F_V3F_SGI                     = $81B4;

      // SGI_index_func
      GL_INDEX_TEST_SGI                          = $81B5;
      GL_INDEX_TEST_FUNC_SGI                     = $81B6;
      GL_INDEX_TEST_REF_SGI                      = $81B7;

      // SGI_index_material
      GL_INDEX_MATERIAL_SGI                      = $81B8;
      GL_INDEX_MATERIAL_PARAMETER_SGI            = $81B9;
      GL_INDEX_MATERIAL_FACE_SGI                 = $81BA;

      // EXT_blend_color
      GL_CONSTANT_COLOR_EXT                      = $8001;
      GL_ONE_MINUS_CONSTANT_COLOR_EXT            = $8002;
      GL_CONSTANT_ALPHA_EXT                      = $8003;
      GL_ONE_MINUS_CONSTANT_ALPHA_EXT            = $8004;
      GL_BLEND_COLOR_EXT                         = $8005;

      // EXT_blend_minmax
      GL_FUNC_ADD_EXT                            = $8006;
      GL_MIN_EXT                                 = $8007;
      GL_MAX_EXT                                 = $8008;
      GL_BLEND_EQUATION_EXT                      = $8009;

      // EXT_blend_subtract
      GL_FUNC_SUBTRACT_EXT                       = $800A;
      GL_FUNC_REVERSE_SUBTRACT_EXT               = $800B;

      // EXT_convolution
      GL_CONVOLUTION_1D_EXT                      = $8010;
      GL_CONVOLUTION_2D_EXT                      = $8011;
      GL_SEPARABLE_2D_EXT                        = $8012;
      GL_CONVOLUTION_BORDER_MODE_EXT             = $8013;
      GL_CONVOLUTION_FILTER_SCALE_EXT            = $8014;
      GL_CONVOLUTION_FILTER_BIAS_EXT             = $8015;
      GL_REDUCE_EXT                              = $8016;
      GL_CONVOLUTION_FORMAT_EXT                  = $8017;
      GL_CONVOLUTION_WIDTH_EXT                   = $8018;
      GL_CONVOLUTION_HEIGHT_EXT                  = $8019;
      GL_MAX_CONVOLUTION_WIDTH_EXT               = $801A;
      GL_MAX_CONVOLUTION_HEIGHT_EXT              = $801B;
      GL_POST_CONVOLUTION_RED_SCALE_EXT          = $801C;
      GL_POST_CONVOLUTION_GREEN_SCALE_EXT        = $801D;
      GL_POST_CONVOLUTION_BLUE_SCALE_EXT         = $801E;
      GL_POST_CONVOLUTION_ALPHA_SCALE_EXT        = $801F;
      GL_POST_CONVOLUTION_RED_BIAS_EXT           = $8020;
      GL_POST_CONVOLUTION_GREEN_BIAS_EXT         = $8021;
      GL_POST_CONVOLUTION_BLUE_BIAS_EXT          = $8022;
      GL_POST_CONVOLUTION_ALPHA_BIAS_EXT         = $8023;

      // EXT_histogram
      GL_HISTOGRAM_EXT                           = $8024;
      GL_PROXY_HISTOGRAM_EXT                     = $8025;
      GL_HISTOGRAM_WIDTH_EXT                     = $8026;
      GL_HISTOGRAM_FORMAT_EXT                    = $8027;
      GL_HISTOGRAM_RED_SIZE_EXT                  = $8028;
      GL_HISTOGRAM_GREEN_SIZE_EXT                = $8029;
      GL_HISTOGRAM_BLUE_SIZE_EXT                 = $802A;
      GL_HISTOGRAM_ALPHA_SIZE_EXT                = $802B;
      GL_HISTOGRAM_LUMINANCE_SIZE_EXT            = $802C;
      GL_HISTOGRAM_SINK_EXT                      = $802D;
      GL_MINMAX_EXT                              = $802E;
      GL_MINMAX_FORMAT_EXT                       = $802F;
      GL_MINMAX_SINK_EXT                         = $8030;

      // EXT_polygon_offset
      GL_POLYGON_OFFSET_EXT                      = $8037;
      GL_POLYGON_OFFSET_FACTOR_EXT               = $8038;
      GL_POLYGON_OFFSET_BIAS_EXT                 = $8039;

      // EXT_texture
      GL_ALPHA4_EXT                              = $803B;
      GL_ALPHA8_EXT                              = $803C;
      GL_ALPHA12_EXT                             = $803D;
      GL_ALPHA16_EXT                             = $803E;
      GL_LUMINANCE4_EXT                          = $803F;
      GL_LUMINANCE8_EXT                          = $8040;
      GL_LUMINANCE12_EXT                         = $8041;
      GL_LUMINANCE16_EXT                         = $8042;
      GL_LUMINANCE4_ALPHA4_EXT                   = $8043;
      GL_LUMINANCE6_ALPHA2_EXT                   = $8044;
      GL_LUMINANCE8_ALPHA8_EXT                   = $8045;
      GL_LUMINANCE12_ALPHA4_EXT                  = $8046;
      GL_LUMINANCE12_ALPHA12_EXT                 = $8047;
      GL_LUMINANCE16_ALPHA16_EXT                 = $8048;
      GL_INTENSITY_EXT                           = $8049;
      GL_INTENSITY4_EXT                          = $804A;
      GL_INTENSITY8_EXT                          = $804B;
      GL_INTENSITY12_EXT                         = $804C;
      GL_INTENSITY16_EXT                         = $804D;
      GL_RGB2_EXT                                = $804E;
      GL_RGB4_EXT                                = $804F;
      GL_RGB5_EXT                                = $8050;
      GL_RGB8_EXT                                = $8051;
      GL_RGB10_EXT                               = $8052;
      GL_RGB12_EXT                               = $8053;
      GL_RGB16_EXT                               = $8054;
      GL_RGBA2_EXT                               = $8055;
      GL_RGBA4_EXT                               = $8056;
      GL_RGB5_A1_EXT                             = $8057;
      GL_RGBA8_EXT                               = $8058;
      GL_RGB10_A2_EXT                            = $8059;
      GL_RGBA12_EXT                              = $805A;
      GL_RGBA16_EXT                              = $805B;
      GL_TEXTURE_RED_SIZE_EXT                    = $805C;
      GL_TEXTURE_GREEN_SIZE_EXT                  = $805D;
      GL_TEXTURE_BLUE_SIZE_EXT                   = $805E;
      GL_TEXTURE_ALPHA_SIZE_EXT                  = $805F;
      GL_TEXTURE_LUMINANCE_SIZE_EXT              = $8060;
      GL_TEXTURE_INTENSITY_SIZE_EXT              = $8061;
      GL_REPLACE_EXT                             = $8062;
      GL_PROXY_TEXTURE_1D_EXT                    = $8063;
      GL_PROXY_TEXTURE_2D_EXT                    = $8064;
      GL_TEXTURE_TOO_LARGE_EXT                   = $8065;

      // EXT_texture_object 
      GL_TEXTURE_PRIORITY_EXT                    = $8066;
      GL_TEXTURE_RESIDENT_EXT                    = $8067;
      GL_TEXTURE_1D_BINDING_EXT                  = $8068;
      GL_TEXTURE_2D_BINDING_EXT                  = $8069;
      GL_TEXTURE_3D_BINDING_EXT                  = $806A;

      // EXT_texture3D
      GL_PACK_SKIP_IMAGES_EXT                    = $806B;
      GL_PACK_IMAGE_HEIGHT_EXT                   = $806C;
      GL_UNPACK_SKIP_IMAGES_EXT                  = $806D;
      GL_UNPACK_IMAGE_HEIGHT_EXT                 = $806E;
      GL_TEXTURE_3D_EXT                          = $806F;
      GL_PROXY_TEXTURE_3D_EXT                    = $8070;
      GL_TEXTURE_DEPTH_EXT                       = $8071;
      GL_TEXTURE_WRAP_R_EXT                      = $8072;
      GL_MAX_3D_TEXTURE_SIZE_EXT                 = $8073;

      // SGI_color_matrix
      GL_COLOR_MATRIX_SGI                        = $80B1;
      GL_COLOR_MATRIX_STACK_DEPTH_SGI            = $80B2;
      GL_MAX_COLOR_MATRIX_STACK_DEPTH_SGI        = $80B3;
      GL_POST_COLOR_MATRIX_RED_SCALE_SGI         = $80B4;
      GL_POST_COLOR_MATRIX_GREEN_SCALE_SGI       = $80B5;
      GL_POST_COLOR_MATRIX_BLUE_SCALE_SGI        = $80B6;
      GL_POST_COLOR_MATRIX_ALPHA_SCALE_SGI       = $80B7;
      GL_POST_COLOR_MATRIX_RED_BIAS_SGI          = $80B8;
      GL_POST_COLOR_MATRIX_GREEN_BIAS_SGI        = $80B9;
      GL_POST_COLOR_MATRIX_BLUE_BIAS_SGI         = $80BA;
      GL_POST_COLOR_MATRIX_ALPHA_BIAS_SGI        = $80BB;

      // SGI_texture_color_table
      GL_TEXTURE_COLOR_TABLE_SGI                 = $80BC;
      GL_PROXY_TEXTURE_COLOR_TABLE_SGI           = $80BD;
      GL_TEXTURE_COLOR_TABLE_BIAS_SGI            = $80BE;
      GL_TEXTURE_COLOR_TABLE_SCALE_SGI           = $80BF;

      // SGI_color_table
      GL_COLOR_TABLE_SGI                         = $80D0;
      GL_POST_CONVOLUTION_COLOR_TABLE_SGI        = $80D1;
      GL_POST_COLOR_MATRIX_COLOR_TABLE_SGI       = $80D2;
      GL_PROXY_COLOR_TABLE_SGI                   = $80D3;
      GL_PROXY_POST_CONVOLUTION_COLOR_TABLE_SGI  = $80D4;
      GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE_SGI = $80D5;
      GL_COLOR_TABLE_SCALE_SGI                   = $80D6;
      GL_COLOR_TABLE_BIAS_SGI                    = $80D7;
      GL_COLOR_TABLE_FORMAT_SGI                  = $80D8;
      GL_COLOR_TABLE_WIDTH_SGI                   = $80D9;
      GL_COLOR_TABLE_RED_SIZE_SGI                = $80DA;
      GL_COLOR_TABLE_GREEN_SIZE_SGI              = $80DB;
      GL_COLOR_TABLE_BLUE_SIZE_SGI               = $80DC;
      GL_COLOR_TABLE_ALPHA_SIZE_SGI              = $80DD;
      GL_COLOR_TABLE_LUMINANCE_SIZE_SGI          = $80DE;
      GL_COLOR_TABLE_INTENSITY_SIZE_SGI          = $80DF;

      // EXT_cmyka
      GL_CMYK_EXT                                = $800C;
      GL_CMYKA_EXT                               = $800D;
      GL_PACK_CMYK_HINT_EXT                      = $800E;
      GL_UNPACK_CMYK_HINT_EXT                    = $800F;

      // EXT_rescale_normal
      GL_RESCALE_NORMAL_EXT                      = $803A;

      // EXT_clip_volume_hint
      GL_CLIP_VOLUME_CLIPPING_HINT_EXT	         = $80F0;

      // EXT_cull_vertex
      GL_CULL_VERTEX_EXT                         = 0; // not yet defined
      GL_CULL_VERTEX_EYE_POSITION_EXT            = 0; // not yet defined
      GL_CULL_VERTEX_OBJECT_POSITION_EXT         = 0; // not yet defined

      // EXT_index_array_formats
      GL_IUI_V2F_EXT                             = 0; // not yet defined
      GL_IUI_V3F_EXT                             = 0; // not yet defined
      GL_IUI_N3F_V2F_EXT                         = 0; // not yet defined
      GL_IUI_N3F_V3F_EXT                         = 0; // not yet defined
      GL_T2F_IUI_V2F_EXT                         = 0; // not yet defined
      GL_T2F_IUI_V3F_EXT                         = 0; // not yet defined
      GL_T2F_IUI_N3F_V2F_EXT                     = 0; // not yet defined
      GL_T2F_IUI_N3F_V3F_EXT                     = 0; // not yet defined

      // EXT_index_func
      GL_INDEX_TEST_EXT                          = 0; // not yet defined
      GL_INDEX_TEST_FUNC_EXT                     = 0; // not yet defined
      GL_INDEX_TEST_REF_EXT                      = 0; // not yet defined

      // EXT_index_material
      GL_INDEX_MATERIAL_EXT                      = 0; // not yet defined
      GL_INDEX_MATERIAL_PARAMETER_EXT            = 0; // not yet defined
      GL_INDEX_MATERIAL_FACE_EXT                 = 0; // not yet defined

      // EXT_misc_attribute
      GL_MISC_BIT_EXT                            = 0; // not yet defined

      // EXT_scene_marker
      GL_SCENE_REQUIRED_EXT                      = 0; // not yet defined

      // EXT_shared_texture_palette
      GL_SHARED_TEXTURE_PALETTE_EXT              = $81FB;

      // EXT_nurbs_tessellator
      GLU_NURBS_MODE_EXT                         = 100160;
      GLU_NURBS_TESSELLATOR_EXT                  = 100161;
      GLU_NURBS_RENDERER_EXT                     = 100162;
      GLU_NURBS_BEGIN_EXT                        = 100164;
      GLU_NURBS_VERTEX_EXT                       = 100165;
      GLU_NURBS_NORMAL_EXT                       = 100166;
      GLU_NURBS_COLOR_EXT                        = 100167;
      GLU_NURBS_TEX_COORD_EXT                    = 100168;
      GLU_NURBS_END_EXT                          = 100169;
      GLU_NURBS_BEGIN_DATA_EXT                   = 100170;
      GLU_NURBS_VERTEX_DATA_EXT                  = 100171;
      GLU_NURBS_NORMAL_DATA_EXT                  = 100172;
      GLU_NURBS_COLOR_DATA_EXT                   = 100173;
      GLU_NURBS_TEX_COORD_DATA_EXT               = 100174;
      GLU_NURBS_END_DATA_EXT                     = 100175;

      // EXT_object_space_tess
      GLU_OBJECT_PARAMETRIC_ERROR_EXT            = 100208;
      GLU_OBJECT_PATH_LENGTH_EXT                 = 100209;

//------------------------------------------------------------------------------

const // ********** GLU generic constants **********

      // Errors: (return value 0 = no error)
      GLU_INVALID_ENUM                           = 100900;
      GLU_INVALID_VALUE                          = 100901;
      GLU_OUT_OF_MEMORY                          = 100902;
      GLU_INCOMPATIBLE_GL_VERSION                = 100903;

      // StringName
      GLU_VERSION                                = 100800;
      GLU_EXTENSIONS                             = 100801;

      // Boolean
      GLU_TRUE                                   = GL_TRUE;
      GLU_FALSE                                  = GL_FALSE;

      // Quadric constants
      // QuadricNormal
      GLU_SMOOTH                                 = 100000;
      GLU_FLAT                                   = 100001;
      GLU_NONE                                   = 100002;

      // QuadricDrawStyle
      GLU_POINT                                  = 100010;
      GLU_LINE                                   = 100011;
      GLU_FILL                                   = 100012;
      GLU_SILHOUETTE                             = 100013;

      // QuadricOrientation
      GLU_OUTSIDE                                = 100020;
      GLU_INSIDE                                 = 100021;

      // Tesselation constants

      GLU_TESS_MAX_COORD                         = 1.0e150;

      // TessProperty
      GLU_TESS_WINDING_RULE                      = 100140;
      GLU_TESS_BOUNDARY_ONLY                     = 100141;
      GLU_TESS_TOLERANCE                         = 100142;

      // TessWinding
      GLU_TESS_WINDING_ODD                       = 100130;
      GLU_TESS_WINDING_NONZERO                   = 100131;
      GLU_TESS_WINDING_POSITIVE                  = 100132;
      GLU_TESS_WINDING_NEGATIVE                  = 100133;
      GLU_TESS_WINDING_ABS_GEQ_TWO               = 100134;

      // TessCallback
      GLU_TESS_BEGIN                             = 100100; // GlUTessBeginProc
      GLU_TESS_VERTEX                            = 100101; // TGLUTessVertexProc
      GLU_TESS_END                               = 100102; // TGLUTessEndProc
      GLU_TESS_ERROR                             = 100103; // TGLUTessErrorProc
      GLU_TESS_EDGE_FLAG                         = 100104; // TGLUTessEdgeFlagProc
      GLU_TESS_COMBINE                           = 100105; // TGLUTessCombineProc
      GLU_TESS_BEGIN_DATA                        = 100106; // TGLUTessBeginDataProc
      GLU_TESS_VERTEX_DATA                       = 100107; // TGLUTessVertexDataProc
      GLU_TESS_END_DATA                          = 100108; // TGLUTessEndDataProc
      GLU_TESS_ERROR_DATA                        = 100109; // TGLUTessErrorDataProc
      GLU_TESS_EDGE_FLAG_DATA                    = 100110; // TGLUTessEdgeFlagDataProc
      GLU_TESS_COMBINE_DATA                      = 100111; // TGLUTessCombineDataProc

      // TessError
      GLU_TESS_ERROR1                            = 100151;
      GLU_TESS_ERROR2                            = 100152;
      GLU_TESS_ERROR3                            = 100153;
      GLU_TESS_ERROR4                            = 100154;
      GLU_TESS_ERROR5                            = 100155;
      GLU_TESS_ERROR6                            = 100156;
      GLU_TESS_ERROR7                            = 100157;
      GLU_TESS_ERROR8                            = 100158;

      GLU_TESS_MISSING_BEGIN_POLYGON             = GLU_TESS_ERROR1;
      GLU_TESS_MISSING_BEGIN_CONTOUR             = GLU_TESS_ERROR2;
      GLU_TESS_MISSING_END_POLYGON               = GLU_TESS_ERROR3;
      GLU_TESS_MISSING_END_CONTOUR               = GLU_TESS_ERROR4;
      GLU_TESS_COORD_TOO_LARGE                   = GLU_TESS_ERROR5;
      GLU_TESS_NEED_COMBINE_CALLBACK             = GLU_TESS_ERROR6;

      // NURBS constants

      // NurbsProperty
      GLU_AUTO_LOAD_MATRIX                       = 100200;
      GLU_CULLING                                = 100201;
      GLU_SAMPLING_TOLERANCE                     = 100203;
      GLU_DISPLAY_MODE                           = 100204;
      GLU_PARAMETRIC_TOLERANCE                   = 100202;
      GLU_SAMPLING_METHOD                        = 100205;
      GLU_U_STEP                                 = 100206;
      GLU_V_STEP                                 = 100207;

      // NurbsSampling
      GLU_PATH_LENGTH                            = 100215;
      GLU_PARAMETRIC_ERROR                       = 100216;
      GLU_DOMAIN_DISTANCE                        = 100217;

      // NurbsTrim
      GLU_MAP1_TRIM_2                            = 100210;
      GLU_MAP1_TRIM_3                            = 100211;

      // NurbsDisplay
      GLU_OUTLINE_POLYGON                        = 100240;
      GLU_OUTLINE_PATCH                          = 100241;

      // NurbsErrors
      GLU_NURBS_ERROR1                           = 100251;
      GLU_NURBS_ERROR2                           = 100252;
      GLU_NURBS_ERROR3                           = 100253;
      GLU_NURBS_ERROR4                           = 100254;
      GLU_NURBS_ERROR5                           = 100255;
      GLU_NURBS_ERROR6                           = 100256;
      GLU_NURBS_ERROR7                           = 100257;
      GLU_NURBS_ERROR8                           = 100258;
      GLU_NURBS_ERROR9                           = 100259;
      GLU_NURBS_ERROR10                          = 100260;
      GLU_NURBS_ERROR11                          = 100261;
      GLU_NURBS_ERROR12                          = 100262;
      GLU_NURBS_ERROR13                          = 100263;
      GLU_NURBS_ERROR14                          = 100264;
      GLU_NURBS_ERROR15                          = 100265;
      GLU_NURBS_ERROR16                          = 100266;
      GLU_NURBS_ERROR17                          = 100267;
      GLU_NURBS_ERROR18                          = 100268;
      GLU_NURBS_ERROR19                          = 100269;
      GLU_NURBS_ERROR20                          = 100270;
      GLU_NURBS_ERROR21                          = 100271;
      GLU_NURBS_ERROR22                          = 100272;
      GLU_NURBS_ERROR23                          = 100273;
      GLU_NURBS_ERROR24                          = 100274;
      GLU_NURBS_ERROR25                          = 100275;
      GLU_NURBS_ERROR26                          = 100276;
      GLU_NURBS_ERROR27                          = 100277;
      GLU_NURBS_ERROR28                          = 100278;
      GLU_NURBS_ERROR29                          = 100279;
      GLU_NURBS_ERROR30                          = 100280;
      GLU_NURBS_ERROR31                          = 100281;
      GLU_NURBS_ERROR32                          = 100282;
      GLU_NURBS_ERROR33                          = 100283;
      GLU_NURBS_ERROR34                          = 100284;
      GLU_NURBS_ERROR35                          = 100285;
      GLU_NURBS_ERROR36                          = 100286;
      GLU_NURBS_ERROR37                          = 100287;

      // Contours types -- obsolete!
      GLU_CW                                     = 100120;
      GLU_CCW                                    = 100121;
      GLU_INTERIOR                               = 100122;
      GLU_EXTERIOR                               = 100123;
      GLU_UNKNOWN                                = 100124;

      // Names without "TESS_" prefix
      GLU_BEGIN                                  = GLU_TESS_BEGIN;
      GLU_VERTEX                                 = GLU_TESS_VERTEX;
      GLU_END                                    = GLU_TESS_END;
      GLU_ERROR                                  = GLU_TESS_ERROR;
      GLU_EDGE_FLAG                              = GLU_TESS_EDGE_FLAG;

//------------------------------------------------------------------------------

// GLU types:

type  TGLUNurbs                   = record end;
      TGLUQuadric                 = record end;
      TGLUTesselator              = record end;

      PGLUNurbs                   = ^TGLUNurbs;
      PGLUQuadric                 = ^TGLUQuadric;
      PGLUTesselator              = ^TGLUTesselator;

      // backwards compatibility:
      GLUNurbsObj                = PGLUNurbs;
      GLUQuadricObj              = PGLUQuadric;
      GLUTesselatorObj           = PGLUTesselator;
      GLUTriangulatorObj         = PGLUTesselator;

      PGLUNurbsObj                = PGLUNurbs;
      PGLUQuadricObj              = PGLUQuadric;
      PGLUTesselatorObj           = PGLUTesselator;
      PGLUTriangulatorObj         = PGLUTesselator;

      // Callback function prototypes
      // GLUQuadricCallback
      TGLUQuadricErrorProc        = procedure(errorCode: GLEnum); stdcall;

      // GLUTessCallback 
      GLUTessBeginProc           = procedure(AType: GLEnum); stdcall;
      GLUTessEdgeFlagProc        = procedure(Flag: GLboolean); stdcall;
      GLUTessVertexProc          = procedure(VertexData: Pointer); stdcall;
      GLUTessEndProc             = procedure; stdcall;
      GLUTessErrorProc           = procedure(ErrNo: GLEnum); stdcall;
      GLUTessCombineProc         = procedure(Coords: PGLdouble; VertexData: Pointer; Weight: PGLdouble; OutData: Pointer); stdcall;
      GLUTessBeginDataProc       = procedure(AType: GLEnum; UserData: Pointer); stdcall;
      GLUTessEdgeFlagDataProc    = procedure(Flag: GLboolean; UserData: Pointer); stdcall;
      GLUTessVertexDataProc      = procedure(VertexData: Pointer; UserData: Pointer); stdcall;
      GLUTessEndDataProc         = procedure(UserData: Pointer); stdcall;
      GLUTessErrorDataProc       = procedure(ErrNo: GLEnum; UserData: Pointer); stdcall;
      GLUTessCombineDataProc     = procedure(Coords: PGLdouble; VertexData: Pointer; Weight: PGLdouble; OutData: Pointer; UserData: Pointer); stdcall;

      // GLUNurbsCallback
      GLUNurbsErrorProc          = procedure(ErrorCode: GLEnum); stdcall;

//------------------------------------------------------------------------------

{$ifndef VER120}
const // additional constants, which are not defined in Windows.pas (D3 and lower)

      // TLayerPlaneDescriptor flags
      LPD_DOUBLEBUFFER            = $00000001;
      LPD_STEREO                  = $00000002;
      LPD_SUPPORT_GDI             = $00000010;
      LPD_SUPPORT_OPENGL          = $00000020;
      LPD_SHARE_DEPTH             = $00000040;
      LPD_SHARE_STENCIL           = $00000080;
      LPD_SHARE_ACCUM             = $00000100;
      LPD_SWAP_EXCHANGE           = $00000200;
      LPD_SWAP_COPY               = $00000400;
      LPD_TRANSPARENT             = $00001000;

      LPD_TYPE_RGBA               = 0;
      LPD_TYPE_COLORINDEX         = 1;

      // wglSwapLayerBuffers flags
      WGL_SWAP_MAIN_PLANE         = $00000001;
      WGL_SWAP_OVERLAY1           = $00000002;
      WGL_SWAP_OVERLAY2           = $00000004;
      WGL_SWAP_OVERLAY3           = $00000008;
      WGL_SWAP_OVERLAY4           = $00000010;
      WGL_SWAP_OVERLAY5           = $00000020;
      WGL_SWAP_OVERLAY6           = $00000040;
      WGL_SWAP_OVERLAY7           = $00000080;
      WGL_SWAP_OVERLAY8           = $00000100;
      WGL_SWAP_OVERLAY9           = $00000200;
      WGL_SWAP_OVERLAY10          = $00000400;
      WGL_SWAP_OVERLAY11          = $00000800;
      WGL_SWAP_OVERLAY12          = $00001000;
      WGL_SWAP_OVERLAY13          = $00002000;
      WGL_SWAP_OVERLAY14          = $00004000;
      WGL_SWAP_OVERLAY15          = $00008000;
      WGL_SWAP_UNDERLAY1          = $00010000;
      WGL_SWAP_UNDERLAY2          = $00020000;
      WGL_SWAP_UNDERLAY3          = $00040000;
      WGL_SWAP_UNDERLAY4          = $00080000;
      WGL_SWAP_UNDERLAY5          = $00100000;
      WGL_SWAP_UNDERLAY6          = $00200000;
      WGL_SWAP_UNDERLAY7          = $00400000;
      WGL_SWAP_UNDERLAY8          = $00800000;
      WGL_SWAP_UNDERLAY9          = $01000000;
      WGL_SWAP_UNDERLAY10         = $02000000;
      WGL_SWAP_UNDERLAY11         = $04000000;
      WGL_SWAP_UNDERLAY12         = $08000000;
      WGL_SWAP_UNDERLAY13         = $10000000;
      WGL_SWAP_UNDERLAY14         = $20000000;
      WGL_SWAP_UNDERLAY15         = $40000000;

type PLayerPlaneDescriptor = ^TLayerPlaneDescriptor;
     TLayerPlaneDescriptor = packed record
                               nSize,
                               nVersion        : Word;
                               dwFlags         : Integer;
                               iPixelType,
                               cColorBits,
                               cRedBits,
                               cRedShift,
                               cGreenBits,
                               cGreenShift,
                               cBlueBits,
                               cBlueShift,
                               cAlphaBits,
                               cAlphaShift,
                               cAccumBits,
                               cAccumRedBits,
                               cAccumGreenBits,
                               cAccumBlueBits,
                               cAccumAlphaBits,
                               cDepthBits,
                               cStencilBits,
                               cAuxBuffers,
                               iLayerPlane,
                               bReserved         : Byte;
                               crTransparent     : COLORREF;
                             end;

{$endif}

//------------------------------------------------------------------------------

// GL functions and procedures:

var glAccum : procedure(op: GLuint; value: GLfloat); stdcall;
    glAlphaFunc : procedure(func: GLEnum; ref: GLclampf); stdcall;
    glAreTexturesResident : function(n: GLsizei; Textures: PGLuint; residences: PGLboolean): GLboolean; stdcall;
    glArrayElement : procedure(i: GLint); stdcall;
    glBegin : procedure(mode: GLEnum); stdcall;
    glBindTexture : procedure(target: GLEnum; texture: GLuint); stdcall;
    glBitmap : procedure(width: GLsizei; height: GLsizei; xorig, yorig: GLfloat; xmove: GLfloat; ymove: GLfloat; bitmap: Pointer); stdcall;
    glBlendFunc : procedure(sfactor: GLEnum; dfactor: GLEnum); stdcall;
    glCallList : procedure(list: GLuint); stdcall;
    glCallLists : procedure(n: GLsizei; atype: GLEnum; lists: Pointer); stdcall;
    glClear : procedure(mask: GLbitfield); stdcall;
    glClearAccum : procedure(red, green, blue, alpha: GLfloat); stdcall;
    glClearColor : procedure(red, green, blue, alpha: GLclampf); stdcall;
    glClearDepth : procedure(depth: GLclampd); stdcall;
    glClearIndex : procedure(c: GLfloat); stdcall;
    glClearStencil : procedure(s: GLint ); stdcall;
    glClipPlane : procedure(plane: GLEnum; equation: PGLdouble); stdcall;
    glColor3b : procedure(red, green, blue: GLbyte); stdcall;
    glColor3bv : procedure(v: PGLbyte); stdcall; 
    glColor3d : procedure(red, green, blue: GLdouble); stdcall;
    glColor3dv : procedure(v: PGLdouble); stdcall; 
    glColor3f : procedure(red, green, blue: GLfloat); stdcall;
    glColor3fv : procedure(v: PGLfloat); stdcall;
    glColor3i : procedure(red, green, blue: GLint); stdcall;
    glColor3iv : procedure(v: PGLint); stdcall;
    glColor3s : procedure(red, green, blue: GLshort); stdcall;
    glColor3sv : procedure(v: PGLshort); stdcall;
    glColor3ub : procedure(red, green, blue: GLubyte); stdcall;
    glColor3ubv : procedure(v: PGLubyte); stdcall;
    glColor3ui : procedure(red, green, blue: GLuint); stdcall;
    glColor3uiv : procedure(v: PGLuint); stdcall;
    glColor3us : procedure(red, green, blue: GLushort); stdcall;
    glColor3usv : procedure(v: PGLushort); stdcall;
    glColor4b : procedure(red, green, blue, alpha: GLbyte); stdcall;
    glColor4bv : procedure(v: PGLbyte); stdcall;
    glColor4d : procedure(red, green, blue, alpha: GLdouble ); stdcall;
    glColor4dv : procedure(v: PGLdouble); stdcall;
    glColor4f : procedure(red, green, blue, alpha: GLfloat); stdcall;
    glColor4fv : procedure(v: PGLfloat); stdcall;
    glColor4i : procedure(red, green, blue, alpha: GLint); stdcall;
    glColor4iv : procedure(v: PGLint); stdcall;
    glColor4s : procedure(red, green, blue, alpha: GLshort); stdcall;
    glColor4sv : procedure(v: GLshort); stdcall;
    glColor4ub : procedure(red, green, blue, alpha: GLubyte); stdcall;
    glColor4ubv : procedure(v: PGLubyte); stdcall;
    glColor4ui : procedure(red, green, blue, alpha: GLuint); stdcall;
    glColor4uiv : procedure(v: PGLuint); stdcall;
    glColor4us : procedure(red, green, blue, alpha: GLushort); stdcall;
    glColor4usv : procedure(v: PGLushort); stdcall;
    glColorMask : procedure(red, green, blue, alpha: GLboolean); stdcall;
    glColorMaterial : procedure(face: GLEnum; mode: GLEnum); stdcall;
    glColorPointer : procedure(size: GLint; atype: GLEnum; stride: GLsizei; data: pointer); stdcall;
    glCopyPixels : procedure(x, y: GLint; width, height: GLsizei; atype: GLEnum); stdcall;
    glCopyTexImage1D : procedure(target: GLEnum; level: GLint; internalFormat: GLEnum; x, y: GLint; width: GLsizei; border: GLint); stdcall;
    glCopyTexImage2D : procedure(target: GLEnum; level: GLint; internalFormat: GLEnum; x, y: GLint; width, height: GLsizei; border: GLint); stdcall;
    glCopyTexSubImage1D : procedure(target: GLEnum; level, xoffset, x, y: GLint; width: GLsizei); stdcall;
    glCopyTexSubImage2D : procedure(target: GLEnum; level, xoffset, yoffset, x, y: GLint; width, height: GLsizei); stdcall;
    glCullFace : procedure(mode: GLEnum); stdcall;
    glDeleteLists : procedure(list: GLuint; range: GLsizei); stdcall;
    glDeleteTextures : procedure(n: GLsizei; textures: PGLuint); stdcall;
    glDepthFunc : procedure(func: GLEnum); stdcall;
    glDepthMask : procedure(flag: GLboolean); stdcall;
    glDepthRange : procedure(zNear, zFar: GLclampd); stdcall;
    glDisable : procedure(cap: GLEnum); stdcall;
    glDisableClientState : procedure(aarray: GLEnum); stdcall;
    glDrawArrays : procedure(mode: GLEnum; first: GLint; count: GLsizei); stdcall;
    glDrawBuffer : procedure(mode: GLEnum); stdcall;
    glDrawElements : procedure(mode: GLEnum; count: GLsizei; atype: GLEnum; indices: Pointer); stdcall;
    glDrawPixels : procedure(width, height: GLsizei; format, atype: GLEnum; pixels: Pointer); stdcall;
    glEdgeFlag : procedure(flag: GLboolean); stdcall;
    glEdgeFlagPointer : procedure(stride: GLsizei; data: pointer); stdcall;
    glEdgeFlagv : procedure(flag: PGLboolean); stdcall;
    glEnable : procedure(cap: GLEnum); stdcall;
    glEnableClientState : procedure(aarray: GLEnum); stdcall;
    glEnd : procedure; stdcall;
    glEndList : procedure; stdcall; 
    glEvalCoord1d : procedure(u: GLdouble); stdcall;
    glEvalCoord1dv : procedure(u: PGLdouble); stdcall;
    glEvalCoord1f : procedure(u: GLfloat); stdcall;
    glEvalCoord1fv : procedure(u: PGLfloat); stdcall;
    glEvalCoord2d : procedure(u: GLdouble; v:GLdouble); stdcall;
    glEvalCoord2dv : procedure(u: PGLdouble); stdcall; 
    glEvalCoord2f : procedure(u, v: GLfloat); stdcall;
    glEvalCoord2fv : procedure(u: PGLfloat); stdcall;
    glEvalMesh1 : procedure(mode: GLEnum; i1, i2: GLint); stdcall;
    glEvalMesh2 : procedure(mode: GLEnum; i1, i2, j1, j2: GLint); stdcall;
    glEvalPoint1 : procedure(i: GLint); stdcall;
    glEvalPoint2 : procedure(i, j: GLint); stdcall;
    glFeedbackBuffer : procedure(size: GLsizei; atype: GLEnum; buffer: PGLfloat); stdcall;
    glFinish : procedure; stdcall;
    glFlush : procedure; stdcall; 
    glFogf : procedure(pname: GLEnum; param: GLfloat); stdcall;
    glFogfv : procedure(pname: GLEnum; params: PGLfloat); stdcall;
    glFogi : procedure(pname: GLEnum; param: GLint); stdcall;
    glFogiv : procedure(pname: GLEnum; params: PGLint); stdcall;
    glFrontFace : procedure(mode: GLEnum); stdcall;
    glFrustum : procedure(left, right, bottom, top, zNear, zFar: GLdouble); stdcall;
    glGenLists : function(range: GLsizei): GLuint; stdcall;
    glGenTextures : procedure(n: GLsizei; textures: PGLuint); stdcall;
    glGetBooleanv : procedure(pname: GLEnum; params: PGLboolean); stdcall;
    glGetClipPlane : procedure(plane: GLEnum; equation: PGLdouble); stdcall;
    glGetDoublev : procedure(pname: GLEnum; params: PGLdouble); stdcall;
    glGetError : function : GLuint; stdcall;
    glGetFloatv : procedure(pname: GLEnum; params: PGLfloat); stdcall;
    glGetIntegerv : procedure(pname: GLEnum; params: PGLint); stdcall;
    glGetLightfv : procedure(light, pname: GLEnum; params: PGLfloat); stdcall;
    glGetLightiv : procedure(light, pname: GLEnum; params: PGLint); stdcall;
    glGetMapdv : procedure(target, query: GLEnum; v: PGLdouble); stdcall;
    glGetMapfv : procedure(target, query: GLEnum; v: PGLfloat); stdcall;
    glGetMapiv : procedure(target, query: GLEnum; v: PGLint); stdcall;
    glGetMaterialfv : procedure(face, pname: GLEnum; params: PGLfloat); stdcall;
    glGetMaterialiv : procedure(face, pname: GLEnum; params: PGLint); stdcall;
    glGetPixelMapfv : procedure(map: GLEnum; values: PGLfloat); stdcall;
    glGetPixelMapuiv : procedure(map: GlEnum; values: PGLuint); stdcall; 
    glGetPixelMapusv : procedure(map: GlEnum; values: PGLushort); stdcall; 
    glGetPointerv : procedure(pname: GlEnum; params: Pointer); stdcall; 
    glGetPolygonStipple : procedure(mask: PGLubyte); stdcall;
    glGetString : function(name: GlEnum): PChar; stdcall;
    glGetTexEnvfv : procedure(target, pname: GlEnum; params: PGLfloat); stdcall;
    glGetTexEnviv : procedure(target, pname: GlEnum; params: PGLint); stdcall;
    glGetTexGendv : procedure(coord, pname: GlEnum; params: PGLdouble); stdcall;
    glGetTexGenfv : procedure(coord, pname: GlEnum; params: PGLfloat); stdcall; 
    glGetTexGeniv : procedure(coord, pname: GlEnum; params: PGLint); stdcall;
    glGetTexImage : procedure(target: GlEnum; level: Glint; format, atype: GlEnum; pixels: Pointer); stdcall; 
    glGetTexLevelParameterfv : procedure(target: GlEnum; level: Glint; pname: GlEnum; params: PGLfloat); stdcall;
    glGetTexLevelParameteriv : procedure(target: GlEnum; level: Glint; pname: GlEnum; params: PGLint); stdcall;
    glGetTexParameterfv : procedure(target, pname: GlEnum; params: PGLfloat); stdcall;
    glGetTexParameteriv : procedure(target, pname: GlEnum; params: PGLint); stdcall; 
    glHint : procedure(target, mode: GlEnum); stdcall; 
    glIndexMask : procedure(mask: Gluint); stdcall;
    glIndexPointer : procedure(atype: GlEnum; stride: Glsizei; data: pointer); stdcall; 
    glIndexd : procedure(c: Gldouble); stdcall;
    glIndexdv : procedure(c: PGLdouble); stdcall; 
    glIndexf : procedure(c: Glfloat); stdcall; 
    glIndexfv : procedure(c: PGLfloat); stdcall; 
    glIndexi : procedure(c: Glint); stdcall; 
    glIndexiv : procedure(c: PGLint); stdcall;
    glIndexs : procedure(c: Glshort); stdcall;
    glIndexsv : procedure(c: PGLshort); stdcall;
    glIndexub : procedure(c: Glubyte); stdcall; 
    glIndexubv : procedure(c: PGLubyte); stdcall; 
    glInitNames : procedure; stdcall;
    glInterleavedArrays : procedure(format: GlEnum; stride: Glsizei; data: pointer); stdcall; 
    glIsEnabled : function(cap: GlEnum): Glboolean; stdcall;
    glIsList : function(list: Gluint): Glboolean; stdcall;
    glIsTexture : function(texture: Gluint): Glboolean; stdcall;
    glLightModelf : procedure(pname: GlEnum; param: Glfloat); stdcall;
    glLightModelfv : procedure(pname: GlEnum; params: PGLfloat); stdcall;
    glLightModeli : procedure(pname: GlEnum; param: Glint); stdcall;
    glLightModeliv : procedure(pname: GlEnum; params: PGLint); stdcall;
    glLightf : procedure(light, pname: GlEnum; param: Glfloat); stdcall;
    glLightfv : procedure(light, pname: GlEnum; params: PGLfloat); stdcall;
    glLighti : procedure(light, pname: GlEnum; param: Glint); stdcall;
    glLightiv : procedure(light, pname: GlEnum; params: PGLint); stdcall; 
    glLineStipple : procedure(factor: Glint; pattern: Glushort); stdcall; 
    glLineWidth : procedure(width: Glfloat); stdcall;
    glListBase : procedure(base: Gluint); stdcall;
    glLoadIdentity : procedure; stdcall;
    glLoadMatrixd : procedure(m: PGLdouble); stdcall;
    glLoadMatrixf : procedure(m: PGLfloat); stdcall;
    glLoadName : procedure(name: Gluint); stdcall; 
    glLogicOp : procedure(opcode: GlEnum); stdcall;
    glMap1d : procedure(target: GlEnum; u1, u2: Gldouble; stride, order: Glint; points: PGLdouble); stdcall;
    glMap1f : procedure(target: GlEnum; u1, u2: Glfloat; stride, order: Glint; points: PGLfloat);   stdcall;
    glMap2d : procedure(target: GlEnum; u1, u2: Gldouble; ustride, uorder: Glint; v1, v2: Gldouble; vstride, vorder: Glint; points: PGLdouble); stdcall;
    glMap2f : procedure(target: GlEnum; u1, u2: Glfloat; ustride, uorder: Glint; v1, v2: Glfloat; vstride, vorder: Glint; points: PGLfloat); stdcall;
    glMapGrid1d : procedure(un: Glint; u1, u2: Gldouble); stdcall;
    glMapGrid1f : procedure(un: Glint; u1, u2: Glfloat); stdcall;
    glMapGrid2d : procedure(un: Glint; u1, u2: Gldouble; vn: Glint; v1, v2: Gldouble); stdcall;
    glMapGrid2f : procedure(un: Glint; u1, u2: Glfloat; vn: Glint; v1, v2: Glfloat); stdcall;
    glMaterialf : procedure(face, pname: GlEnum; param: Glfloat); stdcall;
    glMaterialfv : procedure(face, pname: GlEnum; params: PGLfloat); stdcall;
    glMateriali : procedure(face, pname: GlEnum; param: Glint); stdcall;
    glMaterialiv : procedure(face, pname: GlEnum; params: PGLint); stdcall;
    glMatrixMode : procedure(mode: GlEnum); stdcall;
    glMultMatrixd : procedure(m: PGLdouble); stdcall;
    glMultMatrixf : procedure(m: PGLfloat); stdcall;
    glNewList : procedure(list: Gluint; mode: GlEnum); stdcall;
    glNormal3b : procedure(nx, ny, nz: Glbyte); stdcall;
    glNormal3bv : procedure(v: PGLbyte); stdcall;
    glNormal3d : procedure(nx, ny, nz: Gldouble); stdcall;
    glNormal3dv : procedure(v: PGLdouble); stdcall;
    glNormal3f : procedure(nx, ny, nz: Glfloat); stdcall;
    glNormal3fv : procedure(v: PGLfloat); stdcall;
    glNormal3i : procedure(nx, ny, nz: Glint); stdcall;
    glNormal3iv : procedure(v: PGLint); stdcall;
    glNormal3s : procedure(nx, ny, nz: Glshort); stdcall;
    glNormal3sv : procedure(v: PGLshort); stdcall; 
    glNormalPointer : procedure(atype: GlEnum; stride: Glsizei; data: pointer); stdcall;
    glOrtho : procedure(left, right, bottom, top, zNear, zFar: Gldouble); stdcall; 
    glPassThrough : procedure(token: Glfloat); stdcall; 
    glPixelMapfv : procedure(map: GlEnum; mapsize: Glsizei; values: PGLfloat); stdcall;
    glPixelMapuiv : procedure(map: GlEnum; mapsize: Glsizei; values: PGLuint); stdcall;
    glPixelMapusv : procedure(map: GlEnum; mapsize: Glsizei; values: PGLushort); stdcall;
    glPixelStoref : procedure(pname: GlEnum; param: Glfloat); stdcall;
    glPixelStorei : procedure(pname: GlEnum; param: Glint); stdcall;
    glPixelTransferf : procedure(pname: GlEnum; param: Glfloat); stdcall;
    glPixelTransferi : procedure(pname: GlEnum; param: Glint); stdcall;
    glPixelZoom : procedure(xfactor, yfactor: Glfloat); stdcall;
    glPointSize : procedure(size: Glfloat); stdcall;
    glPolygonMode : procedure(face, mode: GlEnum); stdcall;
    glPolygonOffset : procedure(factor, units: Glfloat); stdcall;
    glPolygonStipple : procedure(mask: PGLubyte); stdcall;
    glPopAttrib : procedure; stdcall;
    glPopClientAttrib : procedure; stdcall;
    glPopMatrix : procedure; stdcall;
    glPopName : procedure; stdcall;
    glPrioritizeTextures : procedure(n: Glsizei; textures: PGLuint; priorities: PGLclampf); stdcall;
    glPushAttrib : procedure(mask: Glbitfield); stdcall; 
    glPushClientAttrib : procedure(mask: Glbitfield); stdcall; 
    glPushMatrix : procedure; stdcall; 
    glPushName : procedure(name: Gluint); stdcall; 
    glRasterPos2d : procedure(x, y: Gldouble); stdcall; 
    glRasterPos2dv : procedure(v: PGLdouble); stdcall; 
    glRasterPos2f : procedure(x, y: Glfloat); stdcall; 
    glRasterPos2fv : procedure(v: PGLfloat); stdcall; 
    glRasterPos2i : procedure(x, y: Glint); stdcall; 
    glRasterPos2iv : procedure(v: PGLint); stdcall;
    glRasterPos2s : procedure(x, y: PGLshort); stdcall;
    glRasterPos2sv : procedure(v: PGLshort); stdcall; 
    glRasterPos3d : procedure(x, y, z: Gldouble); stdcall; 
    glRasterPos3dv : procedure(v: PGLdouble); stdcall; 
    glRasterPos3f : procedure(x, y, z: Glfloat); stdcall; 
    glRasterPos3fv : procedure(v: PGLfloat); stdcall; 
    glRasterPos3i : procedure(x, y, z: Glint); stdcall;
    glRasterPos3iv : procedure(v: PGLint); stdcall;
    glRasterPos3s : procedure(x, y, z: Glshort); stdcall; 
    glRasterPos3sv : procedure(v: PGLshort); stdcall;
    glRasterPos4d : procedure(x, y, z, w: Gldouble); stdcall; 
    glRasterPos4dv : procedure(v: PGLdouble); stdcall; 
    glRasterPos4f : procedure(x, y, z, w: Glfloat); stdcall;
    glRasterPos4fv : procedure(v: PGLfloat); stdcall;
    glRasterPos4i : procedure(x, y, z, w: Glint); stdcall;
    glRasterPos4iv : procedure(v: PGLint); stdcall;
    glRasterPos4s : procedure(x, y, z, w: Glshort); stdcall; 
    glRasterPos4sv : procedure(v: PGLshort); stdcall; 
    glReadBuffer : procedure(mode: GlEnum); stdcall; 
    glReadPixels : procedure(x, y: Glint; width, height: Glsizei; format, atype: GlEnum; pixels: Pointer); stdcall;
    glRectd : procedure(x1, y1, x2, y2: Gldouble); stdcall;
    glRectdv : procedure(v1, v2: PGLdouble); stdcall;
    glRectf : procedure(x1, y1, x2, y2: Glfloat); stdcall; 
    glRectfv : procedure(v1, v2: PGLfloat); stdcall; 
    glRecti : procedure(x1, y1, x2, y2: Glint); stdcall; 
    glRectiv : procedure(v1, v2: PGLint); stdcall; 
    glRects : procedure(x1, y1, x2, y2: Glshort); stdcall; 
    glRectsv : procedure(v1, v2: PGLshort); stdcall; 
    glRenderMode : function(mode: GlEnum): Glint; stdcall;
    glRotated : procedure(angle, x, y, z: Gldouble); stdcall;
    glRotatef : procedure(angle, x, y, z: Glfloat); stdcall; 
    glScaled : procedure(x, y, z: Gldouble); stdcall; 
    glScalef : procedure(x, y, z: Glfloat); stdcall;
    glScissor : procedure(x, y: Glint; width, height: Glsizei); stdcall; 
    glSelectBuffer : procedure(size: Glsizei; buffer: PGLuint); stdcall; 
    glShadeModel : procedure(mode: GlEnum); stdcall; 
    glStencilFunc : procedure(func: GlEnum; ref: Glint; mask: Gluint); stdcall; 
    glStencilMask : procedure(mask: Gluint); stdcall;
    glStencilOp : procedure(fail, zfail, zpass: GlEnum); stdcall; 
    glTexCoord1d : procedure(s: Gldouble); stdcall; 
    glTexCoord1dv : procedure(v: PGLdouble); stdcall;
    glTexCoord1f : procedure(s: Glfloat); stdcall; 
    glTexCoord1fv : procedure(v: PGLfloat); stdcall;
    glTexCoord1i : procedure(s: Glint); stdcall; 
    glTexCoord1iv : procedure(v: PGLint); stdcall; 
    glTexCoord1s : procedure(s: Glshort); stdcall;
    glTexCoord1sv : procedure(v: PGLshort); stdcall;
    glTexCoord2d : procedure(s, t: Gldouble); stdcall;
    glTexCoord2dv : procedure(v: PGLdouble); stdcall;
    glTexCoord2f : procedure(s, t: Glfloat); stdcall; 
    glTexCoord2fv : procedure(v: PGLfloat); stdcall; 
    glTexCoord2i : procedure(s, t: Glint); stdcall;
    glTexCoord2iv : procedure(v: PGLint); stdcall;
    glTexCoord2s : procedure(s, t: Glshort); stdcall;
    glTexCoord2sv : procedure(v: PGLshort); stdcall;
    glTexCoord3d : procedure(s, t, r: Gldouble); stdcall;
    glTexCoord3dv : procedure(v: PGLdouble); stdcall;
    glTexCoord3f : procedure(s, t, r: Glfloat); stdcall;
    glTexCoord3fv : procedure(v: PGLfloat); stdcall;
    glTexCoord3i : procedure(s, t, r: Glint); stdcall;
    glTexCoord3iv : procedure(v: PGLint); stdcall;
    glTexCoord3s : procedure(s, t, r: Glshort); stdcall;
    glTexCoord3sv : procedure(v: PGLshort); stdcall;
    glTexCoord4d : procedure(s, t, r, q: Gldouble); stdcall;
    glTexCoord4dv : procedure(v: PGLdouble); stdcall;
    glTexCoord4f : procedure(s, t, r, q: Glfloat); stdcall;
    glTexCoord4fv : procedure(v: PGLfloat); stdcall;
    glTexCoord4i : procedure(s, t, r, q: Glint); stdcall;
    glTexCoord4iv : procedure(v: PGLint); stdcall;
    glTexCoord4s : procedure(s, t, r, q: Glshort); stdcall;
    glTexCoord4sv : procedure(v: PGLshort); stdcall;
    glTexCoordPointer : procedure(size: Glint; atype: GlEnum; stride: Glsizei; data: pointer); stdcall;
    glTexEnvf : procedure(target, pname: GlEnum; param: Glfloat); stdcall; 
    glTexEnvfv : procedure(target, pname: GlEnum; params: PGLfloat); stdcall;
    glTexEnvi : procedure(target, pname: GlEnum; param: Glint); stdcall; 
    glTexEnviv : procedure(target, pname: GlEnum; params: PGLint); stdcall; 
    glTexGend : procedure(coord, pname: GlEnum; param: Gldouble); stdcall; 
    glTexGendv : procedure(coord, pname: GlEnum; params: PGLdouble); stdcall;
    glTexGenf : procedure(coord, pname: GlEnum; param: Glfloat); stdcall;
    glTexGenfv : procedure(coord, pname: GlEnum; params: PGLfloat); stdcall;
    glTexGeni : procedure(coord, pname: GlEnum; param: Glint); stdcall; 
    glTexGeniv : procedure(coord, pname: GlEnum; params: PGLint); stdcall;
    glTexImage1D : procedure(target: GlEnum; level, internalformat: Glint; width: Glsizei; border: Glint; format, atype: GlEnum; pixels: Pointer); stdcall;
    glTexImage2D : procedure(target: GlEnum; level, internalformat: Glint; width, height: Glsizei; border: Glint; format, atype: GlEnum; Pixels:Pointer); stdcall;
    glTexParameterf : procedure(target, pname: GlEnum; param: Glfloat); stdcall;
    glTexParameterfv : procedure(target, pname: GlEnum; params: PGLfloat); stdcall;
    glTexParameteri : procedure(target, pname: GlEnum; param: Glint); stdcall;
    glTexParameteriv : procedure(target, pname: GlEnum; params: PGLint); stdcall;
    glTexSubImage1D : procedure(target: GlEnum; level, xoffset: Glint; width: Glsizei; format, atype: GlEnum; pixels: Pointer); stdcall;
    glTexSubImage2D : procedure(target: GlEnum; level, xoffset, yoffset: Glint; width, height: Glsizei; format, atype: GlEnum; pixels: Pointer); stdcall;
    glTranslated : procedure(x, y, z: Gldouble); stdcall;
    glTranslatef : procedure(x, y, z: Glfloat); stdcall;
    glVertex2d : procedure(x, y: Gldouble); stdcall;
    glVertex2dv : procedure(v: PGLdouble); stdcall;
    glVertex2f : procedure(x, y: Glfloat); stdcall;
    glVertex2fv : procedure(v: PGLfloat); stdcall;
    glVertex2i : procedure(x, y: Glint); stdcall;
    glVertex2iv : procedure(v: PGLint); stdcall;
    glVertex2s : procedure(x, y: Glshort); stdcall;
    glVertex2sv : procedure(v: PGLshort); stdcall;
    glVertex3d : procedure(x, y, z: Gldouble); stdcall;
    glVertex3dv : procedure(v: PGLdouble); stdcall; 
    glVertex3f : procedure(x, y, z: Glfloat); stdcall; 
    glVertex3fv : procedure(v: PGLfloat); stdcall; 
    glVertex3i : procedure(x, y, z: Glint); stdcall; 
    glVertex3iv : procedure(v: PGLint); stdcall; 
    glVertex3s : procedure(x, y, z: Glshort); stdcall;
    glVertex3sv : procedure(v: PGLshort); stdcall;
    glVertex4d : procedure(x, y, z, w: Gldouble); stdcall;
    glVertex4dv : procedure(v: PGLdouble); stdcall;
    glVertex4f : procedure(x, y, z, w: Glfloat); stdcall;
    glVertex4fv : procedure(v: PGLfloat); stdcall;
    glVertex4i : procedure(x, y, z, w: Glint); stdcall;
    glVertex4iv : procedure(v: PGLint); stdcall;
    glVertex4s : procedure(x, y, z, w: Glshort); stdcall;
    glVertex4sv : procedure(v: PGLshort); stdcall;
    glVertexPointer : procedure(size: Glint; atype: GlEnum; stride: Glsizei; data: pointer); stdcall;
    glViewport : procedure(x, y: Glint; width, height: Glsizei); stdcall;

    // GL 1.2
    glDrawRangeElements: procedure(mode: GlEnum; Astart, Aend: Gluint; count: Glsizei; Atype: GlEnum; indices: Pointer); stdcall;
    glTexImage3D: procedure(target: GlEnum; level: Glint; internalformat: GlEnum; width, height, depth: Glsizei; border: Glint; format: GlEnum; Atype: GlEnum; pixels: Pointer); stdcall;

    // GL 1.2 ARB imaging
    glBlendColor: procedure(red, green, blue, alpha: Glclampf); stdcall;
    glBlendEquation: procedure(mode: GlEnum); stdcall;
    glColorSubTable: procedure(target: GlEnum; start, count: Glsizei; format, Atype: GlEnum; data: Pointer); stdcall;
    glCopyColorSubTable: procedure(target: GlEnum; start: Glsizei; x, y: Glint; width: Glsizei); stdcall;
    glColorTable: procedure(target, internalformat: GlEnum; width: Glsizei; format, Atype: GlEnum; table: Pointer); stdcall;
    glCopyColorTable: procedure(target, internalformat: GlEnum; x, y: Glint; width: Glsizei); stdcall;
    glColorTableParameteriv: procedure(target, pname: GlEnum; params: PGLint); stdcall;
    glColorTableParameterfv: procedure(target, pname: GlEnum; params: PGLfloat); stdcall;
    glGetColorTable: procedure(target, format, Atype: GlEnum; table: Pointer); stdcall;
    glGetColorTableParameteriv: procedure(target, pname: GlEnum; params: PGLint); stdcall;
    glGetColorTableParameterfv: procedure(target, pname: GlEnum; params: PGLfloat); stdcall;
    glConvolutionFilter1D: procedure(target, internalformat: GlEnum; width: Glsizei; format, Atype: GlEnum; image: Pointer); stdcall;
    glConvolutionFilter2D: procedure(target, internalformat: GlEnum; width, height: Glsizei; format, Atype: GlEnum; image: Pointer); stdcall;
    glCopyConvolutionFilter1D: procedure(target, internalformat: GlEnum; x, y: Glint; width: Glsizei); stdcall;
    glCopyConvolutionFilter2D: procedure(target, internalformat: GlEnum; x, y: Glint; width, height: Glsizei); stdcall;
    glGetConvolutionFilter: procedure(target, internalformat, Atype: GlEnum; image: Pointer); stdcall;
    glSeparableFilter2D: procedure(target, internalformat: GlEnum; width, height: Glsizei; format, Atype: GlEnum; row, column: Pointer); stdcall;
    glGetSeparableFilter: procedure(target, format, Atype: GlEnum; row, column, span: Pointer); stdcall;
    glConvolutionParameteri: procedure(target, pname: GlEnum; param: Glint); stdcall;
    glConvolutionParameteriv: procedure(target, pname: GlEnum; params: PGLint); stdcall;
    glConvolutionParameterf: procedure(target, pname: GlEnum; param: Glfloat); stdcall;
    glConvolutionParameterfv: procedure(target, pname: GlEnum; params: PGLfloat); stdcall;
    glGetConvolutionParameteriv: procedure(target, pname: GlEnum; params: PGLint); stdcall;
    glGetConvolutionParameterfv: procedure(target, pname: GlEnum; params: PGLfloat); stdcall;
    glHistogram: procedure(target: GlEnum; width: Glsizei; internalformat: GlEnum; sink: Glboolean); stdcall;
    glResetHistogram: procedure(target: GlEnum); stdcall;
    glGetHistogram: procedure(target: GlEnum; reset: Glboolean; format, Atype: GlEnum; values: Pointer); stdcall;
    glGetHistogramParameteriv: procedure(target, pname: GlEnum; params: PGLint); stdcall;
    glGetHistogramParameterfv: procedure(target, pname: GlEnum; params: PGLfloat); stdcall;
    glMinmax: procedure(target, internalformat: GlEnum; sink: Glboolean); stdcall;
    glResetMinmax: procedure(target: GlEnum); stdcall;
    glGetMinmax: procedure(target: GlEnum; reset: Glboolean; format, Atype: GlEnum; values: Pointer); stdcall;
    glGetMinmaxParameteriv: procedure(target, pname: GlEnum; params: PGLint); stdcall;
    glGetMinmaxParameterfv: procedure(target, pname: GlEnum; params: PGLfloat); stdcall;

//------------------------------------------------------------------------------

    // GL utility functions and procedures
    gluErrorString : function(errCode: GlEnum): PChar; stdcall;
    gluGetString : function(name: GlEnum): PChar; stdcall;
    gluOrtho2D : procedure(left, right, bottom, top: Gldouble); stdcall;
    gluPerspective : procedure(fovy, aspect, zNear, zFar: Gldouble); stdcall;
    gluPickMatrix : procedure(x, y, width, height: Gldouble; viewport: Pointer); stdcall;
    gluLookAt : procedure(eyex, eyey, eyez, centerx, centery, centerz, upx, upy, upz: Gldouble); stdcall;
    gluProject : function(objx, objy, objz: Gldouble; modelMatrix: Pointer; projMatrix: Pointer; viewport: Pointer; winx, winy, winz: PGLdouble): Glint; stdcall;
    gluUnProject : function(winx, winy, winz: Gldouble; modelMatrix: Pointer; projMatrix: Pointer; viewport: Pointer; objx, objy, objz: PGLdouble): Glint; stdcall;
    gluScaleImage : function(format: GlEnum; widthin, heightin: Glint; typein: GlEnum; datain: Pointer; widthout, heightout: Glint; typeout: GlEnum; dataout: Pointer): Glint; stdcall;
    gluBuild1DMipmaps : function(target: GlEnum; components, width: Glint; format, atype: GlEnum; data: Pointer): Glint; stdcall;
    gluBuild2DMipmaps : function(target: GlEnum; components, width, height: Glint; format, atype: GlEnum; Data: Pointer): Glint; stdcall;
    gluNewQuadric : function : PGLUquadric; stdcall;
    gluDeleteQuadric : procedure(state: PGLUquadric); stdcall;
    gluQuadricNormals : procedure(quadObject: PGLUquadric; normals: GlEnum); stdcall;
    gluQuadricTexture : procedure(quadObject: PGLUquadric; textureCoords: Glboolean); stdcall;
    gluQuadricOrientation : procedure(quadObject: PGLUquadric; orientation: GlEnum); stdcall;
    gluQuadricDrawStyle : procedure(quadObject: PGLUquadric; drawStyle: GlEnum); stdcall;
    gluCylinder : procedure(quadObject: PGLUquadric; baseRadius, topRadius, height: Gldouble; slices, stacks: Glint); stdcall;
    gluDisk : procedure(quadObject: PGLUquadric; innerRadius, outerRadius: Gldouble; slices, loops: Glint); stdcall; 
    gluPartialDisk : procedure(quadObject: PGLUquadric; innerRadius, outerRadius: Gldouble; slices, loops: Glint; startAngle, sweepAngle: Gldouble); stdcall;
    gluSphere : procedure(quadObject: PGLUquadric; radius: Gldouble; slices, stacks: Glint); stdcall;
    gluQuadricCallback : procedure(quadObject: PGLUquadric; which: GlEnum; fn: TGlUQuadricErrorProc); stdcall;
    gluNewTess : function : PGLUtesselator; stdcall; 
    gluDeleteTess : procedure(tess: PGLUtesselator); stdcall;
    gluTessBeginPolygon : procedure(tess: PGLUtesselator; polygon_data: Pointer); stdcall;
    gluTessBeginContour : procedure(tess: PGLUtesselator); stdcall;
    gluTessVertex : procedure(tess: PGLUtesselator; coords: Pointer; data: Pointer); stdcall;
    gluTessEndContour : procedure(tess: PGLUtesselator); stdcall;
    gluTessEndPolygon : procedure(tess: PGLUtesselator); stdcall;
    gluTessProperty : procedure(tess: PGLUtesselator; which: GlEnum; value: Gldouble); stdcall;
    gluTessNormal : procedure(tess: PGLUtesselator; x, y, z: Gldouble); stdcall;
    gluTessCallback : procedure(tess: PGLUtesselator; which: GlEnum; fn: Pointer); stdcall;
    gluGetTessProperty : procedure(tess: PGLUtesselator; which: GlEnum; value: PGLdouble); stdcall;
    gluNewNurbsRenderer : function : PGLUnurbs; stdcall;
    gluDeleteNurbsRenderer : procedure(nobj: PGLUnurbs); stdcall;
    gluBeginSurface : procedure(nobj: PGLUnurbs); stdcall;
    gluBeginCurve : procedure(nobj: PGLUnurbs); stdcall;
    gluEndCurve : procedure(nobj: PGLUnurbs); stdcall;
    gluEndSurface : procedure(nobj: PGLUnurbs); stdcall;
    gluBeginTrim : procedure(nobj: PGLUnurbs); stdcall;
    gluEndTrim : procedure(nobj: PGLUnurbs); stdcall;
    gluPwlCurve : procedure(nobj: PGLUnurbs; count: Glint; points: PGLfloat; stride: Glint; atype: GlEnum); stdcall;
    gluNurbsCurve : procedure(nobj: PGLUnurbs; nknots: Glint; knot: PGLfloat; stride: Glint; ctlarray: PGLfloat; order: Glint; atype: GlEnum); stdcall;
    gluNurbsSurface : procedure(nobj: PGLUnurbs; sknot_count: Glint; sknot: PGLfloat; tknot_count: Glint; tknot: PGLfloat; s_stride, t_stride: Glint; ctlarray: PGLfloat; sorder, torder: Glint; atype: GlEnum); stdcall;
    gluLoadSamplingMatrices : procedure(nobj: PGLUnurbs; modelMatrix, projMatrix: Pointer; viewport: Pointer); stdcall;
    gluNurbsProperty : procedure(nobj: PGLUnurbs; aproperty: GlEnum; value: Glfloat); stdcall;
    gluGetNurbsProperty : procedure(nobj: PGLUnurbs; aproperty: GlEnum; value: PGLfloat); stdcall;
    gluNurbsCallback : procedure(nobj: PGLUnurbs; which: GlEnum; fn: GlUNurbsErrorProc); stdcall;
    gluBeginPolygon : procedure(tess: PGLUtesselator); stdcall;
    gluNextContour : procedure(tess: PGLUtesselator; atype: GlEnum); stdcall;
    gluEndPolygon : procedure(tess: PGLUtesselator); stdcall;

//------------------------------------------------------------------------------

    // window support functions
    wglGetProcAddress : function(ProcName: PChar): Pointer; stdcall;
    // DescribePixelFormat is wrongly declared, the last parameter should be a pointer instead of a var 
    DescribePixelFormat : function(DC: HDC; p2: Integer; p3: UINT; PFD: PPixelFormatDescriptor): Integer; stdcall;
{$ifndef VER120}
    wglCopyContext : function(RC1, RC2: HGLRC; p3: Cardinal): BOOL; stdcall;
    wglCreateContext : function(DC: HDC): HGLRC; stdcall;
    wglCreateLayerContext : function(DC: HDC; p2: Integer): HGLRC; stdcall;
    wglDeleteContext : function(RC: HGLRC): BOOL; stdcall;
    wglDescribeLayerPlane : function(p1: HDC; p2, p3: Integer; p4: Cardinal; p5: PLayerPlaneDescriptor): BOOL; stdcall;
    wglGetCurrentContext : function: HGLRC; stdcall;
    wglGetCurrentDC : function: HDC; stdcall;
    wglGetLayerPaletteEntries : function(p1: HDC; p2, p3, p4: Integer; pcr: Pointer): Integer; stdcall;
    wglMakeCurrent : function(DC: HDC; p2: HGLRC): BOOL; stdcall;
    wglRealizeLayerPalette : function(p1: HDC; p2: Integer; p3: BOOL): BOOL; stdcall;
    wglSetLayerPaletteEntries : function(p1: HDC; p2, p3, p4: Integer; pcr: Pointer): Integer; stdcall;
    wglShareLists : function(p1, p2: HGLRC): BOOL; stdcall;
    wglSwapLayerBuffers : function(p1: HDC; p2: Cardinal): BOOL; stdcall;
    wglUseFontBitmapsA : function(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall;
    wglUseFontBitmapsW : function(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall;
    wglUseFontBitmaps : function(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall;
    wglUseFontOutlinesA : function(p1: HDC; p2, p3, p4: DWORD; p5, p6: Single; p7: Integer; p8: PGlyphMetricsFloat): BOOL; stdcall;
    wglUseFontOutlinesW : function(p1: HDC; p2, p3, p4: DWORD; p5, p6: Single; p7: Integer; p8: PGlyphMetricsFloat): BOOL; stdcall;
    wglUseFontOutlines : function(p1: HDC; p2, p3, p4: DWORD; p5, p6: Single; p7: Integer; p8: PGlyphMetricsFloat): BOOL; stdcall;
    ChoosePixelFormat : function(DC: HDC; PFD: PPixelFormatDescriptor): Integer; stdcall;
    GetPixelFormat : function(DC: HDC): Integer; stdcall;
    SetPixelFormat : function(DC: HDC; p2: Integer; PFD: PPixelFormatDescriptor): BOOL; stdcall;
    SwapBuffers : function(DC: HDC): BOOL; stdcall;
{$endif}

//------------------------------------------------------------------------------

    // Extension functions
    glAddSwapHintRectWIN : procedure(x, y: Glint; width, height: Glsizei); stdcall;
    glAreTexturesResidentEXT : function(n: Glsizei; textures: PGLuint; residences: PGLBoolean): Glboolean; stdcall;
    glArrayElementEXT : procedure(i: Glint); stdcall;
    glArrayElementArrayEXT : procedure(mode: GlEnum; count: Glsizei; pi: Pointer); stdcall;
    glBeginSceneEXT : procedure; stdcall;
    glBindTextureEXT : procedure(target: GlEnum; texture: Gluint); stdcall;
    glColorPointerEXT : procedure(size: Glint; atype: GlEnum; stride, count: Glsizei; data: pointer); stdcall;
    glColorTableEXT : procedure(target, internalFormat: GlEnum; width: Glsizei; format, atype: GlEnum; data: Pointer); stdcall;
    glColorSubTableExt : procedure(target: GlEnum; start, count: Glsizei; format, atype: GlEnum; data: Pointer); stdcall;
    glCopyTexImage1DEXT : procedure(target: GlEnum; level: Glint; internalFormat: GlEnum; x, y: Glint; width: Glsizei; border: Glint); stdcall;
    glCopyTexSubImage1DEXT : procedure(target: GlEnum; level, xoffset, x, y: Glint; width: Glsizei); stdcall;
    glCopyTexImage2DEXT : procedure(target: GlEnum; level: Glint; internalFormat: GlEnum; x, y: Glint; width, height: Glsizei; border: Glint); stdcall;
    glCopyTexSubImage2DEXT : procedure(target: GlEnum; level, xoffset, yoffset, x, y: Glint; width, height: Glsizei); stdcall;
    glCopyTexSubImage3DEXT : procedure(target: GlEnum; level, xoffset, yoffset, zoffset, x, y: Glint; width, height: Glsizei); stdcall;
    glCullParameterfvEXT : procedure(pname: GlEnum; params: PGLfloat); stdcall;
    glCullParameterdvEXT : procedure(pname: GlEnum; params: PGLdouble); stdcall;
    glDeleteTexturesEXT : procedure(n: Glsizei; textures: PGLuint); stdcall;
    glDrawArraysEXT : procedure(mode: GlEnum; first: Glint; count: Glsizei); stdcall;
    glEdgeFlagPointerEXT : procedure(stride, count: Glsizei; data: PGLboolean); stdcall;
    glEndSceneEXT : procedure; stdcall;
    glGenTexturesEXT : procedure(n: Glsizei; textures: PGLuint); stdcall;
    glGetColorTableEXT : procedure(target, format, atype: GlEnum; data: Pointer); stdcall;
    glGetColorTablePameterfvEXT : procedure(target, pname: GlEnum; params: Pointer); stdcall;
    glGetColorTablePameterivEXT : procedure(target, pname: GlEnum; params: Pointer); stdcall;
    glGetPointervEXT : procedure(pname: GlEnum; params: Pointer); stdcall;
    glIndexFuncEXT : procedure(func: GlEnum; ref: Glfloat); stdcall;
    glIndexMaterialEXT : procedure(face: GlEnum; mode: GlEnum); stdcall;
    glIndexPointerEXT : procedure(atype: GlEnum; stride, count: Glsizei; data: pointer); stdcall;
    glIsTextureEXT : function(texture: Gluint): Glboolean; stdcall;
    glLockArraysEXT : procedure(first: Glint; count: Glsizei); stdcall;
    glNormalPointerEXT : procedure(atype: GlEnum; stride, count: Glsizei; data: pointer); stdcall;
    glPolygonOffsetEXT: procedure(factor, bias: Glfloat); stdcall;
    glPrioritizeTexturesEXT : procedure(n: Glsizei; textures: PGLuint; priorities: PGLclampf); stdcall;
    glTexCoordPointerEXT : procedure(size: Glint; atype: GlEnum; stride, count: Glsizei; data: pointer); stdcall;
    glTexSubImage1DEXT : procedure(target: GlEnum; level, xoffset: Glint; width: Glsizei; format, Atype: GlEnum; pixels: Pointer); stdcall;
    glTexSubImage2DEXT : procedure(target: GlEnum; level, xoffset, yoffset: Glint; width, height: Glsizei; format, Atype: GlEnum; pixels: Pointer); stdcall;
    glTexSubImage3DEXT : procedure(target: GlEnum; level, xoffset, yoffset, zoffset: Glint; width, height, depth: Glsizei; format, Atype: GlEnum; pixels: Pointer); stdcall;
    glUnlockArraysEXT : procedure; stdcall;
    glVertexPointerExt : procedure(size: Glint; atype: GlEnum; stride, count: Glsizei; data: pointer); stdcall;

    gluNurbsCallbackDataEXT : procedure(nurb: PGLUnurbs; userData: Pointer); stdcall;
    gluNewNurbsTessellatorEXT : function: PGLUnurbs; stdcall;
    gluDeleteNurbsTessellatorEXT : procedure(nurb: PGLUnurbs); stdcall;

//------------------------------------------------------------------------------

procedure CloseOpenGL;
function InitOpenGL: Boolean;
function InitOpenGLFromLibrary(GL_Name, GLU_Name: String): Boolean;

procedure ClearExtensions;
procedure ActivateRenderingContext(DC: HDC; RC: HGLRC);
function  CreateRenderingContext(DC: HDC; Options: TRCOptions; ColorDepth: Integer; StencilBits: Byte): HGLRC;
procedure DeactivateRenderingContext;
procedure DestroyRenderingContext(RC: HGLRC);
procedure ReadExtensions;
procedure ReadImplementationProperties;

//------------------------------------------------------------------------------

implementation

uses SysUtils;

var FirstContext    : Boolean;
    LastPixelFormat : Integer;

var GLHandle, GLUHandle : HINST;

//------------------------------------------------------------------------------

procedure ClearProcAddresses;

begin
  glAccum:=nil;
  glAlphaFunc:=nil;
  glAreTexturesResident:=nil;
  glArrayElement:=nil;
  glBegin:=nil;
  glBindTexture:=nil;
  glBitmap:=nil;
  glBlendFunc:=nil;
  glCallList:=nil;
  glCallLists:=nil;
  glClear:=nil;
  glClearAccum:=nil;
  glClearColor:=nil;
  glClearDepth:=nil;
  glClearIndex:=nil;
  glClearStencil:=nil;
  glClipPlane:=nil;
  glColor3b:=nil;
  glColor3bv:=nil;
  glColor3d:=nil;
  glColor3dv:=nil;
  glColor3f:=nil;
  glColor3fv:=nil;
  glColor3i:=nil;
  glColor3iv:=nil;
  glColor3s:=nil;
  glColor3sv:=nil;
  glColor3ub:=nil;
  glColor3ubv:=nil;
  glColor3ui:=nil;
  glColor3uiv:=nil;
  glColor3us:=nil;
  glColor3usv:=nil;
  glColor4b:=nil;
  glColor4bv:=nil;
  glColor4d:=nil;
  glColor4dv:=nil;
  glColor4f:=nil;
  glColor4fv:=nil;
  glColor4i:=nil;
  glColor4iv:=nil;
  glColor4s:=nil;
  glColor4sv:=nil;
  glColor4ub:=nil;
  glColor4ubv:=nil;
  glColor4ui:=nil;
  glColor4uiv:=nil;
  glColor4us:=nil;
  glColor4usv:=nil;
  glColorMask:=nil;
  glColorMaterial:=nil;
  glColorPointer:=nil;
  glCopyPixels:=nil;                   
  glCopyTexImage1D:=nil;               
  glCopyTexImage2D:=nil;               
  glCopyTexSubImage1D:=nil;            
  glCopyTexSubImage2D:=nil;            
  glCullFace:=nil;                     
  glDeleteLists:=nil;                  
  glDeleteTextures:=nil;               
  glDepthFunc:=nil;                    
  glDepthMask:=nil;                    
  glDepthRange:=nil;                   
  glDisable:=nil;                      
  glDisableClientState:=nil;           
  glDrawArrays:=nil;                   
  glDrawBuffer:=nil;                   
  glDrawElements:=nil;                 
  glDrawPixels:=nil;                   
  glEdgeFlag:=nil;                     
  glEdgeFlagPointer:=nil;              
  glEdgeFlagv:=nil;                    
  glEnable:=nil;                       
  glEnableClientState:=nil;
  glEnd:=nil;                          
  glEndList:=nil;                      
  glEvalCoord1d:=nil;                  
  glEvalCoord1dv:=nil;
  glEvalCoord1f:=nil;                  
  glEvalCoord1fv:=nil;                 
  glEvalCoord2d:=nil;                  
  glEvalCoord2dv:=nil;                 
  glEvalCoord2f:=nil;                  
  glEvalCoord2fv:=nil;
  glEvalMesh1:=nil;                    
  glEvalMesh2:=nil;                    
  glEvalPoint1:=nil;                   
  glEvalPoint2:=nil;                   
  glFeedbackBuffer:=nil;               
  glFinish:=nil;                       
  glFlush:=nil;                        
  glFogf:=nil;                         
  glFogfv:=nil;                        
  glFogi:=nil;                         
  glFogiv:=nil;                        
  glFrontFace:=nil;                    
  glFrustum:=nil;                      
  glGenLists:=nil;                     
  glGenTextures:=nil;                  
  glGetBooleanv:=nil;                  
  glGetClipPlane:=nil;                 
  glGetDoublev:=nil;                   
  glGetError:=nil;                     
  glGetFloatv:=nil;                    
  glGetIntegerv:=nil;                  
  glGetLightfv:=nil;                   
  glGetLightiv:=nil;                   
  glGetMapdv:=nil;                     
  glGetMapfv:=nil;                     
  glGetMapiv:=nil;
  glGetMaterialfv:=nil;                
  glGetMaterialiv:=nil;                
  glGetPixelMapfv:=nil;                
  glGetPixelMapuiv:=nil;
  glGetPixelMapusv:=nil;               
  glGetPointerv:=nil;                  
  glGetPolygonStipple:=nil;
  glGetString:=nil;                    
  glGetTexEnvfv:=nil;                  
  glGetTexEnviv:=nil;                  
  glGetTexGendv:=nil;                  
  glGetTexGenfv:=nil;                  
  glGetTexGeniv:=nil;                  
  glGetTexImage:=nil;                  
  glGetTexLevelParameterfv:=nil;       
  glGetTexLevelParameteriv:=nil;       
  glGetTexParameterfv:=nil;            
  glGetTexParameteriv:=nil;            
  glHint:=nil;                         
  glIndexMask:=nil;                    
  glIndexPointer:=nil;                 
  glIndexd:=nil;                       
  glIndexdv:=nil;                      
  glIndexf:=nil;                       
  glIndexfv:=nil;                      
  glIndexi:=nil;                       
  glIndexiv:=nil;                      
  glIndexs:=nil;                       
  glIndexsv:=nil;                      
  glIndexub:=nil;                      
  glIndexubv:=nil;                     
  glInitNames:=nil;                    
  glInterleavedArrays:=nil;            
  glIsEnabled:=nil;                    
  glIsList:=nil;                       
  glIsTexture:=nil;
  glLightModelf:=nil;                  
  glLightModelfv:=nil;                 
  glLightModeli:=nil;                  
  glLightModeliv:=nil;
  glLightf:=nil;                       
  glLightfv:=nil;                      
  glLighti:=nil;                       
  glLightiv:=nil;                      
  glLineStipple:=nil;                  
  glLineWidth:=nil;                    
  glListBase:=nil;                     
  glLoadIdentity:=nil;                 
  glLoadMatrixd:=nil;                  
  glLoadMatrixf:=nil;                  
  glLoadName:=nil;                     
  glLogicOp:=nil;                      
  glMap1d:=nil;                        
  glMap1f:=nil;                        
  glMap2d:=nil;                        
  glMap2f:=nil;                        
  glMapGrid1d:=nil;                    
  glMapGrid1f:=nil;                    
  glMapGrid2d:=nil;                    
  glMapGrid2f:=nil;                    
  glMaterialf:=nil;                    
  glMaterialfv:=nil;                   
  glMateriali:=nil;                    
  glMaterialiv:=nil;                   
  glMatrixMode:=nil;                   
  glMultMatrixd:=nil;                  
  glMultMatrixf:=nil;                  
  glNewList:=nil;                      
  glNormal3b:=nil;                     
  glNormal3bv:=nil;                    
  glNormal3d:=nil;                     
  glNormal3dv:=nil;
  glNormal3f:=nil;
  glNormal3fv:=nil;                    
  glNormal3i:=nil;                     
  glNormal3iv:=nil;
  glNormal3s:=nil;                     
  glNormal3sv:=nil;                    
  glNormalPointer:=nil;                
  glOrtho:=nil;                        
  glPassThrough:=nil;                  
  glPixelMapfv:=nil;                   
  glPixelMapuiv:=nil;                  
  glPixelMapusv:=nil;                  
  glPixelStoref:=nil;                  
  glPixelStorei:=nil;                  
  glPixelTransferf:=nil;               
  glPixelTransferi:=nil;               
  glPixelZoom:=nil;                    
  glPointSize:=nil;                    
  glPolygonMode:=nil;                  
  glPolygonOffset:=nil;                
  glPolygonStipple:=nil;               
  glPopAttrib:=nil;                    
  glPopClientAttrib:=nil;              
  glPopMatrix:=nil;                    
  glPopName:=nil;                      
  glPrioritizeTextures:=nil;           
  glPushAttrib:=nil;                   
  glPushClientAttrib:=nil;             
  glPushMatrix:=nil;                   
  glPushName:=nil;                     
  glRasterPos2d:=nil;                  
  glRasterPos2dv:=nil;                 
  glRasterPos2f:=nil;                  
  glRasterPos2fv:=nil;
  glRasterPos2i:=nil;                  
  glRasterPos2iv:=nil;
  glRasterPos2s:=nil;                  
  glRasterPos2sv:=nil;                 
  glRasterPos3d:=nil;                  
  glRasterPos3dv:=nil;
  glRasterPos3f:=nil;                  
  glRasterPos3fv:=nil;                 
  glRasterPos3i:=nil;                  
  glRasterPos3iv:=nil;                 
  glRasterPos3s:=nil;                  
  glRasterPos3sv:=nil;                 
  glRasterPos4d:=nil;                  
  glRasterPos4dv:=nil;                 
  glRasterPos4f:=nil;                  
  glRasterPos4fv:=nil;                 
  glRasterPos4i:=nil;                  
  glRasterPos4iv:=nil;                 
  glRasterPos4s:=nil;                  
  glRasterPos4sv:=nil;                 
  glReadBuffer:=nil;                   
  glReadPixels:=nil;                   
  glRectd:=nil;                        
  glRectdv:=nil;                       
  glRectf:=nil;                        
  glRectfv:=nil;                       
  glRecti:=nil;                        
  glRectiv:=nil;                       
  glRects:=nil;                        
  glRectsv:=nil;                       
  glRenderMode:=nil;                   
  glRotated:=nil;                      
  glRotatef:=nil;
  glScaled:=nil;                       
  glScalef:=nil;                       
  glScissor:=nil;                      
  glSelectBuffer:=nil;                 
  glShadeModel:=nil;
  glStencilFunc:=nil;                  
  glStencilMask:=nil;                  
  glStencilOp:=nil;                    
  glTexCoord1d:=nil;
  glTexCoord1dv:=nil;                  
  glTexCoord1f:=nil;                   
  glTexCoord1fv:=nil;                  
  glTexCoord1i:=nil;                   
  glTexCoord1iv:=nil;                  
  glTexCoord1s:=nil;                   
  glTexCoord1sv:=nil;                  
  glTexCoord2d:=nil;                   
  glTexCoord2dv:=nil;                  
  glTexCoord2f:=nil;                   
  glTexCoord2fv:=nil;                  
  glTexCoord2i:=nil;                   
  glTexCoord2iv:=nil;                  
  glTexCoord2s:=nil;                   
  glTexCoord2sv:=nil;                  
  glTexCoord3d:=nil;                   
  glTexCoord3dv:=nil;                  
  glTexCoord3f:=nil;                   
  glTexCoord3fv:=nil;                  
  glTexCoord3i:=nil;                   
  glTexCoord3iv:=nil;                  
  glTexCoord3s:=nil;                   
  glTexCoord3sv:=nil;                  
  glTexCoord4d:=nil;
  glTexCoord4dv:=nil;                  
  glTexCoord4f:=nil;                   
  glTexCoord4fv:=nil;                  
  glTexCoord4i:=nil;                   
  glTexCoord4iv:=nil;                  
  glTexCoord4s:=nil;                   
  glTexCoord4sv:=nil;                  
  glTexCoordPointer:=nil;
  glTexEnvf:=nil;                      
  glTexEnvfv:=nil;                     
  glTexEnvi:=nil;                      
  glTexEnviv:=nil;
  glTexGend:=nil;                      
  glTexGendv:=nil;                     
  glTexGenf:=nil;                      
  glTexGenfv:=nil;                     
  glTexGeni:=nil;                      
  glTexGeniv:=nil;                     
  glTexImage1D:=nil;                   
  glTexImage2D:=nil;                   
  glTexParameterf:=nil;                
  glTexParameterfv:=nil;               
  glTexParameteri:=nil;                
  glTexParameteriv:=nil;               
  glTexSubImage1D:=nil;                
  glTexSubImage2D:=nil;                
  glTranslated:=nil;                   
  glTranslatef:=nil;
  glVertex2d:=nil;                     
  glVertex2dv:=nil;                    
  glVertex2f:=nil;                     
  glVertex2fv:=nil;                    
  glVertex2i:=nil;
  glVertex2iv:=nil;                    
  glVertex2s:=nil;                     
  glVertex2sv:=nil;                    
  glVertex3d:=nil;
  glVertex3dv:=nil;                    
  glVertex3f:=nil;                     
  glVertex3fv:=nil;                    
  glVertex3i:=nil;                     
  glVertex3iv:=nil;
  glVertex3s:=nil;
  glVertex3sv:=nil;
  glVertex4d:=nil;
  glVertex4dv:=nil;
  glVertex4f:=nil;
  glVertex4fv:=nil;
  glVertex4i:=nil;
  glVertex4iv:=nil;
  glVertex4s:=nil;
  glVertex4sv:=nil;
  glVertexPointer:=nil;
  glViewport:=nil;

  DescribePixelFormat:=nil;
  wglGetProcAddress:=nil;
{$ifndef VER120}
  wglCopyContext:=nil;
  wglCreateContext:=nil;
  wglCreateLayerContext:=nil;
  wglDeleteContext:=nil;
  wglGetCurrentContext:=nil;
  wglGetCurrentDC:=nil;
  wglMakeCurrent:=nil;
  wglShareLists:=nil;
  wglUseFontBitmapsA:=nil;
  wglUseFontBitmapsW:=nil;
  wglUseFontBitmaps:=nil;
  wglUseFontOutlinesA:=nil;
  wglUseFontOutlinesW:=nil;
  wglUseFontOutlines:=nil;
  wglDescribeLayerPlane:=nil;
  wglSetLayerPaletteEntries:=nil;
  wglGetLayerPaletteEntries:=nil;
  wglRealizeLayerPalette:=nil;
  wglSwapLayerBuffers:=nil;

  SwapBuffers:=nil;
  ChoosePixelFormat:=nil;
  GetPixelFormat:=nil;
  SetPixelFormat:=nil;
{$endif}

  // GL 1.2
  glDrawRangeElements:=nil;
  glTexImage3D:=nil;
  // GL 1.2 ARB imaging
  glBlendColor:=nil;
  glBlendEquation:=nil;
  glColorSubTable:=nil;
  glCopyColorSubTable:=nil;
  glColorTable:=nil;
  glCopyColorTable:=nil;
  glColorTableParameteriv:=nil;
  glColorTableParameterfv:=nil;
  glGetColorTable:=nil;
  glGetColorTableParameteriv:=nil;
  glGetColorTableParameterfv:=nil;
  glConvolutionFilter1D:=nil;
  glConvolutionFilter2D:=nil;
  glCopyConvolutionFilter1D:=nil;
  glCopyConvolutionFilter2D:=nil;
  glGetConvolutionFilter:=nil;
  glSeparableFilter2D:=nil;
  glGetSeparableFilter:=nil;
  glConvolutionParameteri:=nil;
  glConvolutionParameteriv:=nil;
  glConvolutionParameterf:=nil;
  glConvolutionParameterfv:=nil;
  glGetConvolutionParameteriv:=nil;
  glGetConvolutionParameterfv:=nil;
  glHistogram:=nil;
  glResetHistogram:=nil;
  glGetHistogram:=nil;
  glGetHistogramParameteriv:=nil;
  glGetHistogramParameterfv:=nil;
  glMinmax:=nil;
  glResetMinmax:=nil;
  glGetMinmax:=nil;
  glGetMinmaxParameteriv:=nil;
  glGetMinmaxParameterfv:=nil;
end;

//------------------------------------------------------------------------------

procedure LoadProcAddresses;

begin
  if GLHandle > 0 then
  begin
    glAccum:=GetProcAddress(GLHandle,'glAccum');
    glAlphaFunc:=GetProcAddress(GLHandle,'glAlphaFunc');
    glAreTexturesResident:=GetProcAddress(GLHandle,'glAreTexturesResident');
    glArrayElement:=GetProcAddress(GLHandle,'glArrayElement');
    glBegin:=GetProcAddress(GLHandle,'glBegin');
    glBindTexture:=GetProcAddress(GLHandle,'glBindTexture');
    glBitmap:=GetProcAddress(GLHandle,'glBitmap');
    glBlendFunc:=GetProcAddress(GLHandle,'glBlendFunc');
    glCallList:=GetProcAddress(GLHandle,'glCallList');
    glCallLists:=GetProcAddress(GLHandle,'glCallLists');
    glClear:=GetProcAddress(GLHandle,'glClear');
    glClearAccum:=GetProcAddress(GLHandle,'glClearAccum');
    glClearColor:=GetProcAddress(GLHandle,'glClearColor');
    glClearDepth:=GetProcAddress(GLHandle,'glClearDepth');
    glClearIndex:=GetProcAddress(GLHandle,'glClearIndex');
    glClearStencil:=GetProcAddress(GLHandle,'glClearStencil');
    glClipPlane:=GetProcAddress(GLHandle,'glClipPlane');
    glColor3b:=GetProcAddress(GLHandle,'glColor3b');
    glColor3bv:=GetProcAddress(GLHandle,'glColor3bv');
    glColor3d:=GetProcAddress(GLHandle,'glColor3d');
    glColor3dv:=GetProcAddress(GLHandle,'glColor3dv');
    glColor3f:=GetProcAddress(GLHandle,'glColor3f');
    glColor3fv:=GetProcAddress(GLHandle,'glColor3fv');
    glColor3i:=GetProcAddress(GLHandle,'glColor3i');
    glColor3iv:=GetProcAddress(GLHandle,'glColor3iv');
    glColor3s:=GetProcAddress(GLHandle,'glColor3s');
    glColor3sv:=GetProcAddress(GLHandle,'glColor3sv');
    glColor3ub:=GetProcAddress(GLHandle,'glColor3ub');
    glColor3ubv:=GetProcAddress(GLHandle,'glColor3ubv');
    glColor3ui:=GetProcAddress(GLHandle,'glColor3ui');
    glColor3uiv:=GetProcAddress(GLHandle,'glColor3uiv');
    glColor3us:=GetProcAddress(GLHandle,'glColor3us');
    glColor3usv:=GetProcAddress(GLHandle,'glColor3usv');
    glColor4b:=GetProcAddress(GLHandle,'glColor4b');
    glColor4bv:=GetProcAddress(GLHandle,'glColor4bv');
    glColor4d:=GetProcAddress(GLHandle,'glColor4d');
    glColor4dv:=GetProcAddress(GLHandle,'glColor4dv');
    glColor4f:=GetProcAddress(GLHandle,'glColor4f');
    glColor4fv:=GetProcAddress(GLHandle,'glColor4fv');
    glColor4i:=GetProcAddress(GLHandle,'glColor4i');
    glColor4iv:=GetProcAddress(GLHandle,'glColor4iv');
    glColor4s:=GetProcAddress(GLHandle,'glColor4s');
    glColor4sv:=GetProcAddress(GLHandle,'glColor4sv');
    glColor4ub:=GetProcAddress(GLHandle,'glColor4ub');
    glColor4ubv:=GetProcAddress(GLHandle,'glColor4ubv');
    glColor4ui:=GetProcAddress(GLHandle,'glColor4ui');
    glColor4uiv:=GetProcAddress(GLHandle,'glColor4uiv');
    glColor4us:=GetProcAddress(GLHandle,'glColor4us');
    glColor4usv:=GetProcAddress(GLHandle,'glColor4usv');
    glColorMask:=GetProcAddress(GLHandle,'glColorMask');
    glColorMaterial:=GetProcAddress(GLHandle,'glColorMaterial');
    glColorPointer:=GetProcAddress(GLHandle,'glColorPointer');
    glCopyPixels:=GetProcAddress(GLHandle,'glCopyPixels');
    glCopyTexImage1D:=GetProcAddress(GLHandle,'glCopyTexImage1D');
    glCopyTexImage2D:=GetProcAddress(GLHandle,'glCopyTexImage2D');
    glCopyTexSubImage1D:=GetProcAddress(GLHandle,'glCopyTexSubImage1D');
    glCopyTexSubImage2D:=GetProcAddress(GLHandle,'glCopyTexSubImage2D');
    glCullFace:=GetProcAddress(GLHandle,'glCullFace');
    glDeleteLists:=GetProcAddress(GLHandle,'glDeleteLists');
    glDeleteTextures:=GetProcAddress(GLHandle,'glDeleteTextures');
    glDepthFunc:=GetProcAddress(GLHandle,'glDepthFunc');
    glDepthMask:=GetProcAddress(GLHandle,'glDepthMask');
    glDepthRange:=GetProcAddress(GLHandle,'glDepthRange');
    glDisable:=GetProcAddress(GLHandle,'glDisable');
    glDisableClientState:=GetProcAddress(GLHandle,'glDisableClientState');
    glDrawArrays:=GetProcAddress(GLHandle,'glDrawArrays');
    glDrawBuffer:=GetProcAddress(GLHandle,'glDrawBuffer');
    glDrawElements:=GetProcAddress(GLHandle,'glDrawElements');
    glDrawPixels:=GetProcAddress(GLHandle,'glDrawPixels');
    glEdgeFlag:=GetProcAddress(GLHandle,'glEdgeFlag');
    glEdgeFlagPointer:=GetProcAddress(GLHandle,'glEdgeFlagPointer');
    glEdgeFlagv:=GetProcAddress(GLHandle,'glEdgeFlagv');
    glEnable:=GetProcAddress(GLHandle,'glEnable');
    glEnableClientState:=GetProcAddress(GLHandle,'glEnableClientState');
    glEnd:=GetProcAddress(GLHandle,'glEnd');
    glEndList:=GetProcAddress(GLHandle,'glEndList');
    glEvalCoord1d:=GetProcAddress(GLHandle,'glEvalCoord1d');
    glEvalCoord1dv:=GetProcAddress(GLHandle,'glEvalCoord1dv');
    glEvalCoord1f:=GetProcAddress(GLHandle,'glEvalCoord1f');
    glEvalCoord1fv:=GetProcAddress(GLHandle,'glEvalCoord1fv');
    glEvalCoord2d:=GetProcAddress(GLHandle,'glEvalCoord2d');
    glEvalCoord2dv:=GetProcAddress(GLHandle,'glEvalCoord2dv');
    glEvalCoord2f:=GetProcAddress(GLHandle,'glEvalCoord2f');
    glEvalCoord2fv:=GetProcAddress(GLHandle,'glEvalCoord2fv');
    glEvalMesh1:=GetProcAddress(GLHandle,'glEvalMesh1');
    glEvalMesh2:=GetProcAddress(GLHandle,'glEvalMesh2');
    glEvalPoint1:=GetProcAddress(GLHandle,'glEvalPoint1');
    glEvalPoint2:=GetProcAddress(GLHandle,'glEvalPoint2');
    glFeedbackBuffer:=GetProcAddress(GLHandle,'glFeedbackBuffer');
    glFinish:=GetProcAddress(GLHandle,'glFinish');
    glFlush:=GetProcAddress(GLHandle,'glFlush');
    glFogf:=GetProcAddress(GLHandle,'glFogf');
    glFogfv:=GetProcAddress(GLHandle,'glFogfv');
    glFogi:=GetProcAddress(GLHandle,'glFogi');
    glFogiv:=GetProcAddress(GLHandle,'glFogiv');
    glFrontFace:=GetProcAddress(GLHandle,'glFrontFace');
    glFrustum:=GetProcAddress(GLHandle,'glFrustum');
    glGenLists:=GetProcAddress(GLHandle,'glGenLists');
    glGenTextures:=GetProcAddress(GLHandle,'glGenTextures');
    glGetBooleanv:=GetProcAddress(GLHandle,'glGetBooleanv');
    glGetClipPlane:=GetProcAddress(GLHandle,'glGetClipPlane');
    glGetDoublev:=GetProcAddress(GLHandle,'glGetDoublev');
    glGetError:=GetProcAddress(GLHandle,'glGetError');
    glGetFloatv:=GetProcAddress(GLHandle,'glGetFloatv');
    glGetIntegerv:=GetProcAddress(GLHandle,'glGetIntegerv');
    glGetLightfv:=GetProcAddress(GLHandle,'glGetLightfv');
    glGetLightiv:=GetProcAddress(GLHandle,'glGetLightiv');
    glGetMapdv:=GetProcAddress(GLHandle,'glGetMapdv');
    glGetMapfv:=GetProcAddress(GLHandle,'glGetMapfv');
    glGetMapiv:=GetProcAddress(GLHandle,'glGetMapiv');
    glGetMaterialfv:=GetProcAddress(GLHandle,'glGetMaterialfv');
    glGetMaterialiv:=GetProcAddress(GLHandle,'glGetMaterialiv');
    glGetPixelMapfv:=GetProcAddress(GLHandle,'glGetPixelMapfv');
    glGetPixelMapuiv:=GetProcAddress(GLHandle,'glGetPixelMapuiv');
    glGetPixelMapusv:=GetProcAddress(GLHandle,'glGetPixelMapusv');
    glGetPointerv:=GetProcAddress(GLHandle,'glGetPointerv');
    glGetPolygonStipple:=GetProcAddress(GLHandle,'glGetPolygonStipple');
    glGetString:=GetProcAddress(GLHandle,'glGetString');
    glGetTexEnvfv:=GetProcAddress(GLHandle,'glGetTexEnvfv');
    glGetTexEnviv:=GetProcAddress(GLHandle,'glGetTexEnviv');
    glGetTexGendv:=GetProcAddress(GLHandle,'glGetTexGendv');
    glGetTexGenfv:=GetProcAddress(GLHandle,'glGetTexGenfv');
    glGetTexGeniv:=GetProcAddress(GLHandle,'glGetTexGeniv');
    glGetTexImage:=GetProcAddress(GLHandle,'glGetTexImage');
    glGetTexLevelParameterfv:=GetProcAddress(GLHandle,'glGetTexLevelParameterfv');
    glGetTexLevelParameteriv:=GetProcAddress(GLHandle,'glGetTexLevelParameteriv');
    glGetTexParameterfv:=GetProcAddress(GLHandle,'glGetTexParameterfv');
    glGetTexParameteriv:=GetProcAddress(GLHandle,'glGetTexParameteriv');
    glHint:=GetProcAddress(GLHandle,'glHint');
    glIndexMask:=GetProcAddress(GLHandle,'glIndexMask');
    glIndexPointer:=GetProcAddress(GLHandle,'glIndexPointer');
    glIndexd:=GetProcAddress(GLHandle,'glIndexd');
    glIndexdv:=GetProcAddress(GLHandle,'glIndexdv');
    glIndexf:=GetProcAddress(GLHandle,'glIndexf');
    glIndexfv:=GetProcAddress(GLHandle,'glIndexfv');
    glIndexi:=GetProcAddress(GLHandle,'glIndexi');
    glIndexiv:=GetProcAddress(GLHandle,'glIndexiv');
    glIndexs:=GetProcAddress(GLHandle,'glIndexs');
    glIndexsv:=GetProcAddress(GLHandle,'glIndexsv');
    glIndexub:=GetProcAddress(GLHandle,'glIndexub');
    glIndexubv:=GetProcAddress(GLHandle,'glIndexubv');
    glInitNames:=GetProcAddress(GLHandle,'glInitNames');
    glInterleavedArrays:=GetProcAddress(GLHandle,'glInterleavedArrays');
    glIsEnabled:=GetProcAddress(GLHandle,'glIsEnabled');
    glIsList:=GetProcAddress(GLHandle,'glIsList');
    glIsTexture:=GetProcAddress(GLHandle,'glIsTexture');
    glLightModelf:=GetProcAddress(GLHandle,'glLightModelf');
    glLightModelfv:=GetProcAddress(GLHandle,'glLightModelfv');
    glLightModeli:=GetProcAddress(GLHandle,'glLightModeli');
    glLightModeliv:=GetProcAddress(GLHandle,'glLightModeliv');
    glLightf:=GetProcAddress(GLHandle,'glLightf');
    glLightfv:=GetProcAddress(GLHandle,'glLightfv');
    glLighti:=GetProcAddress(GLHandle,'glLighti');
    glLightiv:=GetProcAddress(GLHandle,'glLightiv');
    glLineStipple:=GetProcAddress(GLHandle,'glLineStipple');
    glLineWidth:=GetProcAddress(GLHandle,'glLineWidth');
    glListBase:=GetProcAddress(GLHandle,'glListBase');
    glLoadIdentity:=GetProcAddress(GLHandle,'glLoadIdentity');
    glLoadMatrixd:=GetProcAddress(GLHandle,'glLoadMatrixd');
    glLoadMatrixf:=GetProcAddress(GLHandle,'glLoadMatrixf');
    glLoadName:=GetProcAddress(GLHandle,'glLoadName');
    glLogicOp:=GetProcAddress(GLHandle,'glLogicOp');
    glMap1d:=GetProcAddress(GLHandle,'glMap1d');
    glMap1f:=GetProcAddress(GLHandle,'glMap1f');
    glMap2d:=GetProcAddress(GLHandle,'glMap2d');
    glMap2f:=GetProcAddress(GLHandle,'glMap2f');
    glMapGrid1d:=GetProcAddress(GLHandle,'glMapGrid1d');
    glMapGrid1f:=GetProcAddress(GLHandle,'glMapGrid1f');
    glMapGrid2d:=GetProcAddress(GLHandle,'glMapGrid2d');
    glMapGrid2f:=GetProcAddress(GLHandle,'glMapGrid2f');
    glMaterialf:=GetProcAddress(GLHandle,'glMaterialf');
    glMaterialfv:=GetProcAddress(GLHandle,'glMaterialfv');
    glMateriali:=GetProcAddress(GLHandle,'glMateriali');
    glMaterialiv:=GetProcAddress(GLHandle,'glMaterialiv');
    glMatrixMode:=GetProcAddress(GLHandle,'glMatrixMode');
    glMultMatrixd:=GetProcAddress(GLHandle,'glMultMatrixd');
    glMultMatrixf:=GetProcAddress(GLHandle,'glMultMatrixf');
    glNewList:=GetProcAddress(GLHandle,'glNewList');
    glNormal3b:=GetProcAddress(GLHandle,'glNormal3b');
    glNormal3bv:=GetProcAddress(GLHandle,'glNormal3bv');
    glNormal3d:=GetProcAddress(GLHandle,'glNormal3d');
    glNormal3dv:=GetProcAddress(GLHandle,'glNormal3dv');
    glNormal3f:=GetProcAddress(GLHandle,'glNormal3f');
    glNormal3fv:=GetProcAddress(GLHandle,'glNormal3fv');
    glNormal3i:=GetProcAddress(GLHandle,'glNormal3i');
    glNormal3iv:=GetProcAddress(GLHandle,'glNormal3iv');
    glNormal3s:=GetProcAddress(GLHandle,'glNormal3s');
    glNormal3sv:=GetProcAddress(GLHandle,'glNormal3sv');
    glNormalPointer:=GetProcAddress(GLHandle,'glNormalPointer');
    glOrtho:=GetProcAddress(GLHandle,'glOrtho');
    glPassThrough:=GetProcAddress(GLHandle,'glPassThrough');
    glPixelMapfv:=GetProcAddress(GLHandle,'glPixelMapfv');
    glPixelMapuiv:=GetProcAddress(GLHandle,'glPixelMapuiv');
    glPixelMapusv:=GetProcAddress(GLHandle,'glPixelMapusv');
    glPixelStoref:=GetProcAddress(GLHandle,'glPixelStoref');
    glPixelStorei:=GetProcAddress(GLHandle,'glPixelStorei');
    glPixelTransferf:=GetProcAddress(GLHandle,'glPixelTransferf');
    glPixelTransferi:=GetProcAddress(GLHandle,'glPixelTransferi');
    glPixelZoom:=GetProcAddress(GLHandle,'glPixelZoom');
    glPointSize:=GetProcAddress(GLHandle,'glPointSize');
    glPolygonMode:=GetProcAddress(GLHandle,'glPolygonMode');
    glPolygonOffset:=GetProcAddress(GLHandle,'glPolygonOffset');
    glPolygonStipple:=GetProcAddress(GLHandle,'glPolygonStipple');
    glPopAttrib:=GetProcAddress(GLHandle,'glPopAttrib');
    glPopClientAttrib:=GetProcAddress(GLHandle,'glPopClientAttrib');
    glPopMatrix:=GetProcAddress(GLHandle,'glPopMatrix');
    glPopName:=GetProcAddress(GLHandle,'glPopName');
    glPrioritizeTextures:=GetProcAddress(GLHandle,'glPrioritizeTextures');
    glPushAttrib:=GetProcAddress(GLHandle,'glPushAttrib');
    glPushClientAttrib:=GetProcAddress(GLHandle,'glPushClientAttrib');
    glPushMatrix:=GetProcAddress(GLHandle,'glPushMatrix');
    glPushName:=GetProcAddress(GLHandle,'glPushName');
    glRasterPos2d:=GetProcAddress(GLHandle,'glRasterPos2d');
    glRasterPos2dv:=GetProcAddress(GLHandle,'glRasterPos2dv');
    glRasterPos2f:=GetProcAddress(GLHandle,'glRasterPos2f');
    glRasterPos2fv:=GetProcAddress(GLHandle,'glRasterPos2fv');
    glRasterPos2i:=GetProcAddress(GLHandle,'glRasterPos2i');
    glRasterPos2iv:=GetProcAddress(GLHandle,'glRasterPos2iv');
    glRasterPos2s:=GetProcAddress(GLHandle,'glRasterPos2s');
    glRasterPos2sv:=GetProcAddress(GLHandle,'glRasterPos2sv');
    glRasterPos3d:=GetProcAddress(GLHandle,'glRasterPos3d');
    glRasterPos3dv:=GetProcAddress(GLHandle,'glRasterPos3dv');
    glRasterPos3f:=GetProcAddress(GLHandle,'glRasterPos3f');
    glRasterPos3fv:=GetProcAddress(GLHandle,'glRasterPos3fv');
    glRasterPos3i:=GetProcAddress(GLHandle,'glRasterPos3i');
    glRasterPos3iv:=GetProcAddress(GLHandle,'glRasterPos3iv');
    glRasterPos3s:=GetProcAddress(GLHandle,'glRasterPos3s');
    glRasterPos3sv:=GetProcAddress(GLHandle,'glRasterPos3sv');
    glRasterPos4d:=GetProcAddress(GLHandle,'glRasterPos4d');
    glRasterPos4dv:=GetProcAddress(GLHandle,'glRasterPos4dv');
    glRasterPos4f:=GetProcAddress(GLHandle,'glRasterPos4f');
    glRasterPos4fv:=GetProcAddress(GLHandle,'glRasterPos4fv');
    glRasterPos4i:=GetProcAddress(GLHandle,'glRasterPos4i');
    glRasterPos4iv:=GetProcAddress(GLHandle,'glRasterPos4iv');
    glRasterPos4s:=GetProcAddress(GLHandle,'glRasterPos4s');
    glRasterPos4sv:=GetProcAddress(GLHandle,'glRasterPos4sv');
    glReadBuffer:=GetProcAddress(GLHandle,'glReadBuffer');
    glReadPixels:=GetProcAddress(GLHandle,'glReadPixels');
    glRectd:=GetProcAddress(GLHandle,'glRectd');
    glRectdv:=GetProcAddress(GLHandle,'glRectdv');
    glRectf:=GetProcAddress(GLHandle,'glRectf');
    glRectfv:=GetProcAddress(GLHandle,'glRectfv');
    glRecti:=GetProcAddress(GLHandle,'glRecti');
    glRectiv:=GetProcAddress(GLHandle,'glRectiv');
    glRects:=GetProcAddress(GLHandle,'glRects');
    glRectsv:=GetProcAddress(GLHandle,'glRectsv');
    glRenderMode:=GetProcAddress(GLHandle,'glRenderMode');
    glRotated:=GetProcAddress(GLHandle,'glRotated');
    glRotatef:=GetProcAddress(GLHandle,'glRotatef');
    glScaled:=GetProcAddress(GLHandle,'glScaled');
    glScalef:=GetProcAddress(GLHandle,'glScalef');
    glScissor:=GetProcAddress(GLHandle,'glScissor');
    glSelectBuffer:=GetProcAddress(GLHandle,'glSelectBuffer');
    glShadeModel:=GetProcAddress(GLHandle,'glShadeModel');
    glStencilFunc:=GetProcAddress(GLHandle,'glStencilFunc');
    glStencilMask:=GetProcAddress(GLHandle,'glStencilMask');
    glStencilOp:=GetProcAddress(GLHandle,'glStencilOp');
    glTexCoord1d:=GetProcAddress(GLHandle,'glTexCoord1d');
    glTexCoord1dv:=GetProcAddress(GLHandle,'glTexCoord1dv');
    glTexCoord1f:=GetProcAddress(GLHandle,'glTexCoord1f');
    glTexCoord1fv:=GetProcAddress(GLHandle,'glTexCoord1fv');
    glTexCoord1i:=GetProcAddress(GLHandle,'glTexCoord1i');
    glTexCoord1iv:=GetProcAddress(GLHandle,'glTexCoord1iv');
    glTexCoord1s:=GetProcAddress(GLHandle,'glTexCoord1s');
    glTexCoord1sv:=GetProcAddress(GLHandle,'glTexCoord1sv');
    glTexCoord2d:=GetProcAddress(GLHandle,'glTexCoord2d');
    glTexCoord2dv:=GetProcAddress(GLHandle,'glTexCoord2dv');
    glTexCoord2f:=GetProcAddress(GLHandle,'glTexCoord2f');
    glTexCoord2fv:=GetProcAddress(GLHandle,'glTexCoord2fv');
    glTexCoord2i:=GetProcAddress(GLHandle,'glTexCoord2i');
    glTexCoord2iv:=GetProcAddress(GLHandle,'glTexCoord2iv');
    glTexCoord2s:=GetProcAddress(GLHandle,'glTexCoord2s');
    glTexCoord2sv:=GetProcAddress(GLHandle,'glTexCoord2sv');
    glTexCoord3d:=GetProcAddress(GLHandle,'glTexCoord3d');
    glTexCoord3dv:=GetProcAddress(GLHandle,'glTexCoord3dv');
    glTexCoord3f:=GetProcAddress(GLHandle,'glTexCoord3f');
    glTexCoord3fv:=GetProcAddress(GLHandle,'glTexCoord3fv');
    glTexCoord3i:=GetProcAddress(GLHandle,'glTexCoord3i');
    glTexCoord3iv:=GetProcAddress(GLHandle,'glTexCoord3iv');
    glTexCoord3s:=GetProcAddress(GLHandle,'glTexCoord3s');
    glTexCoord3sv:=GetProcAddress(GLHandle,'glTexCoord3sv');
    glTexCoord4d:=GetProcAddress(GLHandle,'glTexCoord4d');
    glTexCoord4dv:=GetProcAddress(GLHandle,'glTexCoord4dv');
    glTexCoord4f:=GetProcAddress(GLHandle,'glTexCoord4f');
    glTexCoord4fv:=GetProcAddress(GLHandle,'glTexCoord4fv');
    glTexCoord4i:=GetProcAddress(GLHandle,'glTexCoord4i');
    glTexCoord4iv:=GetProcAddress(GLHandle,'glTexCoord4iv');
    glTexCoord4s:=GetProcAddress(GLHandle,'glTexCoord4s');
    glTexCoord4sv:=GetProcAddress(GLHandle,'glTexCoord4sv');
    glTexCoordPointer:=GetProcAddress(GLHandle,'glTexCoordPointer');
    glTexEnvf:=GetProcAddress(GLHandle,'glTexEnvf');
    glTexEnvfv:=GetProcAddress(GLHandle,'glTexEnvfv');
    glTexEnvi:=GetProcAddress(GLHandle,'glTexEnvi');
    glTexEnviv:=GetProcAddress(GLHandle,'glTexEnviv');
    glTexGend:=GetProcAddress(GLHandle,'glTexGend');
    glTexGendv:=GetProcAddress(GLHandle,'glTexGendv');
    glTexGenf:=GetProcAddress(GLHandle,'glTexGenf');
    glTexGenfv:=GetProcAddress(GLHandle,'glTexGenfv');
    glTexGeni:=GetProcAddress(GLHandle,'glTexGeni');
    glTexGeniv:=GetProcAddress(GLHandle,'glTexGeniv');
    glTexImage1D:=GetProcAddress(GLHandle,'glTexImage1D');
    glTexImage2D:=GetProcAddress(GLHandle,'glTexImage2D');
    glTexParameterf:=GetProcAddress(GLHandle,'glTexParameterf');
    glTexParameterfv:=GetProcAddress(GLHandle,'glTexParameterfv');
    glTexParameteri:=GetProcAddress(GLHandle,'glTexParameteri');
    glTexParameteriv:=GetProcAddress(GLHandle,'glTexParameteriv');
    glTexSubImage1D:=GetProcAddress(GLHandle,'glTexSubImage1D');
    glTexSubImage2D:=GetProcAddress(GLHandle,'glTexSubImage2D');
    glTranslated:=GetProcAddress(GLHandle,'glTranslated');
    glTranslatef:=GetProcAddress(GLHandle,'glTranslatef');
    glVertex2d:=GetProcAddress(GLHandle,'glVertex2d');
    glVertex2dv:=GetProcAddress(GLHandle,'glVertex2dv');
    glVertex2f:=GetProcAddress(GLHandle,'glVertex2f');
    glVertex2fv:=GetProcAddress(GLHandle,'glVertex2fv');
    glVertex2i:=GetProcAddress(GLHandle,'glVertex2i');
    glVertex2iv:=GetProcAddress(GLHandle,'glVertex2iv');
    glVertex2s:=GetProcAddress(GLHandle,'glVertex2s');
    glVertex2sv:=GetProcAddress(GLHandle,'glVertex2sv');
    glVertex3d:=GetProcAddress(GLHandle,'glVertex3d');
    glVertex3dv:=GetProcAddress(GLHandle,'glVertex3dv');
    glVertex3f:=GetProcAddress(GLHandle,'glVertex3f');
    glVertex3fv:=GetProcAddress(GLHandle,'glVertex3fv');
    glVertex3i:=GetProcAddress(GLHandle,'glVertex3i');
    glVertex3iv:=GetProcAddress(GLHandle,'glVertex3iv');
    glVertex3s:=GetProcAddress(GLHandle,'glVertex3s');
    glVertex3sv:=GetProcAddress(GLHandle,'glVertex3sv');
    glVertex4d:=GetProcAddress(GLHandle,'glVertex4d');
    glVertex4dv:=GetProcAddress(GLHandle,'glVertex4dv');
    glVertex4f:=GetProcAddress(GLHandle,'glVertex4f');
    glVertex4fv:=GetProcAddress(GLHandle,'glVertex4fv');
    glVertex4i:=GetProcAddress(GLHandle,'glVertex4i');
    glVertex4iv:=GetProcAddress(GLHandle,'glVertex4iv');
    glVertex4s:=GetProcAddress(GLHandle,'glVertex4s');
    glVertex4sv:=GetProcAddress(GLHandle,'glVertex4sv');
    glVertexPointer:=GetProcAddress(GLHandle,'glVertexPointer');
    glViewport:=GetProcAddress(GLHandle,'glViewport');
    wglGetProcAddress:=GetprocAddress(GLHandle,'wglGetProcAddress');
    {$ifndef VER120}
      wglCopyContext:=GetProcAddress(GLHandle,'wglCopyContext');
      wglCreateContext:=GetProcAddress(GLHandle,'wglCreateContext');
      wglCreateLayerContext:=GetProcAddress(GLHandle,'wglCreateLayerContext');
      wglDeleteContext:=GetProcAddress(GLHandle,'wglDeleteContext');
      wglGetCurrentContext:=GetProcAddress(GLHandle,'wglGetCurrentContext');
      wglGetCurrentDC:=GetProcAddress(GLHandle,'wglGetCurrentDC');
      wglMakeCurrent:=GetProcAddress(GLHandle,'wglMakeCurrent');
      wglShareLists:=GetProcAddress(GLHandle,'wglShareLists');
      wglUseFontBitmapsA:=GetProcAddress(GLHandle,'wglUseFontBitmapsA');
      wglUseFontBitmapsW:=GetProcAddress(GLHandle,'wglUseFontBitmapsW');
      wglUseFontBitmaps:=GetProcAddress(GLHandle,'wglUseFontBitmapsA');
      wglUseFontOutlinesA:=GetProcAddress(GLHandle,'wglUseFontOutlinesA');
      wglUseFontOutlinesW:=GetProcAddress(GLHandle,'wglUseFontOutlinesW');
      wglUseFontOutlines:=GetProcAddress(GLHandle,'wglUseFontOutlinesA');
      wglDescribeLayerPlane:=GetProcAddress(GLHandle,'wglDescribeLayerPlane');
      wglSetLayerPaletteEntries:=GetProcAddress(GLHandle,'wglSetLayerPaletteEntries');
      wglGetLayerPaletteEntries:=GetProcAddress(GLHandle,'wglGetLayerPaletteEntries');
      wglRealizeLayerPalette:=GetProcAddress(GLHandle,'wglRealizeLayerPalette');
      wglSwapLayerBuffers:=GetProcAddress(GLHandle,'wglSwapLayerBuffers');
    {$endif}
    // GL 1.2
    glDrawRangeElements:=GetProcAddress(GLHandle,'glDrawRangeElements');
    glTexImage3D:=GetProcAddress(GLHandle,'glTexImage3D');
    // GL 1.2 ARB imaging
    glBlendColor:=GetProcAddress(GLHandle,'glBlendColor');
    glBlendEquation:=GetProcAddress(GLHandle,'glBlendEquation');
    glColorSubTable:=GetProcAddress(GLHandle,'glColorSubTable');
    glCopyColorSubTable:=GetProcAddress(GLHandle,'glCopyColorSubTable');
    glColorTable:=GetProcAddress(GLHandle,'glCopyColorSubTable');
    glCopyColorTable:=GetProcAddress(GLHandle,'glCopyColorTable');
    glColorTableParameteriv:=GetProcAddress(GLHandle,'glColorTableParameteriv');
    glColorTableParameterfv:=GetProcAddress(GLHandle,'glColorTableParameterfv');
    glGetColorTable:=GetProcAddress(GLHandle,'glGetColorTable');
    glGetColorTableParameteriv:=GetProcAddress(GLHandle,'glGetColorTableParameteriv');
    glGetColorTableParameterfv:=GetProcAddress(GLHandle,'glGetColorTableParameterfv');
    glConvolutionFilter1D:=GetProcAddress(GLHandle,'glConvolutionFilter1D');
    glConvolutionFilter2D:=GetProcAddress(GLHandle,'glConvolutionFilter2D');
    glCopyConvolutionFilter1D:=GetProcAddress(GLHandle,'glCopyConvolutionFilter1D');
    glCopyConvolutionFilter2D:=GetProcAddress(GLHandle,'glCopyConvolutionFilter2D');
    glGetConvolutionFilter:=GetProcAddress(GLHandle,'glGetConvolutionFilter');
    glSeparableFilter2D:=GetProcAddress(GLHandle,'glSeparableFilter2D');
    glGetSeparableFilter:=GetProcAddress(GLHandle,'glGetSeparableFilter');
    glConvolutionParameteri:=GetProcAddress(GLHandle,'glConvolutionParameteri');
    glConvolutionParameteriv:=GetProcAddress(GLHandle,'glConvolutionParameteriv');
    glConvolutionParameterf:=GetProcAddress(GLHandle,'glConvolutionParameterf');
    glConvolutionParameterfv:=GetProcAddress(GLHandle,'glConvolutionParameterfv');
    glGetConvolutionParameteriv:=GetProcAddress(GLHandle,'glGetConvolutionParameteriv');
    glGetConvolutionParameterfv:=GetProcAddress(GLHandle,'glGetConvolutionParameterfv');
    glHistogram:=GetProcAddress(GLHandle,'glHistogram');
    glResetHistogram:=GetProcAddress(GLHandle,'glResetHistogram');
    glGetHistogram:=GetProcAddress(GLHandle,'glGetHistogram');
    glGetHistogramParameteriv:=GetProcAddress(GLHandle,'glGetHistogramParameteriv');
    glGetHistogramParameterfv:=GetProcAddress(GLHandle,'glGetHistogramParameterfv');
    glMinmax:=GetProcAddress(GLHandle,'glMinmax');
    glResetMinmax:=GetProcAddress(GLHandle,'glResetMinmax');
    glGetMinmax:=GetProcAddress(GLHandle,'glGetMinmax');
    glGetMinmaxParameteriv:=GetProcAddress(GLHandle,'glGetMinmaxParameteriv');
    glGetMinmaxParameterfv:=GetProcAddress(GLHandle,'glGetMinmaxParameterfv');
  end;

  if GLUHandle > 0 then
  begin
    gluBeginCurve:=GetProcAddress(GLUHandle,'gluBeginCurve');
    gluBeginPolygon:=GetProcAddress(GLUHandle,'gluBeginPolygon');
    gluBeginSurface:=GetProcAddress(GLUHandle,'gluBeginSurface');
    gluBeginTrim:=GetProcAddress(GLUHandle,'gluBeginTrim');
    gluBuild1DMipmaps:=GetProcAddress(GLUHandle,'gluBuild1DMipmaps');
    gluBuild2DMipmaps:=GetProcAddress(GLUHandle,'gluBuild2DMipmaps');
    gluCylinder:=GetProcAddress(GLUHandle,'gluCylinder');
    gluDeleteNurbsRenderer:=GetProcAddress(GLUHandle,'gluDeleteNurbsRenderer');
    gluDeleteQuadric:=GetProcAddress(GLUHandle,'gluDeleteQuadric');
    gluDeleteTess:=GetProcAddress(GLUHandle,'gluDeleteTess');
    gluDisk:=GetProcAddress(GLUHandle,'gluDisk');
    gluEndCurve:=GetProcAddress(GLUHandle,'gluEndCurve');
    gluEndPolygon:=GetProcAddress(GLUHandle,'gluEndPolygon');
    gluEndSurface:=GetProcAddress(GLUHandle,'gluEndSurface');
    gluEndTrim:=GetProcAddress(GLUHandle,'gluEndTrim');
    gluErrorString:=GetProcAddress(GLUHandle,'gluErrorString');
    gluGetNurbsProperty:=GetProcAddress(GLUHandle,'gluGetNurbsProperty');
    gluGetString:=GetProcAddress(GLUHandle,'gluGetString');
    gluGetTessProperty:=GetProcAddress(GLUHandle,'gluGetTessProperty');
    gluLoadSamplingMatrices:=GetProcAddress(GLUHandle,'gluLoadSamplingMatrices');
    gluLookAt:=GetProcAddress(GLUHandle,'gluLookAt');
    gluNewNurbsRenderer:=GetProcAddress(GLUHandle,'gluNewNurbsRenderer');
    gluNewQuadric:=GetProcAddress(GLUHandle,'gluNewQuadric');
    gluNewTess:=GetProcAddress(GLUHandle,'gluNewTess');
    gluNextContour:=GetProcAddress(GLUHandle,'gluNextContour');
    gluNurbsCallback:=GetProcAddress(GLUHandle,'gluNurbsCallback');
    gluNurbsCurve:=GetProcAddress(GLUHandle,'gluNurbsCurve');
    gluNurbsProperty:=GetProcAddress(GLUHandle,'gluNurbsProperty');
    gluNurbsSurface:=GetProcAddress(GLUHandle,'gluNurbsSurface');
    gluOrtho2D:=GetProcAddress(GLUHandle,'gluOrtho2D');
    gluPartialDisk:=GetProcAddress(GLUHandle,'gluPartialDisk');
    gluPerspective:=GetProcAddress(GLUHandle,'gluPerspective');
    gluPickMatrix:=GetProcAddress(GLUHandle,'gluPickMatrix');
    gluProject:=GetProcAddress(GLUHandle,'gluProject');
    gluPwlCurve:=GetProcAddress(GLUHandle,'gluPwlCurve');
    gluQuadricCallback:=GetProcAddress(GLUHandle,'gluQuadricCallback');
    gluQuadricDrawStyle:=GetProcAddress(GLUHandle,'gluQuadricDrawStyle');
    gluQuadricNormals:=GetProcAddress(GLUHandle,'gluQuadricNormals');
    gluQuadricOrientation:=GetProcAddress(GLUHandle,'gluQuadricOrientation');
    gluQuadricTexture:=GetProcAddress(GLUHandle,'gluQuadricTexture');
    gluScaleImage:=GetProcAddress(GLUHandle,'gluScaleImage');
    gluSphere:=GetProcAddress(GLUHandle,'gluSphere');
    gluTessBeginContour:=GetProcAddress(GLUHandle,'gluTessBeginContour');
    gluTessBeginPolygon:=GetProcAddress(GLUHandle,'gluTessBeginPolygon');
    gluTessCallback:=GetProcAddress(GLUHandle,'gluTessCallback');
    gluTessEndContour:=GetProcAddress(GLUHandle,'gluTessEndContour');
    gluTessEndPolygon:=GetProcAddress(GLUHandle,'gluTessEndPolygon');
    gluTessNormal:=GetProcAddress(GLUHandle,'gluTessNormal');
    gluTessProperty:=GetProcAddress(GLUHandle,'gluTessProperty');
    gluTessVertex:=GetProcAddress(GLUHandle,'gluTessVertex');
    gluUnProject:=GetProcAddress(GLUHandle,'gluUnProject');
  end;

  DescribePixelFormat:=GetProcAddress(GLHandle,'wglDescribePixelFormat');
  {$ifndef VER120}
    SwapBuffers:=GetProcAddress(GLHandle,'wglSwapBuffers');
    ChoosePixelFormat:=GetProcAddress(GLHandle,'wglChoosePixelFormat');
    GetPixelFormat:=GetProcAddress(GLHandle,'wglGetPixelFormat');
    SetPixelFormat:=GetProcAddress(GLHandle,'SetPixelFormat');
    if not assigned(SetPixelFormat) then SetPixelFormat:=Windows.SetPixelFormat;
  {$endif}
end;

//------------------------------------------------------------------------------

procedure ClearExtensions;

begin
  glArrayElementEXT:=nil;
  glDrawArraysEXT:=nil;
  glVertexPointerEXT:=nil;
  glNormalPointerEXT:=nil;
  glColorPointerEXT:=nil;
  glIndexPointerEXT:=nil;
  glTexCoordPointerEXT:=nil;
  glEdgeFlagPointerEXT:=nil;
  glGetPointervEXT:=nil;
  glArrayElementArrayEXT:=nil;
  glAddSwapHintRectWIN:=nil;
  glColorTableEXT:=nil;
  glColorSubTableEXT:=nil;
  glGetColorTableEXT:=nil;
  glGetColorTablePameterivEXT:=nil;
  glGetColorTablePameterfvEXT:=nil;
  gluNurbsCallbackDataEXT:=nil;
  gluNewNurbsTessellatorEXT:=nil;
  gluDeleteNurbsTessellatorEXT:=nil;
  glLockArraysEXT:=nil;
  glUnlockArraysEXT:=nil;
  glCopyTexImage1DEXT:=nil;
  glCopyTexSubImage1DEXT:=nil;
  glCopyTexImage2DEXT:=nil;
  glCopyTexSubImage2DEXT:=nil;
  glCopyTexSubImage3DEXT:=nil;
  glCullParameterfvEXT:=nil;
  glCullParameterdvEXT:=nil;
  glIndexFuncEXT:=nil;
  glIndexMaterialEXT:=nil;
  glPolygonOffsetEXT:=nil;
  glTexSubImage1DEXT:=nil;
  glTexSubImage2DEXT:=nil;
  glTexSubImage3DEXT:=nil;
  glGenTexturesEXT:=nil;
  glDeleteTexturesEXT:=nil;
  glBindTextureEXT:=nil;
  glPrioritizeTexturesEXT:=nil;
  glAreTexturesResidentEXT:=nil;
  glIsTextureEXT:=nil;

  LastPixelFormat:=0; // to get synchronized again, if this proc was called from outside
end;

//------------------------------------------------------------------------------

procedure ReadExtensions;

// to be used in an active rendering context only!

begin
  // GL extensions
  glArrayElementEXT:=wglGetProcAddress('glArrayElementEXT');
  glDrawArraysEXT:=wglGetProcAddress('glDrawArraysEXT');
  glVertexPointerEXT:=wglGetProcAddress('glVertexPointerEXT');
  glNormalPointerEXT:=wglGetProcAddress('glNormalPointerEXT');
  glColorPointerEXT:=wglGetProcAddress('glColorPointerEXT');
  glIndexPointerEXT:=wglGetProcAddress('glIndexPointerEXT');
  glTexCoordPointerEXT:=wglGetProcAddress('glTexCoordPointerEXT');
  glEdgeFlagPointerEXT:=wglGetProcAddress('glEdgeFlagPointerEXT');
  glGetPointervEXT:=wglGetProcAddress('glGetPointervEXT');
  glArrayElementArrayEXT:=wglGetProcAddress('glArrayElementArrayEXT');
  glAddSwapHintRectWIN:=wglGetProcAddress('glAddSwapHintRectWIN');
  glColorTableEXT:=wglGetProcAddress('glColorTableEXT');
  glColorSubTableEXT:=wglGetProcAddress('glColorSubTableEXT');
  glGetColorTableEXT:=wglGetProcAddress('glGetColorTableEXT');
  glGetColorTablePameterivEXT:=wglGetProcAddress('glGetColorTablePameterivEXT');
  glGetColorTablePameterfvEXT:=wglGetProcAddress('glGetColorTablePameterfvEXT');
  glLockArraysEXT:=wglGetProcAddress('glLockArraysEXT');
  glUnlockArraysEXT:=wglGetProcAddress('glUnlockArraysEXT');
  glCopyTexImage1DEXT:=wglGetProcAddress('glCopyTexImage1DEXT');
  glCopyTexSubImage1DEXT:=wglGetProcAddress('glCopyTexSubImage1DEXT');
  glCopyTexImage2DEXT:=wglGetProcAddress('glCopyTexImage2DEXT');
  glCopyTexSubImage2DEXT:=wglGetProcAddress('glCopyTexSubImage2DEXT');
  glCopyTexSubImage3DEXT:=wglGetProcAddress('glCopyTexSubImage3DEXT');
  glCullParameterfvEXT:=wglGetProcAddress('glCullParameterfvEXT');
  glCullParameterdvEXT:=wglGetProcAddress('glCullParameterdvEXT');
  glIndexFuncEXT:=wglGetProcAddress('glIndexFuncEXT');
  glIndexMaterialEXT:=wglGetProcAddress('glIndexMaterialEXT');
  glPolygonOffsetEXT:=wglGetProcAddress('glPolygonOffsetEXT');
  glTexSubImage1DEXT:=wglGetProcAddress('glTexSubImage1DEXT');
  glTexSubImage2DEXT:=wglGetProcAddress('glTexSubImage2DEXT');
  glTexSubImage3DEXT:=wglGetProcAddress('glTexSubImage3DEXT');
  glGenTexturesEXT:=wglGetProcAddress('glGenTexturesEXT');
  glDeleteTexturesEXT:=wglGetProcAddress('glDeleteTexturesEXT');
  glBindTextureEXT:=wglGetProcAddress('glBindTextureEXT');
  glPrioritizeTexturesEXT:=wglGetProcAddress('glPrioritizeTexturesEXT');
  glAreTexturesResidentEXT:=wglGetProcAddress('glAreTexturesResidentEXT');
  glIsTextureEXT:=wglGetProcAddress('glIsTextureEXT');

  // GLU extensions
  gluNurbsCallbackDataEXT:=wglGetProcAddress('gluNurbsCallbackDataEXT');
  gluNewNurbsTessellatorEXT:=wglGetProcAddress('gluNewNurbsTessellatorEXT');
  gluDeleteNurbsTessellatorEXT:=wglGetProcAddress('gluDeleteNurbsTessellatorEXT');

  LastPixelFormat:=0; // to get synchronized again, if this proc was called from outside
end;

//------------------------------------------------------------------------------

procedure TrimAndSplitVersionString(Buffer: String; var Max,Min: Integer);

// peel out the X.Y form

var Separator : Integer;

begin
  try
    // there must be at least one dot to separate major and minor version number
    Separator:=Pos('.',Buffer);
    // at least one number must be before and one after the dot
    if (Separator > 1) and (Separator < Length(Buffer)-1) and
       (Buffer[Separator-1] in ['0'..'9']) and (Buffer[Separator+1] in ['0'..'9']) then
    begin
      // ok, it's a valid version string
      // now remove unnecessary parts
      Dec(Separator);
      // find last non-numeric character before version number
      while (Separator > 0) and (Buffer[Separator] in ['0'..'9']) do Dec(Separator);
      // delete leading characters not belonging to the version string
      Delete(Buffer,1,Separator);
      Separator:=Pos('.',Buffer)+1;
      // find first non-numeric character after version number
      while (Separator <= Length(Buffer)) and (Buffer[Separator] in ['0'..'9']) do Inc(Separator);
      // delete trailing characters not belonging to the version string
      Delete(Buffer,Separator,255);
      // now translate the numbers
      Separator:=Pos('.',Buffer); // necessary, because the buffer length may be changed
      Max:=StrToInt(Copy(Buffer,1,Separator-1));
      Min:=StrToInt(Copy(Buffer,Separator+1,255));
    end
    else Abort;
  except
    Min:=0;
    Max:=0;
  end;
end;

//------------------------------------------------------------------------------

procedure ReadImplementationProperties;

var Buffer        : String;
    MajorVersion,
    MinorVersion  : Integer;

begin
  // determine version of implementation
  // GL
  Buffer:=StrPas(PChar(glGetString(GL_VERSION)));
  TrimAndSplitVersionString(Buffer,Majorversion,MinorVersion);
  GL_VERSION_1_0:=True;
  GL_VERSION_1_1:=False;
  GL_VERSION_1_2:=False;
  if MajorVersion > 0 then
  begin
    if MinorVersion > 0 then
    begin
      GL_VERSION_1_1:=True;
      if MinorVersion > 1 then GL_VERSION_1_2:=True;
    end;
  end;
  // GLU
  GLU_VERSION_1_1:=False;
  GLU_VERSION_1_2:=False;
  // gluGetString is valid for version 1.1 or later
  if assigned(gluGetString) then
  begin
    Buffer:=StrPas(PChar(gluGetString(GLU_VERSION)));
    TrimAndSplitVersionString(Buffer,Majorversion,MinorVersion);
    GLU_VERSION_1_1:=True;
    if MinorVersion > 1 then GLU_VERSION_1_2:=True;
  end;

  // check supported extensions
  // -- first GL
  Buffer:=StrPas(PChar(glGetString(GL_EXTENSIONS)));
  GL_EXT_abgr:=Pos('GL_EXT_abgr',Buffer) > 0;
  GL_EXT_bgra:=Pos('GL_EXT_bgra',Buffer) > 0;
  GL_EXT_packed_pixels:=Pos('GL_EXT_packed_pixels',Buffer) > 0;
  GL_EXT_paletted_texture:=Pos('GL_EXT_paletted_texture',Buffer) > 0;
  GL_EXT_vertex_array:=Pos('GL_EXT_vertex_array',Buffer) > 0;
  GL_SGI_compiled_vertex_array:=Pos('GL_SGI_compiled_vertex_array',Buffer) > 0;
  GL_SGI_cull_vertex:=Pos('GL_SGI_cull_vertex',Buffer) > 0;
  GL_SGI_index_array_formats:=Pos('GL_SGI_index_array_formats',Buffer) > 0;
  GL_SGI_index_func:=Pos('GL_SGI_index_func',Buffer) > 0;
  GL_SGI_index_material:=Pos('GL_SGI_index_material',Buffer) > 0;
  GL_SGI_index_texture:=Pos('GL_SGI_index_texture',Buffer) > 0;
  GL_WIN_swap_hint:=Pos('GL_WIN_swap_hint',Buffer) > 0;
  GL_EXT_blend_color:=Pos('GL_EXT_blend_color',Buffer) > 0;
  GL_EXT_blend_logic_op:=Pos('GL_EXT_blend_logic_op',Buffer) > 0;
  GL_EXT_blend_minmax:=Pos('GL_EXT_blend_minmax',Buffer) > 0;
  GL_EXT_blend_subtract:=Pos('GL_EXT_blend_subtract',Buffer) > 0;
  GL_EXT_convolution:=Pos('GL_EXT_convolution',Buffer) > 0;
  GL_EXT_copy_texture:=Pos('GL_EXT_copy_texture',Buffer) > 0;
  GL_EXT_histogram:=Pos('GL_EXT_histogram',Buffer) > 0;
  GL_EXT_polygon_offset:=Pos('GL_EXT_polygon_offset',Buffer) > 0;
  GL_EXT_subtexture:=Pos('GL_EXT_subtexture',Buffer) > 0;
  GL_EXT_texture:=Pos('GL_EXT_texture',Buffer) > 0;
  GL_EXT_texture_object:=Pos('GL_EXT_texture_object',Buffer) > 0;
  GL_EXT_texture3D:=Pos('GL_EXT_texture3D',Buffer) > 0;
  GL_EXT_cmyka:=Pos('GL_EXT_cmyka',Buffer) > 0;
  GL_EXT_rescale_normal:=Pos('GL_EXT_rescale_normal',Buffer) > 0;
  GL_SGI_color_matrix:=Pos('GL_SGI_color_matrix',Buffer) > 0;
  GL_SGI_texture_color_table:=Pos('GL_SGI_texture_color_table',Buffer) > 0;
  GL_SGI_color_table:=Pos('GL_SGI_color_table',Buffer) > 0;
  GL_EXT_clip_volume_hint:=Pos('GL_EXT_clip_volume_hint',Buffer) > 0;
  GL_EXT_misc_attribute:=Pos('GL_EXT_misc_attribute',Buffer) > 0;
  GL_EXT_scene_marker:=Pos('GL_EXT_scene_marker',Buffer) > 0;
  GL_EXT_shared_texture_palette:=Pos('GL_EXT_shared_texture_palette',Buffer) > 0;

  // -- second GLU
  Buffer:=StrPas(PChar(gluGetString(GLU_EXTENSIONS)));
  GLU_EXT_TEXTURE:=Pos('GLU_EXT_TEXTURE',Buffer) > 0;
  GLU_EXT_object_space_tess:=Pos('GLU_EXT_object_space_tess',Buffer) > 0;
  GLU_EXT_nurbs_tessellator:=Pos('GLU_EXT_nurbs_tessellator',Buffer) > 0;
end;

//------------------------------------------------------------------------------

procedure SetupPalette(DC: HDC; PFD: TPixelFormatDescriptor);

var nColors, I   : Integer;
    lpPalette    : PLogPalette;
    byRedMask,
    byGreenMask,
    byBlueMask   : Byte;
    Palette      : HPalette;

begin
  nColors:=1 shl Pfd.cColorBits;
  GetMem(lpPalette,SizeOf(TLogPalette)+(nColors*SizeOf(TPaletteEntry)));
  try
    lpPalette^.palVersion:=$300;
    lpPalette^.palNumEntries:=nColors;
    byRedMask  :=(1 shl Pfd.cRedBits)-1;
    byGreenMask:=(1 shl Pfd.cGreenBits)-1;
    byBlueMask :=(1 shl Pfd.cBlueBits)-1;
    {$ifopt R+} {$define RangeCheck} {$endif} {$R-}
    for I:=0 to nColors-1 do
    begin
      lpPalette^.palPalEntry[I].peRed  :=(((I shr Pfd.cRedShift)   and byRedMask)  *255) DIV byRedMask;
      lpPalette^.palPalEntry[I].peGreen:=(((I shr Pfd.cGreenShift) and byGreenMask)*255) DIV byGreenMask;
      lpPalette^.palPalEntry[I].peBlue :=(((I shr Pfd.cBlueShift)  and byBlueMask) *255) DIV byBlueMask;
      lpPalette^.palPalEntry[I].peFlags:=0;
    end;
    {$ifdef RangeCheck} {$R+} {$undef RangeCheck} {$endif}
    Palette:=CreatePalette(lpPalette^);
    if (Palette <> 0) then
    begin
      SelectPalette(DC,Palette,False);
      RealizePalette(DC);
    end;
  finally
    FreeMem(lpPalette);
  end;
end;

//------------------------------------------------------------------------------

function CreateRenderingContext(DC: HDC; Options: TRCOptions; ColorDepth: Integer; StencilBits: Byte): HGLRC;

// Set the OpenGL properties required to draw to the given canvas and
// create a rendering context for it.

var PFDescriptor : TPixelFormatDescriptor;
    PixelFormat  : Integer;

begin
  FillChar(PFDescriptor,SizeOf(PFDescriptor),0);
  with PFDescriptor do
  begin
    nSize:=SizeOf(PFDescriptor);
    nVersion:=1;
    dwFlags:=PFD_SUPPORT_OPENGL;
    if GetObjectType(DC) in [OBJ_MEMDC,OBJ_METADC,OBJ_ENHMETADC]
      then dwFlags:=dwFlags or PFD_DRAW_TO_BITMAP
      else dwFlags:=dwFlags or PFD_DRAW_TO_WINDOW;
    if opDoubleBuffered in Options then dwFlags:=dwFlags or PFD_DOUBLEBUFFER;
    if opGDI in Options then dwFlags:=dwFlags or PFD_SUPPORT_GDI;
    if opStereo in Options then dwFlags:=dwFlags or PFD_STEREO;
    iPixelType:=PFD_TYPE_RGBA;
    cColorBits:=ColorDepth;
    cDepthBits:=32;
    cStencilBits:=StencilBits;
    iLayerType:=Byte(PFD_MAIN_PLANE);
  end;

  InitOpenGL; // just in case it didn't happen already
  PixelFormat:=ChoosePixelFormat(DC,@PFDescriptor);
  SetPixelFormat(DC,PixelFormat,@PFDescriptor);
  // check the properties just set
  DescribePixelFormat(DC,PixelFormat,SizeOf(PFDescriptor),@PFDescriptor);
  with PFDescriptor do
    if (dwFlags and PFD_NEED_PALETTE) <> 0 then SetupPalette(DC,PFDescriptor);

  Result:=wglCreateContext(DC);

  // read implementation properties
  if FirstContext and (Result > 0) then
  begin
    wglMakeCurrent(DC,Result);
    ReadImplementationProperties;
    FirstContext:=False;
    wglMakeCurrent(0,0);
  end;
end;

//------------------------------------------------------------------------------

procedure ActivateRenderingContext(DC: HDC; RC: HGLRC);

// read extension addresses

var PixelFormat : Integer;

begin
  // The extension function addresses are unique for each pixel format. All rendering
  // contexts of a given pixel format share the same extension function addresses.
  PixelFormat:=GetPixelFormat(DC);
  wglMakeCurrent(DC,RC);
  if PixelFormat <> LastPixelFormat then
  begin
    ClearExtensions;
    ReadExtensions;
    LastPixelFormat:=PixelFormat;
  end;
end;

//------------------------------------------------------------------------------

procedure DeactivateRenderingContext;

begin
  wglMakeCurrent(0,0);
end;

//------------------------------------------------------------------------------

procedure DestroyRenderingContext(RC: HGLRC);

begin
  if RC > 0 then wglDeleteContext(RC);
end;

//------------------------------------------------------------------------------

procedure CloseOpenGL;

begin
  if GLHandle > 0 then begin FreeLibrary(GLHandle); GLHandle:=0; end;
  if GLUHandle > 0 then begin FreeLibrary(GLUHandle); GLUHandle:=0; end;
  ClearProcAddresses;
  ClearExtensions;
  FirstContext:=True;
end;

//------------------------------------------------------------------------------

function InitOpenGL: Boolean;

begin
  if (GLHandle = 0) or (GLUHandle = 0) then
  begin
    Result:=InitOpenGLFromLibrary('OpenGL32.DLL','GLU32.DLL');
  end
  else Result:=True;
end;

//------------------------------------------------------------------------------

function InitOpenGLFromLibrary(GL_Name, GLU_Name: String): Boolean;

var OldError: Longint;

begin
  Result:=False;
  CloseOpenGL;
  OldError:=SetErrorMode(SEM_NOOPENFILEERRORBOX);
  try
    GLHandle:=LoadLibrary(PChar(GL_Name));
    GLUHandle:=LoadLibrary(PChar(GLU_Name));
    if (GLHandle > 0) and (GLUHandle > 0) then
    begin
      LoadProcAddresses;
      Result:=True;
    end
    else
    begin
      if GLHandle > 0 then FreeLibrary(GLHandle);
      if GLUHandle > 0 then FreeLibrary(GLUHandle);
    end;
  finally
    SetErrorMode(OldError);
  end;
end;

//------------------------------------------------------------------------------

initialization
  Set8087CW($133F);
  FirstContext:=True;
finalization
  CloseOpenGL;
  Set8087CW($1333);
end.
