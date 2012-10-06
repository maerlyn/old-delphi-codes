#include <math.h>		// Header File For Windows
#include <windows.h>		// Header File For Windows
#include <stdio.h>			// Header File For Standard Input/Output
#include <stdarg.h>		// Header File For Variable Argument Routines	( ADD )
#include <gl\gl.h>			// Header File For The OpenGL32 Library
#include <gl\glu.h>			// Header File For The GLu32 Library
#include <gl\glaux.h>		// Header File For The Glaux Library
#include <time.h>

HDC		hDC=NULL;			// Private GDI Device Context
HGLRC	hRC=NULL;			// Permanent Rendering Context
HWND	hWnd=NULL;			// Holds Our Window Handle

int DIFF = 1;
float length;

bool	keys[256];			// Array Used For The Keyboard Routine
bool	active=TRUE;		// Window Active Flag Set To TRUE By Default
bool	fullscreen=TRUE;	// Fullscreen Flag Set To Fullscreen Mode By Default
bool	light;				// Lighting ON/OFF
bool    wireframe = FALSE;  // Wireframe ON/OFF
bool    water = TRUE;  // Wireframe ON/OFF
bool    multitexture = TRUE;  // Wireframe ON/OFF
bool	lp;					// L Pressed?
bool	fp;					// F Pressed?
bool    sp;                 // Spacebar Pressed? ( NEW )
bool    wp;                 // W pressed?
bool    mp;                 // M pressed?

bool    aq;
bool    sq;

GLUquadricObj *quadratic;	// Storage For Our Quadratic Objects ( NEW )
GLuint	texture[5];

GLfloat ABS(GLfloat A)
{
  if (A < 0)
  A = -A;
  return A;
}

GLfloat LightAmbient[]=		{ 0.35f, 0.75f, 0.55f, 1.0f };
GLfloat LightDiffuse[]=		{ 1.0f, 1.0f, 1.0f, 1.0f };
GLfloat LightSpecular[]=	{ 1.0f, 1.0f, 1.0f, 1.0f };
GLfloat LightPosition[]=	{ 0.0f, 0.0f, 0.0f, 1.0f };

GLuint	fogMode[]= { GL_EXP, GL_EXP2, GL_LINEAR };	// Storage For Three Types Of Fog
GLuint	fogfilter = 0;								// Which Fog Mode To Use 
GLfloat	fogColor[4] = {0.9019f, 0.8588f, 0.7882f, 1};		// Fog Color

GLuint	base;				// Base Display List For The Font Set
GLuint	filter;				// Which Filter To Use
GLuint  object=0;			// Which Object To Draw (NEW)

LRESULT	CALLBACK WndProc(HWND, UINT, WPARAM, LPARAM);	// Declaration For WndProc

// Create A Structure For The Timer Information
struct
{
  __int64       frequency;          // Timer Frequency
  float            resolution;          // Timer Resolution
  unsigned long mm_timer_start;     
  
  // Multimedia Timer Start Value
  unsigned long mm_timer_elapsed;      // Multimedia Timer Elapsed Time
  bool   performance_timer;    
  
  // Using The Performance Timer?
  __int64       performance_timer_start;      // Performance Timer Start Value
  __int64       performance_timer_elapsed; // Performance Timer Elapsed Time
} timer;

// Initialize Our Timer
void TimerInit(void)
{
     memset(&timer, 0, sizeof(timer));   
 // Clear Our Timer Structure
     // Check To See If A Performance Counter Is Available
     // If One Is Available The Timer Frequency Will Be Updated
     if (!QueryPerformanceFrequency((LARGE_INTEGER *) &timer.frequency))
     {
          // No Performace Counter Available
          timer.performance_timer = FALSE;                      // Set Performance Timer To FALSE
          timer.mm_timer_start = timeGetTime();                 // Use timeGetTime()
          timer.resolution  = 1.0f/1000.0f;                           // Set Our Timer Resolution To .001f
          timer.frequency   = 1000;                                     // Set Our Timer Frequency To 1000
          timer.mm_timer_elapsed = timer.mm_timer_start; // Set The Elapsed Time
     }
     else
     {
          // Performance Counter Is Available, Use It Instead Of The Multimedia Timer
          // Get The Current Time And Store It In performance_timer_start
          QueryPerformanceCounter((LARGE_INTEGER *) &timer.performance_timer_start);
          timer.performance_timer   = TRUE;    // Set Performance Timer To TRUE
          // Calculate The Timer Resolution Using The Timer Frequency
          timer.resolution    = (float) (((double)1.0f)/((double)timer.frequency));
          // Set The Elapsed Time To The Current Time
          timer.performance_timer_elapsed = timer.performance_timer_start;
     }
}

// Get Time In Milliseconds
float TimerGetTime()
{
     __int64 time;                                  // 'time' Will Hold A 64 Bit Integer
     if (timer.performance_timer)           // Are We Using The Performance Timer?
     {
          QueryPerformanceCounter((LARGE_INTEGER *) &time); // Current Performance Time
          // Return The Time Elapsed since TimerInit was called
          return ( (float) ( time - timer.performance_timer_start) * timer.resolution)*1000.0f;
     }
     else
     {
          // Return The Time Elapsed since TimerInit was called
          return ( (float) ( timeGetTime() - timer.mm_timer_start) * timer.resolution)*1000.0f;
     }
}


GLvoid BuildFont(GLvoid)								// Build Our Bitmap Font
{
	HFONT	font;										// Windows Font ID
base = glGenLists(96);
	

	font = CreateFont(	-16,							// Height Of Font
						0,								// Width Of Font
						0,								// Angle Of Escapement
						0,								// Orientation Angle
						FW_BOLD,						// Font Weight
						FALSE,							// Italic
						FALSE,							// Underline
						FALSE,							// Strikeout
						ANSI_CHARSET,					// Character Set Identifier
						OUT_TT_PRECIS,					// Output Precision
						CLIP_DEFAULT_PRECIS,			// Clipping Precision
						ANTIALIASED_QUALITY,			// Output Quality
						FF_DONTCARE|DEFAULT_PITCH,		// Family And Pitch
						"Verdana");					// Font Name

	SelectObject(hDC, font);							// Selects The Font We Want

	wglUseFontBitmaps(hDC, 32, 96, base);				// Builds 96 Characters Starting At Character 32
}

GLvoid KillFont(GLvoid)									// Delete The Font
{
	glDeleteLists(base, 96);							// Delete All 96 Characters
}

GLvoid glPrint(const char *fmt, ...)					// Custom GL "Print" Routine
{
	char		text[256];								// Holds Our String
	va_list		ap;										// Pointer To List Of Arguments

	if (fmt == NULL)									// If There's No Text
		return;											// Do Nothing

	va_start(ap, fmt);									// Parses The String For Variables
	    vsprintf(text, fmt, ap);						// And Converts Symbols To Actual Numbers
	va_end(ap);											// Results Are Stored In Text

	glPushAttrib(GL_LIST_BIT);							// Pushes The Display List Bits
	glListBase(base - 32);								// Sets The Base Character to 32
	glCallLists(strlen(text), GL_UNSIGNED_BYTE, text);	// Draws The Display List Text
	glPopAttrib();										// Pops The Display List Bits
}


AUX_RGBImageRec *LoadBMP(char *Filename)				// Loads A Bitmap Image
{
	FILE *File=NULL;									// File Handle

	if (!Filename)										// Make Sure A Filename Was Given
	{
		return NULL;									// If Not Return NULL
	}

	File=fopen(Filename,"r");							// Check To See If The File Exists

	if (File)											// Does The File Exist?
	{
		fclose(File);									// Close The Handle
		return auxDIBImageLoad(Filename);				// Load The Bitmap And Return A Pointer
	}

	return NULL;										// If Load Failed Return NULL
}


int LoadGLTextures()									// Load Bitmap And Convert To A Texture
{
        int Status=FALSE;								// Status Indicator
        AUX_RGBImageRec *TextureImage[1];				// Create Storage Space For The Textures
        memset(TextureImage,0,sizeof(void *)*1);		// Set The Pointer To NULL

        if (TextureImage[0]=LoadBMP("texture/asphalt.bmp"))	// Load Particle Texture
        {
			Status=TRUE;								// Set The Status To TRUE
			glGenTextures(1, &texture[0]);				// Create One Texture

					// Create MipMapped Texture
    		glBindTexture(GL_TEXTURE_2D, texture[0]);
	    	glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
	    	glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_NEAREST);
	    	gluBuild2DMipmaps(GL_TEXTURE_2D, 3, TextureImage[0]->sizeX, TextureImage[0]->sizeY, GL_RGB, GL_UNSIGNED_BYTE, TextureImage[0]->data);           
        }

        if (TextureImage[0]=LoadBMP("texture/sky.bmp"))	// Load Particle Texture
        {
			Status=TRUE;								// Set The Status To TRUE
			glGenTextures(1, &texture[1]);				// Create One Texture

					// Create MipMapped Texture
    		glBindTexture(GL_TEXTURE_2D, texture[1]);
	    	glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
	    	glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_NEAREST);
	    	gluBuild2DMipmaps(GL_TEXTURE_2D, 3, TextureImage[0]->sizeX, TextureImage[0]->sizeY, GL_RGB, GL_UNSIGNED_BYTE, TextureImage[0]->data);           
        }

        if (TextureImage[0]=LoadBMP("texture/stone.bmp"))	// Load Particle Texture
        {
			Status=TRUE;								// Set The Status To TRUE
			glGenTextures(1, &texture[2]);				// Create One Texture

					// Create MipMapped Texture
    		glBindTexture(GL_TEXTURE_2D, texture[2]);
	    	glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
	    	glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_NEAREST);
	    	gluBuild2DMipmaps(GL_TEXTURE_2D, 3, TextureImage[0]->sizeX, TextureImage[0]->sizeY, GL_RGB, GL_UNSIGNED_BYTE, TextureImage[0]->data);           
        }

        if (TextureImage[0]=LoadBMP("texture/Loading.bmp"))	// Load Particle Texture
        {
			Status=TRUE;								// Set The Status To TRUE
			glGenTextures(1, &texture[3]);				// Create One Texture

					// Create MipMapped Texture
    		glBindTexture(GL_TEXTURE_2D, texture[3]);
	    	glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
	    	glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_NEAREST);
	    	gluBuild2DMipmaps(GL_TEXTURE_2D, 3, TextureImage[0]->sizeX, TextureImage[0]->sizeY, GL_RGB, GL_UNSIGNED_BYTE, TextureImage[0]->data);           
        }

        if (TextureImage[0]=LoadBMP("texture/Water.bmp"))	// Load Particle Texture
        {
			Status=TRUE;								// Set The Status To TRUE
			glGenTextures(1, &texture[4]);				// Create One Texture

					// Create MipMapped Texture
    		glBindTexture(GL_TEXTURE_2D, texture[4]);
	    	glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
	    	glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_NEAREST);
	    	gluBuild2DMipmaps(GL_TEXTURE_2D, 3, TextureImage[0]->sizeX, TextureImage[0]->sizeY, GL_RGB, GL_UNSIGNED_BYTE, TextureImage[0]->data);           
        }


        if (TextureImage[0])							// If Texture Exists
		{
			if (TextureImage[0]->data)					// If Texture Image Exists
			{
				free(TextureImage[0]->data);			// Free The Texture Image Memory
			}
			free(TextureImage[0]);						// Free The Image Structure
		}

        return Status;									// Return The Status
}


GLvoid ReSizeGLScene(GLsizei width, GLsizei height)		// Resize And Initialize The GL Window
{
	if (height==0)										// Prevent A Divide By Zero By
	{
		height=1;										// Making Height Equal One
	}

	glViewport(0,0,width,height);						// Reset The Current Viewport

	glMatrixMode(GL_PROJECTION);						// Select The Projection Matrix
	glLoadIdentity();									// Reset The Projection Matrix

	// Calculate The Aspect Ratio Of The Window
	gluPerspective(45.0f,(GLfloat)width/(GLfloat)height,0.1f,50000.0f);

	glMatrixMode(GL_MODELVIEW);							// Select The Modelview Matrix
	glLoadIdentity();									// Reset The Modelview Matrix
}


const int MAX = 1000; // LENGTH AND WIDTH OF MATRIX(2D array)
const int skyMAX = 50; // LENGTH AND WIDTH OF MATRIX(2D array)

struct vertex
{
	float x, y, z;
};
vertex v1,v2,t1,t2,t3;
vertex n1[MAX][MAX];
vertex n2[MAX][MAX];
vertex n3[MAX][MAX];
vertex n4[MAX][MAX];
vertex field[MAX][MAX];
vertex sky[skyMAX][skyMAX];
float alpha[MAX][MAX];

GLfloat xtrans = MAX/2;
GLfloat xptrans = 0;
GLfloat ytrans = 0;
GLfloat yptrans = 0;
GLfloat ztrans = MAX/2;
GLfloat zptrans = 0;

float Progress = 0;

void DrawProgress()
{
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); 
  glLoadIdentity();
  glTranslatef(0,0,0);

  Progress += 0.25f;   
  
  glColor4f(1,1,1,1);
  
  glBindTexture(GL_TEXTURE_2D, texture[3]);
  glBegin(GL_TRIANGLE_STRIP);
  glLoadIdentity();
  glTexCoord2f(1,1); glVertex3f(1,0-4,-15);
  glTexCoord2f(1,0); glVertex3f(1,-.5-4,-15);
  glTexCoord2f(0,1); glVertex3f(-1,0-4,-15);
  glTexCoord2f(0,0); glVertex3f(-1,-.5-4,-15);
  glEnd();

  glColor4f(0,0,1,1);
  glBegin(GL_QUADS);
    glVertex3f(-1.8f,-1.55f,-5.0f);
    glVertex3f(-1.8f,-1.7f,-5.0f);
    glVertex3f(-1.8f+Progress,-1.7f,-5.0f);
    glVertex3f(-1.8f+Progress,-1.55f,-5.0f);
  glEnd(); 
}


int InitGL(GLvoid)										// All Setup For OpenGL Goes Here
{
	if (!LoadGLTextures())								// Jump To Texture Loading Routine
	{
		return FALSE;									// If Texture Didn't Load Return FALSE
	}

    TimerInit(); //initialize timer

	glClearColor(0.9019f, 0.8588f, 0.7882f, 1);				// Black Background
	glClearDepth(1.0f);		   							// Depth Buffer Setup
  
	glEnable(GL_TEXTURE_2D);							// Enable Texture Mapping
    glShadeModel(GL_SMOOTH);							// Enable Smooth Shading
	glEnable(GL_DEPTH_TEST);
    glDepthFunc(GL_LEQUAL);								// The Type Of Depth Testing To Do
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);

	glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);	// Really Nice Perspective Calculations
        
/*
	glEnable(GL_COLOR_MATERIAL);
    glColorMaterial(GL_FRONT_AND_BACK,GL_SPECULAR);

    float m_ambient[4] =  {1,1,1,1};
    float m_diffuse[4] =  {1,1,1,1};
    float m_specular[4] = {1,1,1,1};
    float m_shininess = 10.f;    
	glMaterialfv( GL_FRONT_AND_BACK, GL_AMBIENT, m_ambient );
	glMaterialfv( GL_FRONT_AND_BACK, GL_DIFFUSE, m_diffuse );
	glMaterialfv( GL_FRONT_AND_BACK, GL_SPECULAR, m_specular );
	glMaterialf ( GL_FRONT_AND_BACK, GL_SHININESS, m_shininess );
    glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER, GL_FALSE);
    glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE);
    glLightfv(GL_LIGHT1, GL_AMBIENT, LightAmbient);		// Setup The Ambient Light
	glLightfv(GL_LIGHT1, GL_DIFFUSE, LightDiffuse);		// Setup The Diffuse Light
	glLightfv(GL_LIGHT1, GL_SPECULAR,LightSpecular);	// Setup The Diffuse Light
	glLightfv(GL_LIGHT1, GL_POSITION,LightPosition);	// Position The Light
    //glLightf (GL_LIGHT1, GL_SPOT_CUTOFF, 15.f);       // SPOTLIGHT
    //glLightf (GL_LIGHT1, GL_SPOT_EXPONENT, 128.f);       // SPOTLIGHT

	glEnable(GL_LIGHT1);								// Enable Light One
	glEnable(GL_LIGHTING);
*/

	glFogi(GL_FOG_MODE, fogMode[2]);			        // Fog Mode
	glFogfv(GL_FOG_COLOR, fogColor);					// Set Fog Color
	glFogf(GL_FOG_DENSITY, 0.594f);						// How Dense Will The Fog Be
	glHint(GL_FOG_HINT, GL_NICEST);					    // Fog Hint Value
	glFogf(GL_FOG_START, 10.0f);						// Fog Start Depth
	glFogf(GL_FOG_END, 60.0f);							// Fog End Depth
	glEnable(GL_FOG);									// Enables GL_FOG

	quadratic=gluNewQuadric();							// Create A Pointer To The Quadric Object (Return 0 If No Memory) (NEW)
	gluQuadricNormals(quadratic, GLU_SMOOTH);			// Create Smooth Normals (NEW)
	gluQuadricTexture(quadratic, GL_TRUE);				// Create Texture Coords (NEW)

    BuildFont();   
    wglUseFontBitmaps(hDC, 32, 96, base);				// Builds 96 Characters Sta

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    SwapBuffers(hDC);

    int i,i2;     
    
    SwapBuffers(hDC);
    DrawProgress();
    float hskyMAX = hypot(skyMAX,skyMAX);   
    for (i = 0; i < skyMAX; i++)
    {
      for (i2 = 0; i2 < skyMAX; i2++)
      {
        sky[i][i2].y=(hskyMAX/2-hypot(i-skyMAX/2,i2-skyMAX/2))*400-13200;  
      }  
    }

//  GENERATE LANDSCAPE     
    for (i = 0; i < MAX; i++)
    {     
      for (i2 = 0; i2 < MAX; i2++)
      {
			if (i<75 || i2<75 || i>MAX-75 || i2>MAX-75)
              field[i][i2].y=15;   
            else
            {
              field[i][i2].y=(float(rand()%100)-50)/4; //Calculate the y coordinate on the same principle. 				
            }
      }
    }        
//SMOOTH LANDSCAPE
    for (int cnt = 0; cnt < 8; cnt++)
    {  
      SwapBuffers(hDC);
      DrawProgress();         

      for (int t = 1; t < MAX-1; t++)
      {
        for (int t2 = 1; t2 < MAX-1; t2++)
        {
          field[t][t2].y = (field[t+1][t2].y+field[t][t2-1].y+field[t-1][t2].y+field[t][t2+1].y)/4;           

              if (rand()%500==1 && field[t][t2].y > 0 && cnt == 0) 
              {
                field[t][t2].y*=10;
                 
                field[t-1][t2].y=field[t][t2].y/2;
                field[t+1][t2].y=field[t][t2].y/2;
                field[t][t2-1].y=field[t][t2].y/2;
                field[t][t2+1].y=field[t][t2].y/2;
                //for (i = t-5; i < t+5; i++)
                //  for (i2 = t2-5; i2 < t2+5; i2++)
                //  { 
                    //field[t+1][t].y+=field[t][t2].y/2;
                    //field[t-1][t].y+=field[t][t2].y/2;
                    //field[t][t+1].y+=field[t][t2].y/2;
                    //field[t][t-1].y+=field[t][t2].y/2;
                //  }               
              } 

        }
      }

    }

//  GENERATE NORMALS for good looking lighting     
    for (i = 0; i < MAX; i++)
    {     
      for (i2 = 0; i2 < MAX; i2++)
      {
		    /*
            //Get the three points for calculating the vectors
		    t1=field[i+1][i2]; t2=field[i][i2]; t3=field[i][i2+1];
 
		    //Calculate the first vector from points t1 and t2
		    v1.x=t2.x-t1.x; 
		    v1.y=t2.y-t1.y; 
		    v1.z=t2.z-t1.z;
 
		    //Calculate the second vector from points t2 and t3
		    v2.x=t3.x-t2.x; 
		    v2.y=t3.y-t2.y; 
		    v2.z=t3.z-t2.z;
 
		    //Calculate the cross product of the vectors v1 and v2.
		    //That is our normal for the first vertex of a quad.
		    n1[i][i2].x=((v1.y*v2.z)-(v1.z*v2.y));
		    n1[i][i2].y=((v1.z*v2.x)-(v1.x*v2.z));
		    n1[i][i2].z=((v1.x*v2.y)-(v1.y*v2.x));
 		    
			
			//Now we do the same thing for the rest three vertices of a quad
		    t1=field[i][i2]; t2=field[i][i2+1]; t3=field[i+1][i2+1];
		    v1.x=t2.x-t1.x; 
		    v1.y=t2.y-t1.y; 
		    v1.z=t2.z-t1.z;
 
		    v2.x=t3.x-t2.x; 
		    v2.y=t3.y-t2.y; 
		    v2.z=t3.z-t2.z;
 
		    n2[i][i2].x=((v1.y*v2.z)-(v1.z*v2.y));
		    n2[i][i2].y=((v1.z*v2.x)-(v1.x*v2.z));
		    n2[i][i2].z=((v1.x*v2.y)-(v1.y*v2.x));

			t1=field[i][i2+1]; t2=field[i+1][i2+1]; t3=field[i+1][i2];
		    v1.x=t2.x-t1.x; 
		    v1.y=t2.y-t1.y; 
		    v1.z=t2.z-t1.z;
 
		    v2.x=t3.x-t2.x; 
		    v2.y=t3.y-t2.y; 
		    v2.z=t3.z-t2.z;
 
		    n3[i][i2].x=((v1.y*v2.z)-(v1.z*v2.y));
		    n3[i][i2].y=((v1.z*v2.x)-(v1.x*v2.z));
		    n3[i][i2].z=((v1.x*v2.y)-(v1.y*v2.x));
 
			
			t1=field[i+1][i2+1]; t2=field[i+1][i2]; t3=field[i][i2];
		    v1.x=t2.x-t1.x; 
		    v1.y=t2.y-t1.y; 
		    v1.z=t2.z-t1.z;
 
		    v2.x=t3.x-t2.x; 
		    v2.y=t3.y-t2.y; 
		    v2.z=t3.z-t2.z;
 
		    n4[i][i2].x=((v1.y*v2.z)-(v1.z*v2.y));
		    n4[i][i2].y=((v1.z*v2.x)-(v1.x*v2.z));
		    n4[i][i2].z=((v1.x*v2.y)-(v1.y*v2.x));


			//Convert normals to unit vectors.
			length=sqrt((n1[i][i2].x*n1[i][i2].x) + (n1[i][i2].y*n1[i][i2].y) + (n1[i][i2].z*n1[i][i2].z));
			n1[i][i2].x/=length;
			n1[i][i2].y/=length;
			n1[i][i2].z/=length;
 
			length=sqrt((n2[i][i2].x*n2[i][i2].x) + (n2[i][i2].y*n2[i][i2].y) + (n2[i][i2].z*n2[i][i2].z));
			n2[i][i2].x/=length;
			n2[i][i2].y/=length;
			n2[i][i2].z/=length;

			length=sqrt((n3[i][i2].x*n3[i][i2].x) + (n3[i][i2].y*n3[i][i2].y) + (n3[i][i2].z*n3[i][i2].z));
			n3[i][i2].x/=length;
			n3[i][i2].y/=length;
			n3[i][i2].z/=length;

			length=sqrt((n4[i][i2].x*n4[i][i2].x) + (n4[i][i2].y*n4[i][i2].y) + (n4[i][i2].z*n4[i][i2].z));
			n4[i][i2].x/=length;
			n4[i][i2].y/=length;
			n4[i][i2].z/=length;*/
// GENERATE ALPHA INTENSITIES FOR MULTITEXTURE
            if (field[i][i2].y > 0)
              alpha[i][i2]=field[i][i2].y;
            else
              alpha[i][i2]=0;              
      }
    }    
//SMOOTH NORMALS AND ALPHA INTENSITIES 
    for (cnt = 0; cnt < 3; cnt++)
    {    
      SwapBuffers(hDC);
      DrawProgress();   

      for (int t = 1; t < MAX-1; t++)
      {
        for (int t2 = 1; t2 < MAX-1; t2++)
        {
          //NORMALS
          /*n1[t][t2].x = (n1[t+1][t2].x+n1[t][t2-1].x+n1[t-1][t2].x+n1[t][t2+1].x)/4;           
          n1[t][t2].y = (n1[t+1][t2].y+n1[t][t2-1].y+n1[t-1][t2].y+n1[t][t2+1].y)/4;           
          n1[t][t2].z = (n1[t+1][t2].z+n1[t][t2-1].z+n1[t-1][t2].z+n1[t][t2+1].z)/4;           
          n2[t][t2].x = (n2[t+1][t2].x+n2[t][t2-1].x+n2[t-1][t2].x+n2[t][t2+1].x)/4;           
          n2[t][t2].y = (n2[t+1][t2].y+n2[t][t2-1].y+n2[t-1][t2].y+n2[t][t2+1].y)/4;           
          n2[t][t2].z = (n2[t+1][t2].z+n2[t][t2-1].z+n2[t-1][t2].z+n2[t][t2+1].z)/4;           
          n3[t][t2].x = (n3[t+1][t2].x+n3[t][t2-1].x+n3[t-1][t2].x+n3[t][t2+1].x)/4;           
          n3[t][t2].y = (n3[t+1][t2].y+n3[t][t2-1].y+n3[t-1][t2].y+n3[t][t2+1].y)/4;           
          n3[t][t2].z = (n3[t+1][t2].z+n3[t][t2-1].z+n3[t-1][t2].z+n3[t][t2+1].z)/4;           
          n4[t][t2].x = (n4[t+1][t2].x+n4[t][t2-1].x+n4[t-1][t2].x+n4[t][t2+1].x)/4;           
          n4[t][t2].y = (n4[t+1][t2].y+n4[t][t2-1].y+n4[t-1][t2].y+n4[t][t2+1].y)/4;           
          n4[t][t2].z = (n4[t+1][t2].z+n4[t][t2-1].z+n4[t-1][t2].z+n4[t][t2+1].z)/4;
          */
          alpha[t][t2] = (alpha[t+1][t2]+alpha[t][t2-1]+alpha[t-1][t2]+alpha[t][t2+1])/4;             
        }
      }
    }                

	return TRUE;										// Initialization Went OK
}


GLfloat	xrot=0;				// X Rotation
GLfloat	yrot=0;				// Y Rotation
GLfloat	zrot=0;				// Y Rotation
GLfloat Speed;

const float piover180 = 0.0174532925f;
GLfloat XPOS = -MAX/2;
GLfloat ZPOS = -MAX/2;
GLfloat XP=0;
GLfloat ZP=0;

GLfloat sceneroty;
GLfloat heading;
GLfloat walkbias = 0;
GLfloat walkbiasangle = 0;
GLfloat zprot;
GLfloat yptrans2 = 0;

int quality = 4;

GLfloat H = 0;
GLfloat angle;

GLfloat xdist;
GLfloat zdist;
GLfloat Hypot;

int frames = 0;
float Time1;
float Time2;
float DiffTime;
float FPS = 0;
float multiplier = 360/(3.14159*2); // multiplier is necessary for conversion from 360 degrees.


int DrawGLScene(GLvoid)									// Here's Where We Do All The Drawing and Animation
{ 

    glClear(GL_DEPTH_BUFFER_BIT | GL_COLOR_BUFFER_BIT);	// Clear The Screen And The Depth Buffer   
    glLoadIdentity();

    int i;    
    int i2;    

    if (XPOS < -MAX+80) XPOS = -MAX+80; 
    else if(XPOS > -80) XPOS = -80;
    if (ZPOS < -MAX+80) ZPOS = -MAX+80; 
    else if(ZPOS > -80) ZPOS = -80;
    xtrans = -XPOS;
	ztrans = -ZPOS;
    
    zprot*=0.975f;
    GLfloat yrot2=sceneroty;
    heading += zprot/2;
    yrot = heading;
    
	sceneroty = 360.0f - yrot;

    H += (sceneroty-yrot2);   
    if (H > 360) H = 0;
    else if (H < 0) H = 360;
   
    int xpos = MAX-int(xtrans);
    int zpos = MAX-int(ztrans);

// CALCULATE ALTITUDE ACCORDING TO LANDSCAPE
    GLfloat GROUNDLEVEL = -((field[xpos-1][zpos-1].y*3)
                +(field[xpos][zpos-1].y*4)
                +(field[xpos+1][zpos-1].y*3)
                
                +(field[xpos-1][zpos].y*4)               
                +(field[xpos][zpos].y*5)
                +(field[xpos+1][zpos].y*4)
                
                +(field[xpos-1][zpos+1].y*3)               
                +(field[xpos][zpos+1].y*4)
                +(field[xpos+1][zpos+1].y*3))/(33);

    yptrans +=  GROUNDLEVEL - ytrans;
    yptrans *=0.95f;
   
    ytrans += yptrans/750+yptrans2/50;      
    ytrans *= 0.999f;
    
// CALCULATE YOUR HORIZONTAL SPEED
    Speed = hypot(XP,ZP);   

    glRotatef(-zprot*15,0,0,1);
    glRotatef(Speed*1.5,1,0,0);
    glRotatef(sceneroty,0,1,0);

    glTranslatef(xtrans,ytrans-3-Speed/2,ztrans);    
          
    GLfloat xtex; 
    GLfloat ytex; 
    GLfloat xtex2; 
    GLfloat ytex2; 
    GLfloat xtexa; 
    GLfloat ytexa; 
    GLfloat xtexa2; 
    GLfloat ytexa2; 
    
    int xrange1 = int(MAX-xtrans - 70); 
    int xrange2 = int(MAX-xtrans + 70);
    int zrange1 = int(MAX-ztrans - 70);
    int zrange2 = int(MAX-ztrans + 70);   
  
  if (quality != 1)
  {
    xrange1 /= quality;
    xrange1 *= quality;
    xrange2 /= quality;
    xrange2 *= quality;

    zrange1 /= quality;
    zrange1 *= quality;
    zrange2 /= quality;
    zrange2 *= quality;
  }    

  if (wireframe)
  {    
    glColor4f(0.0f,0.0f,0.0f,1.0f);
    for (i = xrange1; i < xrange2; i+=quality)
    {
      for (i2 = zrange1; i2 < zrange2; i2+=quality)
      {
        xdist = (MAX-xtrans)-i;   
        zdist = (MAX-ztrans)-i2;
        Hypot = hypot(xdist,zdist); 
        if (H > 180)
          angle = (acos(-zdist/Hypot)*multiplier)+180;
        else
          angle = acos(zdist/Hypot)*multiplier;
        
      if (Hypot < 70)
        if (ABS(angle-H) < 52 || Hypot < quality*4)
        {
          glBegin(GL_LINE_STRIP);
          glNormal3f(0,1,0);
          glVertex3f(i-MAX,field[i][i2].y,i2-MAX);        
          glVertex3f(i-MAX+quality,field[i+quality][i2].y,i2-MAX);
          glVertex3f(i-MAX,field[i][i2].y,i2-MAX);        
          glVertex3f(i-MAX,field[i][i2+quality].y,i2-MAX+quality);
          glVertex3f(i-MAX,field[i][i2].y,i2-MAX);        
          glVertex3f(i-MAX+quality,field[i+quality][i2+quality].y,i2-MAX+quality);
          glEnd();
        } 
      }
    }
  }
 
  else
  {   
	glBindTexture(GL_TEXTURE_2D, texture[0]);
    glColor4f(1,1,1,1.f);
    //glEnable(GL_CULL_FACE); 
// FIRST PASS OF DRAWING THE LANDSCAPE
    for (i = xrange1; i < xrange2; i+=quality)
    {   
 	  xtexa = (float(i)/MAX)*37;
 	  xtexa2 = (float(i+quality)/MAX)*37;
      int coord=i-MAX;

      for (i2 = zrange1; i2 < zrange2; i2+=quality)
      {                                     
        xdist = (MAX-xtrans)-i;   
        zdist = (MAX-ztrans)-i2;
        Hypot = hypot(xdist,zdist); 
        if (H > 180)
          angle = (acos(-zdist/Hypot)*multiplier)+180;
        else
          angle = acos(zdist/Hypot)*multiplier;
        
        if (Hypot < 70)
          if (ABS(angle-H) < 55 || Hypot < quality*4)  //Determines which triangle are in the estimated frustum
          {
            ytexa = (float(i2)/MAX)*37;
            ytexa2 = (float(i2+quality)/MAX)*37;
            int coord2=i2-MAX; 
                             
            glBegin(GL_TRIANGLE_STRIP);
	        //glNormal3f(n4[i][i2].x,n4[i][i2].y,n4[i][i2].z);
            glTexCoord2f(xtexa2,ytexa2);  glVertex3f(coord+quality,field[i+quality][i2+quality].y,  coord2+quality);
            //glNormal3f(n1[i][i2].x,n1[i][i2].y,n1[i][i2].z);
            glTexCoord2f(xtexa2,ytexa);   glVertex3f(coord+quality,field[i+quality][i2].y,coord2); 
            //glNormal3f(n3[i][i2].x,n3[i][i2].y,n3[i][i2].z);
            glTexCoord2f(xtexa,ytexa2);   glVertex3f(coord,field[i][i2+quality].y,coord2+quality); 
            //glNormal3f(n2[i][i2].x,n2[i][i2].y,n2[i][i2].z);
            glTexCoord2f(xtexa,ytexa);   glVertex3f(coord,field[i][i2].y,coord2); 
            glEnd();
          }       
      }   
    }
// SECOND PASS OF DRAWING THE LANDSCAPE(multitexturing)
    if (multitexture)
    {    
      glBindTexture(GL_TEXTURE_2D, texture[2]);
      glColor4f(1,.7,.6,.5f);
      for (i = xrange1; i < xrange2; i+=quality)
       {   
 	    xtexa = (float(i)/MAX)*45;
 	    xtexa2 = (float(i+quality)/MAX)*45;
        int coord=i-MAX;

        for (i2 = zrange1; i2 < zrange2; i2+=quality)
        {               
          if (alpha[i+quality][i2+quality] > 0 ||alpha[i+quality][i2] > 0 ||alpha[i][i2+quality] > 0 ||alpha[i][i2] > 0)//(alpha1 > 2 || alpha2 > 2 || alpha3 > 2 || alpha4 > 2)
          {       
            xdist = (MAX-xtrans)-i;   
            zdist = (MAX-ztrans)-i2;
            Hypot = hypot(xdist,zdist); 
            if (H > 180)
              angle = (acos(-zdist/Hypot)*multiplier)+180;
            else
              angle = acos(zdist/Hypot)*multiplier;
        
            if (Hypot < 70)
              if (ABS(angle-H) < 55 || Hypot < quality*4)
              {              
                ytexa = (float(i2)/MAX)*45;
                ytexa2 = (float(i2+quality)/MAX)*45;       
                int coord2=i2-MAX;
        
        
                glBegin(GL_TRIANGLE_STRIP);
                glColor4f(1,.7,.6,alpha[i+quality][i2+quality]);
                //glNormal3f(n4[i][i2].x,n4[i][i2].y,n4[i][i2].z);
                glTexCoord2f(xtexa2,ytexa2);  glVertex3f(coord+quality,field[i+quality][i2+quality].y,  coord2+quality);
                glColor4f(1,.7,.6,alpha[i+quality][i2]);
                //glNormal3f(n1[i][i2].x,n1[i][i2].y,n1[i][i2].z);
                glTexCoord2f(xtexa2,ytexa);   glVertex3f(coord+quality,field[i+quality][i2].y,coord2); 
                glColor4f(1,.7,.6,alpha[i][i2+quality]);
                //glNormal3f(n3[i][i2].x,n3[i][i2].y,n3[i][i2].z);
                glTexCoord2f(xtexa,ytexa2);   glVertex3f(coord,field[i][i2+quality].y,coord2+quality); 
                glColor4f(1,.7,.6,alpha[i][i2]);
                //glNormal3f(n2[i][i2].x,n2[i][i2].y,n2[i][i2].z);
                glTexCoord2f(xtexa,ytexa);   glVertex3f(coord,field[i][i2].y,coord2); 
                glEnd();
        
              }   

          }       
        }
      }   
    }  
       
   //glDisable(GL_CULL_FACE);

// SKYDOME GENERATED WITH A PRECISELY POSITIONED SPHERE(a shortcut to the real thing)
	glFogf(GL_FOG_START, MAX*2);							// Fog Start Depth
	glFogf(GL_FOG_END, MAX*15);							// Fog End Depth
    glColor4f(1,1,1,1.f);
    glBindTexture(GL_TEXTURE_2D, texture[1]);
/*    float MS = float(MAX);
    for (i = 0; i < skyMAX-1; i++)
      for (i2 = 0; i2 < skyMAX-1; i2++)
      {
 	    if (hypot(i-(skyMAX/2),i2-(skyMAX/2)) < skyMAX/2)
        { 
        xtexa = float(float(i)/float(skyMAX));
 	    xtexa2 = float(float(i+1)/float(skyMAX));
 	    ytexa = float(float(i)/float(skyMAX));
 	    ytexa2 = float(float(i+1)/float(skyMAX));
        float x1,x2,x3,x4;        
        float z1,z2,z3,z4;
        x1 = ((i+1)*MS)-(skyMAX*MS/2); z1 = ((i2+1)*MS)-(skyMAX*MS/2);
        x2 = ((i+1)*MS)-(skyMAX*MS/2); z2 = ((i2)*MS)-(skyMAX*MS/2);
        x3 = ((i)*MS)-(skyMAX*MS/2);   z3 = ((i2+1)*MS)-(skyMAX*MS/2);
        x4 = ((i)*MS)-(skyMAX*MS/2);   z4 = ((i2)*MS)-(skyMAX*MS/2);
        glBegin(GL_TRIANGLE_STRIP);
        glTexCoord2f(xtexa2,ytexa2); glVertex3f(x1,sky[i+1][i2+1].y,z1);
        glTexCoord2f(xtexa2,ytexa);  glVertex3f(x2,sky[i+1][i2].y,z2);
        glTexCoord2f(xtexa,ytexa2);  glVertex3f(x3,sky[i][i2+1].y,z3);
        glTexCoord2f(xtexa,ytexa);   glVertex3f(x4,sky[i][i2].y,z4); 
        glEnd();
        }      
      }
*/
/*    glColor4f(0,0,1,0.5f);
    glBindTexture(GL_TEXTURE_2D, texture[0]);
    for (i = 0; i < skyMAX-1; i++)
      for (i2 = 0; i2 < skyMAX-1; i2++)
      {
 	    xtexa = float(float(i)/float(skyMAX))*50;
 	    xtexa2 = float(float(i+1)/float(skyMAX))*50;
 	    ytexa = float(float(i)/float(skyMAX))*50;
 	    ytexa2 = float(float(i+1)/float(skyMAX))*50;
        float x1,x2,x3,x4;        
        float z1,z2,z3,z4;
        x1 = ((i+1)*MS)-(skyMAX*MS/2); z1 = ((i2+1)*MS)-(skyMAX*MS/2);
        x2 = ((i+1)*MS)-(skyMAX*MS/2); z2 = ((i2)*MS)-(skyMAX*MS/2);
        x3 = ((i)*MS)-(skyMAX*MS/2);   z3 = ((i2+1)*MS)-(skyMAX*MS/2);
        x4 = ((i)*MS)-(skyMAX*MS/2);   z4 = ((i2)*MS)-(skyMAX*MS/2);
        glBegin(GL_TRIANGLE_STRIP);
        glTexCoord2f(xtexa2,ytexa2); glVertex3f(x1,sky[i+1][i2+1].y,z1);
        glTexCoord2f(xtexa2,ytexa);  glVertex3f(x2,sky[i+1][i2].y,z2);
        glTexCoord2f(xtexa,ytexa2);  glVertex3f(x3,sky[i][i2+1].y,z3);
        glTexCoord2f(xtexa,ytexa);   glVertex3f(x4,sky[i][i2].y,z4); 
        glEnd();      
      }
*/
    glTranslatef(-xtrans,-MAX*48,-ztrans);
    glRotatef(90,1,0,0);
    gluSphere(quadratic,MAX*50,32,32);

	glFogf(GL_FOG_START, 10.0f);						// Fog Start Depth
	glFogf(GL_FOG_END, 60.0f);							// Fog End Depth
//glEnable(GL_BLEND);


    if(water)
    { 
    //glEnable(GL_CULL_FACE);
    glLoadIdentity();
    glRotatef(-zprot*15,0,0,1);
    glRotatef(Speed*1.5,1,0,0);
    glRotatef(sceneroty,0,1,0);

    glTranslatef(xtrans,ytrans-3-Speed/2,ztrans);    

    xtexa = (float(xrange1)/MAX)*50;
    xtexa2 = (float(xrange2)/MAX)*50;
    ytexa = (float(zrange1)/MAX)*50;
    ytexa2 = (float(zrange2)/MAX)*50;
// WATER
    glBindTexture(GL_TEXTURE_2D, texture[4]);
    glColor4f(.5,.5,1,.75f);
    glBegin(GL_TRIANGLE_STRIP);
    glTexCoord2f(xtexa2,ytexa2); glVertex3f(xrange2-MAX,-1,zrange2-MAX);
    glTexCoord2f(xtexa2,ytexa);  glVertex3f(xrange2-MAX,-1,zrange1-MAX); 
    glTexCoord2f(xtexa,ytexa2);  glVertex3f(xrange1-MAX,-1,zrange2-MAX); 
    glTexCoord2f(xtexa,ytexa);   glVertex3f(xrange1-MAX,-1,zrange1-MAX); 
    glEnd();
    }
  }        
   
//  IF WINDOWED, ESTABLISH A FPS FRAMERATE COUNTER
  if(!fullscreen)
  { 
    frames++;    
    if (frames%5 == 1)
    {
      Time1 = TimerGetTime()/1000;
      
    }
    else if (frames%5 == 0) 
    {
      Time2 = TimerGetTime()/1000;
      DiffTime = ABS(Time2-Time1);      
    }  

    FPS = 5/(DiffTime);         
    glLoadIdentity();
    glTranslatef(0.0f,0.0f,-5.0f);
    glColor4f(1,.5f,0,1);
    glRasterPos2f(-2.2f, 1.4f);
    glPrint("FPS: %7.0f", FPS);           
  }

   return TRUE;										// Keep Going
}

GLvoid KillGLWindow(GLvoid)								// Properly Kill The Window
{
	if (hRC)											// Do We Have A Rendering Context?
	{
		if (!wglMakeCurrent(NULL,NULL))					// Are We Able To Release The DC And RC Contexts?
		{
			MessageBox(NULL,"Release Of DC And RC Failed.","SHUTDOWN ERROR",MB_OK | MB_ICONINFORMATION);
		}

		if (!wglDeleteContext(hRC))						// Are We Able To Delete The RC?
		{
			MessageBox(NULL,"Release Rendering Context Failed.","SHUTDOWN ERROR",MB_OK | MB_ICONINFORMATION);
		}
		hRC=NULL;										// Set RC To NULL
	}

	if (hDC && !ReleaseDC(hWnd,hDC))					// Are We Able To Release The DC
	{
		MessageBox(NULL,"Release Device Context Failed.","SHUTDOWN ERROR",MB_OK | MB_ICONINFORMATION);
		hDC=NULL;										// Set DC To NULL
	}

	if (hWnd && !DestroyWindow(hWnd))					// Are We Able To Destroy The Window?
	{
		MessageBox(NULL,"Could Not Release hWnd.","SHUTDOWN ERROR",MB_OK | MB_ICONINFORMATION);
		hWnd=NULL;										// Set hWnd To NULL
	}

	if (fullscreen)										// Are We In Fullscreen Mode?
	{
		ChangeDisplaySettings(NULL,0);					// If So Switch Back To The Desktop
		ShowCursor(TRUE);								// Show Mouse Pointer
	}
    KillFont();
}

/*	This Code Creates Our OpenGL Window.  Parameters Are:					*
 *	title			- Title To Appear At The Top Of The Window				*
 *	width			- Width Of The GL Window Or Fullscreen Mode				*
 *	height			- Height Of The GL Window Or Fullscreen Mode			*
 *	bits			- Number Of Bits To Use For Color (8/16/24/32)			*
 *	fullscreenflag	- Use Fullscreen Mode (TRUE) Or Windowed Mode (FALSE)	*/
 
BOOL CreateGLWindow(char* title, int width, int height, int bits, bool fullscreenflag)
{
	GLuint		PixelFormat;			// Holds The Results After Searching For A Match
	HINSTANCE	hInstance;				// Holds The Instance Of The Application
	WNDCLASS	wc;						// Windows Class Structure
	DWORD		dwExStyle;				// Window Extended Style
	DWORD		dwStyle;				// Window Style
	RECT		WindowRect;				// Grabs Rectangle Upper Left / Lower Right Values
	WindowRect.left=(long)0;			// Set Left Value To 0
	WindowRect.right=(long)width;		// Set Right Value To Requested Width
	WindowRect.top=(long)0;				// Set Top Value To 0
	WindowRect.bottom=(long)height;		// Set Bottom Value To Requested Height

	fullscreen=fullscreenflag;			// Set The Global Fullscreen Flag

	hInstance			= GetModuleHandle(NULL);				// Grab An Instance For Our Window
	wc.style			= CS_HREDRAW | CS_VREDRAW | CS_OWNDC;	// Redraw On Size, And Own DC For Window.
	wc.lpfnWndProc		= (WNDPROC) WndProc;					// WndProc Handles Messages
	wc.cbClsExtra		= 0;									// No Extra Window Data
	wc.cbWndExtra		= 0;									// No Extra Window Data
	wc.hInstance		= hInstance;							// Set The Instance
	wc.hIcon			= LoadIcon(NULL, IDI_WINLOGO);			// Load The Default Icon
	wc.hCursor			= LoadCursor(NULL, IDC_ARROW);			// Load The Arrow Pointer
	wc.hbrBackground	= NULL;									// No Background Required For GL
	wc.lpszMenuName		= NULL;									// We Don't Want A Menu
	wc.lpszClassName	= "OpenGL";								// Set The Class Name

	if (!RegisterClass(&wc))									// Attempt To Register The Window Class
	{
		MessageBox(NULL,"Failed To Register The Window Class.","ERROR",MB_OK|MB_ICONEXCLAMATION);
		return FALSE;											// Return FALSE
	}
	
	if (fullscreen)												// Attempt Fullscreen Mode?
	{
		DEVMODE dmScreenSettings;								// Device Mode
		memset(&dmScreenSettings,0,sizeof(dmScreenSettings));	// Makes Sure Memory's Cleared
		dmScreenSettings.dmSize=sizeof(dmScreenSettings);		// Size Of The Devmode Structure
		dmScreenSettings.dmPelsWidth	= width;				// Selected Screen Width
		dmScreenSettings.dmPelsHeight	= height;				// Selected Screen Height
		dmScreenSettings.dmBitsPerPel	= bits;					// Selected Bits Per Pixel
		dmScreenSettings.dmFields=DM_BITSPERPEL|DM_PELSWIDTH|DM_PELSHEIGHT;

		// Try To Set Selected Mode And Get Results.  NOTE: CDS_FULLSCREEN Gets Rid Of Start Bar.
		if (ChangeDisplaySettings(&dmScreenSettings,CDS_FULLSCREEN)!=DISP_CHANGE_SUCCESSFUL)
		{
			// If The Mode Fails, Offer Two Options.  Quit Or Use Windowed Mode.
			if (MessageBox(NULL,"The Requested Fullscreen Mode Is Not Supported By\nYour Video Card. Use Windowed Mode Instead?","NeHe GL",MB_YESNO|MB_ICONEXCLAMATION)==IDYES)
			{
				fullscreen=FALSE;		// Windowed Mode Selected.  Fullscreen = FALSE
			}
			else
			{
				// Pop Up A Message Box Letting User Know The Program Is Closing.
				MessageBox(NULL,"Program Will Now Close.","ERROR",MB_OK|MB_ICONSTOP);
				return FALSE;									// Return FALSE
			}
		}
	}

	if (fullscreen)												// Are We Still In Fullscreen Mode?
	{
		dwExStyle=WS_EX_APPWINDOW;								// Window Extended Style
		dwStyle=WS_POPUP | WS_CLIPSIBLINGS | WS_CLIPCHILDREN;	// Windows Style
		ShowCursor(FALSE);										// Hide Mouse Pointer
	}
	else
	{
		dwExStyle=WS_EX_APPWINDOW | WS_EX_WINDOWEDGE;						// Window Extended Style
		dwStyle=WS_OVERLAPPEDWINDOW | WS_CLIPSIBLINGS | WS_CLIPCHILDREN;	// Windows Style
	}

	AdjustWindowRectEx(&WindowRect, dwStyle, FALSE, dwExStyle);		// Adjust Window To True Requested Size

	// Create The Window
	if (!(hWnd=CreateWindowEx(	dwExStyle,							// Extended Style For The Window
								"OpenGL",							// Class Name
								title,								// Window Title
								dwStyle |							// Defined Window Style
								WS_CLIPSIBLINGS |					// Required Window Style
								WS_CLIPCHILDREN,					// Required Window Style
								0, 0,								// Window Position
								WindowRect.right-WindowRect.left,	// Calculate Window Width
								WindowRect.bottom-WindowRect.top,	// Calculate Window Height
								NULL,								// No Parent Window
								NULL,								// No Menu
								hInstance,							// Instance
								NULL)))								// Dont Pass Anything To WM_CREATE
	{
		KillGLWindow();								// Reset The Display
		MessageBox(NULL,"Window Creation Error.","ERROR",MB_OK|MB_ICONEXCLAMATION);
		return FALSE;								// Return FALSE
	}

	static	PIXELFORMATDESCRIPTOR pfd=				// pfd Tells Windows How We Want Things To Be
	{
		sizeof(PIXELFORMATDESCRIPTOR),				// Size Of This Pixel Format Descriptor
		1,											// Version Number
		PFD_DRAW_TO_WINDOW |						// Format Must Support Window
		PFD_SUPPORT_OPENGL |						// Format Must Support OpenGL
		PFD_DOUBLEBUFFER,							// Must Support Double Buffering
		PFD_TYPE_RGBA,								// Request An RGBA Format
		bits,										// Select Our Color Depth
		0, 0, 0, 0, 0, 0,							// Color Bits Ignored
		0,											// No Alpha Buffer
		0,											// Shift Bit Ignored
		0,											// No Accumulation Buffer
		0, 0, 0, 0,									// Accumulation Bits Ignored
		16,											// 16Bit Z-Buffer (Depth Buffer)  
		0,											// No Stencil Buffer
		0,											// No Auxiliary Buffer
		PFD_MAIN_PLANE,								// Main Drawing Layer
		0,											// Reserved
		0, 0, 0										// Layer Masks Ignored
	};
	
	if (!(hDC=GetDC(hWnd)))							// Did We Get A Device Context?
	{
		KillGLWindow();								// Reset The Display
		MessageBox(NULL,"Can't Create A GL Device Context.","ERROR",MB_OK|MB_ICONEXCLAMATION);
		return FALSE;								// Return FALSE
	}

	if (!(PixelFormat=ChoosePixelFormat(hDC,&pfd)))	// Did Windows Find A Matching Pixel Format?
	{
		KillGLWindow();								// Reset The Display
		MessageBox(NULL,"Can't Find A Suitable PixelFormat.","ERROR",MB_OK|MB_ICONEXCLAMATION);
		return FALSE;								// Return FALSE
	}

	if(!SetPixelFormat(hDC,PixelFormat,&pfd))		// Are We Able To Set The Pixel Format?
	{
		KillGLWindow();								// Reset The Display
		MessageBox(NULL,"Can't Set The PixelFormat.","ERROR",MB_OK|MB_ICONEXCLAMATION);
		return FALSE;								// Return FALSE
	}

	if (!(hRC=wglCreateContext(hDC)))				// Are We Able To Get A Rendering Context?
	{
		KillGLWindow();								// Reset The Display
		MessageBox(NULL,"Can't Create A GL Rendering Context.","ERROR",MB_OK|MB_ICONEXCLAMATION);
		return FALSE;								// Return FALSE
	}

	if(!wglMakeCurrent(hDC,hRC))					// Try To Activate The Rendering Context
	{
		KillGLWindow();								// Reset The Display
		MessageBox(NULL,"Can't Activate The GL Rendering Context.","ERROR",MB_OK|MB_ICONEXCLAMATION);
		return FALSE;								// Return FALSE
	}

	ShowWindow(hWnd,SW_SHOW);						// Show The Window
	SetForegroundWindow(hWnd);						// Slightly Higher Priority
	SetFocus(hWnd);									// Sets Keyboard Focus To The Window
	ReSizeGLScene(width, height);					// Set Up Our Perspective GL Screen

	if (!InitGL())									// Initialize Our Newly Created GL Window
	{
		KillGLWindow();								// Reset The Display
		MessageBox(NULL,"Initialization Failed.","ERROR",MB_OK|MB_ICONEXCLAMATION);
		return FALSE;								// Return FALSE
	}

    

	return TRUE;									// Success
}

LRESULT CALLBACK WndProc(	HWND	hWnd,			// Handle For This Window
							UINT	uMsg,			// Message For This Window
							WPARAM	wParam,			// Additional Message Information
							LPARAM	lParam)			// Additional Message Information
{
	switch (uMsg)									// Check For Windows Messages
	{
		case WM_ACTIVATE:							// Watch For Window Activate Message
		{
			if (!HIWORD(wParam))					// Check Minimization State
			{
				active=TRUE;						// Program Is Active
			}
			else
			{
				active=FALSE;						// Program Is No Longer Active
			}

			return 0;								// Return To The Message Loop
		}

		case WM_SYSCOMMAND:							// Intercept System Commands
		{
			switch (wParam)							// Check System Calls
			{
				case SC_SCREENSAVE:					// Screensaver Trying To Start?
				case SC_MONITORPOWER:				// Monitor Trying To Enter Powersave?
				return 0;							// Prevent From Happening
			}
			break;									// Exit
		}

		case WM_CLOSE:								// Did We Receive A Close Message?
		{
			PostQuitMessage(0);						// Send A Quit Message
			return 0;								// Jump Back
		}

		case WM_KEYDOWN:							// Is A Key Being Held Down?
		{
			keys[wParam] = TRUE;					// If So, Mark It As TRUE
			return 0;								// Jump Back
		}

		case WM_KEYUP:								// Has A Key Been Released?
		{
			keys[wParam] = FALSE;					// If So, Mark It As FALSE
			return 0;								// Jump Back
		}

		case WM_SIZE:								// Resize The OpenGL Window
		{
			ReSizeGLScene(LOWORD(lParam),HIWORD(lParam));  // LoWord=Width, HiWord=Height
			return 0;								// Jump Back
		}
	}

	// Pass All Unhandled Messages To DefWindowProc
	return DefWindowProc(hWnd,uMsg,wParam,lParam);
}




int WINAPI WinMain(	HINSTANCE	hInstance,			// Instance
					HINSTANCE	hPrevInstance,		// Previous Instance
					LPSTR		lpCmdLine,			// Command Line Parameters
					int			nCmdShow)			// Window Show State
{
	
    MSG		msg;									// Windows Message Structure
	BOOL	done=FALSE;								// Bool Variable To Exit Loop
   
    fullscreen=true;

    // Ask The User Which Screen Mode They Prefer
	if (MessageBox(NULL,"Would You Like To Run In Fullscreen Mode?", "Start FullScreen?",MB_YESNO|MB_ICONQUESTION)==IDNO)
	{
		fullscreen=FALSE;							// Windowed Mode
	}
    
    // Create Our OpenGL Window
	if (!CreateGLWindow("The Abyss",800,600,16,fullscreen))
	{
		return 0;									// Quit If Window Was Not Created
	}

	while(!done)									// Loop That Runs While done=FALSE
	{
		if (PeekMessage(&msg,NULL,0,0,PM_REMOVE))	// Is There A Message Waiting?
		{
			if (msg.message==WM_QUIT)				// Have We Received A Quit Message?
			{
				done=TRUE;							// If So done=TRUE
			}
			else									// If Not, Deal With Window Messages
			{
				TranslateMessage(&msg);				// Translate The Message
				DispatchMessage(&msg);				// Dispatch The Message
			}
		}
		else										// If There Are No Messages
		{
			
            // Draw The Scene.  Watch For ESC Key And Quit Messages From DrawGLScene()
			if ((active && !DrawGLScene()) || keys[VK_ESCAPE])	// Active?  Was There A Quit Received?
			{
				done=TRUE;							// ESC or DrawGLScene Signalled A Quit
			}
			else									// Not Time To Quit, Update Screen
			{
				SwapBuffers(hDC);					// Swap Buffers (Double Buffering)
				
                if (keys[VK_UP])
                {
					XP -= (float)sin(heading*piover180) * 0.1f;	
					ZP -= (float)cos(heading*piover180) * 0.1f;
                }

                else if (keys[VK_DOWN])
                {
					XP += (float)sin(heading*piover180) * 0.1f;	
					ZP += (float)cos(heading*piover180) * 0.1f;
                }

				XP *= 0.995f; 
				ZP *= 0.995f; 
				XPOS += XP/30;//(float)sin(heading*piover180) * 0.25f;
				ZPOS += ZP/30;

                if (keys[VK_LEFT])
                {
				  zprot += 0.045f;               
                }
               
                else if (keys[VK_RIGHT])
                {
  				  zprot -= 0.045f;
                }
                
                if (keys['M'] && !mp)
                {
 				  mp=TRUE;
				  multitexture=!multitexture;                   
                }

				if (!keys['M'])
				{
					mp=FALSE;
				}

                if (keys['L'] && !lp)
                {
 				  lp=TRUE;
				  water=!water;                   
                }

				if (!keys['L'])
				{
					lp=FALSE;
				}

                if (keys['W'] && !wp)
                {
 				  wp=TRUE;
				  wireframe=!wireframe;                   
                }

				if (!keys['W'])
				{
					wp=FALSE;
				}

                if (keys[VK_ADD] && !aq)
                {                 
                  aq=TRUE;
                  if (quality <= 1) quality = 1;
                  else quality--;    
                }
                if (!keys[VK_ADD])
                {
                  aq=FALSE;                    
                }

                if (keys[VK_SUBTRACT] && !sq)
                {                 
                  sq=TRUE;
                  if (quality >= 8) quality = 8;
                  else quality++;    
                }
                if (!keys[VK_SUBTRACT])
                {
                  sq=FALSE;                    
                }

                if(keys[VK_NEXT])
                {
                  yptrans2+=0.05f;
                  if (yptrans2 > 0) yptrans2*=0.9f;
                }

                if(keys[VK_PRIOR])
                {
                  yptrans2-=0.05f;
                  if (yptrans2 < -8) yptrans2*=0.99f;
                }

			}
		}
	}

	// Shutdown
	KillGLWindow();									// Kill The Window
    return (msg.wParam);							// Exit The Program
}
