TFire ver 3.1
Native Delphi 3.0/4.0 VCL
by Tan Qunzhao
Email: tqz@163.net
Homepage: http://longjoy.freeservers.com

Copyright ?1999

----------------
INTRODUCTION
----------------

  TFire draws a flame blazing(It's live!) in the window,which has really magnificent effact.it could be drawn transparently,with everything behind its gap visible.A mask bitmap can be added to make the fire burning inside/outside the mask.Much other properties could be changed as color palette, heat, chaos, etc...

  This TFire component is written by Tan Qunzhao (tqz@163.net), which is based on a flame algorithm developed by Frank Jan Sorensen Alias:Frank Patxi (frank.s@post4.tele.dk)

  Rebuild Demo to obtain the usage of TFire.

  This software is free for non-commercial use. If you want to use it in a Commercial Software and/or want to have a look at its source code, please send 10 US dollars to :

  Bank of China Shanghai Branch
  Account: 4039503-070029135
  Name: Tan Qunzhao 

  If you found it's too expensive or inconvenient for you to transfer the money to the account, you can send the cheque or even the bill itself to me by letter.My postal address is:

  Mailbox 272, No.130, MeiLong Road, Shanghai, China 
  postalcode: 200237
  addressee is: Tan Qunzhao

  Please drop me a email after you have done it,and I will send the  source promptly.

--------
Methods and Properties
--------
  Most methods and properties of TFire are self-explained.The KEY methods and properties are as below:
  
  Methods:
    procedure About;
              Show the about message.
 
    procedure UpdateBackground;
              get the bitmap of background under fire.
              When the fire is drawn transparently (Transparent=true) ,and AutoUpdate = false,you should run UpdateBackground manually when need.

  Properties:
    AutoUpdate : Boolean;
                 if true then Background bitmap will be updated every time before fire be drawn.
                 Note: selecting this will slow down the speed of blazing,though maybe not evidently. 
    Burning : Boolean;
              whether the fire should blaze or stop.

    EdgeRatio: Single ;
              the percent of width should fire be displayed.from 0 to 1.

    FirePalette: TPalColor ;
                 the palette of the fire.
          
    Interval : Integer;
               the interval of the internal timer,controls the blaze rate.
  
    MaxBurn : integer;
              the percent of height should fire be displayed.from 0 to 100.

    Mask : TBitmap ;
           a bitmap that defines the region that the fire blazing in.

    MaskColor : Tcolor;
                the color that is transparent to the mask bitmap.That is to say, fire can display in the region that the mask bitmap's color is MaskColor.

    Transparent : Boolean;
                 if true then background could be seen in the gap of the Fire,which could even receive mouse event as if nothing was in front of it.

    OnBurning : TNotifyEvent ; 
                called each time fire is drawn.  

    OnUpdating : TNotifyEvent ;
                 called each time background is updated.  


 Other properties controls the appearance of the fire,most time modification is not needed. include:
    property Decay: integer; 
    property Flammability: integer ;
    property MaxHeat: integer ;
    property SpreadRate: integer ;
    property Smoothness: integer ;
    property Distribution: integer ;
    property Chaos: integer ;

----------------
INSTALLATION
----------------
Extract All files in the folder(D3 or D4) that corresponding to your Delphi's versionto a folder that in delphi's search path(if you have both Delphi3 and Delphi4,make sure that extract files in different path,which must not all under one version of delphi's search path).

In Delphi,Open the Fire3.dpk(make sure that has correct version),and rebuild it.Then the new control(TFire)will appear at the Sample palette.


---------
WARRANTY
---------

Users must accept this  disclaimer of warranty:

 THE SOFTWARE IS SUPPLIED AS IS. THE AUTHOR DISCLAIMS ALL WARRANTIES, EXPRESSED OR IMPLIED,  INCLUDING,  WITHOUT LIMITATION, THE WARRANTIES OF MERCHANTABILITY AND OF  FITNESS FOR  ANY PURPOSE.  THE AUTHOR ASSUMES NO LIABILITY  FOR DAMAGES,  DIRECT OR CONSEQUENTIAL,  WHICH MAY RESULT FROM THE USE OF THIS SOFTWARE.

  If you have any questions, comments, suggestions, feel free to e-mail me. My e-mail address is tqz@163.net

  Have Fun!
                                                    
---------
HISTORY
---------

v1.0  First release.
v1.0p Fix some compile bugs.
v2.0  Add Mask property;
      add TransparentColor property;
      no longer compatible with D2;
      fix the bugs in convert to ActiveX and be placed in ActiveForm;
      ancentor changes from TGraphicControl to TCustomControl.
v2.1  Fix the bug that makes TransparentColor does not work correctly;
      some performance improvement;
      Exe demo removed.

v3.0  Fix the bug that die in Win98:)
      add Transparent effact
      property TransparentColor -> MaskColor
      some performance improvement;
v3.1  Fix the bug when applying mask bitmap