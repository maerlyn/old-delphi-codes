unit TitleButton; 

interface 

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs; 

type TChangedProperty=(cpdown,cpallowallup,cpgroupindex);
type
  TTitleButton = class(Tcomponent)
  private 
   fbuttonrect:trect; 
   fpressed,ffocused:boolean; 
   fbuttoncaption:string; 
   fwidth:integer; 
   fleft:integer; 
   fvisible:boolean; 
   fhintshow:boolean; 
   fhint:thintwindow; 
   fhinttext:string; 
   fgroupindex:integer; 
   fdown:boolean; 
   fallowallup:boolean; 
   fparent:Tform; 
   fparentwidth:integer; 
   ficonwidth:integer; 
   fcallinheritedevent:boolean; 
   fdefaultwidth:integer; 
   fdefaultheight:integer; 
   ffont:Tfont; 
   ficon:Ticon; 
   fborder3d,fborderthickness:integer; 
   fbuttondown:tnotifyevent; 
   fbuttonmove:tmousemoveevent; 
   fbuttonup:tnotifyevent; 
   pmsghandler:Twndmethod; 
   ppaint:Tnotifyevent; 
   presize:tnotifyevent; 
   gtmp1,gtmp2,gtmp3:boolean; 
   procedure initializevariables; 
   procedure IconChange(Sender:tobject); 
   procedure setbuttonwidth(awidth:integer); 
   procedure setbuttonleft(aleft:integer); 
   procedure setbuttoncaption(acaption:string); 
   procedure setbuttonfont(afont:tfont); 
   procedure setbuttonvisible(avisible:boolean); 
   procedure seticon(aicon:ticon); 
   procedure setdown(adown:boolean); 
   procedure setallowallup(aallowallup:boolean); 
   procedure setgroupindex(agroupindex:integer); 
   procedure UpdateProperties(achangedproperty:TChangedProperty); 
  protected 
   procedure messagehandler(var msg:tmessage); 
   procedure CaptionPaint(var msg:tmessage); 
   procedure CaptionMouseMove(var msg:tmessage); 
   procedure CaptionMouseDown(var msg:tmessage); 
   procedure CaptionMouseUp(var msg:tmessage); 
   procedure CaptionRightMouseDown(var msg:tmessage); 
   procedure CaptionDoubleClick(var msg:tmessage); 
   procedure CaptionActivate(var msg:tmessage); 
   procedure CaptionHitTest(var msg:Tmessage); 
   procedure CaptionChange(var msg:Tmessage); 
   procedure ParentMouseMove(var msg:tmessage); 
   procedure ParentMouseUp(var msg:tmessage); 
   procedure ButtonUp(var msg:tmessage); 
   procedure ParentPaint(sender:tobject); 
   procedure ParentResize(sender:tobject); 
   procedure DisplaySettingChange(var msg:tmessage); 
   procedure loaded;override; 
  public 
   constructor create(aowner:tcomponent);override; 
   destructor destroy;override; 
  published 
   property Width:integer read fwidth write setbuttonwidth; 
   property Position:integer read fleft write setbuttonleft; 
   property Caption:string read fbuttoncaption write setbuttoncaption; 
   property Font:Tfont read ffont write SetButtonFont; 
   property Icon:Ticon read ficon write seticon; 
   property TipText:string read fhinttext write fhinttext; 
   property Visible:boolean read fvisible write setbuttonvisible; 
   property AllowAllUp:boolean read fallowallup write setallowallup; 
   property Down:boolean read fdown write setdown; 
   property GroupIndex:integer read fgroupindex write setgroupindex; 
   property OnMouseDown:tnotifyevent read fbuttondown write fbuttondown; 
   property OnMouseMove:tmousemoveevent read fbuttonmove write fbuttonmove; 
   property OnMouseUp:tnotifyevent read fbuttonup write fbuttonup; 
  end; 

const TTB_SETBUTTONUP=WM_USER+1;

procedure Register;

implementation

constructor TTitleButton.create(aowner:tcomponent);
begin
inherited; 
  fparent:=(owner as tform); 
  ffont:=tfont.create; 
  fhint:=thintwindow.create(self); 
  ficon:=ticon.create; 
end; 

destructor TTitleButton.destroy; 
begin 
if assigned(ficon) then 
  ficon.free; 
if assigned(ffont) then 
  ffont.free;
if assigned(fhint) then 
  fhint.free; 
inherited; 
end; 

procedure TTitleButton.loaded; 
begin 
inherited; 
initializevariables; 
end; 

procedure TTitleButton.UpdateProperties(achangedproperty:TChangedProperty); 
var 
amsg:tmessage; 
begin 
amsg.Msg:=TTB_SETBUTTONUP; 
amsg.WParam:=integer(self); 
amsg.LParamlo:=fgroupindex; 
amsg.LParamHi:=word(achangedproperty); 
amsg.Result:=0; 
fparent.perform(amsg.msg,amsg.wparam,amsg.lparam); 
end; 

procedure TTitleButton.initializevariables; 
begin 
if assigned(fparent.WindowProc) then 
  pmsghandler:=fparent.WindowProc; 
fparent.WindowProc:=messagehandler; 
if not(csdesigning in componentstate) then 
  begin 
   if assigned(fparent.onpaint) then 
    ppaint:=fparent.onpaint; 
   if assigned(fparent.onresize) then 
    presize:=fparent.onresize; 
   fparent.onpaint:=parentpaint; 
   fparent.onresize:=parentresize; 
end; 
fparentwidth:=fparent.width; 
zeromemory(@fbuttonrect,sizeof(fbuttonrect)); 
fpressed:=false; 
ffocused:=false; 
fhintshow:=false; 
ficonwidth:=16; 
ficon.Transparent:=true; 
ficon.OnChange:=IconChange; 
fhint.Color:=clInfoBk; 
fcallinheritedevent:=false; 
fdefaultwidth:=GetSystemMetrics(SM_CXSIZE); 
if fwidth<fdefaultwidth then  fwidth:=fdefaultwidth;
fdefaultheight:=GetSystemMetrics(SM_CYSIZE); 
fborder3d:=GetSystemMetrics(SM_CYEDGE); 
fborderthickness:=GetSystemMetrics(SM_CYSIZEFRAME); 
gtmp3:=false; 
end; 

procedure TTitleButton.IconChange(Sender:tobject); 
begin 
parentpaint(fparent); 
end; 

procedure TTitleButton.messagehandler(var msg:tmessage); 
begin 
if csdesigning in componentstate then 
  begin 
   if msg.Msg=TTB_SETBUTTONUP then 
    begin 
     ButtonUp(msg); 
     if (assigned(pmsghandler)) and (fcallinheritedevent) then 
      pmsghandler(msg); 
    end 
   else 
    pmsghandler(msg); 
  end 
else 
  begin 
   if msg.Msg=WM_NCPAINT then 
    begin 
     CaptionPaint(msg); 
     if (assigned(pmsghandler)) and (fcallinheritedevent) then 
      pmsghandler(msg); 
    end 
   else if msg.Msg=WM_NCLBUTTONDOWN then 
    begin 
     CaptionMouseDown(msg); 
     if (assigned(pmsghandler)) and (fcallinheritedevent) then 
      pmsghandler(msg); 
    end 
   else if msg.Msg=WM_NCMOUSEMOVE then 
    begin 
     CaptionMouseMove(msg); 
     if (assigned(pmsghandler)) and (fcallinheritedevent) then 
      pmsghandler(msg); 
    end 
   else if msg.Msg=WM_NCLBUTTONUP then 
    begin 
     CaptionMouseUp(msg); 
     if (assigned(pmsghandler)) and (fcallinheritedevent) then 
      pmsghandler(msg); 
    end 
   else if msg.Msg=WM_NCACTIVATE then 
    begin 
     CaptionActivate(msg); 
     if (assigned(pmsghandler)) and (fcallinheritedevent) then 
      pmsghandler(msg); 
    end 
   else if msg.Msg=WM_NCHITTEST then 
    begin 
     CaptionHitTest(msg); 
     if (assigned(pmsghandler)) and (fcallinheritedevent) then 
      pmsghandler(msg); 
    end 
   else if msg.Msg=WM_LBUTTONUP then 
    begin 
     ParentMouseUp(msg); 
     if (assigned(pmsghandler)) and (fcallinheritedevent) then 
      pmsghandler(msg); 
    end 
   else if msg.Msg=WM_MOUSEMOVE then 
    begin 
     ParentMouseMove(msg); 
     if (assigned(pmsghandler)) and (fcallinheritedevent) then 
      pmsghandler(msg); 
    end 
   else if msg.Msg=WM_NCRBUTTONDOWN then 
    begin 
     CaptionRightMouseDown(msg); 
     if (assigned(pmsghandler)) and (fcallinheritedevent) then 
      pmsghandler(msg); 
    end 
   else if msg.Msg=WM_NCLBUTTONDBLCLK then 
    begin 
     CaptionDoubleClick(msg); 
     if (assigned(pmsghandler)) and (fcallinheritedevent) then 
      pmsghandler(msg); 
    end 
   else if msg.Msg=WM_NCLBUTTONDBLCLK then 
    begin 
     CaptionDoubleClick(msg); 
     if (assigned(pmsghandler)) and (fcallinheritedevent) then 
      pmsghandler(msg); 
    end 
   else if msg.Msg=WM_SETTEXT then 
    begin 
     CaptionChange(msg); 
     if (assigned(pmsghandler)) and (fcallinheritedevent) then 
      pmsghandler(msg); 
    end 
   else if msg.Msg=WM_SETTINGCHANGE then 
    begin 
     DisplaySettingChange(msg); 
     if (assigned(pmsghandler)) and (fcallinheritedevent) then 
      pmsghandler(msg); 
    end 
   else if msg.Msg=TTB_SETBUTTONUP then 
    begin 
     ButtonUp(msg); 
     if (assigned(pmsghandler)) and (fcallinheritedevent) then 
      pmsghandler(msg); 
    end 
   else 
    pmsghandler(msg); 
end; 
end; 

procedure TTitleButton.CaptionPaint(var msg:tmessage); 
begin 
fcallinheritedevent:=true; 
if fvisible=false then 
  exit; 
invalidaterect(fparent.handle,@fbuttonrect,false); 
end; 

procedure TTitleButton.CaptionMouseMove(var msg:tmessage); 
var 
pt:tpoint; 
tmpstate:tshiftstate; 
fhintwidth:integer; 
begin 
fcallinheritedevent:=true; 
if fvisible=false then 
  exit; 
gtmp1:=fpressed; 
gtmp2:=ffocused; 
pt.x:=msg.LParamLo-fparent.left; 
pt.y:=msg.LParamHi-fparent.top; 
if PtInRect(fbuttonrect,pt) then 
  begin 
   ffocused:=true; 
   {if (gtmp1<>fpressed) or (gtmp2<>ffocused) then 
    begin 
     invalidaterect(fparent.handle,@fbuttonrect,false); 
     gtmp1:=fpressed; 
     gtmp2:=ffocused; 
    end;} 
   fhintwidth:=fhint.Canvas.TextWidth(fhinttext); 
   if (fhintshow=false) and (length(trim(fhinttext))<>0) then 
    fhint.ActivateHint(rect(mouse.cursorpos.x,mouse.cursorpos.y+10,mouse.cursorpos.x+fhintwidth+7,mouse.cursorpos.y+25),fhinttext); 
   fhintshow:=true; 
   if assigned(fbuttonmove) then 
    fbuttonmove(fparent,tmpstate,pt.x,pt.y); 
  end 
else 
  begin 
   ffocused:=false; 
   fhint.ReleaseHandle; 
   fhintshow:=false; 
  end; 
fcallinheritedevent:=true; 
end; 

procedure TTitleButton.CaptionMouseDown(var msg:tmessage); 
var 
pt:tpoint; 
tmp1:boolean; 
callevent:boolean; 
begin 
callevent:=false; 
fcallinheritedevent:=true; 
if fvisible=false then 
  exit; 
fhintshow:=false; 
fhint.releasehandle; 
if fhintshow=true then 
  fhint.ReleaseHandle; 
setforegroundwindow(fparent.handle); 
tmp1:=fpressed; 
pt.x:=msg.LParamLo-fparent.left; 
pt.y:=msg.LParamhi-fparent.top; 
if ptinrect(fbuttonrect,pt) then 
  begin 
   gtmp3:=true; 
   if fgroupindex=0 then 
    begin 
     callevent:=true; 
    end 
   else 
    begin 
     if not(fdown) then 
      if assigned(fbuttondown) then 
       fbuttondown(fparent); 
    end; 
   fpressed:=true; 
   ffocused:=true; 
   setcapture(fparent.handle); 
  end 
else 
  begin 
   fpressed:=false; 
   ffocused:=false; 
  end; 
if (tmp1<>fpressed) then 
  fcallinheritedevent:=false; 
gtmp1:=fpressed; 
gtmp2:=ffocused; 
  parentpaint(fparent); 
if (callevent) and assigned(fbuttondown) then 
  fbuttondown(fparent); 
end; 

procedure TTitleButton.CaptionMouseUp(var msg:tmessage); 
var 
pt:Tpoint; 
tmp1,tmp2:boolean; 
begin 
fcallinheritedevent:=true; 
if fvisible=false then 
  exit; 
releasecapture; 
tmp1:=fpressed; 
tmp2:=ffocused; 
pt.x:=msg.LParamLo-fparent.left; 
pt.y:=msg.LParamhi-fparent.top; 
if (ptinrect(fbuttonrect,pt)) and (ffocused = true) then 
  fpressed:=false 
else 
  ffocused:=false; 
if ((tmp1<>fpressed) or (tmp2<>ffocused)) and (fallowallup and fdown)  then 
  invalidaterect(fparent.handle,@fbuttonrect,true); 
fcallinheritedevent:=true; 
end; 

procedure TTitleButton.CaptionRightMouseDown(var msg:tmessage); 
var 
pt:tpoint; 
begin 
fcallinheritedevent:=true; 
if fvisible=false then 
  exit; 
fhint.releasehandle; 
pt.x:=msg.LParamlo-fparent.left; 
pt.y:=msg.LParamHi-fparent.top; 
if not ptinrect(fbuttonrect,pt) then 
  fcallinheritedevent:=true 
else 
  fcallinheritedevent:=false; 
end; 

procedure TTitleButton.CaptionDoubleClick(var msg:tmessage); 
var 
pt:tpoint; 
begin 
fcallinheritedevent:=true; 
if fvisible=false then 
  exit; 
pt.x:=msg.LParamlo-fparent.left; 
pt.y:=msg.LParamhi-fparent.top; 
if not(ptinrect(fbuttonrect,pt)) then 
  fcallinheritedevent:=true 
else 
  begin 
   fcallinheritedevent:=false; 
   fparent.perform(WM_NCLBUTTONDOWN,msg.wparam,msg.LParam); 
  end; 
end; 


procedure TTitleButton.CaptionActivate(var msg:tmessage); 
begin 
fcallinheritedevent:=true; 
if not visible then 
  exit; 
invalidaterect(fparent.handle,@fbuttonrect,false); 
end; 

procedure TTitleButton.CaptionHitTest(var msg:Tmessage); 
var 
tmp:boolean; 
pt:tpoint; 
begin 
fcallinheritedevent:=true; 
if fvisible=false then 
  exit; 
if fpressed then 
  begin 
   tmp:=ffocused; 
   pt.x:=msg.LParamlo-fparent.left; 
   pt.y:=msg.LParamhi-fparent.top; 
   if ptinrect(fbuttonrect,pt) then 
    begin 
     ffocused:=true 
    end 
   else 
    ffocused:=false; 
    if ffocused<>tmp then 
     invalidaterect(fparent.handle,@fbuttonrect,false); 
  end; 
if ffocused=false then 
  fhint.releasehandle; 
gtmp1:=fpressed; 
gtmp2:=ffocused; 
fcallinheritedevent:=true; 
end; 

procedure TTitleButton.CaptionChange(var msg:Tmessage); 
begin 
fcallinheritedevent:=true; 
if not fvisible then 
  exit; 
invalidaterect(fparent.handle,@fbuttonrect,false); 
end; 

procedure TTitleButton.ButtonUp(var msg:tmessage); 
var 
sender:ttitlebutton; 
tmp:boolean; 
begin 
tmp:=fdown; 
fcallinheritedevent:=true; 
sender:=(tcomponent(msg.WParam) as ttitlebutton); 
if (sender<>self) and (msg.LParamLo=fgroupindex) then 
  begin 
   if tchangedproperty(msg.lparamhi)=cpdown then 
    fdown:=false; 
   fallowallup:=sender.fallowallup; 
   if tmp<>fdown then 
    invalidaterect(fparent.handle,@fbuttonrect,false); 
  end; 
end; 

procedure TTitleButton.ParentMouseMove(var msg:tmessage); 
var 
pt:tpoint; 
tmppt:tpoint; 
tmprect:trect; 
tmpstate:Tshiftstate; 
begin 
fcallinheritedevent:=true; 
if fvisible=false then 
  exit; 
ffocused:=false; 
pt.x:=msg.lparamlo; 
pt.y:=msg.lparamhi-fparent.top; 
tmppt:=pt; 
tmppt.x:=tmppt.x+4; 
tmppt.y:=65536-tmppt.y-fparent.top; 
tmprect:=fbuttonrect; 
inflaterect(tmprect,1,1); 
if ptinrect(tmprect,tmppt) then 
  begin 
   ffocused:=true; 
   if assigned(fbuttonmove) then 
    fbuttonmove(fparent,tmpstate,tmppt.x,tmppt.y); 
   if (gtmp1<>fpressed) or (gtmp2<>ffocused) then// if fpressed then 
    begin 
     invalidaterect(fparent.handle,@fbuttonrect,false); 
     gtmp1:=fpressed; 
     gtmp2:=ffocused; 
    end; 
  end; 
if (gtmp1<>fpressed) or (gtmp2<>ffocused) then 
  begin 
   invalidaterect(fparent.handle,@fbuttonrect,false); 
   gtmp1:=fpressed; 
   gtmp2:=ffocused; 
  end; 
fhintshow:=false; 
fhint.releasehandle; 
end; 


procedure TTitleButton.ParentMouseUp(var msg:tmessage); 
var 
pt:tpoint; 
tmp:tpoint; 
tmprect:trect; 
tmpcallevent:boolean; 
begin 
fcallinheritedevent:=true; 
if fvisible=false then 
  exit; 
tmpcallevent:=false; 
fhint.ReleaseHandle; 
fhintshow:=true; 
ReleaseCapture; 
fpressed:=false; 
pt.x:=msg.lParamlo; 
pt.y:=msg.lParamhi-fparent.top; 
tmp:=pt; 
tmp.x:=tmp.x+4; 
tmp.y:=65536-tmp.y; 
tmp.y:=tmp.y-fparent.top; 
tmprect:=fbuttonrect; 
inflaterect(tmprect,0,2); 
if tmp.y<(fparent.top+fparent.Height) then 
  pt:=tmp; 
if (ptinrect(tmprect,pt)) and (ffocused) and (gtmp3) then 
  begin 
   if fgroupindex<>0 then 
    begin 
     if fallowallup=true then 
      fdown:=not(fdown) 
     else 
      fdown:=true; 
     gtmp3:=false; 
     updateproperties(cpdown); 
     if not(fdown) then 
      tmpcallevent:=true; 
    end 
   else 
    tmpcallevent:=true; 
   parentpaint(fparent); 
   if (tmpcallevent=true) and assigned(fbuttonup) then 
    fbuttonup(fparent); 
  end 
else 
  gtmp3:=false; 
fcallinheritedevent:=true; 
end; 

procedure TTitleButton.parentpaint(sender:tobject); 
var 
ButtonCanvas:TCanvas; 
textrect:trect; 
iconrect:trect; 
tmpwidth:integer; 
begin 
if fvisible=false then 
  begin 
   if assigned(ppaint)then 
    ppaint(sender); 
   exit; 
  end; 
if not(csdesigning in componentstate) then 
  begin 
   if fwidth<fdefaultwidth then    fwidth:=fdefaultwidth;
   if fleft=0 then 
    fleft:=fwidth+1; 
   fbuttonrect.left:=fparent.width-fleft-(3*fdefaultwidth)-(fborder3d+fborderthickness); 
   fbuttonrect.right:=fbuttonrect.left+fwidth; 
   fbuttonrect.top:=fborder3d+fborderthickness; 
   fbuttonrect.bottom:=fbuttonrect.top+fdefaultheight-(2*fborder3d); 
   ButtonCanvas:=tcanvas.Create; 
   ButtonCanvas.Handle:=getwindowdc(fparent.handle); 
   fillrect(buttoncanvas.Handle,fbuttonrect,HBRUSH(COLOR_BTNFACE+1)); 
   tmpwidth:=fdefaultheight-2; 
   iconrect.left:=fbuttonrect.left; 
   iconrect.top:=fbuttonrect.top; 
   iconrect.right:=iconrect.left+tmpwidth; 
   iconrect.bottom:=fbuttonrect.top+fdefaultheight-2*fborder3d; 
   if ficon.handle<>0 then 
    subtractrect(textrect,fbuttonrect,iconrect) 
   else 
    textrect:=fbuttonrect; 
   if (ffocused and fpressed) or fdown then 
    begin 
     drawedge(ButtonCanvas.Handle,fbuttonrect,EDGE_SUNKEN,BF_SOFT or BF_RECT); 
     textrect.left:=textrect.left+2; 
     textrect.Top:=textrect.Top+1; 
     textrect.right:=textrect.right-1; 
     iconrect.left:=iconrect.left+3; 
     iconrect.top:=iconrect.top+2; 
    end; 
   if (not(fpressed) or not(ffocused)) and not(fdown) then 
    begin 
     drawedge(ButtonCanvas.Handle,fbuttonrect,EDGE_RAISED,BF_SOFT or BF_RECT); 
     textrect.left:=textrect.left+1; 
     textrect.right:=textrect.right-1; 
     iconrect.top:=iconrect.top+1; 
     iconrect.left:=iconrect.left+2; 
   end; 
   ButtonCanvas.Brush.Style:=bsclear; 
   ButtonCanvas.Font.assign(ffont); 
   if ficon.Handle<>0  then 
    begin 
     drawiconex(buttoncanvas.handle,iconrect.left+1,iconrect.top+1,ficon.handle,tmpwidth-5,fdefaultheight-8,0,0,DI_NORMAL); 
     if length(trim(fbuttoncaption))>0 then 
      DrawTextEx(ButtonCanvas.Handle,PChar(fButtonCaption),Length(fbuttoncaption),textrect,DT_LEFT or DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS or DT_PATH_ELLIPSIS or DT_MODIFYSTRING,nil); 
    end 
   else 
    DrawText(ButtonCanvas.Handle,PChar(fButtonCaption),Length(fbuttoncaption),textrect,DT_CENTER or DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS or DT_PATH_ELLIPSIS or DT_MODIFYSTRING); 
   ButtonCanvas.Free; 
   if assigned(ppaint) then 
    ppaint(sender); 
  end; 
end; 

procedure TTitleButton.parentresize(sender:tobject); 
begin 
fcallinheritedevent:=true; 
if fvisible=false then 
  begin 
   if assigned(presize) then 
    presize(sender); 
   exit; 
  end; 
parentpaint(sender); 
if assigned(presize) then 
  presize(self); 
end; 


procedure TTitleButton.DisplaySettingChange(var msg:tmessage); 
begin 
fcallinheritedevent:=true; 
if fvisible=false then 
  exit; 
fdefaultwidth:=GetSystemMetrics(SM_CXSIZE); 
if fwidth<fdefaultwidth then  fwidth:=fdefaultwidth;
fdefaultheight:=GetSystemMetrics(SM_CYSIZE); 
fborder3d:=GetSystemMetrics(SM_CYEDGE); 
fborderthickness:=GetSystemMetrics(SM_CYSIZEFRAME); 
parentpaint(fparent); 
msg.result:=0; 
end; 

procedure TTitleButton.setbuttonwidth(awidth:integer); 
begin 
if awidth>0 then 
  fwidth:=awidth 
else 
  fwidth:=fdefaultwidth; 
parentpaint(fparent); 
end; 

procedure TTitleButton.setbuttonleft(aleft:integer); 
begin
if (aleft<FPARENT.LEFT+FPARENT.WIDTH) then fleft:=aleft;
parentpaint(fparent); 
end; 

procedure TTitleButton.setbuttoncaption(acaption:string); 
begin 
fbuttoncaption:=acaption; 
parentpaint(fparent); 
end; 

procedure TTitleButton.setbuttonfont(afont:tfont); 
begin 
ffont.assign(afont); 
parentpaint(fparent); 
end; 

procedure TTitleButton.seticon(aicon:ticon); 
begin 
ficon.assign(aicon); 
parentpaint(fparent); 
end; 

procedure TTitleButton.setbuttonvisible(avisible:boolean); 
begin 
fvisible:=avisible; 
fparent.perform(WM_NCACTIVATE,integer(true),0); 
end; 


procedure TTitleButton.setdown(adown:boolean); 
var 
tmp:boolean; 
begin 
tmp:=fdown; 
if csloading in componentstate then 
  fdown:=adown 
else 
  begin 
   if fdown<>adown then 
    begin 
     if fgroupindex=0 then 
      fdown:=false 
     else 
      begin 
       if fallowallup=true then 
        fdown:=adown 
       else 
        fdown:=true; 
      end; 
    end; 
  end; 
if tmp<>fdown then 
  updateproperties(cpdown); 

end; 

procedure TTitleButton.setallowallup(aallowallup:boolean); 
var 
tmp:boolean; 
begin 
fcallinheritedevent:=true; 
tmp:=fallowallup; 
if csloading in componentstate then 
  fallowallup:=aallowallup 
else 
  begin 
   if fgroupindex<>0 then 
    fallowallup:=aallowallup; 
   if tmp<>fallowallup then 
    updateproperties(cpallowallup); 
  end; 
end; 


procedure TTitleButton.setgroupindex(agroupindex:integer); 
var 
tmp:integer; 
begin 
tmp:=fgroupindex; 
if csloading in componentstate then 
  fgroupindex:=agroupindex 
else 
  begin 
   if agroupindex >=65535 then 
    agroupindex:=0; 
   if (agroupindex>=0) then 
    fgroupindex:=agroupindex; 
   if fgroupindex=0 then 
    begin 
     fallowallup:=false; 
     fdown:=false; 
    end;   
   if tmp<>fgroupindex then 
    updateproperties(cpgroupindex); 
  end;   
end; 

procedure Register; 
begin 
  RegisterComponents('Sajat', [TTitleButton]);
end; 

end.
