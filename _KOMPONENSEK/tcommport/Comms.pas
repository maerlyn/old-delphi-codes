// ------- this is a freeware --------
// TComPort component, version 1.01
//   for Delphi 2.0, 3.0, 4.0
// written by Dejan Crnila
//   email: emilija.crnila@guest.arnes.si
// ------- this is a freeware --------

unit Comms;

interface

uses
  Windows, Classes, SysUtils;

type
  TBaudRate = (br110, br300, br600, br1200, br2400, br4800, br9600,
               br14400, br19200, br38400, br56000, br57600, br115200);
  TPortType = (COM1, COM2, COM3, COM4);
  TStopBits = (sbOneStopBit, sbOne5StopBits, sbTwoStopBits);
  TParity = (prNone, prOdd, prEven, prMark, prSpace);
  TFlowControl = (fcNone, fcRtsCts, fcXonXoff);
  TEvent = (evRxChar, evTxEmpty, evRxFlag, evRing, evBreak, evCTS,
            evDSR, evError, evRLSD);
  TEvents = set of TEvent;

  TRxCharEvent = procedure(Sender: TObject; InQue: Integer) of object;

  TComPort = class;

  TComThread = class(TThread)
  private
    Owner: TComPort;
    Mask: DWORD;
    StopEvent: THandle;
  protected
    procedure Execute; override;
    procedure DoEvents;
    procedure Stop;
  public
    constructor Create(AOwner: TComPort);
    destructor Destroy; override;
  end;

  TComPort = class(TComponent)
  private
    ComHandle: THandle;
    EventThread: TComThread;
    FConnected: Boolean;
    FBaudRate: TBaudRate;
    FPortType: TPortType;
    FParity: TParity;
    FStopBits: TStopBits;
    FFlowControl: TFlowControl;
    FDataBits: Byte;
    FEvents: TEvents;
    FEnableDTR: Boolean;
    FWriteBufSize: Integer;
    FReadBufSize: Integer;
    FOnRxChar: TRxCharEvent;
    FOnTxEmpty: TNotifyEvent;
    FOnBreak: TNotifyEvent;
    FOnRing: TNotifyEvent;
    FOnCTS: TNotifyEvent;
    FOnDSR: TNotifyEvent;
    FOnRLSD: TNotifyEvent;
    FOnError: TNotifyEvent;
    FOnRxFlag: TNotifyEvent;
    FOnOpen: TNotifyEvent;
    FOnClose: TNotifyEvent;
    procedure SetDataBits(Value: Byte);
    function ComString: String;
    procedure DoOnRxChar;
    procedure DoOnTxEmpty;
    procedure DoOnBreak;
    procedure DoOnRing;
    procedure DoOnRxFlag;
    procedure DoOnCTS;
    procedure DoOnDSR;
    procedure DoOnError;
    procedure DoOnRLSD;
  protected
    procedure CreateHandle;
    procedure DestroyHandle;
    procedure SetupState;
    function ValidHandle: Boolean;
  public
    property Connected: Boolean read FConnected;
    procedure Open;
    procedure Close;
    function InQue: Integer;
    function OutQue: Integer;
    function ActiveCTS: Boolean;
    function ActiveDSR: Boolean;
    function ActiveRLSD: Boolean;
    function Write(var Buffer; Count: Integer): Integer;
    function WriteString(Str: String): Integer;
    function Read(var Buffer; Count: Integer): Integer;
    function ReadString(var Str: String; Count: Integer): Integer;
    procedure PurgeIn;
    procedure PurgeOut;
    function GetComHandle: THandle;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BaudRate: TBaudRate read FBaudRate write FBaudRate;
    property Port: TPortType read FPortType write FPortType;
    property Parity: TParity read FParity write FParity;
    property StopBits: TStopBits read FStopBits write FStopBits;
    property FlowControl: TFlowControl read FFlowControl write FFlowControl;
    property DataBits: Byte read FDataBits write SetDataBits;
    property Events: TEvents read FEvents write FEvents;
    property EnableDTR: Boolean read FEnableDTR write FEnableDTR;
    property WriteBufSize: Integer read FWriteBufSize write FWriteBufSize;
    property ReadBufSize: Integer read FReadBufSize write FReadBufSize;
    property OnRxChar: TRxCharEvent read FOnRxChar write FOnRxChar;
    property OnTxEmpty: TNotifyEvent read FOnTxEmpty write FOnTxEmpty;
    property OnBreak: TNotifyEvent read FOnBreak write FOnBreak;
    property OnRing: TNotifyEvent read FOnRing write FOnRing;
    property OnCTS: TNotifyEvent read FOnCTS write FOnCTS;
    property OnDSR: TNotifyEvent read FOnDSR write FOnDSR;
    property OnRLSD: TNotifyEvent read FOnRLSD write FOnRLSD;
    property OnRxFlag: TNotifyEvent read FOnRxFlag write FOnRxFlag;
    property OnError: TNotifyEvent read FOnError write FOnError;
    property OnOpen: TNotifyEvent read FOnOpen write FOnOpen;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
  end;

  EComHandle = class(Exception);
  EComState  = class(Exception);
  EComWrite  = class(Exception);
  EComRead   = class(Exception);
  EComStatus = class(Exception);
  EComPurge = class(Exception);

const
  dcb_Binary           = $00000001;
  dcb_Parity           = $00000002;
  dcb_OutxCtsFlow      = $00000004;
  dcb_OutxDsrFlow      = $00000008;
  dcb_DtrControl       = $00000030;
  dcb_DsrSensivity     = $00000040;
  dcb_TXContinueOnXOff = $00000080;
  dcb_OutX             = $00000100;
  dcb_InX              = $00000200;
  dcb_ErrorChar        = $00000400;
  dcb_Null             = $00000800;
  dcb_RtsControl       = $00003000;
  dcb_AbortOnError     = $00004000;

function ShowPropForm(ComPort: TComPort): Boolean;
procedure Register;

implementation

uses DsgnIntf, CommForm, Controls;

type
  TComPortEditor = class(TComponentEditor)
  private
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

// Component code

function LastErr: String;
begin
  Result := IntToStr(GetLastError);
end;

constructor TComThread.Create(AOwner: TComPort);
var AMask: Integer;
begin
  inherited Create(True);
  StopEvent := CreateEvent(nil, True, False, nil);
  Owner := AOwner;
  AMask := 0;
  if evRxChar in Owner.FEvents then AMask := AMask or EV_RXCHAR;
  if evRxFlag in Owner.FEvents then AMask := AMask or EV_RXFLAG;
  if evTxEmpty in Owner.FEvents then AMask := AMask or EV_TXEMPTY;
  if evRing in Owner.FEvents then AMask := AMask or EV_RING;
  if evCTS in Owner.FEvents then AMask := AMask or EV_CTS;
  if evDSR in Owner.FEvents then AMask := AMask or EV_DSR;
  if evRLSD in Owner.FEvents then AMask := AMask or EV_RLSD;
  if evError in Owner.FEvents then AMask := AMask or EV_ERR;
  if evBreak in Owner.FEvents then AMask := AMask or EV_BREAK;
  SetCommMask(Owner.ComHandle, AMask);
  Resume;
end;

procedure TComThread.Execute;
var EventHandles: Array[0..1] of THandle;
    Overlapped: TOverlapped;
    dwSignaled, BytesTrans: DWORD;
begin
  FillChar(Overlapped, SizeOf(Overlapped), 0);
  Overlapped.hEvent := CreateEvent(nil, True, True, nil);
  EventHandles[0] := StopEvent;
  EventHandles[1] := Overlapped.hEvent;
  repeat
    WaitCommEvent(Owner.ComHandle, Mask, @Overlapped);
    dwSignaled := WaitForMultipleObjects(2, @EventHandles, False, INFINITE);
    case dwSignaled of
      WAIT_OBJECT_0:Break;
      WAIT_OBJECT_0 + 1: if GetOverlappedResult(Owner.ComHandle, Overlapped,
                              BytesTrans, False) then Synchronize(DoEvents);
      else Break;
    end;
  until False;
  Owner.PurgeIn;
  Owner.PurgeOut;
  CloseHandle(Overlapped.hEvent);
  CloseHandle(StopEvent);
end;

procedure TComThread.Stop;
begin
  SetEvent(StopEvent);
end;

destructor TComThread.Destroy;
begin
  Stop;
  inherited Destroy;
end;

procedure TComThread.DoEvents;
begin
  if (EV_RXCHAR and Mask) > 0 then Owner.DoOnRxChar;
  if (EV_TXEMPTY and Mask) > 0 then Owner.DoOnTxEmpty;
  if (EV_BREAK and Mask) > 0 then Owner.DoOnBreak;
  if (EV_RING and Mask) > 0 then Owner.DoOnRing;
  if (EV_CTS and Mask) > 0 then Owner.DoOnCTS;
  if (EV_DSR and Mask) > 0 then Owner.DoOnDSR;
  if (EV_RXFLAG and Mask) > 0 then Owner.DoOnRxFlag;
  if (EV_RLSD and Mask) > 0 then Owner.DoOnRLSD;
  if (EV_ERR and Mask) > 0 then Owner.DoOnError;
end;

constructor TComPort.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnected := false;
  FBaudRate := br9600;
  FParity := prNone;
  FPortType := COM1;
  FStopBits := sbOneStopBit;
  FDataBits := 8;
  FEvents := [evRxChar, evTxEmpty, evRxFlag, evRing, evBreak,
             evCTS, evDSR, evError, evRLSD];
  FEnableDTR := True;
  FWriteBufSize := 2048;
  FReadBufSize := 2048;
  ComHandle := INVALID_HANDLE_VALUE;
end;

destructor TComPort.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TComPort.CreateHandle;
begin
  ComHandle := CreateFile(
    PChar(ComString),
    GENERIC_READ or GENERIC_WRITE,
    0,
    nil,
    OPEN_EXISTING,
    FILE_FLAG_OVERLAPPED,
    0);

  if not ValidHandle then
    raise EComHandle.Create('Unable to open com port: ' + LastErr);
end;

procedure TComPort.DestroyHandle;
begin
  if ValidHandle then
    CloseHandle(ComHandle);
end;

function TComPort.ValidHandle: Boolean;
begin
  if ComHandle = INVALID_HANDLE_VALUE then
    Result := False
  else
    Result := True;
end;

procedure TComPort.Open;
begin
  Close;
  CreateHandle;
  SetupState;
  EventThread := TComThread.Create(Self);
  FConnected := True;
  if Assigned(FOnOpen) then FOnOpen(Self);
end;

procedure TComPort.Close;
begin
  if FConnected then begin
    EventThread.Free;
    DestroyHandle;
    FConnected := False;
    if Assigned(FOnClose) then FOnClose(Self);
  end;
end;

procedure TComPort.SetupState;
var DCB: TDCB;
    Timeouts: TCommTimeouts;
begin
  FillChar(DCB, SizeOf(DCB), 0);

  DCB.DCBlength := SizeOf(DCB);
  DCB.XonChar := #17;
  DCB.XoffChar := #19;
  DCB.XonLim := FWriteBufSize div 4;
  DCB.XoffLim := 1;

  DCB.Flags := DCB.Flags or dcb_Binary;
  if FEnableDTR then
    DCB.Flags := DCB.Flags or (dcb_DtrControl and (DTR_CONTROL_ENABLE shl 4));

  case FFlowControl of
    fcRtsCts: begin
      DCB.Flags := DCB.Flags or dcb_OutxCtsFlow or
        (dcb_RtsControl and (RTS_CONTROL_HANDSHAKE shl 12));
    end;
    fcXonXoff: DCB.Flags := DCB.Flags or dcb_OutX or dcb_InX;
  end;

  case FParity of
    prNone:  DCB.Parity := NOPARITY;
    prOdd:   DCB.Parity := ODDPARITY;
    prEven:  DCB.Parity := EVENPARITY;
    prMark:  DCB.Parity := MARKPARITY;
    prSpace: DCB.Parity := SPACEPARITY;
  end;

  case FStopBits of
    sbOneStopBit:   DCB.StopBits := ONESTOPBIT;
    sbOne5StopBits: DCB.StopBits := ONE5STOPBITS;
    sbTwoStopBits:  DCB.StopBits := TWOSTOPBITS;
  end;

  case FBaudRate of
    br110:    DCB.BaudRate := CBR_110;
    br300:    DCB.BaudRate := CBR_300;
    br600:    DCB.BaudRate := CBR_600;
    br1200:   DCB.BaudRate := CBR_1200;
    br2400:   DCB.BaudRate := CBR_2400;
    br4800:   DCB.BaudRate := CBR_4800;
    br9600:   DCB.BaudRate := CBR_9600;
    br14400:  DCB.BaudRate := CBR_14400;
    br19200:  DCB.BaudRate := CBR_19200;
    br38400:  DCB.BaudRate := CBR_38400;
    br56000:  DCB.BaudRate := CBR_56000;
    br57600:  DCB.BaudRate := CBR_57600;
    br115200: DCB.BaudRate := CBR_115200;
  end;

  DCB.ByteSize := FDataBits;

  if not SetCommState(ComHandle, DCB) then
    raise EComState.Create('Unable to set com state: ' + LastErr);

  if not GetCommTimeouts(ComHandle, Timeouts) then
    raise EComState.Create('Unable to set com state: ' + LastErr);

  Timeouts.ReadIntervalTimeout := MAXDWORD;
  Timeouts.ReadTotalTimeoutMultiplier := 0;
  Timeouts.ReadTotalTimeoutConstant := 0;
  Timeouts.WriteTotalTimeoutMultiplier := 1000;
  Timeouts.WriteTotalTimeoutConstant := 1500;

  if not SetCommTimeouts(ComHandle, Timeouts) then
    raise EComState.Create('Unable to set com state: ' + LastErr);

  if not SetupComm(ComHandle, FReadBufSize, FWriteBufSize) then
    raise EComState.Create('Unable to set com state: ' + LastErr);
end;

function TComPort.InQue: Integer;
var Errors: DWORD;
    ComStat: TComStat;
begin
  if not ClearCommError(ComHandle, Errors, @ComStat) then
    raise EComStatus.Create('Unable to read com status: ' + LastErr);
  Result := ComStat.cbInQue;
end;

function TComPort.OutQue: Integer;
var Errors: DWORD;
    ComStat: TComStat;
begin
  if not ClearCommError(ComHandle, Errors, @ComStat) then
    raise EComStatus.Create('Unable to read com status: ' + LastErr);
  Result := ComStat.cbOutQue;
end;

function TComPort.ActiveCTS: Boolean;
var Errors: DWORD;
    ComStat: TComStat;
begin
  if not ClearCommError(ComHandle, Errors, @ComStat) then
    raise EComStatus.Create('Unable to read com status: ' + LastErr);
  Result := not (fCtlHold in ComStat.Flags);
end;

function TComPort.ActiveDSR: Boolean;
var Errors: DWORD;
    ComStat: TComStat;
begin
  if not ClearCommError(ComHandle, Errors, @ComStat) then
    raise EComStatus.Create('Unable to read com status: ' + LastErr);
  Result := not (fDsrHold in ComStat.Flags);
end;

function TComPort.ActiveRLSD: Boolean;
var Errors: DWORD;
    ComStat: TComStat;
begin
  if not ClearCommError(ComHandle, Errors, @ComStat) then
    raise EComStatus.Create('Unable to read com status: ' + LastErr);
  Result := not (fRlsHold in ComStat.Flags);
end;

function TComPort.Write(var Buffer; Count: Integer): Integer;
var Overlapped: TOverlapped;
    BytesWritten: DWORD;
begin
  FillChar(Overlapped, SizeOf(Overlapped), 0);
  Overlapped.hEvent := CreateEvent(nil, True, True, nil);
  WriteFile(ComHandle, Buffer, Count, BytesWritten, @Overlapped);

  WaitForSingleObject(Overlapped.hEvent, INFINITE);
  if not GetOverlappedResult(ComHandle, Overlapped, BytesWritten, False) then
    raise EWriteError.Create('Unable to write to port: ' + LastErr);
  CloseHandle(Overlapped.hEvent);
  Result := BytesWritten;
end;

function TComPort.WriteString(Str: String): Integer;
var Overlapped: TOverlapped;
    BytesWritten: DWORD;
begin
  FillChar(Overlapped, SizeOf(Overlapped), 0);
  Overlapped.hEvent := CreateEvent(nil, True, True, nil);
  WriteFile(ComHandle, Str[1], Length(Str), BytesWritten, @Overlapped);

  WaitForSingleObject(Overlapped.hEvent, INFINITE);
  if not GetOverlappedResult(ComHandle, Overlapped, BytesWritten, False) then
    raise EWriteError.Create('Unable to write to port: ' + LastErr);
  CloseHandle(Overlapped.hEvent);
  Result := BytesWritten;
end;

function TComPort.Read(var Buffer; Count: Integer): Integer;
var Overlapped: TOverlapped;
    BytesRead: DWORD;
begin
  FillChar(Overlapped, SizeOf(Overlapped), 0);
  Overlapped.hEvent := CreateEvent(nil, True, True, nil);
  ReadFile(ComHandle, Buffer, Count, BytesRead, @Overlapped);
  WaitForSingleObject(Overlapped.hEvent, INFINITE);
  if not GetOverlappedResult(ComHandle, Overlapped, BytesRead, False) then
    raise EWriteError.Create('Unable to write to port: ' + LastErr);
  CloseHandle(Overlapped.hEvent);
  Result := BytesRead;
end;

function TComPort.ReadString(var Str: String; Count: Integer): Integer;
var Overlapped: TOverlapped;
    BytesRead: DWORD;
begin
  SetLength(Str, Count);
  FillChar(Overlapped, SizeOf(Overlapped), 0);
  Overlapped.hEvent := CreateEvent(nil, True, True, nil);
  ReadFile(ComHandle, Str[1], Count, BytesRead, @Overlapped);
  WaitForSingleObject(Overlapped.hEvent, INFINITE);
  if not GetOverlappedResult(ComHandle, Overlapped, BytesRead, False) then
    raise EWriteError.Create('Unable to write to port: ' + LastErr);
  CloseHandle(Overlapped.hEvent);
  SetLength(Str, BytesRead);
  Result := BytesRead;
end;

procedure TComPort.PurgeIn;
begin
  if not PurgeComm(ComHandle, PURGE_RXABORT or PURGE_RXCLEAR) then
    raise EComPurge.Create('Unable to purge com: ' + LastErr);
end;

procedure TComPort.PurgeOut;
begin
  if not PurgeComm(ComHandle, PURGE_TXABORT or PURGE_TXCLEAR) then
    raise EComPurge.Create('Unable to purge com: ' + LastErr);
end;

function TComPort.GetComHandle: THandle;
begin
  Result := ComHandle;
end;

procedure TComPort.SetDataBits(Value: Byte);
begin
  if Value <> FDataBits then
    if Value > 8 then FDataBits := 8 else
      if Value < 5 then FDataBits := 5 else
        FDataBits := Value;
end;

procedure TComPort.DoOnRxChar;
begin
  if Assigned(FOnRxChar) then FOnRxChar(Self, InQue);
end;

procedure TComPort.DoOnBreak;
begin
  if Assigned(FOnBreak) then FOnBreak(Self);
end;

procedure TComPort.DoOnRing;
begin
  if Assigned(FOnRing) then FOnRing(Self);
end;

procedure TComPort.DoOnTxEmpty;
begin
  if Assigned(FOnTxEmpty) then FOnTxEmpty(Self);
end;

procedure TComPort.DoOnCTS;
begin
  if Assigned(FOnCTS) then FOnCTS(Self);
end;

procedure TComPort.DoOnDSR;
begin
  if Assigned(FOnDSR) then FOnDSR(Self);
end;

procedure TComPort.DoOnRLSD;
begin
  if Assigned(FOnRLSD) then FOnRLSD(Self);
end;

procedure TComPort.DoOnError;
begin
  if Assigned(FOnError) then FOnError(Self);
end;

procedure TComPort.DoOnRxFlag;
begin
  if Assigned(FOnRxFlag) then FOnRxFlag(Self);
end;

function TComPort.ComString: String;
begin
  case FPortType of
    COM1: Result := 'COM1';
    COM2: Result := 'COM2';
    COM3: Result := 'COM3';
    COM4: Result := 'COM4';
  end;
end;

function ShowPropForm(ComPort: TComPort): Boolean;
begin
  with TCommFrm.Create(nil) do begin
    ComboBox1.ItemIndex := Integer(ComPort.Port);
    ComboBox2.ItemIndex := Integer(ComPort.BaudRate);
    ComboBox3.ItemIndex := Integer(ComPort.StopBits);
    ComboBox4.ItemIndex := ComPort.DataBits - 5;
    ComboBox5.ItemIndex := Integer(ComPort.Parity);
    ComboBox6.ItemIndex := Integer(ComPort.FlowControl);
    if ShowModal = mrOK then begin
      ComPort.Port := TPortType(ComboBox1.ItemIndex);
      ComPort.BaudRate := TBaudRate(ComboBox2.ItemIndex);
      ComPort.StopBits := TStopBits(ComboBox3.ItemIndex);
      ComPort.DataBits := ComboBox4.ItemIndex + 5;
      ComPort.Parity := TParity(ComboBox5.ItemIndex);
      ComPort.FlowControl := TFlowControl(ComboBox6.ItemIndex);
      Result := True;
    end
    else
      Result := False;
    Free;
  end;
end;

procedure TComPortEditor.ExecuteVerb(Index: Integer);
begin
  if Index = 0 then
    if ShowPropForm(TComPort(Component)) then
      Designer.Modified;
end;

function TComPortEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Edit Properties';
  end;
end;

function TComPortEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure Register;
begin
  RegisterComponents('Custom', [TComPort]);
  RegisterComponentEditor(TComPort, TComPortEditor);
end;

end.
