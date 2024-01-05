unit ADRLineNotify;

interface

Uses
  SysUtils, Classes, Windows, StrUtils,
  IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient,
  IdHTTP, IdIOHandler, IdIOHandlerSocket,
  IdSSLOpenSSL, IdSSLOpenSSLHeaders, IdGlobal, IdMultipartFormData,
  IdIOHandlerStack, IdSSL;

  {
  ADRLineNotify 1.0
  Base Component Indy 10.6.2.0
  }

type

  TLogMode = (lmComponent, lmLib, lmAll, lmNone);
  TOnLog = procedure (const ALog: string) of object;

  TLineNotify =  class
  private

  FIdHttpRequest: TIdHTTP;
  FRequestParams: TIdMultiPartFormDataStream;
  FIdSSLOpenSSL: TIdSSLIOHandlerSocketOpenSSL;

  FLogExecute: TOnLog;
  FLogMode: TLogMode;
  FMessages: TStringList;
  FPackageStickerID: Byte;
  FStickerID: Byte;
  FResponseBody: String;

  FSSL: Boolean;
  FTLS: Boolean;

  FConnectMaxReconnection: Integer;
  FConnectCountReconnect: Integer;
  FSendMaxReconnection: Integer;
  FSendCountReconnect: Integer;

  procedure Reconnect(AResend: Boolean = False);
  
  procedure LogStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
  procedure LogSSLStatus(const AMsg: string);
  procedure Log(const ALog: string; const AForced: Boolean = False);
  
  public
  
  function AccessTOKEN(const AValue: string): TLineNotify;
  function ValidateAccessTOKEN: Boolean;
  function Messages(const AMessage: string): TLineNotify;
  function PackageStickerID(const AValue: Byte): TLineNotify;
  function StickerID(const AValue: Byte): TLineNotify;
  function ImageThumbnail(const AValueURL: string): TLineNotify;
  function imageFullsize(const AValueURL: string): TLineNotify;
  function imageFile(const AFileName: string): TLineNotify;
  function SendToLineServer(const ADisconnectAfterSending: Boolean = True): TLineNotify;

  function TLS(const AValue: Boolean): TLineNotify;
  function SSL(const AValue: Boolean): TLineNotify;
  function Connect: TLineNotify;
  function Disconnect: TLineNotify;
  function IsConnected: Boolean;

  constructor Create;
  destructor Destroy; override;

  function OnLog(const AExecute: TOnLog; const ALogMode: TLogMode = lmComponent): TLineNotify;


  class function New: TLineNotify;

  end;

  const
    BaseURL = 'https://notify-api.line.me/api/notify';
    StatusURL =  'https://notify-api.line.me/api/status';
  

implementation

{ TLineNotify }

constructor TLineNotify.Create;
begin
   FIdHttpRequest := TIdHTTP.Create;
   FIdSSLOpenSSL  := TIdSSLIOHandlerSocketOpenSSL.Create;
   FRequestParams := TIdMultiPartFormDataStream.Create;
   FMessages := TStringList.Create;
   FMessages.Clear;
   FMessages.Delimiter := ',';
   FMessages.QuoteChar := #0;




   SSL(False);
   TLS(True);
   
   FLogExecute := nil;
   FLogMode := lmNone;

   FConnectMaxReconnection := 5;
   FConnectCountReconnect := 0;
   FSendMaxReconnection := 5;
   FSendCountReconnect := 0;

  with FIdHttpRequest do
  begin
  Request.ContentType := 'application/x-www-form-urlencoded';
  end;

  with FIdSSLOpenSSL do
  begin
    ConnectTimeout := 100000;
    ReadTimeout := 100000;
    PassThrough := True;
    SSLOptions.SSLVersions := [SslvSSLv2, SslvSSLv23, SslvSSLv3, SslvTLSv1, SslvTLSv1_1, SslvTLSv1_2];
    SSLOptions.Mode := sslmBoth;
    SSLOptions.VerifyMode := [];
    SSLOptions.VerifyDepth := 0;
    OnStatus := LogStatus;
    OnStatusInfo := LogSSLStatus;
  end;

  FIdHttpRequest.IOHandler := FIdSSLOpenSSL;
end;

class function TLineNotify.New: TLineNotify;
begin
    Result := Self.Create;
end;

function CheckUpperAndContains(Const AText, ASubText: string): Boolean;
begin
   Result := AnsiContainsText(UpperCase(AText), UpperCase(ASubText));
end;

function TLineNotify.SendToLineServer(const ADisconnectAfterSending: Boolean = True): TLineNotify;
begin
   Result := Self;

     if not IsConnected then
    Connect;

  try
    try
      Log('Sending LineNotify');
      FRequestParams.AddFormField('message', UTF8Encode(FMessages.Text), 'utf-8').ContentTransfer := '8bit';
      FResponseBody := FIdHttpRequest.Post(BaseURL, FRequestParams);
      Log('LineNotify sent');

      FSendCountReconnect := 0;
    except
      on E: Exception do
      begin
        Log('Except: ' + E.Message, True);

               

        if
        CheckUpperAndContains('CLOSING CONNECTION', E.Message) or
        CheckUpperAndContains('TOO MANY MESSAGES', E.Message)  or
        CheckUpperAndContains('CONNECTION CLOSED', E.Message) 
        then
        begin
          if FSendCountReconnect < FSendMaxReconnection then
          begin
            Sleep(100);
            Inc(FSendCountReconnect);
            Reconnect(True);
            Exit;
          end
          else
          begin
            FSendCountReconnect := 0;
            raise Exception.Create(E.Message);
          end;
        end;
         
         
        if CheckUpperAndContains('NOT CONNECTED', E.Message) then
          raise Exception.Create('Not connected to internet!');

        raise Exception.Create(E.Message);
      end;
    end;
  finally
    if ADisconnectAfterSending then
      Disconnect;
  end;

  {
  try

    FRequestParams.AddFormField('message', UTF8Encode(FMessages.Text), 'utf-8').ContentTransfer := '8bit';
    FResponseBody := FIdHttpRequest.Post(BaseURL, FRequestParams);
  except
    on E: EIdHTTPProtocolException do
    begin
      Log(e.Message);
      Log(e.ErrorMessage);
    end;
    on E: Exception do
    begin
      Log(e.Message);
    end;
  end; }

end;

function TLineNotify.ValidateAccessTOKEN: Boolean;
begin
  Result := False;
  try
    FIdHttpRequest.Get(StatusURL);
    Result := FIdHttpRequest.ResponseCode = 200;
    Log('Validate AccessTOKEN: ' + FIdHttpRequest.ResponseText);
  except
    on E: EIdHTTPProtocolException do
    begin
      Log(e.Message);
      Log(e.ErrorMessage);
    end;
    on E: Exception do
    begin
      Log(e.Message);
    end;
  end;
end;


destructor TLineNotify.Destroy;
begin
  FreeAndNil(FIdHttpRequest);
  FreeAndNil(FIdSSLOpenSSL);
  FreeAndNil(FRequestParams);
  FreeAndNil(FMessages);
  FLogExecute := nil;
  inherited;
end;


function TLineNotify.Connect: TLineNotify;
var
  LLastResult: string;
begin
  Result := Self;

  if IsConnected then
    Exit;

  if FSSL or FTLS then
  begin
    if FSSL and FTLS then
      Log('Defining encryption: SSL/TLS')
    else
      if FSSL then
        Log('Defining encryption: SSL')
      else
        Log('Defining encryption: TLS');


    Log('Loading DLL');
    if not LoadOpenSSLLibrary then
    begin
      Log(WhichFailedToLoad);
      raise Exception.Create(Self.ClassName + ' > ' + WhichFailedToLoad);
    end;
    Log('Loaded DLL');

    if FSSL then
      FIdSSLOpenSSL.SSLOptions.Method := SslvSSLv23;

    if FTLS then
      FIdSSLOpenSSL.SSLOptions.Method := SslvTLSv1_2;


    if not ValidateAccessTOKEN then {Check Token}
         Exit;
  end
  else
  begin
    Log('Defining encryption: None');
    FIdHttpRequest.IOHandler := nil;
  end;

  try
    Log('Connecting');
    Log('Connected');
    if IsConnected then
    FConnectCountReconnect := 0;
  except
    on E: Exception do
    begin
      if FIdHttpRequest.LastCmdResult.ReplyExists then
      begin
        LLastResult := Format('Last Result: %s', [FIdHttpRequest.LastCmdResult.FormattedReply.Text]);

        if
          CheckUpperAndContains('AUTHENTICATION SUCCEEDED', LLastResult) or
          CheckUpperAndContains('250 OK', LLastResult)
        then
        begin
          Log(LLastResult, True);

          if FConnectCountReconnect < FConnectMaxReconnection then
          begin
            Sleep(100);
            Inc(FConnectCountReconnect);
            Reconnect(False);
            Exit;
          end
          else
          begin
            FConnectCountReconnect := 0;
            raise Exception.Create(E.Message);
          end;
        end;
      end;

      try
        if CheckUpperAndContains('INCORRECT AUTHENTICATION DATA', E.Message)  then
          raise Exception.Create('Incorrect authentication!');

        if CheckUpperAndContains('SOCKET ERROR # 10013', E.Message)  then
          raise Exception.Create('Firewall is blocking access to the internet!');

        if CheckUpperAndContains('SOCKET ERROR # 10054', E.Message)  then
          raise Exception.Create('Connection terminated!');

        if CheckUpperAndContains('SOCKET ERROR # 11001', E.Message)  then
          raise Exception.Create('Host not found!');

        if CheckUpperAndContains(LLastResult, E.Message) and not (Trim(LLastResult) <> '')  then
          raise Exception.Create(LLastResult + sLineBreak + 'Message: ' + E.Message)
        else
          raise Exception.Create(E.Message);
      except
        on E: Exception do
        begin
          Log('Except: ' + E.Message, True);
          Log('LineNotify not connected!');
          raise;
        end;
      end;
    end;
  end;
end;

procedure TLineNotify.Log(const ALog: string; const AForced: Boolean = False);
begin
  if Assigned(FLogExecute) and ((FLogMode in [lmLib, lmAll]) or AForced) and not(FLogMode = lmNone) then
    FLogExecute('LIB: ' + ALog);
end;

function TLineNotify.OnLog(const AExecute: TOnLog; const ALogMode: TLogMode = lmComponent): TLineNotify;
begin
  Result := Self;

  FLogExecute := AExecute;
  FLogMode := ALogMode;
end;


procedure TLineNotify.LogStatus(ASender: TObject;
  const AStatus: TIdStatus; const AStatusText: string);
begin
  if Assigned(FLogExecute) and (FLogMode in [lmComponent, lmAll]) then
    FLogExecute('LogStatus: ' + AStatusText);
end;
procedure TLineNotify.LogSSLStatus(const AMsg: string);
begin
  if Assigned(FLogExecute) and (FLogMode in [lmComponent, lmAll]) then
    FLogExecute('SSL: ' + ReplaceAll(UpperCase(AMsg), 'SSL STATUS: ', ''));
end;

function TLineNotify.AccessTOKEN(const AValue: string): TLineNotify;
begin
  Result := Self;
  if Trim(AValue) = EmptyStr then
    Exit;
    
  FIdHttpRequest.Request.CustomHeaders.Text := Format('Authorization:Bearer %s',[AValue]);
  Log(Format('AccessTOKEN: %s', [AValue]));
end;


function TLineNotify.Messages(const AMessage: string): TLineNotify;
begin
   Result := Self;
   if Trim(AMessage) = EmptyStr then
    Exit;
    
    FMessages.Add(AMessage);
   Log(Format('Message: %s', [AMessage]));
end;

function TLineNotify.StickerID(const AValue: Byte): TLineNotify;
begin
   Result := Self;
   if (AValue) <= 0 then
    Exit;
    
   FStickerID := AValue;
   FRequestParams.AddFormField('stickerId', IntToStr(AValue), 'utf-8').ContentTransfer := '8bit';
   Log(Format('StickerID: %d', [AValue]));
end;

function TLineNotify.PackageStickerID(const AValue: Byte): TLineNotify;
begin
   Result := Self;
   if (AValue) <= 0 then
    Exit;

   FPackageStickerID := AValue; 
   FRequestParams.AddFormField('stickerPackageId', IntToStr(AValue), 'utf-8').ContentTransfer := '8bit';
   Log(Format('PackageStickerID: %d', [AValue]));
end;

function TLineNotify.ImageFile(const AFileName: string): TLineNotify;
begin
   Result := Self;
   if Trim(AFileName) = EmptyStr then
    Exit;
   FRequestParams.AddFile('imageFile',UTF8Encode(AFileName),'image/*').ContentTransfer := '8bit';
   Log(Format('imageFile: %s', [AFileName]));
end;

function TLineNotify.imageFullsize(const AValueURL: string): TLineNotify;
begin
   Result := Self;
   if Trim(AValueURL) = EmptyStr then
    Exit;
    
   FRequestParams.AddFormField('imageFullsize',UTF8Encode(AValueURL), 'utf-8').ContentTransfer := '8bit';
   Log(Format('imageFullsize: %s', [AValueURL]));
end;

function TLineNotify.ImageThumbnail(const AValueURL: string): TLineNotify;
begin
   Result := Self;
   if Trim(AValueURL) = EmptyStr then
    Exit;
    
   FRequestParams.AddFormField('imageThumbnail',UTF8Encode(AValueURL), 'utf-8').ContentTransfer := '8bit';
   Log(Format('imageThumbnail: %s', [AValueURL]));
end;


function TLineNotify.TLS(const AValue: Boolean): TLineNotify;
begin
  Result := Self;
  FTLS := AValue;
end;

function TLineNotify.SSL(const AValue: Boolean): TLineNotify;
begin
  Result := Self;
  FSSL := AValue;
end;


function TLineNotify.Disconnect: TLineNotify;
begin
  Result := Self;

  if IsConnected then
    try
      Log('Disconnecting');
      FIdHttpRequest.Disconnect(False);
      Log('Disconnected');
    except
      Log('Except: Disconnected with error');
    end;
end;

function TLineNotify.IsConnected: Boolean;
begin
  Result := False;

  try
    Result := FIdHttpRequest.Connected;
  except
    on E: Exception do
    begin
      if CheckUpperAndContains('CLOSING CONNECTION', E.Message) or
        CheckUpperAndContains('SSL3_GET_RECORD', E.Message) 
      then
        try
          Reconnect(False);
          Result := True;
        except
          Exit;
        end;
    end;
  end;
end;

procedure TLineNotify.Reconnect(AResend: Boolean = False);
begin
  if AResend then
    Log('Reconnecting: ' + IntToStr(FSendCountReconnect), True)
  else
    Log('Reconnecting: ' + IntToStr(FConnectCountReconnect), True);

  Disconnect;
  Connect;

  if AResend then
     SendToLineServer;
end;

end.


