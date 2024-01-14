unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, StdCtrls, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdHTTP, IdIOHandler, IdIOHandlerSocket,
  IdSSLOpenSSL, IdGlobal, IdMultipartFormData, StrUtils, ExtDlgs, ExtCtrls, jpeg,
  IdIOHandlerStack, IdSSL,   ADRLineNotify;

type
  TForm1 = class(TForm)
    idHttp: TIdHTTP;
    memo1: TMemo;
    SpeedButton1: TSpeedButton;
    Label1: TLabel;
    txtMessage: TEdit;
    Image1: TImage;
    OpenPictureDialog1: TOpenPictureDialog;
    SpeedButton2: TSpeedButton;
    IdSSL: TIdSSLIOHandlerSocketOpenSSL;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    procedure DoLog(const ALog: string);
  public
    { Public declarations }
  end;

  const TOKEN = '';

var
  Form1: TForm1;
  LineNotify: TLineNotify;

implementation




{$R *.dfm}

procedure TForm1.SpeedButton1Click(Sender: TObject);
const
  url = 'https://notify-api.line.me/api/notify';
var
//  RequestBody: TStream;
  ResponseBody: string;
  Params: TIdMultiPartFormDataStream;
begin

//  idHttp.Request.ContentType := 'multipart/form-data';
  idHttp.Request.ContentType := 'application/x-www-form-urlencoded';
//  idHttp.Request.ContentType := 'application/json';
  idHttp.Request.CustomHeaders.Text := 'Authorization:Bearer fXSsTz6dIdZ75UnLgF7DyiQmpAyjjCgxJ3SWSmAl9qI';
  try
    try
//      RequestBody := TStringStream.Create('message="'+ UTF8Encode(txtMessage.Text) +'"');    // UTF8Encode à¾ÃÒÐµÑÇÍÑ¡ÉÃà»ç¹ÀÒÉÂä·Â
//      ResponseBody := 'message='+ UTF8Encode(txtMessage.Text) +'&';
//      ResponseBody := ResponseBody + 'stickerId=125&';
//      ResponseBody := ResponseBody + 'stickerPackageId=1&';
//      ResponseBody := ResponseBody + 'imageFile=Horned_logo.jpeg';
//      SS := TStringStream.Create('', TEncoding.UTF8);
      Image1.Picture.Graphic := nil;
      Image1.Picture.LoadFromFile('Horned_logo.jpeg');
      Params :=  TIdMultiPartFormDataStream.Create;
      Params.AddFormField('message',UTF8Encode(txtMessage.Text), 'utf-8').ContentTransfer := '8bit';
      Params.AddFormField('stickerId','125');
      Params.AddFormField('stickerPackageId','1');
      params.AddFile('imageFile','Horned_logo.jpeg','image/*');
//      ResponseBody := ResponseBody + IfThen(OpenPictureDialog1.FileName <> '','&imageFile='+UTF8Encode(OpenPictureDialog1.FileName)+' ');
//      RequestBody := TStringStream.Create(ResponseBody);
      try
        memo1.Lines.LoadFromStream(Params);
        ResponseBody := idHttp.Post(url, Params);
        memo1.lines.add(ResponseBody);
      finally
        Params.Free;
//        RequestBody.free;
      end;
    except
      on E: EIdHTTPProtocolException do
      begin
        memo1.Lines.Add(e.Message);
        memo1.Lines.Add(e.ErrorMessage);
      end;
      on E: Exception do
      begin
        memo1.Lines.Add(e.Message);
      end;
    end;
  finally
//  xData.free;
  end;
end;


procedure TForm1.FormCreate(Sender: TObject);
begin
    txtMessage.Text := '·´ÊÍºÊè§¨Ò¡ Delphi 7 Indy áÅÐÃÙ»ÀÒ¾ ';
    Label1.Caption := 'Delphi 7 Indy ' + gsIdVersion;
    LineNotify :=  TLineNotify.New;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin

  if OpenPictureDialog1.Execute then
      Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
end;

procedure TForm1.SpeedButton3Click(Sender: TObject);
begin
  LineNotify
    .OnLog(DoLog, lmAll)
    .AccessTOKEN(TOKEN)
    .Messages(txtMessage.Text)
    .StickerID(125)
    .PackageStickerID(1)
    .imageFile('Horned_logo.jpeg')
//    .imageFullsize('https://scdn.line-apps.com/n/line_notice/img/pc/img_api_document1.png')
//    .ImageThumbnail('https://scdn.line-apps.com/n/line_notice/img/pc/img_api_document1.png')
    .SendToLineServer;
end;

procedure TForm1.DoLog(const ALog: string);
begin
    memo1.Lines.Add(Format('%s ' + ALog, [FormatDateTime('dd/mm/yyyy hh:MM:ss', Now)]));
end;

procedure TForm1.SpeedButton4Click(Sender: TObject);
begin
    Close;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
    LineNotify.Free;
end;

end.
