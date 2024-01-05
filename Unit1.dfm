object Form1: TForm1
  Left = 702
  Top = 271
  Width = 411
  Height = 562
  Caption = 'Delphi7 sends LINE messages via HTTPS'
  Color = 8421440
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    395
    523)
  PixelsPerInch = 96
  TextHeight = 13
  object SpeedButton1: TSpeedButton
    Left = 264
    Top = 160
    Width = 112
    Height = 49
    Anchors = [akRight, akBottom]
    Caption = 'POST'
    OnClick = SpeedButton1Click
  end
  object Label1: TLabel
    Left = 8
    Top = 456
    Width = 32
    Height = 13
    Caption = 'Label1'
    Color = clAqua
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Image1: TImage
    Left = 256
    Top = 42
    Width = 129
    Height = 113
    Stretch = True
  end
  object SpeedButton2: TSpeedButton
    Left = 256
    Top = 6
    Width = 129
    Height = 33
    Caption = 'Load image'
    OnClick = SpeedButton2Click
  end
  object SpeedButton3: TSpeedButton
    Left = 256
    Top = 240
    Width = 129
    Height = 49
    Caption = 'ADRLineNotify Test'
    OnClick = SpeedButton3Click
  end
  object SpeedButton4: TSpeedButton
    Left = 256
    Top = 312
    Width = 129
    Height = 49
    Caption = 'Close'
    OnClick = SpeedButton4Click
  end
  object memo1: TMemo
    Left = 8
    Top = 40
    Width = 243
    Height = 409
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelInner = bvSpace
    BevelOuter = bvNone
    TabOrder = 0
  end
  object txtMessage: TEdit
    Left = 8
    Top = 8
    Width = 241
    Height = 25
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object idHttp: TIdHTTP
    IOHandler = IdSSL
    HandleRedirects = True
    ProxyParams.BasicAuthentication = False
    ProxyParams.ProxyPort = 0
    Request.ContentLength = -1
    Request.ContentRangeEnd = -1
    Request.ContentRangeStart = -1
    Request.ContentRangeInstanceLength = -1
    Request.ContentType = 'text/html'
    Request.Accept = 'text/html, */*'
    Request.BasicAuthentication = False
    Request.UserAgent = 'Mozilla/3.0 (compatible; Indy Library)'
    Request.Ranges.Units = 'bytes'
    Request.Ranges = <>
    HTTPOptions = []
    Left = 224
    Top = 128
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Left = 304
    Top = 72
  end
  object IdSSL: TIdSSLIOHandlerSocketOpenSSL
    MaxLineAction = maException
    Port = 0
    DefaultPort = 0
    SSLOptions.Method = sslvTLSv1_2
    SSLOptions.SSLVersions = [sslvTLSv1_2]
    SSLOptions.Mode = sslmUnassigned
    SSLOptions.VerifyMode = []
    SSLOptions.VerifyDepth = 0
    Left = 248
    Top = 184
  end
end
