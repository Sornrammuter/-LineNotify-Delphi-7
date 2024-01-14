
Line-Notify-Delphi-7
Line Notify คืออะไร Line Notify คือบริการที่ LINE ให้เราส่งข้อความ หรือแจ้งเตือนอัตโนมัติ ไม่ว่าจะส่งเข้าผ่าน Group หรือบัญชีส่วนตัว ผ่าน API ของ LINE โดยตรง  Component Indy 10.6.2.0 delphi
 ## ⚡️ Quickstart Delphi
```delphi
  LineNotify
    .OnLog(DoLog, lmAll)
    .AccessTOKEN(TOKEN)
    .Messages(txtMessage.Text)
    .StickerID(125)
    .PackageStickerID(1)
    .imageFile('Horned_logo.jpeg')
    .SendToLineServer;
```
## Delphi Versions
`Line-Notify` works with Delphi 7.

