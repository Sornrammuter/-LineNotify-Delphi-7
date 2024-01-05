  LineNotify
    .OnLog(DoLog, lmAll)
    .AccessTOKEN(TOKEN)
    .Messages(txtMessage.Text)
    .StickerID(125)
    .PackageStickerID(1)
    .imageFile('Horned_logo.jpeg')
    .SendToLineServer;
