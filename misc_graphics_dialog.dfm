object MiscGraphicsDialog: TMiscGraphicsDialog
  Left = 192
  Top = 114
  BorderStyle = bsDialog
  Caption = 'Miscellaneous graphics'
  ClientHeight = 449
  ClientWidth = 777
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object GraphicsImage: TImage
    Left = 128
    Top = 8
    Width = 640
    Height = 400
  end
  object lbImageSize: TLabel
    Left = 128
    Top = 420
    Width = 3
    Height = 13
  end
  object lbxImageFileList: TListBox
    Left = 0
    Top = 0
    Width = 121
    Height = 449
    Align = alLeft
    ItemHeight = 13
    TabOrder = 0
    OnClick = lbxImageFileListClick
  end
  object btnExportImage: TButton
    Left = 192
    Top = 416
    Width = 89
    Height = 25
    Caption = 'Export image'
    TabOrder = 1
    OnClick = btnExportImageClick
  end
  object btnImportImage: TButton
    Left = 400
    Top = 416
    Width = 89
    Height = 25
    Caption = 'Import image'
    TabOrder = 2
    OnClick = btnImportImageClick
  end
  object rbExportPng: TRadioButton
    Left = 288
    Top = 420
    Width = 49
    Height = 17
    Caption = 'PNG'
    Checked = True
    TabOrder = 3
    TabStop = True
  end
  object rbExportBmp: TRadioButton
    Left = 336
    Top = 420
    Width = 49
    Height = 17
    Caption = 'BMP'
    TabOrder = 4
  end
  object cbImportUseUpperPal: TCheckBox
    Left = 496
    Top = 420
    Width = 161
    Height = 17
    Caption = 'Utilize upper part of palette'
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
  object ExportDialogPng: TSaveDialog
    DefaultExt = 'png'
    Filter = 'PNG Image (*.png)|*.png'
    Title = 'Export sprites'
    Left = 664
    Top = 416
  end
  object ExportDialogBmp: TSaveDialog
    DefaultExt = 'bmp'
    Filter = 'BMP Image (*.bmp)|*.bmp'
    Title = 'Export sprites'
    Left = 696
    Top = 416
  end
  object ImportDialog: TOpenDialog
    DefaultExt = 'png'
    Filter = 'Supported image files (*.png, *.bmp)|*.png;*.bmp'
    Title = 'Import sprites'
    Left = 728
    Top = 416
  end
end
