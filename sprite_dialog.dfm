object SpriteDialog: TSpriteDialog
  Left = 166
  Top = 116
  BorderStyle = bsDialog
  Caption = 'Sprites'
  ClientHeight = 529
  ClientWidth = 1016
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object SpriteImage: TImage
    Left = 128
    Top = 8
    Width = 880
    Height = 320
    OnMouseDown = SpriteImageMouseDown
  end
  object lbSpriteSetNum: TLabel
    Left = 128
    Top = 340
    Width = 53
    Height = 13
    Caption = 'Sprite set 0'
  end
  object lbSpriteNameCustom: TLabel
    Left = 336
    Top = 340
    Width = 3
    Height = 13
  end
  object lbxSpriteSetList: TListBox
    Left = 0
    Top = 0
    Width = 121
    Height = 529
    Align = alLeft
    ItemHeight = 13
    TabOrder = 0
    OnClick = lbxSpriteSetListClick
  end
  object edSpriteName: TEdit
    Left = 200
    Top = 336
    Width = 129
    Height = 21
    MaxLength = 21
    TabOrder = 1
    OnChange = edSpriteNameChange
  end
  object btnBackgroundColor: TButton
    Left = 616
    Top = 336
    Width = 121
    Height = 25
    Caption = 'Background color...'
    TabOrder = 2
    OnClick = btnBackgroundColorClick
  end
  object btnSaveChanges: TBitBtn
    Left = 744
    Top = 336
    Width = 121
    Height = 25
    Caption = 'Save changes'
    TabOrder = 3
    OnClick = btnSaveChangesClick
    Kind = bkOK
  end
  object cbShowNumbers: TCheckBox
    Left = 480
    Top = 340
    Width = 121
    Height = 17
    Caption = 'Show sprite numbers'
    TabOrder = 4
    OnClick = cbShowNumbersClick
  end
  object btnUndoChanges: TBitBtn
    Left = 872
    Top = 336
    Width = 121
    Height = 25
    Caption = 'Undo changes'
    TabOrder = 5
    OnClick = btnUndoChangesClick
    Kind = bkCancel
  end
  object gbExpImpSprites: TGroupBox
    Left = 616
    Top = 368
    Width = 393
    Height = 153
    Caption = ' Export / Import sprites '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    object lbImportTranspIndex: TLabel
      Left = 176
      Top = 72
      Width = 114
      Height = 13
      Caption = 'Transparent color index:'
    end
    object lbImportOptions: TLabel
      Left = 176
      Top = 48
      Width = 69
      Height = 13
      Caption = 'Import options:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lbLayoutData: TLabel
      Left = 280
      Top = 20
      Width = 35
      Height = 13
      Caption = 'Layout:'
    end
    object lbPixelData: TLabel
      Left = 280
      Top = 40
      Width = 30
      Height = 13
      Caption = 'Pixels:'
    end
    object lbLayoutSize: TLabel
      Left = 320
      Top = 20
      Width = 34
      Height = 13
      Caption = '0 bytes'
    end
    object lbPixelSize: TLabel
      Left = 320
      Top = 40
      Width = 34
      Height = 13
      Caption = '0 bytes'
    end
    object btnExportSprites: TButton
      Left = 10
      Top = 120
      Width = 123
      Height = 25
      Caption = 'Export sprites'
      TabOrder = 0
      OnClick = btnExportSpritesClick
    end
    object rbExpImpSingle: TRadioButton
      Left = 12
      Top = 24
      Width = 89
      Height = 17
      Caption = 'Single sprite:'
      Checked = True
      TabOrder = 1
      TabStop = True
    end
    object rbExpImpAllMultFiles: TRadioButton
      Left = 12
      Top = 48
      Width = 145
      Height = 17
      Caption = 'All sprites in multiple files'
      TabOrder = 2
    end
    object rbExpImpAllOneFile: TRadioButton
      Left = 12
      Top = 72
      Width = 113
      Height = 17
      Caption = 'All sprites in one file'
      TabOrder = 3
    end
    object pnExportFormat: TPanel
      Left = 12
      Top = 96
      Width = 161
      Height = 21
      BevelOuter = bvNone
      TabOrder = 4
      object lbExportAs: TLabel
        Left = 0
        Top = 1
        Width = 47
        Height = 13
        Caption = 'Export as:'
      end
      object rbExportPng: TRadioButton
        Left = 56
        Top = 0
        Width = 49
        Height = 17
        Caption = 'PNG'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
      object rbExportBmp: TRadioButton
        Left = 104
        Top = 0
        Width = 49
        Height = 17
        Caption = 'BMP'
        TabOrder = 1
      end
    end
    object seExpImpSpriteNum: TSpinEdit
      Left = 100
      Top = 20
      Width = 65
      Height = 22
      MaxValue = 39
      MinValue = 0
      TabOrder = 5
      Value = 0
      OnChange = seExpImpSpriteNumChange
    end
    object btnImportSprites: TButton
      Left = 142
      Top = 120
      Width = 123
      Height = 25
      Caption = 'Import sprites'
      TabOrder = 6
      OnClick = btnImportSpritesClick
    end
    object seImportTranspIndex: TSpinEdit
      Left = 304
      Top = 68
      Width = 65
      Height = 22
      MaxValue = 255
      MinValue = -1
      TabOrder = 7
      Value = 255
    end
    object btnEraseSprite: TButton
      Left = 174
      Top = 20
      Width = 95
      Height = 22
      Caption = 'Erase this sprite'
      TabOrder = 8
      OnClick = btnEraseSpriteClick
    end
    object cbImportUseUpperPal: TCheckBox
      Left = 176
      Top = 96
      Width = 169
      Height = 17
      Caption = 'Utilize upper part of palette'
      TabOrder = 9
    end
  end
  object gbSpriteSetProperties: TGroupBox
    Left = 128
    Top = 360
    Width = 481
    Height = 161
    Caption = ' Sprite set properties '
    TabOrder = 7
    object lbSpriteWidth: TLabel
      Left = 8
      Top = 24
      Width = 58
      Height = 13
      Caption = 'Sprite width:'
    end
    object lbSpriteHeight: TLabel
      Left = 8
      Top = 52
      Width = 62
      Height = 13
      Caption = 'Sprite height:'
    end
    object lbStandFrames: TLabel
      Left = 8
      Top = 80
      Width = 65
      Height = 13
      Caption = 'Stand frames:'
    end
    object lbWalkFrames: TLabel
      Left = 8
      Top = 108
      Width = 62
      Height = 13
      Caption = 'Walk frames:'
    end
    object lbShootDashFrames: TLabel
      Left = 8
      Top = 136
      Width = 95
      Height = 13
      Caption = 'Dash/Shoot frames:'
    end
    object lbTo1: TLabel
      Left = 166
      Top = 80
      Width = 9
      Height = 13
      Caption = 'to'
    end
    object lbTo2: TLabel
      Left = 166
      Top = 108
      Width = 9
      Height = 13
      Caption = 'to'
    end
    object lbTo3: TLabel
      Left = 166
      Top = 136
      Width = 9
      Height = 13
      Caption = 'to'
    end
    object lbProjectileWidth: TLabel
      Left = 248
      Top = 24
      Width = 74
      Height = 13
      Caption = 'Projectile width:'
    end
    object lbProjectileHeight: TLabel
      Left = 248
      Top = 52
      Width = 78
      Height = 13
      Caption = 'Projectile height:'
    end
    object lbProjectileYOff: TLabel
      Left = 248
      Top = 80
      Width = 85
      Height = 13
      Caption = 'Projectile Y-offset:'
    end
    object lbProjectileFrames: TLabel
      Left = 248
      Top = 108
      Width = 80
      Height = 13
      Caption = 'Projectile frames:'
    end
    object lbJumpFallFrame: TLabel
      Left = 248
      Top = 136
      Width = 94
      Height = 13
      Caption = 'Jump and fall frame:'
    end
    object lbTo4: TLabel
      Left = 406
      Top = 108
      Width = 9
      Height = 13
      Caption = 'to'
    end
    object seShootDashFrameFirst: TSpinEdit
      Left = 112
      Top = 132
      Width = 49
      Height = 22
      MaxValue = 19
      MinValue = -1
      TabOrder = 0
      Value = 0
      OnChange = seSpriteSetPropertyChange
    end
    object seWalkFrameFirst: TSpinEdit
      Left = 112
      Top = 104
      Width = 49
      Height = 22
      MaxValue = 19
      MinValue = -1
      TabOrder = 1
      Value = 0
      OnChange = seSpriteSetPropertyChange
    end
    object seStandFrameFirst: TSpinEdit
      Left = 112
      Top = 76
      Width = 49
      Height = 22
      MaxValue = 19
      MinValue = -1
      TabOrder = 2
      Value = 0
      OnChange = seSpriteSetPropertyChange
    end
    object seSpriteHeight: TSpinEdit
      Left = 112
      Top = 48
      Width = 65
      Height = 22
      MaxValue = 200
      MinValue = 0
      TabOrder = 3
      Value = 0
      OnChange = seSpriteSizeChange
    end
    object seSpriteWidth: TSpinEdit
      Left = 112
      Top = 20
      Width = 65
      Height = 22
      Increment = 4
      MaxValue = 320
      MinValue = 0
      TabOrder = 4
      Value = 0
      OnChange = seSpriteSizeChange
    end
    object seShootDashFrameLast: TSpinEdit
      Left = 184
      Top = 132
      Width = 49
      Height = 22
      MaxValue = 19
      MinValue = -1
      TabOrder = 5
      Value = 0
      OnChange = seSpriteSetPropertyChange
    end
    object seWalkFrameLast: TSpinEdit
      Left = 184
      Top = 104
      Width = 49
      Height = 22
      MaxValue = 19
      MinValue = -1
      TabOrder = 6
      Value = 0
      OnChange = seSpriteSetPropertyChange
    end
    object seStandFrameLast: TSpinEdit
      Left = 184
      Top = 76
      Width = 49
      Height = 22
      MaxValue = 19
      MinValue = -1
      TabOrder = 7
      Value = 0
      OnChange = seSpriteSetPropertyChange
    end
    object seJumpFrame: TSpinEdit
      Left = 352
      Top = 132
      Width = 49
      Height = 22
      MaxValue = 19
      MinValue = -1
      TabOrder = 8
      Value = 0
      OnChange = seSpriteSetPropertyChange
    end
    object seProjectileFrameFirst: TSpinEdit
      Left = 352
      Top = 104
      Width = 49
      Height = 22
      MaxValue = 19
      MinValue = -1
      TabOrder = 9
      Value = 0
      OnChange = seSpriteSetPropertyChange
    end
    object seProjectileYOff: TSpinEdit
      Left = 352
      Top = 76
      Width = 65
      Height = 22
      MaxValue = 200
      MinValue = -1
      TabOrder = 10
      Value = 0
      OnChange = seSpriteSetPropertyChange
    end
    object seProjectileHeight: TSpinEdit
      Left = 352
      Top = 48
      Width = 65
      Height = 22
      MaxValue = 200
      MinValue = -1
      TabOrder = 11
      Value = 0
      OnChange = seSpriteSetPropertyChange
    end
    object seProjectileWidth: TSpinEdit
      Left = 352
      Top = 20
      Width = 65
      Height = 22
      Increment = 4
      MaxValue = 320
      MinValue = -1
      TabOrder = 12
      Value = 0
      OnChange = seSpriteSetPropertyChange
    end
    object seProjectileFrameLast: TSpinEdit
      Left = 424
      Top = 104
      Width = 49
      Height = 22
      MaxValue = 19
      MinValue = -1
      TabOrder = 13
      Value = 0
      OnChange = seSpriteSetPropertyChange
    end
    object seFallFrame: TSpinEdit
      Left = 424
      Top = 132
      Width = 49
      Height = 22
      MaxValue = 19
      MinValue = -1
      TabOrder = 14
      Value = 0
      OnChange = seSpriteSetPropertyChange
    end
  end
  object ColorDialog: TColorDialog
    Color = 16769184
    Left = 984
    Top = 336
  end
  object ExportDialogBmp: TSaveDialog
    DefaultExt = 'bmp'
    Filter = 'BMP Image (*.bmp)|*.bmp'
    Title = 'Export sprites'
    Left = 920
    Top = 488
  end
  object ImportDialog: TOpenDialog
    DefaultExt = 'png'
    Filter = 'Supported image files (*.png, *.bmp)|*.png;*.bmp'
    Title = 'Import sprites'
    Left = 952
    Top = 488
  end
  object ExportDialogPng: TSaveDialog
    DefaultExt = 'png'
    Filter = 'PNG Image (*.png)|*.png'
    Title = 'Export sprites'
    Left = 888
    Top = 488
  end
end
