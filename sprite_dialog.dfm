object SpriteDialog: TSpriteDialog
  Left = 166
  Top = 116
  BorderStyle = bsDialog
  Caption = 'Sprites'
  ClientHeight = 538
  ClientWidth = 1016
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
  object Image1: TImage
    Left = 128
    Top = 8
    Width = 880
    Height = 320
  end
  object lbSpriteSetNum: TLabel
    Left = 128
    Top = 340
    Width = 53
    Height = 13
    Caption = 'Sprite set 0'
  end
  object lbSpriteWidth: TLabel
    Left = 128
    Top = 376
    Width = 58
    Height = 13
    Caption = 'Sprite width:'
  end
  object lbSpriteHeight: TLabel
    Left = 128
    Top = 408
    Width = 62
    Height = 13
    Caption = 'Sprite height:'
  end
  object lbProjectileWidth: TLabel
    Left = 376
    Top = 376
    Width = 74
    Height = 13
    Caption = 'Projectile width:'
  end
  object lbProjectileHeight: TLabel
    Left = 376
    Top = 408
    Width = 78
    Height = 13
    Caption = 'Projectile height:'
  end
  object lbProjectileYOff: TLabel
    Left = 376
    Top = 440
    Width = 85
    Height = 13
    Caption = 'Projectile Y-offset:'
  end
  object lbStandFrames: TLabel
    Left = 128
    Top = 440
    Width = 65
    Height = 13
    Caption = 'Stand frames:'
  end
  object lbWalkFrames: TLabel
    Left = 128
    Top = 472
    Width = 62
    Height = 13
    Caption = 'Walk frames:'
  end
  object lbShootDashFrames: TLabel
    Left = 128
    Top = 504
    Width = 95
    Height = 13
    Caption = 'Dash/Shoot frames:'
  end
  object lbProjectileFrames: TLabel
    Left = 376
    Top = 472
    Width = 80
    Height = 13
    Caption = 'Projectile frames:'
  end
  object lbJumpFallFrame: TLabel
    Left = 376
    Top = 504
    Width = 94
    Height = 13
    Caption = 'Jump and fall frame:'
  end
  object lbTo1: TLabel
    Left = 286
    Top = 440
    Width = 9
    Height = 13
    Caption = 'to'
  end
  object lbTo2: TLabel
    Left = 286
    Top = 472
    Width = 9
    Height = 13
    Caption = 'to'
  end
  object lbTo3: TLabel
    Left = 286
    Top = 504
    Width = 9
    Height = 13
    Caption = 'to'
  end
  object Label1: TLabel
    Left = 534
    Top = 472
    Width = 9
    Height = 13
    Caption = 'to'
  end
  object lbxSpriteSetList: TListBox
    Left = 0
    Top = 0
    Width = 121
    Height = 538
    Align = alLeft
    ItemHeight = 13
    TabOrder = 0
    OnClick = lbxSpriteSetListClick
  end
  object seSpriteWidth: TSpinEdit
    Left = 232
    Top = 372
    Width = 65
    Height = 22
    Increment = 4
    MaxValue = 320
    MinValue = -1
    TabOrder = 1
    Value = 0
  end
  object seSpriteHeight: TSpinEdit
    Left = 232
    Top = 404
    Width = 65
    Height = 22
    MaxValue = 200
    MinValue = -1
    TabOrder = 2
    Value = 0
  end
  object seProjectileWidth: TSpinEdit
    Left = 480
    Top = 372
    Width = 65
    Height = 22
    Increment = 4
    MaxValue = 320
    MinValue = -1
    TabOrder = 3
    Value = 0
  end
  object seProjectileHeight: TSpinEdit
    Left = 480
    Top = 404
    Width = 65
    Height = 22
    MaxValue = 200
    MinValue = -1
    TabOrder = 4
    Value = 0
  end
  object seProjectileYOff: TSpinEdit
    Left = 480
    Top = 436
    Width = 65
    Height = 22
    MaxValue = 200
    MinValue = -1
    TabOrder = 5
    Value = 0
  end
  object seStandFrameFirst: TSpinEdit
    Left = 232
    Top = 436
    Width = 49
    Height = 22
    MaxValue = 39
    MinValue = -1
    TabOrder = 6
    Value = 0
  end
  object seWalkFrameFirst: TSpinEdit
    Left = 232
    Top = 468
    Width = 49
    Height = 22
    MaxValue = 39
    MinValue = -1
    TabOrder = 7
    Value = 0
  end
  object seShootDashFrameFirst: TSpinEdit
    Left = 232
    Top = 500
    Width = 49
    Height = 22
    MaxValue = 39
    MinValue = -1
    TabOrder = 8
    Value = 0
  end
  object seStandFrameLast: TSpinEdit
    Left = 304
    Top = 436
    Width = 49
    Height = 22
    MaxValue = 39
    MinValue = -1
    TabOrder = 9
    Value = 0
  end
  object seWalkFrameLast: TSpinEdit
    Left = 304
    Top = 468
    Width = 49
    Height = 22
    MaxValue = 39
    MinValue = -1
    TabOrder = 10
    Value = 0
  end
  object seShootDashFrameLast: TSpinEdit
    Left = 304
    Top = 500
    Width = 49
    Height = 22
    MaxValue = 39
    MinValue = -1
    TabOrder = 11
    Value = 0
  end
  object seProjectileFrameFirst: TSpinEdit
    Left = 480
    Top = 468
    Width = 49
    Height = 22
    MaxValue = 39
    MinValue = -1
    TabOrder = 12
    Value = 0
  end
  object seProjectileFrameLast: TSpinEdit
    Left = 552
    Top = 468
    Width = 49
    Height = 22
    MaxValue = 39
    MinValue = -1
    TabOrder = 13
    Value = 0
  end
  object seJumpFrame: TSpinEdit
    Left = 480
    Top = 500
    Width = 49
    Height = 22
    MaxValue = 39
    MinValue = -1
    TabOrder = 14
    Value = 0
  end
  object seFallFrame: TSpinEdit
    Left = 552
    Top = 500
    Width = 49
    Height = 22
    MaxValue = 39
    MinValue = -1
    TabOrder = 15
    Value = 0
  end
  object edSpriteName: TEdit
    Left = 200
    Top = 336
    Width = 177
    Height = 21
    MaxLength = 21
    TabOrder = 16
  end
end
