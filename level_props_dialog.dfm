object LevelPropertiesDialog: TLevelPropertiesDialog
  Left = 190
  Top = 60
  BorderStyle = bsDialog
  Caption = 'Level properties'
  ClientHeight = 617
  ClientWidth = 633
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
  object gbLevelSettings: TGroupBox
    Left = 8
    Top = 8
    Width = 617
    Height = 217
    Caption = ' Level settings '
    TabOrder = 0
    object lbLevelMonsterShootDelay: TLabel
      Left = 8
      Top = 24
      Width = 98
      Height = 13
      Caption = 'Monster shoot delay:'
    end
    object imgBackdropImage: TImage
      Left = 290
      Top = 10
      Width = 320
      Height = 200
      OnMouseDown = imgTilesetImageMouseDown
    end
    object lbLevelParTime: TLabel
      Left = 8
      Top = 56
      Width = 90
      Height = 13
      Caption = 'Par time (seconds):'
    end
    object lbLevelBackdrop: TLabel
      Left = 8
      Top = 88
      Width = 80
      Height = 13
      Caption = 'Backdrop image:'
    end
    object lbLevelMusic: TLabel
      Left = 8
      Top = 120
      Width = 31
      Height = 13
      Caption = 'Music:'
    end
    object lbLevelElevatorTiles: TLabel
      Left = 8
      Top = 160
      Width = 63
      Height = 13
      Caption = 'Elevator tiles:'
    end
    object imgLevelElevatorLeft: TImage
      Tag = 4
      Left = 96
      Top = 150
      Width = 32
      Height = 32
      OnClick = imgTileClick
      OnMouseDown = imgLevelElevatorMouseDown
    end
    object lbLevelElevatorLeft: TLabel
      Left = 144
      Top = 158
      Width = 12
      Height = 13
      Caption = '(0)'
    end
    object imgLevelElevatorRight: TImage
      Tag = 5
      Left = 184
      Top = 150
      Width = 32
      Height = 32
      OnClick = imgTileClick
      OnMouseDown = imgLevelElevatorMouseDown
    end
    object lbLevelElevatorRight: TLabel
      Left = 232
      Top = 158
      Width = 12
      Height = 13
      Caption = '(0)'
    end
    object seLevelMonsterShootDelay: TSpinEdit
      Left = 120
      Top = 20
      Width = 73
      Height = 22
      MaxValue = 65535
      MinValue = 0
      TabOrder = 0
      Value = 0
      OnChange = LevelPropertyChange
    end
    object seLevelParTime: TSpinEdit
      Left = 120
      Top = 52
      Width = 73
      Height = 22
      MaxValue = 65535
      MinValue = 0
      TabOrder = 1
      Value = 0
      OnChange = LevelPropertyChange
    end
    object cbxLevelBackdrop: TComboBox
      Left = 120
      Top = 84
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
      OnChange = LevelPropertyChange
    end
    object cbxLevelMusic: TComboBox
      Left = 120
      Top = 116
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 3
      OnChange = LevelPropertyChange
    end
  end
  object gbAnimationSettings: TGroupBox
    Left = 8
    Top = 232
    Width = 617
    Height = 177
    Caption = ' Animation settings '
    TabOrder = 1
    object lbAnimType: TLabel
      Left = 280
      Top = 24
      Width = 72
      Height = 13
      Caption = 'Animation type:'
    end
    object imgAnimAnimatedTiles: TImage
      Left = 144
      Top = 128
      Width = 256
      Height = 32
    end
    object lbAnimTiles: TLabel
      Left = 144
      Top = 24
      Width = 68
      Height = 13
      Caption = 'Animated tiles:'
    end
    object lbAnimBlankTile: TLabel
      Left = 432
      Top = 24
      Width = 46
      Height = 13
      Caption = 'Blank tile:'
    end
    object imgAnimBlankTile: TImage
      Left = 528
      Top = 16
      Width = 32
      Height = 32
      OnClick = imgTileClick
    end
    object imgAnimSwitchDownTile: TImage
      Tag = 1
      Left = 528
      Top = 56
      Width = 32
      Height = 32
      OnClick = imgTileClick
    end
    object imgAnimSwitchUpTile: TImage
      Tag = 2
      Left = 528
      Top = 96
      Width = 32
      Height = 32
      OnClick = imgTileClick
    end
    object imgAnimShootableTile: TImage
      Tag = 3
      Left = 528
      Top = 136
      Width = 32
      Height = 32
      OnClick = imgTileClick
    end
    object lbAnimSwitchDownTile: TLabel
      Left = 432
      Top = 64
      Width = 82
      Height = 13
      Caption = 'Switch Down tile:'
    end
    object lbAnimSwitchUpTile: TLabel
      Left = 432
      Top = 104
      Width = 68
      Height = 13
      Caption = 'Switch Up tile:'
    end
    object lbAnimShootableTile: TLabel
      Left = 432
      Top = 144
      Width = 67
      Height = 13
      Caption = 'Shootable tile:'
    end
    object lbAnimBlankTileIndex: TLabel
      Left = 576
      Top = 24
      Width = 12
      Height = 13
      Caption = '(0)'
    end
    object lbAnimSwitchDownTileIndex: TLabel
      Left = 576
      Top = 64
      Width = 12
      Height = 13
      Caption = '(0)'
    end
    object lbAnimSwitchUpTileIndex: TLabel
      Left = 576
      Top = 104
      Width = 12
      Height = 13
      Caption = '(0)'
    end
    object lbAnimShootableTileIndex: TLabel
      Left = 576
      Top = 144
      Width = 12
      Height = 13
      Caption = '(0)'
    end
    object Bevel1: TBevel
      Left = 416
      Top = 16
      Width = 9
      Height = 153
      Shape = bsLeftLine
    end
    object lbAnimFirstIndex: TLabel
      Left = 144
      Top = 52
      Width = 50
      Height = 13
      Caption = 'First index:'
    end
    object lbAnimLastIndex: TLabel
      Left = 144
      Top = 84
      Width = 51
      Height = 13
      Caption = 'Last index:'
    end
    object lstAnimationList: TListBox
      Left = 8
      Top = 24
      Width = 121
      Height = 145
      ItemHeight = 13
      TabOrder = 0
      OnClick = lstAnimationListClick
    end
    object rbAnimTypeNone: TRadioButton
      Left = 280
      Top = 48
      Width = 113
      Height = 17
      Caption = 'No animation'
      TabOrder = 1
      OnClick = rbAnimTypeClick
    end
    object rbAnimTypePermanent: TRadioButton
      Tag = 1
      Left = 280
      Top = 72
      Width = 121
      Height = 17
      Caption = 'Permanent animation'
      TabOrder = 2
      OnClick = rbAnimTypeClick
    end
    object rbAnimTypeRandom: TRadioButton
      Tag = 2
      Left = 280
      Top = 96
      Width = 113
      Height = 17
      Caption = 'Random animation'
      TabOrder = 3
      OnClick = rbAnimTypeClick
    end
    object seAnimFirstIndex: TSpinEdit
      Left = 200
      Top = 48
      Width = 65
      Height = 22
      MaxValue = 255
      MinValue = 0
      TabOrder = 4
      Value = 0
      OnChange = seAnimFirstIndexChange
    end
    object seAnimLastIndex: TSpinEdit
      Left = 200
      Top = 80
      Width = 65
      Height = 22
      MaxValue = 255
      MinValue = 0
      TabOrder = 5
      Value = 0
      OnChange = seAnimLastIndexChange
    end
  end
  object gbMonsterSettings: TGroupBox
    Left = 8
    Top = 416
    Width = 617
    Height = 193
    Caption = ' Monster settings '
    TabOrder = 2
    object lbMonsterSpriteSet: TLabel
      Left = 144
      Top = 20
      Width = 47
      Height = 13
      Caption = 'Sprite set:'
    end
    object lbMonsterHealth: TLabel
      Left = 144
      Top = 50
      Width = 34
      Height = 13
      Caption = 'Health:'
    end
    object lbMonsterProjectileHSpeed: TLabel
      Left = 144
      Top = 80
      Width = 126
      Height = 13
      Caption = 'Projectile horizontal speed:'
    end
    object lbMonsterProjectileVSpeed: TLabel
      Left = 144
      Top = 110
      Width = 115
      Height = 13
      Caption = 'Projectile vertical speed:'
    end
    object lbMonsterProjectileOffset: TLabel
      Left = 144
      Top = 140
      Width = 85
      Height = 13
      Hint = 'For vertical projectiles only'
      Caption = 'Projectile X-offset:'
      ParentShowHint = False
      ShowHint = True
    end
    object lbMonsterHealthInfo1: TLabel
      Left = 288
      Top = 50
      Width = 59
      Height = 13
      Hint = 'BOSS = Show health bar, kill on touch'
      Caption = '20+ = BOSS'
      ParentShowHint = False
      ShowHint = True
    end
    object lbMonsterBehavior: TLabel
      Left = 376
      Top = 20
      Width = 45
      Height = 13
      Caption = 'Behavior:'
    end
    object lbMonsterPixelData: TLabel
      Left = 376
      Top = 168
      Width = 86
      Height = 13
      Caption = 'Pixel data: 0 bytes'
    end
    object imgSpriteImage: TImage
      Left = 504
      Top = 72
      Width = 104
      Height = 80
    end
    object lstMonsterList: TListBox
      Left = 8
      Top = 24
      Width = 121
      Height = 161
      ItemHeight = 13
      TabOrder = 0
      OnClick = lstMonsterListClick
    end
    object cbxMonsterSpriteSet: TComboBox
      Left = 200
      Top = 16
      Width = 161
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
      OnChange = MonsterPropertyChange
    end
    object seMonsterHealth: TSpinEdit
      Left = 200
      Top = 46
      Width = 73
      Height = 22
      MaxValue = 65535
      MinValue = 0
      TabOrder = 2
      Value = 0
      OnChange = MonsterPropertyChange
    end
    object seMonsterProjectileHSpeed: TSpinEdit
      Left = 288
      Top = 76
      Width = 73
      Height = 22
      MaxValue = 65535
      MinValue = 0
      TabOrder = 3
      Value = 0
      OnChange = MonsterPropertyChange
    end
    object seMonsterProjectileVSpeed: TSpinEdit
      Left = 288
      Top = 106
      Width = 73
      Height = 22
      MaxValue = 65535
      MinValue = 0
      TabOrder = 4
      Value = 0
      OnChange = MonsterPropertyChange
    end
    object cbMonsterTargetPlayer: TCheckBox
      Left = 376
      Top = 70
      Width = 89
      Height = 17
      Hint = 
        'Projectiles are targeting player, flying monsters fly towards pl' +
        'ayer'
      Caption = 'Target player'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      OnClick = MonsterPropertyChange
    end
    object cbMonsterShootProjectiles: TCheckBox
      Left = 376
      Top = 46
      Width = 145
      Height = 17
      Caption = 'Monster fires projectiles'
      TabOrder = 6
      OnClick = MonsterPropertyChange
    end
    object cbMonsterWobblyProjectiles: TCheckBox
      Left = 376
      Top = 94
      Width = 121
      Height = 17
      Caption = 'Wobbly projectiles'
      TabOrder = 7
      OnClick = MonsterPropertyChange
    end
    object cbxMonsterBehavior: TComboBox
      Left = 432
      Top = 16
      Width = 177
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 8
      OnChange = MonsterPropertyChange
      Items.Strings = (
        '0: Walk'
        '1: Walk and charge'
        '2: Fly'
        '3: Fly, face player'
        '4: Stand still (Monk)'
        '5: Stand still'
        '6: Fly fast, face player '
        '7: (unused)'
        '8: Stand still (Dragon)'
        '9: (unused)'
        '10: (unused)'
        '99: Trolodon')
    end
    object seMonsterProjectileOffset: TSpinEdit
      Left = 288
      Top = 136
      Width = 73
      Height = 22
      MaxValue = 65535
      MinValue = 0
      TabOrder = 9
      Value = 0
      OnChange = MonsterPropertyChange
    end
    object cbMonsterUnknown2: TCheckBox
      Left = 376
      Top = 118
      Width = 89
      Height = 17
      Caption = 'Unknown 1'
      TabOrder = 10
      OnClick = MonsterPropertyChange
    end
    object cbMonsterUnknown3: TCheckBox
      Left = 376
      Top = 142
      Width = 97
      Height = 17
      Caption = 'Unknown 2'
      TabOrder = 11
      OnClick = MonsterPropertyChange
    end
    object btnClearMonster: TButton
      Left = 144
      Top = 164
      Width = 97
      Height = 22
      Caption = 'Clear monster'
      TabOrder = 12
      OnClick = btnClearMonsterClick
    end
    object btnVRAMUsage: TButton
      Left = 512
      Top = 164
      Width = 97
      Height = 22
      Caption = 'VRAM Usage...'
      TabOrder = 13
      OnClick = btnVRAMUsageClick
    end
  end
  object pnTilesetImage: TPanel
    Left = 144
    Top = 224
    Width = 329
    Height = 209
    TabOrder = 3
    Visible = False
    object imgTilesetImage: TImage
      Left = 4
      Top = 4
      Width = 320
      Height = 200
      OnMouseDown = imgTilesetImageMouseDown
    end
  end
  object pnVRAMUsage: TPanel
    Left = 376
    Top = 224
    Width = 249
    Height = 384
    TabOrder = 4
    Visible = False
    object vlVRAMUsage: TValueListEditor
      Left = 4
      Top = 4
      Width = 241
      Height = 349
      TabOrder = 0
      TitleCaptions.Strings = (
        'Attribute'
        'Value')
      ColWidths = (
        141
        94)
    end
    object btnVRAMUsageClose: TButton
      Left = 88
      Top = 356
      Width = 72
      Height = 22
      Caption = 'Close'
      TabOrder = 1
      OnClick = btnVRAMUsageCloseClick
    end
  end
end
