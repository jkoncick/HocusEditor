object SetDialog: TSetDialog
  Left = 365
  Top = 188
  BorderStyle = bsDialog
  Caption = 'SetDialog'
  ClientHeight = 159
  ClientWidth = 176
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
  object SetMapSize_Menu: TPanel
    Left = 0
    Top = 0
    Width = 177
    Height = 97
    BevelOuter = bvNone
    TabOrder = 0
    object SetMapSize_LbWidth: TLabel
      Left = 16
      Top = 16
      Width = 49
      Height = 13
      Caption = 'Map width'
    end
    object SetMapSize_LbHeight: TLabel
      Left = 16
      Top = 48
      Width = 53
      Height = 13
      Caption = 'Map height'
    end
    object SetMapSize_Width: TSpinEdit
      Left = 80
      Top = 16
      Width = 81
      Height = 22
      Enabled = False
      Increment = 2
      MaxValue = 240
      MinValue = 240
      TabOrder = 0
      Value = 32
    end
    object SetMapSize_Height: TSpinEdit
      Left = 80
      Top = 48
      Width = 81
      Height = 22
      Enabled = False
      Increment = 2
      MaxValue = 60
      MinValue = 60
      TabOrder = 1
      Value = 32
    end
  end
  object BtnOK: TButton
    Left = 8
    Top = 128
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 1
    OnClick = BtnOKClick
  end
  object BtnCancel: TButton
    Left = 96
    Top = 128
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = BtnCancelClick
  end
  object ShiftMap_Menu: TPanel
    Left = 0
    Top = 0
    Width = 177
    Height = 97
    BevelOuter = bvNone
    TabOrder = 3
    object ShiftMap_LbNumTiles: TLabel
      Left = 16
      Top = 64
      Width = 55
      Height = 13
      Caption = 'Tiles count:'
    end
    object ShiftMap_RbUp: TRadioButton
      Tag = 2
      Left = 32
      Top = 8
      Width = 57
      Height = 17
      Caption = 'Up'
      TabOrder = 0
      OnClick = ShiftMap_SelectDirection
    end
    object ShiftMap_RbDown: TRadioButton
      Tag = 4
      Left = 32
      Top = 32
      Width = 57
      Height = 17
      Caption = 'Down'
      TabOrder = 1
      OnClick = ShiftMap_SelectDirection
    end
    object ShiftMap_RbLeft: TRadioButton
      Tag = 1
      Left = 96
      Top = 8
      Width = 57
      Height = 17
      Caption = 'Left'
      TabOrder = 2
      OnClick = ShiftMap_SelectDirection
    end
    object ShiftMap_RbRight: TRadioButton
      Tag = 3
      Left = 96
      Top = 32
      Width = 57
      Height = 17
      Caption = 'Right'
      TabOrder = 3
      OnClick = ShiftMap_SelectDirection
    end
    object ShiftMap_NumTiles: TSpinEdit
      Left = 80
      Top = 64
      Width = 73
      Height = 22
      MaxValue = 128
      MinValue = 0
      TabOrder = 4
      Value = 1
    end
  end
  object LevelSelection_Menu: TPanel
    Left = 0
    Top = 0
    Width = 177
    Height = 121
    BevelOuter = bvNone
    TabOrder = 4
    object LevelSelection_List: TListBox
      Left = 0
      Top = 0
      Width = 177
      Height = 121
      Align = alClient
      Columns = 4
      ItemHeight = 13
      TabOrder = 0
      OnDblClick = BtnOKClick
    end
  end
  object TilesetSelection_Menu: TPanel
    Left = 0
    Top = 0
    Width = 177
    Height = 109
    BevelOuter = bvNone
    TabOrder = 5
    object TilesetSelection_List: TListBox
      Left = 0
      Top = 0
      Width = 177
      Height = 109
      Align = alClient
      Columns = 2
      ItemHeight = 13
      TabOrder = 0
      OnDblClick = BtnOKClick
    end
  end
end
