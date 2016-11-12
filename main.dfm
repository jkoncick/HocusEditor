object MainWindow: TMainWindow
  Left = 190
  Top = 111
  Width = 950
  Height = 650
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  Caption = 'Hocus Pocus Level Editor'
  Color = clBtnFace
  Constraints.MinHeight = 544
  Constraints.MinWidth = 528
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Menu = AppMenu
  OldCreateOrder = False
  Visible = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  OnKeyDown = FormKeyDown
  OnMouseWheelDown = FormMouseWheelDown
  OnMouseWheelUp = FormMouseWheelUp
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object MapCanvas: TImage
    Left = 4
    Top = 4
    Width = 1
    Height = 1
    OnDblClick = MapCanvasDblClick
    OnMouseDown = MapCanvasMouseDown
    OnMouseMove = MapCanvasMouseMove
    OnMouseUp = MapCanvasMouseUp
  end
  object CursorImage: TImage
    Left = 4
    Top = 4
    Width = 1
    Height = 1
    Visible = False
    OnMouseDown = CursorImageMouseDown
    OnMouseMove = CursorImageMouseMove
    OnMouseUp = MapCanvasMouseUp
  end
  object MapScrollH: TScrollBar
    Left = 4
    Top = 456
    Width = 480
    Height = 16
    LargeChange = 4
    PageSize = 0
    TabOrder = 0
    OnChange = MapScrollChange
  end
  object MapScrollV: TScrollBar
    Left = 488
    Top = 4
    Width = 16
    Height = 448
    Kind = sbVertical
    LargeChange = 4
    PageSize = 0
    TabOrder = 1
    OnChange = MapScrollChange
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 585
    Width = 942
    Height = 19
    Panels = <
      item
        Width = 80
      end
      item
        Width = 64
      end
      item
        Text = 'Tileset name'
        Width = 70
      end
      item
        Width = 64
      end
      item
        Text = 'No map loaded'
        Width = 180
      end
      item
        Width = 250
      end
      item
        Width = 100
      end
      item
        Text = 'v1.2'
        Width = 0
      end>
  end
  object EditorMenu: TPanel
    Left = 512
    Top = 0
    Width = 280
    Height = 585
    TabOrder = 3
    object MiniMapFrame: TBevel
      Left = 18
      Top = 6
      Width = 244
      Height = 64
      Shape = bsFrame
      Style = bsRaised
    end
    object MiniMap: TImage
      Left = 20
      Top = 8
      Width = 240
      Height = 60
      OnMouseDown = MiniMapMouseDown
      OnMouseMove = MiniMapMouseMove
    end
    object sbBackgroundLayer: TSpeedButton
      Left = 8
      Top = 80
      Width = 38
      Height = 22
      Hint = 'Background layer (Ctrl+B)'
      AllowAllUp = True
      GroupIndex = 1
      Down = True
      Caption = 'Bgnd'
      ParentShowHint = False
      ShowHint = True
      OnClick = RenderSettingChange
    end
    object sbForegroundLayer: TSpeedButton
      Left = 46
      Top = 80
      Width = 38
      Height = 22
      Hint = 'Foreground layer (Ctrl+F)'
      AllowAllUp = True
      GroupIndex = 2
      Down = True
      Caption = 'Fgnd'
      ParentShowHint = False
      ShowHint = True
      OnClick = RenderSettingChange
    end
    object sbHiddenLayer: TSpeedButton
      Left = 84
      Top = 80
      Width = 38
      Height = 22
      Hint = 'Hidden layer (Ctrl+H)'
      AllowAllUp = True
      GroupIndex = 3
      Down = True
      Caption = 'Hdn'
      ParentShowHint = False
      ShowHint = True
      OnClick = RenderSettingChange
    end
    object sbObjectLayer: TSpeedButton
      Left = 122
      Top = 80
      Width = 38
      Height = 22
      Hint = 'Object layer (Ctrl+J)'
      AllowAllUp = True
      GroupIndex = 4
      Down = True
      Caption = 'Obj'
      ParentShowHint = False
      ShowHint = True
      OnClick = RenderSettingChange
    end
    object sbShowMarkers: TSpeedButton
      Left = 160
      Top = 80
      Width = 38
      Height = 22
      Hint = 'Show markers (Ctrl+M)'
      AllowAllUp = True
      GroupIndex = 5
      Down = True
      Caption = 'Mark'
      ParentShowHint = False
      ShowHint = True
      OnClick = RenderSettingChange
    end
    object sbShowGrid: TSpeedButton
      Left = 236
      Top = 80
      Width = 38
      Height = 22
      Hint = 'Show grid (Ctrl+G)'
      AllowAllUp = True
      GroupIndex = 7
      Caption = 'Grid'
      ParentShowHint = False
      ShowHint = True
      OnClick = RenderSettingChange
    end
    object sbDrawSprites: TSpeedButton
      Left = 198
      Top = 80
      Width = 38
      Height = 22
      Hint = 'Draw sprites (Ctrl+P)'
      AllowAllUp = True
      GroupIndex = 6
      Caption = 'Sprit'
      ParentShowHint = False
      ShowHint = True
      OnClick = RenderSettingChange
    end
    object LayerPages: TPageControl
      Left = 1
      Top = 112
      Width = 278
      Height = 472
      ActivePage = LrPageBackground
      Align = alBottom
      TabOrder = 0
      OnChange = LayerPagesChange
      object LrPageBackground: TTabSheet
        Caption = 'Background'
        object TileLayerControlsPanel: TPanel
          Left = 0
          Top = 0
          Width = 270
          Height = 444
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          object BlockFrame: TBevel
            Left = 4
            Top = 4
            Width = 260
            Height = 260
            Shape = bsFrame
            Style = bsRaised
          end
          object BlockImage: TImage
            Left = 6
            Top = 6
            Width = 256
            Height = 256
            OnMouseDown = BlockImageMouseDown
          end
          object cbAllLayers: TCheckBox
            Left = 24
            Top = 342
            Width = 97
            Height = 17
            Caption = 'All layers mode'
            TabOrder = 3
            OnClick = EditingModeChange
          end
          object rbTileMode: TRadioButton
            Left = 8
            Top = 272
            Width = 125
            Height = 17
            Caption = 'Tile mode'
            Checked = True
            TabOrder = 0
            TabStop = True
            OnClick = EditingModeChange
          end
          object rbBlockMode: TRadioButton
            Left = 8
            Top = 320
            Width = 77
            Height = 17
            Caption = 'Block mode'
            TabOrder = 2
            OnClick = EditingModeChange
          end
          object rbPatternMode: TRadioButton
            Left = 8
            Top = 296
            Width = 125
            Height = 17
            Caption = 'Pattern mode'
            TabOrder = 1
            OnClick = EditingModeChange
          end
          object PaintMethodPanel: TPanel
            Left = 140
            Top = 272
            Width = 125
            Height = 81
            BevelOuter = bvNone
            TabOrder = 4
            object lbBrushWidth: TLabel
              Left = 0
              Top = 24
              Width = 11
              Height = 13
              Caption = 'W'
            end
            object lbBrushHeight: TLabel
              Left = 0
              Top = 56
              Width = 8
              Height = 13
              Caption = 'H'
            end
            object lbBrushWidthVal: TLabel
              Left = 112
              Top = 24
              Width = 6
              Height = 13
              Caption = '1'
            end
            object lbBrushHeightVal: TLabel
              Left = 112
              Top = 56
              Width = 6
              Height = 13
              Caption = '1'
            end
            object lbBrushSize: TLabel
              Left = 0
              Top = 0
              Width = 51
              Height = 13
              Caption = 'Brush size:'
            end
            object tbBrushWidth: TTrackBar
              Left = 11
              Top = 19
              Width = 100
              Height = 30
              Max = 8
              Min = 1
              PageSize = 1
              Position = 1
              TabOrder = 0
              ThumbLength = 16
              OnChange = tbBrushSizeChange
            end
            object tbBrushHeight: TTrackBar
              Left = 11
              Top = 51
              Width = 100
              Height = 30
              Max = 8
              Min = 1
              PageSize = 1
              Position = 1
              TabOrder = 1
              ThumbLength = 16
              OnChange = tbBrushSizeChange
            end
          end
          object btnSavePreset: TButton
            Left = 140
            Top = 356
            Width = 121
            Height = 21
            Caption = 'Save pattern as preset'
            TabOrder = 5
            Visible = False
            OnClick = btnSavePresetClick
          end
        end
      end
      object LrPageForeground: TTabSheet
        Caption = 'Foreground'
        ImageIndex = 1
      end
      object LrPageHidden: TTabSheet
        Caption = 'Hidden       '
        ImageIndex = 2
      end
      object LrPageObject: TTabSheet
        Caption = 'Objects       '
        ImageIndex = 3
        object sbStartPosition: TSpeedButton
          Tag = 99
          Left = 92
          Top = 96
          Width = 40
          Height = 40
          Hint = 'Start Position'
          AllowAllUp = True
          GroupIndex = 10
          Glyph.Data = {
            36080000424D3608000000000000360400002800000020000000200000000100
            08000000000000040000130B0000130B00000001000000000000000000000000
            00000C0C180038202000202038002C2820000028450028284100202C59003C38
            2C0030304D0000385D00593C3C002C386D004D49380038498600615949007959
            590045559E0071695500D77914000061AE005565B600867965009A797900EB96
            1C00968A71006175CF00008EC700AA9A82007186E700BA9E9E0045A2C700869A
            FF0000C3E300DBC7C700CBCBDB006DD3E300FFF3F3009AFFFF00F7F7FF000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000280000000505
            1A1A101003030C0C11111F1F1818111100000303030300000000000000000505
            1A1A101003030C0C11111F1F1818111100000303030300000000000000000505
            13131A1A10100C0C1111111111110C0C11110303000000000000000000000505
            13131A1A10100C0C1111111111110C0C11110303000000000000000000000505
            0909131317171010090909090C0C030300000000000000000000000000000505
            0909131317171010090909090C0C030300000000000000000000000000000000
            050509090909101018181A1A1D1D18180C0C0303000000000000000000000000
            050509090909101018181A1A1D1D18180C0C0303000000000000000000000000
            090911111F1F23231F1F0D0D12121B1B1E1E1616000000000000000000000000
            090911111F1F23231F1F0D0D12121B1B1E1E1616000000000000050505051111
            1F1F2626232307070D0D16161B1B161612120F0F161600000000050505051111
            1F1F2626232307070D0D16161B1B161612120F0F161600000000030300002323
            232308080F0F161612121B1B21211E1E1E1E2121212100000000030300002323
            232308080F0F161612121B1B21211E1E1E1E21212121000000000C0C00000000
            04040F0F1B1B0F0F121216161B1B1B1B242414141B1B212100000C0C00000000
            04040F0F1B1B0F0F121216161B1B1B1B242414141B1B21210000111105050000
            04040D0D12121B1B121216161E1E161628281919161600000000111105050000
            04040D0D12121B1B121216161E1E161628281919161600000000181811110505
            020206060A0A0D0D121216161B1B1E1E121216161B1B00000000181811110505
            020206060A0A0D0D121216161B1B1E1E121216161B1B00000000101018180E0E
            050500000A0A0D0D0D0D0F0F1212161612120F0F0D0D00000000101018180E0E
            050500000A0A0D0D0D0D0F0F1212161612120F0F0D0D00000000000013131F1F
            1010050505050B0B151520202525272722221C1C151500000000000013131F1F
            1010050505050B0B151520202525272722221C1C151500000000000010101818
            232313130E0E0909050513131A1A1D1D1A1A1313000000000000000010101818
            232313130E0E0909050513131A1A1D1D1A1A1313000000000000000000001313
            18181F1F18181313101010101313101009090000000000000000000000001313
            18181F1F18181313101010101313101009090000000000000000000000000000
            0000101013131717171713130E0E090900000000000000000000000000000000
            0000101013131717171713130E0E090900000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000}
          ParentShowHint = False
          ShowHint = True
          OnClick = sbObjectClick
        end
        object lbStartPosition: TLabel
          Left = 140
          Top = 120
          Width = 61
          Height = 13
          Caption = 'Start at: 0 , 0'
        end
        object lbActiveObjectMode: TLabel
          Left = 140
          Top = 100
          Width = 31
          Height = 13
          Caption = 'None'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object ObjectTypePages: TPageControl
          Left = 0
          Top = 144
          Width = 270
          Height = 300
          ActivePage = OTPageMonsters
          Align = alBottom
          MultiLine = True
          TabOrder = 0
          OnChange = ObjectTypePagesChange
          object OTPageMonsters: TTabSheet
            Caption = 'Monsters'
            ImageIndex = 1
            object lbMonsterList: TLabel
              Left = 96
              Top = 0
              Width = 133
              Height = 13
              Caption = 'Triggered monsters (max. 8):'
            end
            object lbMonsterTypeList: TLabel
              Left = 96
              Top = 168
              Width = 96
              Height = 13
              Caption = 'Select monster type:'
            end
            object sbMonsterTrigger: TSpeedButton
              Tag = 101
              Left = 184
              Top = 136
              Width = 77
              Height = 22
              Hint = 'Monster trigger'
              AllowAllUp = True
              GroupIndex = 10
              Caption = 'Place trigger'
              Layout = blGlyphRight
              OnClick = sbObjectClick
            end
            object sbMonsterMonster: TSpeedButton
              Tag = 100
              Left = 96
              Top = 136
              Width = 81
              Height = 22
              Hint = 'Monster'
              AllowAllUp = True
              GroupIndex = 10
              Caption = 'Place monster'
              OnClick = sbObjectClick
            end
            object lstMonsterTriggerList: TListBox
              Left = 0
              Top = 0
              Width = 89
              Height = 254
              Align = alLeft
              ItemHeight = 13
              TabOrder = 0
              OnClick = lstMonsterTriggerListClick
              OnDblClick = lstMonsterListDblClick
            end
            object lstMonsterTypeList: TListBox
              Left = 96
              Top = 184
              Width = 161
              Height = 113
              ItemHeight = 13
              TabOrder = 1
            end
            object lstMonsterList: TListBox
              Left = 96
              Top = 16
              Width = 161
              Height = 113
              ItemHeight = 13
              TabOrder = 2
              OnDblClick = lstMonsterListDblClick
            end
          end
          object OTPageTeleports: TTabSheet
            Caption = 'Teleports'
            object lbTeleportStartPos: TLabel
              Left = 96
              Top = 4
              Width = 61
              Height = 13
              Caption = 'Start at: 0 , 0'
              OnDblClick = lbTeleportStartPosDblClick
            end
            object lbTeleportTargetPos: TLabel
              Left = 96
              Top = 52
              Width = 70
              Height = 13
              Caption = 'Target at: 0 , 0'
              OnDblClick = lbTeleportTargetPosDblClick
            end
            object sbTeleportStart: TSpeedButton
              Tag = 200
              Left = 96
              Top = 24
              Width = 69
              Height = 22
              Hint = 'Teleport start'
              AllowAllUp = True
              GroupIndex = 10
              Caption = 'Set start'
              Layout = blGlyphRight
              OnClick = sbObjectClick
            end
            object sbTeleportTarget: TSpeedButton
              Tag = 201
              Left = 96
              Top = 72
              Width = 69
              Height = 22
              Hint = 'Teleport target'
              AllowAllUp = True
              GroupIndex = 10
              Caption = 'Set target'
              Layout = blGlyphRight
              OnClick = sbObjectClick
            end
            object lstTeleportList: TListBox
              Left = 0
              Top = 0
              Width = 89
              Height = 254
              Align = alLeft
              ItemHeight = 13
              TabOrder = 0
              OnClick = lstTeleportListClick
              OnDblClick = lbTeleportStartPosDblClick
            end
          end
          object OTPageSwitches: TTabSheet
            Caption = 'Switches'
            ImageIndex = 2
            object lbSwitchList: TLabel
              Left = 96
              Top = 48
              Width = 127
              Height = 13
              Caption = 'Switches in group (max. 4):'
            end
            object sbSwitchUp: TSpeedButton
              Tag = 301
              Left = 176
              Top = 152
              Width = 81
              Height = 22
              Hint = 'Switch (up)'
              AllowAllUp = True
              GroupIndex = 10
              Caption = 'Up-state'
              OnClick = sbObjectClick
            end
            object sbSwitchDown: TSpeedButton
              Tag = 300
              Left = 96
              Top = 152
              Width = 77
              Height = 22
              Hint = 'Switch (down)'
              AllowAllUp = True
              GroupIndex = 10
              Caption = 'Down-state'
              Layout = blGlyphRight
              OnClick = sbObjectClick
            end
            object lbSwitchPlace: TLabel
              Left = 96
              Top = 136
              Width = 74
              Height = 13
              Caption = 'Place switches:'
            end
            object lbSwitchTarget: TLabel
              Left = 96
              Top = 184
              Width = 112
              Height = 13
              Caption = 'Target area: 0 : 0 , 0 : 0'
              OnDblClick = lbSwitchTargetDblClick
            end
            object sbSwitchTarget: TSpeedButton
              Tag = 302
              Left = 96
              Top = 200
              Width = 113
              Height = 22
              Hint = 'Switch target area'
              AllowAllUp = True
              GroupIndex = 10
              Caption = 'Set Target area'
              Layout = blGlyphRight
              OnClick = sbObjectClick
            end
            object lbSwitchType: TLabel
              Left = 96
              Top = 0
              Width = 58
              Height = 13
              Caption = 'Switch type:'
            end
            object lstSwitchGroupList: TListBox
              Left = 0
              Top = 0
              Width = 89
              Height = 254
              Align = alLeft
              ItemHeight = 13
              TabOrder = 0
              OnClick = lstSwitchGroupListClick
              OnDblClick = lbSwitchTargetDblClick
            end
            object clbSwitchList: TCheckListBox
              Left = 96
              Top = 64
              Width = 161
              Height = 65
              OnClickCheck = clbSwitchListClickCheck
              ItemHeight = 13
              TabOrder = 1
              OnDblClick = clbSwitchListDblClick
            end
            object cbxSwitchType: TComboBox
              Left = 96
              Top = 16
              Width = 161
              Height = 21
              Style = csDropDownList
              ItemHeight = 13
              ItemIndex = 0
              TabOrder = 2
              Text = 'Remove wall'
              Items.Strings = (
                'Remove wall'
                'Insert wall')
            end
            object btnSwitchTargetClear: TButton
              Left = 212
              Top = 200
              Width = 45
              Height = 22
              Caption = 'Clear'
              TabOrder = 3
              OnClick = btnSwitchTargetClearClick
            end
          end
          object OTPageInsertWall: TTabSheet
            Caption = 'Insert wall'
            ImageIndex = 3
            object sbInsertTarget: TSpeedButton
              Tag = 401
              Left = 96
              Top = 88
              Width = 113
              Height = 22
              Hint = 'Insert wall target'
              AllowAllUp = True
              GroupIndex = 10
              Caption = 'Set Target area'
              Layout = blGlyphRight
              OnClick = sbObjectClick
            end
            object lbInsertTarget: TLabel
              Left = 96
              Top = 72
              Width = 112
              Height = 13
              Caption = 'Target area: 0 : 0 , 0 : 0'
              OnDblClick = lbInsertTargetDblClick
            end
            object lbInsertKeyType: TLabel
              Left = 96
              Top = 0
              Width = 66
              Height = 13
              Caption = 'Required key:'
            end
            object lbInsertTrigger: TLabel
              Left = 96
              Top = 48
              Width = 61
              Height = 13
              Caption = 'Trigger tile: 0'
            end
            object sbInsertTrigger: TSpeedButton
              Tag = 400
              Left = 180
              Top = 44
              Width = 77
              Height = 22
              Hint = 'Insert wall trigger'
              AllowAllUp = True
              GroupIndex = 10
              Caption = 'Place trigger'
              Layout = blGlyphRight
              OnClick = sbObjectClick
            end
            object lstInsertTriggerList: TListBox
              Left = 0
              Top = 0
              Width = 89
              Height = 254
              Align = alLeft
              ItemHeight = 13
              TabOrder = 0
              OnClick = lstInsertTriggerListClick
              OnDblClick = lbInsertTargetDblClick
            end
            object cbxInsertKeyType: TComboBox
              Left = 96
              Top = 16
              Width = 161
              Height = 21
              Style = csDropDownList
              ItemHeight = 13
              ItemIndex = 0
              TabOrder = 1
              Text = 'No key required'
              Items.Strings = (
                'No key required'
                'Silver key'
                'Gold key')
            end
            object btnInsertTargetClear: TButton
              Left = 212
              Top = 88
              Width = 45
              Height = 22
              Caption = 'Clear'
              TabOrder = 2
              OnClick = btnInsertTargetClearClick
            end
          end
          object OTPageRemoveWall: TTabSheet
            Caption = 'Remove wall'
            ImageIndex = 4
            object lbRemoveKeyType: TLabel
              Left = 96
              Top = 0
              Width = 66
              Height = 13
              Caption = 'Required key:'
            end
            object lbRemoveTarget: TLabel
              Left = 96
              Top = 72
              Width = 112
              Height = 13
              Caption = 'Target area: 0 : 0 , 0 : 0'
              OnDblClick = lbRemoveTargetDblClick
            end
            object sbRemoveTarget: TSpeedButton
              Tag = 501
              Left = 96
              Top = 88
              Width = 113
              Height = 22
              Hint = 'Remove wall target'
              AllowAllUp = True
              GroupIndex = 10
              Caption = 'Set Target area'
              Layout = blGlyphRight
              OnClick = sbObjectClick
            end
            object lbRemoveTrigger: TLabel
              Left = 96
              Top = 48
              Width = 61
              Height = 13
              Caption = 'Trigger tile: 0'
            end
            object sbRemoveTrigger: TSpeedButton
              Tag = 500
              Left = 180
              Top = 44
              Width = 77
              Height = 22
              Hint = 'Remove wall trigger'
              AllowAllUp = True
              GroupIndex = 10
              Caption = 'Place trigger'
              Layout = blGlyphRight
              OnClick = sbObjectClick
            end
            object lstRemoveTriggerList: TListBox
              Left = 0
              Top = 0
              Width = 89
              Height = 254
              Align = alLeft
              ItemHeight = 13
              TabOrder = 0
              OnClick = lstRemoveTriggerListClick
              OnDblClick = lbRemoveTargetDblClick
            end
            object cbxRemoveKeyType: TComboBox
              Left = 96
              Top = 16
              Width = 161
              Height = 21
              Style = csDropDownList
              ItemHeight = 13
              ItemIndex = 0
              TabOrder = 1
              Text = 'No key required'
              Items.Strings = (
                'No key required'
                'Silver key'
                'Gold key')
            end
            object btnRemoveTargetClear: TButton
              Left = 212
              Top = 88
              Width = 45
              Height = 22
              Caption = 'Clear'
              TabOrder = 2
              OnClick = btnRemoveTargetClearClick
            end
          end
          object OTPageMessages: TTabSheet
            Caption = 'Messages'
            ImageIndex = 5
            object sbMessagePosition: TSpeedButton
              Tag = 600
              Left = 96
              Top = 28
              Width = 95
              Height = 22
              Hint = 'Place message'
              AllowAllUp = True
              GroupIndex = 10
              Caption = 'Place message'
              Layout = blGlyphRight
              OnClick = sbObjectClick
            end
            object lbMessagePosition: TLabel
              Left = 96
              Top = 8
              Width = 64
              Height = 13
              Caption = 'Position: 0 , 0'
              OnDblClick = lbMessagePositionDblClick
            end
            object lstMessageList: TListBox
              Left = 0
              Top = 0
              Width = 89
              Height = 254
              Align = alLeft
              ItemHeight = 13
              TabOrder = 0
              OnClick = lstMessageListClick
              OnDblClick = lbMessagePositionDblClick
            end
            object mMessageText: TMemo
              Left = 96
              Top = 56
              Width = 161
              Height = 161
              MaxLength = 500
              ScrollBars = ssHorizontal
              TabOrder = 1
              WordWrap = False
            end
            object btnMessageTextSave: TButton
              Left = 194
              Top = 28
              Width = 63
              Height = 22
              Caption = 'Save text'
              TabOrder = 2
              OnClick = btnMessageTextSaveClick
            end
          end
        end
      end
    end
  end
  object AppMenu: TMainMenu
    object File1: TMenuItem
      Caption = 'File'
      object Newmap1: TMenuItem
        Caption = 'New map'
        ShortCut = 16462
        OnClick = Newmap1Click
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Openmap1: TMenuItem
        Caption = 'Open map'
        ShortCut = 16463
        OnClick = Openmap1Click
      end
      object Reopenmap1: TMenuItem
        Caption = 'Reopen map'
        ShortCut = 16466
        OnClick = Reopenmap1Click
      end
      object Importmap1: TMenuItem
        Caption = 'Import map'
        ShortCut = 16457
        OnClick = Importmap1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Savemap1: TMenuItem
        Caption = 'Save map'
        ShortCut = 16467
        OnClick = Savemap1Click
      end
      object Savemapas1: TMenuItem
        Caption = 'Save map as...'
        OnClick = Savemapas1Click
      end
      object Exportmap1: TMenuItem
        Caption = 'Export map'
        ShortCut = 16453
        OnClick = Exportmap1Click
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object Savemapimage1: TMenuItem
        Caption = 'Save map image'
        OnClick = Savemapimage1Click
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object Tools1: TMenuItem
      Caption = 'Tools'
      object Exportfile1: TMenuItem
        Caption = 'Export file'
        ShortCut = 120
        OnClick = Exportfile1Click
      end
      object Importfile1: TMenuItem
        Caption = 'Import file'
        ShortCut = 121
        OnClick = Importfile1Click
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object Sprites1: TMenuItem
        Caption = 'Sprites...'
        ShortCut = 122
        OnClick = Sprites1Click
      end
      object Miscgraphics1: TMenuItem
        Caption = 'Graphics...'
        OnClick = Miscgraphics1Click
      end
      object N12: TMenuItem
        Caption = '-'
      end
      object Createexepatch1: TMenuItem
        Caption = 'Create exe patch'
        OnClick = Createexepatch1Click
      end
      object Applyexepatch1: TMenuItem
        Caption = 'Apply exe patch'
        OnClick = Applyexepatch1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Applymodpatch1: TMenuItem
        Caption = 'Apply mod patch'
        OnClick = Applymodpatch1Click
      end
    end
    object Edit1: TMenuItem
      Caption = 'Edit'
      object Undo1: TMenuItem
        Caption = 'Undo'
        Enabled = False
        ShortCut = 16474
        OnClick = Undo1Click
      end
      object Redo1: TMenuItem
        Caption = 'Redo'
        Enabled = False
        ShortCut = 16473
        OnClick = Redo1Click
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object Copy1: TMenuItem
        Caption = 'Copy'
        ShortCut = 16451
        OnClick = Copy1Click
      end
      object Paste1: TMenuItem
        Caption = 'Paste'
        ShortCut = 16470
        OnClick = Paste1Click
      end
    end
    object ileset1: TMenuItem
      Caption = 'Tileset'
      object Selecttileset1: TMenuItem
        Caption = 'Change tileset...'
        OnClick = Selecttileset1Click
      end
      object Selectnext1: TMenuItem
        Caption = 'Select next'
        ShortCut = 16468
        OnClick = Selectnext1Click
      end
      object Loadcustomimage1: TMenuItem
        Caption = 'Load custom image'
        OnClick = Loadcustomimage1Click
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Usepredefinedtiles1: TMenuItem
        AutoCheck = True
        Caption = 'Use predefined tiles'
        Checked = True
        ShortCut = 16469
        OnClick = Usepredefinedtiles1Click
      end
    end
    object Map1: TMenuItem
      Caption = 'Map'
      object Setmapsize1: TMenuItem
        Caption = 'Set map size'
        ShortCut = 116
        OnClick = Setmapsize1Click
      end
      object Shiftmap1: TMenuItem
        Caption = 'Shift map'
        ShortCut = 117
        OnClick = Shiftmap1Click
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object Levelproperties1: TMenuItem
        Caption = 'Level properties...'
        ShortCut = 118
        OnClick = Levelproperties1Click
      end
    end
    object Test1: TMenuItem
      Caption = 'Test'
      object LaunchGame1: TMenuItem
        Caption = 'Launch game'
        ShortCut = 119
        OnClick = LaunchGame1Click
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object Easy1: TMenuItem
        AutoCheck = True
        Caption = 'Easy'
        GroupIndex = 5
        RadioItem = True
        OnClick = TestMapDifficultyClick
      end
      object Moderate1: TMenuItem
        Tag = 1
        AutoCheck = True
        Caption = 'Moderate'
        Checked = True
        GroupIndex = 5
        RadioItem = True
        OnClick = TestMapDifficultyClick
      end
      object Hard1: TMenuItem
        Tag = 2
        AutoCheck = True
        Caption = 'Hard'
        GroupIndex = 5
        RadioItem = True
        OnClick = TestMapDifficultyClick
      end
    end
    object Help1: TMenuItem
      Caption = 'Help'
      object KeyShortcuts1: TMenuItem
        Caption = 'Key Shortcuts'
        OnClick = KeyShortcuts1Click
      end
      object Mouseactions1: TMenuItem
        Caption = 'Mouse actions'
        OnClick = Mouseactions1Click
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object About1: TMenuItem
        Caption = 'About...'
        OnClick = About1Click
      end
    end
  end
  object MapImportDialog: TOpenDialog
    DefaultExt = 'hpm'
    Filter = 'Hocus Pocus map (*.hpm)|*.hpm|All files (*.*)|*.*'
    Title = 'Import map'
    Left = 32
  end
  object TilesetOpenDialog: TOpenDialog
    DefaultExt = 'bmp'
    Filter = 'Tileset image (*.bmp, *.png)|*.bmp;*.png'
    InitialDir = '.\tilesets'
    Title = 'Load Tileset image'
    Left = 96
  end
  object MapExportDialog: TSaveDialog
    DefaultExt = 'map'
    Filter = 'Hocus Pocus map (*.hpm)|*.hpm|All files (*.*)|*.*'
    Title = 'Export map'
    Left = 64
  end
  object XPManifest1: TXPManifest
    Left = 160
  end
  object MapImageSaveDialog: TSaveDialog
    DefaultExt = 'png'
    Filter = 'Image (*.png)|*.png'
    Title = 'Save map image'
    Left = 128
  end
  object FileImportDialog: TOpenDialog
    Filter = 'All files (*.*)|*.*'
    Title = 'Source file to import'
    Left = 32
    Top = 32
  end
  object FileExportDialog: TSaveDialog
    Filter = 'All files (*.*)|*.*'
    Title = 'Target filename to export'
    Left = 64
    Top = 32
  end
  object ModPatchDialog: TOpenDialog
    DefaultExt = 'ini'
    Filter = 'Mod patch definition (*.ini)|*.ini'
    Title = 'Apply mod patch'
    Left = 96
    Top = 32
  end
  object OriginalExeOpenDialog: TOpenDialog
    DefaultExt = 'exe'
    Filter = 'The original unmodified HOCUS.EXE|*.exe'
    Title = 'Select the original unmodified executable'
    Left = 32
    Top = 64
  end
  object ExePatchSaveDialog: TSaveDialog
    DefaultExt = 'pat'
    FileName = 'hocus.pat'
    Filter = 'Hocus Pocus exe patch (*.pat)|*.pat'
    Title = 'Save exe patch'
    Left = 64
    Top = 64
  end
  object ExePatchOpenDialog: TOpenDialog
    DefaultExt = 'pat'
    Filter = 'Hocus Pocus exe patch (*.pat)|*.pat'
    Title = 'Apply exe patch'
    Left = 96
    Top = 64
  end
end
