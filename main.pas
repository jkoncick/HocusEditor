unit main;

interface

uses
  // System libraries
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, Menus, StdCtrls, XPMan, Math, Spin, Buttons,
  ShellApi, IniFiles, Clipbrd, CheckLst,
  // Dialogs
  set_dialog, block_preset_dialog, level_props_dialog,
  // Units
  _renderer, _map, _tileset, _settings, _archive, _savegame;

type
  TImage = class(ExtCtrls.TImage)
    protected
      procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  end;

const layer_marker_color: array[0..2] of TColor = ($C020C0, $0000FF, $8080FF);

type
  SelectedMode = (mTileMode, mPatternMode, mBlockMode, mPainting, mSelecting, mRightBtnScroll, mTileLayer, mObject);

type
  TMainWindow = class(TForm)
    MapCanvas: TImage;
    MapScrollH: TScrollBar;
    MapScrollV: TScrollBar;
    AppMenu: TMainMenu;
    File1: TMenuItem;
    StatusBar: TStatusBar;
    EditorMenu: TPanel;
    Importmap1: TMenuItem;
    MapImportDialog: TOpenDialog;
    Savemap1: TMenuItem;
    ileset1: TMenuItem;
    Loadcustomimage1: TMenuItem;
    TilesetOpenDialog: TOpenDialog;
    MapExportDialog: TSaveDialog;
    Selecttileset1: TMenuItem;
    MiniMap: TImage;
    XPManifest1: TXPManifest;
    Reopenmap1: TMenuItem;
    N1: TMenuItem;
    Savemapas1: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    Selectnext1: TMenuItem;
    N3: TMenuItem;
    Map1: TMenuItem;
    Setmapsize1: TMenuItem;
    Shiftmap1: TMenuItem;
    LayerPages: TPageControl;
    LrPageBackground: TTabSheet;
    LrPageForeground: TTabSheet;
    rbBlockMode: TRadioButton;
    rbTileMode: TRadioButton;
    BlockImage: TImage;
    Newmap1: TMenuItem;
    N4: TMenuItem;
    Help1: TMenuItem;
    KeyShortcuts1: TMenuItem;
    About1: TMenuItem;
    Mouseactions1: TMenuItem;
    N5: TMenuItem;
    MiniMapFrame: TBevel;
    Savemapimage1: TMenuItem;
    N8: TMenuItem;
    CursorImage: TImage;
    MapImageSaveDialog: TSaveDialog;
    cbAllLayers: TCheckBox;
    Edit1: TMenuItem;
    Undo1: TMenuItem;
    Redo1: TMenuItem;
    Test1: TMenuItem;
    LaunchGame1: TMenuItem;
    N10: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    BlockFrame: TBevel;
    TileLayerControlsPanel: TPanel;
    LrPageHidden: TTabSheet;
    LrPageObject: TTabSheet;
    sbBackgroundLayer: TSpeedButton;
    sbForegroundLayer: TSpeedButton;
    sbHiddenLayer: TSpeedButton;
    sbObjectLayer: TSpeedButton;
    sbShowMarkers: TSpeedButton;
    sbShowGrid: TSpeedButton;
    rbPatternMode: TRadioButton;
    tbBrushWidth: TTrackBar;
    tbBrushHeight: TTrackBar;
    PaintMethodPanel: TPanel;
    lbBrushWidth: TLabel;
    lbBrushHeight: TLabel;
    lbBrushWidthVal: TLabel;
    lbBrushHeightVal: TLabel;
    Openmap1: TMenuItem;
    lbBrushSize: TLabel;
    Exportmap1: TMenuItem;
    btnSavePreset: TButton;
    Usepredefinedtiles1: TMenuItem;
    sbStartPosition: TSpeedButton;
    lbStartPosition: TLabel;
    ObjectTypePages: TPageControl;
    OTPageTeleports: TTabSheet;
    OTPageMonsters: TTabSheet;
    OTPageSwitches: TTabSheet;
    OTPageInsertWall: TTabSheet;
    OTPageRemoveWall: TTabSheet;
    lbActiveObjectMode: TLabel;
    lstMonsterTriggerList: TListBox;
    lstMonsterTypeList: TListBox;
    lbMonsterList: TLabel;
    lbMonsterTypeList: TLabel;
    sbMonsterTrigger: TSpeedButton;
    sbMonsterMonster: TSpeedButton;
    lstSwitchGroupList: TListBox;
    lstRemoveTriggerList: TListBox;
    lstTeleportList: TListBox;
    lstInsertTriggerList: TListBox;
    lbTeleportStartPos: TLabel;
    lbTeleportTargetPos: TLabel;
    sbTeleportStart: TSpeedButton;
    sbTeleportTarget: TSpeedButton;
    lbSwitchList: TLabel;
    clbSwitchList: TCheckListBox;
    sbSwitchUp: TSpeedButton;
    sbSwitchDown: TSpeedButton;
    lbSwitchPlace: TLabel;
    lbSwitchTarget: TLabel;
    sbSwitchTarget: TSpeedButton;
    lbSwitchType: TLabel;
    cbxSwitchType: TComboBox;
    lbRemoveKeyType: TLabel;
    cbxRemoveKeyType: TComboBox;
    lbRemoveTarget: TLabel;
    sbRemoveTarget: TSpeedButton;
    sbInsertTarget: TSpeedButton;
    lbInsertTarget: TLabel;
    cbxInsertKeyType: TComboBox;
    lbInsertKeyType: TLabel;
    lbInsertTrigger: TLabel;
    sbInsertTrigger: TSpeedButton;
    lbRemoveTrigger: TLabel;
    sbRemoveTrigger: TSpeedButton;
    OTPageMessages: TTabSheet;
    lstMessageList: TListBox;
    sbMessagePosition: TSpeedButton;
    lbMessagePosition: TLabel;
    mMessageText: TMemo;
    lstMonsterList: TListBox;
    btnSwitchTargetClear: TButton;
    btnInsertTargetClear: TButton;
    btnRemoveTargetClear: TButton;
    btnMessageTextSave: TButton;
    N6: TMenuItem;
    Levelproperties1: TMenuItem;
    N7: TMenuItem;
    Easy1: TMenuItem;
    Moderate1: TMenuItem;
    Hard1: TMenuItem;
    Archive1: TMenuItem;
    Exportfile1: TMenuItem;
    Importfile1: TMenuItem;
    FileImportDialog: TOpenDialog;
    FileExportDialog: TSaveDialog;
    N9: TMenuItem;
    Applymodpatch1: TMenuItem;
    ModPatchDialog: TOpenDialog;
    // Main form events
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
    procedure CMDialogKey(var AMessage: TCMDialogKey); message CM_DIALOGKEY;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    // Main menu events
    procedure Newmap1Click(Sender: TObject);
    procedure Openmap1Click(Sender: TObject);
    procedure Importmap1Click(Sender: TObject);
    procedure Reopenmap1Click(Sender: TObject);
    procedure Savemap1Click(Sender: TObject);
    procedure Savemapas1Click(Sender: TObject);
    procedure Exportmap1Click(Sender: TObject);
    procedure Savemapimage1Click(Sender: TObject);
    procedure Applymodpatch1Click(Sender: TObject);    
    procedure Exit1Click(Sender: TObject);
    procedure Exportfile1Click(Sender: TObject);
    procedure Importfile1Click(Sender: TObject);
    procedure Undo1Click(Sender: TObject);
    procedure Redo1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure Selecttileset1Click(Sender: TObject);
    procedure Selectnext1Click(Sender: TObject);
    procedure Loadcustomimage1Click(Sender: TObject);
    procedure Usepredefinedtiles1Click(Sender: TObject);
    procedure Setmapsize1Click(Sender: TObject);
    procedure Shiftmap1Click(Sender: TObject);
    procedure Levelproperties1Click(Sender: TObject);
    procedure LaunchGame1Click(Sender: TObject);
    procedure TestMapDifficultyClick(Sender: TObject);
    procedure KeyShortcuts1Click(Sender: TObject);
    procedure Mouseactions1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    // Main form components events
    procedure MapScrollChange(Sender: TObject);
    procedure MapCanvasMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure MapCanvasMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CursorImageMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure CursorImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MapCanvasDblClick(Sender: TObject);
    procedure MapCanvasMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageMouseLeave(Sender: TObject);
    // Editing menu component events
    procedure MiniMapMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MiniMapMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure RenderSettingChange(Sender: TObject);
    procedure LayerPagesChange(Sender: TObject);
    // Layer editing component events
    procedure BlockImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditingModeChange(Sender: TObject);
    procedure tbBrushSizeChange(Sender: TObject);
    procedure btnSavePresetClick(Sender: TObject);
    // Object editing component events
    procedure ObjectTypePagesChange(Sender: TObject);
    procedure sbObjectClick(Sender: TObject);
    procedure lstMonsterTriggerListClick(Sender: TObject);
    procedure lstTeleportListClick(Sender: TObject);
    procedure lstSwitchGroupListClick(Sender: TObject);
    procedure lstInsertTriggerListClick(Sender: TObject);
    procedure lstRemoveTriggerListClick(Sender: TObject);
    procedure lstMessageListClick(Sender: TObject);
    procedure clbSwitchListClickCheck(Sender: TObject);
    procedure btnMessageTextSaveClick(Sender: TObject);
    // Clear area buttons
    procedure btnSwitchTargetClearClick(Sender: TObject);
    procedure btnInsertTargetClearClick(Sender: TObject);
    procedure btnRemoveTargetClearClick(Sender: TObject);
    // Double-click events to center map to selected object/area
    procedure lstMonsterListDblClick(Sender: TObject);
    procedure lbTeleportStartPosDblClick(Sender: TObject);
    procedure lbTeleportTargetPosDblClick(Sender: TObject);
    procedure lbSwitchTargetDblClick(Sender: TObject);
    procedure clbSwitchListDblClick(Sender: TObject);
    procedure lbInsertTargetDblClick(Sender: TObject);
    procedure lbRemoveTargetDblClick(Sender: TObject);
    procedure lbMessagePositionDblClick(Sender: TObject);

  public

    item_buttons: array[0..Length(item_info)-1] of TSpeedButton;

    // Map canvas variables
    map_canvas_width: word;
    map_canvas_height: word;
    map_canvas_left: word;
    map_canvas_top: word;

    // Mouse and keyboard related variables
    mouse_old_x: word;
    mouse_old_y: word;
    mouse_already_clicked: boolean;
    mouse_last_button: TMouseButton;
    cur_shift_state: TShiftState;

    // Minimap variables
    mmap_border_x: word;
    mmap_border_y: word;
    minimap_buffer: TBitmap;

    // Editing variables
    cur_layer: integer;
    cur_preset_group: integer;
    cur_preset_layer: integer;
    cur_tile_index: byte;
    cur_predefined_tile: array[0..1] of integer;
    cur_selected_preset: array[0..1, 0..1] of integer;
    cur_block: TSelectionBlock;
    cur_copied_pattern: array[0..1] of TBlockPreset;
    cur_copied_block: array[0..1] of TSelectionBlock;
    cur_object_mode: integer;
    cur_object_speedbtn: TSpeedButton;

    // Selection variables
    selection_started: boolean;
    selection_start_x: word;
    selection_start_y: word;
    selection_end_x: word;
    selection_end_y: word;

    // Clipboard variables
    clipboard_format: cardinal;

    // Rendering procedures
    procedure resize_map_canvas;
    procedure render_map;
    procedure render_minimap;
    procedure render_minimap_position_marker;
    procedure render_editing_marker;
    procedure render_tileset;

    // Level data GUI procedures
    procedure update_level_data;
    procedure update_monster_entry(index: integer);
    procedure update_teleport_entry(index: integer);
    procedure update_switch_entry(index: integer);
    procedure update_insert_entry(index: integer);
    procedure update_remove_entry(index: integer);
    procedure update_message_entry(index: integer);

    // Map loading & saving procedures
    procedure load_map_from_archive(index: integer);
    procedure save_map_to_archive(index: integer);
    procedure load_map_from_file(filename: String);
    procedure save_map_to_file(filename: String);
    function check_map_errors: boolean;
    procedure set_window_titles(map_name: String);

    // Map testing procedures
    function check_map_can_be_tested: boolean;
    procedure launch_game;

    // Miscellaneous helper procedures
    function mode(m: SelectedMode): boolean;
    function mouse_over_map_canvas: boolean;
    procedure center_map_to(x, y: integer);
    procedure show_statistics;
    procedure update_editing_mode;
    procedure select_current_preset;

    // Procedures related to cursor and block image
    procedure resize_cursor_image;
    procedure draw_cursor_image;
    procedure draw_block_image;

    // Procedures called from other dialog
    procedure set_map_size(new_width, new_height: integer);
    procedure shift_map(direction: TDirection; num_tiles: integer);
  end;

var
  current_dir: String;
  MainWindow: TMainWindow;

implementation

{$R *.dfm}

procedure TImage.CMMouseLeave(var Message: TMessage);
begin
  MainWindow.ImageMouseLeave(self);
end;

procedure TMainWindow.FormCreate(Sender: TObject);
var
  i: integer;
  btn: TSpeedButton;
begin
  // Miscellaneous initializations
  randomize;
  current_dir := ExtractFilePath(Application.ExeName);
  Application.HintPause := 500;
  Application.HintHidePause:= 10000;
  DragAcceptFiles(Handle, True);
  clipboard_format := RegisterClipboardFormat('HocusEditorBlock');
  top := 60;
  // Load settings
  Settings.load_precreate_editor_settings;
  // Initialize archive
  Archive.init;
  // Load and initialize graphics
  Renderer.init;
  minimap_buffer := TBitmap.Create;
  minimap_buffer.PixelFormat := pf32bit;
  minimap_buffer.Width := MiniMap.Width;
  minimap_buffer.Height := MiniMap.Height;
  // Initialize tilesets
  Tileset.init;
  // Create buttons for placing items
  for i := 0 to Length(item_info) - 1 do
  begin
    btn := TSpeedButton.Create(self);
    btn.Width := 40;
    btn.Height := 40;
    btn.Glyph.Width := 32;
    btn.Glyph.Height := 32;
    btn.Left := 4 + (i mod 6) * 44;
    btn.Top := 8 + (i div 6) * 44;
    btn.Hint := item_info[i].name;
    btn.ShowHint := true;
    btn.GroupIndex := 10;
    btn.Tag := i;
    btn.OnClick := sbObjectClick;
    btn.Parent := LrPageObject;
    item_buttons[i] := btn;
  end;
  sbObjectClick(item_buttons[0]);
  lstMonsterTriggerList.ItemIndex := 0;
  lstMonsterTypeList.ItemIndex := 0;
  lstTeleportList.ItemIndex := 0;
  lstSwitchGroupList.ItemIndex := 0;
  lstInsertTriggerList.ItemIndex := 0;
  lstRemoveTriggerList.ItemIndex := 0;
  // Set up test map difficulty
  case Settings.TestMapDifficulty of
    0: Easy1.Checked := true;
    1: Moderate1.Checked := true;
    2: Hard1.Checked := true;
  end;
end;

procedure TMainWindow.FormDestroy(Sender: TObject);
begin
  DragAcceptFiles(Handle, False);
end;

procedure TMainWindow.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Map.loaded and Settings.AlwaysAskOnQuit then
  begin
    if Application.MessageBox('Do you really want to quit?','HocusEditor', MB_YESNO or MB_ICONQUESTION) = IDNO then
    begin
      Action := caNone;
      exit;
    end;
  end;
  Settings.save_editor_settings;
  Tileset.save_config;
  MainWindow.OnResize := nil;
end;

procedure TMainWindow.FormResize(Sender: TObject);
begin
  resize_map_canvas;
  EditorMenu.Left := ClientWidth - EditorMenu.Width;
  EditorMenu.Height := ClientHeight - StatusBar.Height;
  LayerPages.Height := EditorMenu.Height - 112;
  ObjectTypePages.Height := LayerPages.Height - 172;
  StatusBar.Panels[4].Width := ClientWidth - 682;
  if Map.loaded then
  begin
    render_minimap_position_marker;
    render_map;
  end;
end;

procedure TMainWindow.FormDeactivate(Sender: TObject);
begin
  Undo1.ShortCut := 0;
  Redo1.ShortCut := 0;
  Copy1.ShortCut := 0;
  Paste1.ShortCut := 0;
end;

procedure TMainWindow.FormActivate(Sender: TObject);
begin
  Undo1.ShortCut := 16474;
  Redo1.ShortCut := 16473;
  Copy1.ShortCut := 16451;
  Paste1.ShortCut := 16470;
end;

procedure TMainWindow.WMDropFiles(var Msg: TWMDropFiles);
var
  filename: string;
  length: integer;
begin
  length := DragQueryFile(Msg.Drop, 0, nil, 0);
  setlength(filename, length);
  DragQueryFile(Msg.Drop, 0, PChar(filename), length + 1);
  load_map_from_file(filename);
  DragFinish(Msg.Drop);
end;

procedure TMainWindow.CMDialogKey(var AMessage: TCMDialogKey);
begin
  if AMessage.CharCode = VK_TAB then
  begin
    if LayerPages.TabIndex < 3 then
    begin
      // Toggle background/foregrounf layer
      LayerPages.TabIndex := (LayerPages.TabIndex + 1) and 1;
      LayerPagesChange(nil);
    end else
    begin
      // Toggle object editing buttons
      case cur_object_mode of
        100: sbObjectClick(sbMonsterTrigger);
        101: sbObjectClick(sbMonsterMonster);
        200: sbObjectClick(sbTeleportTarget);
        201: sbObjectClick(sbTeleportStart);
        300: sbObjectClick(sbSwitchUp);
        301: sbObjectClick(sbSwitchDown);
        400: sbObjectClick(sbInsertTarget);
        401: sbObjectClick(sbInsertTrigger);
        500: sbObjectClick(sbRemoveTarget);
        501: sbObjectClick(sbRemoveTrigger);
      end;
    end;
    AMessage.Result := 1;
  end else
    inherited;
end;

procedure TMainWindow.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  index: integer;
  step: integer;
begin
  cur_shift_state := Shift;
  mouse_already_clicked := false;
  case key of
    27: // Esc:
    begin
      rbTileMode.Checked := true;
    end;
    32: // Space: open tileset window
    begin
      if mode(mTileLayer) then
      begin
        if ssShift in shift then
          rbPatternMode.Checked := true;
        if (ssCtrl in shift) or mode(mTileMode) then
          rbBlockMode.Checked := true;
        BlockPresetDialog.Show;
        key := 0;
        exit;
      end;
    end;
    107: // Num+
    begin
      Map.adjust_objects_in_block(1, Addr(cur_block));
      draw_cursor_image;
      draw_block_image;
    end;
    109: // Num-
    begin
      Map.adjust_objects_in_block(-1, Addr(cur_block));
      draw_cursor_image;
      draw_block_image;
    end;
    192: // ` (key under ESC)
    begin
      tbBrushWidth.Position := 1;
      tbBrushHeight.Position := 1;
      render_editing_marker;
    end;
  end;
  // Shift + arrows = same as Num keys
  if ssShift in Shift then
  begin
    case key of
      37: {Left arrow}  begin key := 100; Shift := []; end;
      38: {Up arrow}    begin key := 104; Shift := []; end;
      39: {Right arrow} begin key := 102; Shift := []; end;
      40: {Down arrow}  begin key := 98; Shift := []; end;
    end;
  end else
  // Arrows - scroll map
  if LayerPages.ActivePageIndex <> 3 then
  case key of
    37: {Left arrow}  begin MapScrollH.Position := MapScrollH.Position - 1; key := 0; end;
    38: {Up arrow}    begin MapScrollV.Position := MapScrollV.Position - 1; key := 0; end;
    39: {Right arrow} begin MapScrollH.Position := MapScrollH.Position + 1; key := 0; end;
    40: {Down arrow}  begin MapScrollV.Position := MapScrollV.Position + 1; key := 0; end;
  end;
  // F1-F4 - Select layer to edit
  if (key >= 112) and (key <= 115) then
  begin
    index := key - 112;
    LayerPages.ActivePageIndex := index;
    LayerPagesChange(nil);
  end;
  // Shift+key
  if ssShift in Shift then
  begin
    // Brush size preset selection
    if (key >= ord('1')) and (key <= ord('8')) then
    begin
      tbBrushWidth.Position := key - ord('0');
      tbBrushHeight.Position := key - ord('0');
      render_editing_marker;
    end;
    // Editing mode selection
    case key of
      ord('E'): begin rbTileMode.Checked := true; end;
      ord('D'): begin rbPatternMode.Checked := true; end;
      ord('C'): begin rbBlockMode.Checked := true; end;
      ord('A'): begin cbAllLayers.Checked := not cbAllLayers.Checked; end;
      ord('S'): if btnSavePreset.Visible then btnSavePresetClick(nil);
    end;
    exit;
  end;
  // Ctrl+key
  if ssCtrl in Shift then
  begin
    // View layer toggle
    case key of
      ord('A'): begin sbHiddenLayer.Down := not sbForegroundLayer.Down; sbForegroundLayer.Down := not sbForegroundLayer.Down; end;
      ord('B'): begin sbBackgroundLayer.Down := not sbBackgroundLayer.Down; end;
      ord('F'): begin sbForegroundLayer.Down := not sbForegroundLayer.Down; end;
      ord('H'): begin sbHiddenLayer.Down := not sbHiddenLayer.Down; end;
      ord('J'): begin sbObjectLayer.Down := not sbObjectLayer.Down; end;
      ord('M'): begin sbShowMarkers.Down := not sbShowMarkers.Down; end;
      ord('G'): begin sbShowgrid.Down := not sbShowgrid.Down; end;
      else
        exit;
    end;
    RenderSettingChange(nil);
    exit;
  end;
  // Alphanumeric keys
  if (LayerPages.ActivePageIndex < 3) and (rbPatternMode.Checked or rbBlockMode.Checked) then
  begin
    // Block key presets
    if ((key >= ord('0')) and (key <= ord('9'))) or ((key >= ord('A')) and (key <= ord('Z'))) or (key = 186) or (key = 188) or (key = 190) or (key = 191) then
    begin
      if key = 188 then key := ord('<');
      if key = 190 then key := ord('>');
      if key = 186 then key := ord(':');
      if key = 191 then key := ord('?');
      cur_selected_preset[cur_preset_group, cur_preset_layer] := Tileset.block_key_to_index(key);
      update_editing_mode;
      exit;
    end;
  end;
  // Numeric keyboard keys
  if mode(mTileMode) and not Usepredefinedtiles1.Checked then
  begin
    case key of
      // Change current tile index
      98:  {Num2} step := 16;
      100: {Num4} step := -1;
      102: {Num6} step :=  1;
      104: {Num8} step :=-16;
      else
        exit;
    end;
    cur_tile_index := (cur_tile_index + step) and 255;
    update_editing_mode;
    exit;
  end else
  if mode(mTileMode) and Usepredefinedtiles1.Checked then
  begin
    case key of
      // Change current tile index
      98:  {Num2} step :=  8;
      100: {Num4} step := -1;
      102: {Num6} step :=  1;
      104: {Num8} step := -8;
      else
        exit;
    end;
    cur_predefined_tile[cur_preset_layer] := (cur_predefined_tile[cur_preset_layer] + step) and (cnt_predefined_tiles - 1);
    update_editing_mode;
    exit;
  end else
  if mode(mPatternMode) then
  begin
    case key of
      // Rotate pattern
      98:  {Num2} begin Map.rotate_pattern(drDown); btnSavePreset.Visible := true; end;
      100: {Num4} begin Map.rotate_pattern(drLeft); btnSavePreset.Visible := true; end;
      102: {Num6} begin Map.rotate_pattern(drRight); btnSavePreset.Visible := true; end;
      104: {Num8} begin Map.rotate_pattern(drUp); btnSavePreset.Visible := true; end;
      else
        exit;
    end;
    draw_block_image;
    cur_selected_preset[bpgPatternPreset, cur_preset_layer] := -1;
    cur_copied_pattern[cur_preset_layer] := Map.pattern;
    exit;
  end else
  if mode(mBlockMode) then
  case key of
    // Move cursor image
    98:  {Num2} begin CursorImage.Top := CursorImage.Top + 32; resize_cursor_image; end;
    100: {Num4} begin CursorImage.Left := CursorImage.Left - 32; resize_cursor_image; end;
    102: {Num6} begin CursorImage.Left := CursorImage.Left + 32; resize_cursor_image; end;
    104: {Num8} begin CursorImage.Top := CursorImage.Top - 32; resize_cursor_image; end;
    end
  else if mode(mObject) and (ObjectTypePages.ActivePageIndex = 0) then
  begin
    // Select monster type
    if (key >= 96) and (key <= 105) then
      lstMonsterTypeList.ItemIndex := key - 96;
  end;
end;

procedure TMainWindow.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if (MousePos.X - left) < (mapCanvas.Width + 30) then
  begin
    if (MousePos.Y - top) < (mapCanvas.Height - 30)
    then
      MapScrollV.Position := MapScrollV.Position - 2
    else
      MapScrollH.Position := MapScrollH.Position - 2;
    Handled := true;
  end;
end;

procedure TMainWindow.FormMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if (MousePos.X - left) < (mapCanvas.Width + 30) then
  begin
    if (MousePos.Y - top) < (mapCanvas.Height - 30)
    then
      MapScrollV.Position := MapScrollV.Position + 2
    else
      MapScrollH.Position := MapScrollH.Position + 2;
    Handled := true;
  end;
end;

procedure TMainWindow.Newmap1Click(Sender: TObject);
var
  tileset_index: integer;
begin
  SetDialog.select_menu(4);
  if SetDialog.ModalResult = mrCancel then
    exit;
  tileset_index := SetDialog.TilesetSelection_List.ItemIndex;
  Map.new_map(tileset_index);
  // Update status bar and title
  StatusBar.Panels[3].Text := inttostr(Map.width)+' x '+inttostr(Map.height);
  StatusBar.Panels[4].Text := 'Map not saved';
  set_window_titles('Untitled');
  LevelPropertiesDialog.update_contents;
  // Finish it
  resize_map_canvas;
  render_minimap;
  render_map;
  update_level_data;
end;

procedure TMainWindow.Openmap1Click(Sender: TObject);
var
  level_index: integer;
begin
  SetDialog.select_menu(3);
  if SetDialog.ModalResult = mrCancel then
    exit;
  level_index := SetDialog.LevelSelection_List.ItemIndex;
  load_map_from_archive(level_index);
end;

procedure TMainWindow.Importmap1Click(Sender: TObject);
begin
  if MapImportDialog.Execute then
  begin
    load_map_from_file(MapImportDialog.FileName);
  end;
end;


procedure TMainWindow.Reopenmap1Click(Sender: TObject);
begin
  if Map.loaded and (Map.index <> -1) then
    load_map_from_archive(Map.index);
end;

procedure TMainWindow.Savemap1Click(Sender: TObject);
begin
  if not Map.loaded then
    exit;
  if Map.index = -1 then
    Savemapas1Click(Sender)
  else begin
    if Settings.CheckMapErrorsOnSave then
      check_map_errors;
    save_map_to_archive(Map.index);
  end;
end;

procedure TMainWindow.Savemapas1Click(Sender: TObject);
var
  level_index: integer;
begin
  if not Map.loaded then
    exit;
  if Settings.CheckMapErrorsOnSave then
    check_map_errors;
  SetDialog.select_menu(3);
  if SetDialog.ModalResult = mrCancel then
    exit;
  level_index := SetDialog.LevelSelection_List.ItemIndex;
  save_map_to_archive(level_index);
end;

procedure TMainWindow.Exportmap1Click(Sender: TObject);
begin
  if not Map.loaded then
    exit;
  if Settings.CheckMapErrorsOnSave then
    check_map_errors;
  if MapExportDialog.Execute then
    save_map_to_file(MapExportDialog.FileName);
end;

procedure TMainWindow.Savemapimage1Click(Sender: TObject);
var
  tmp_bitmap: TBitmap;
begin
  if not Map.loaded then
    exit;
  if MapImageSaveDialog.Execute then
  begin
    tmp_bitmap := TBitmap.Create;
    tmp_bitmap.Width := Map.width * 32;
    tmp_bitmap.Height := Map.height * 32;
    Renderer.render_map_contents(tmp_bitmap.Canvas, 0, 0, Map.width, Map.height, 0, 0, Addr(Map.data),
      sbBackgroundLayer.Down, sbForegroundLayer.Down, sbHiddenLayer.Down, sbObjectLayer.Down, sbShowMarkers.Down, sbShowGrid.Down,
      false);
    tmp_bitmap.SaveToFile(MapImageSaveDialog.FileName);
    tmp_bitmap.Destroy;
  end;
end;

procedure TMainWindow.Applymodpatch1Click(Sender: TObject);
begin
  if ModPatchDialog.Execute then
  begin
    if Application.MessageBox('You are about to apply a mod patch to your game.'#13'Remember to backup your game files (HOCUS.EXE and HOCUS.DAT).'#13#13'Continue?', 'Apply mod patch', MB_ICONQUESTION or MB_YESNO) = IDNO then
      exit;
    Archive.apply_mod_patch(ModPatchDialog.FileName);
    Application.MessageBox('Mod patch was applied.', 'Apply mod patch', MB_ICONINFORMATION or MB_OK);
    // Restart the application because it ends in inconsistent state after importing levels
    Close;
    ShellExecute(Handle, nil, PChar(Application.ExeName), nil, nil, SW_SHOWNORMAL);
  end;
end;

procedure TMainWindow.Exit1Click(Sender: TObject);
begin
  MainWindow.OnResize := nil;
  application.Terminate;
end;

procedure TMainWindow.Exportfile1Click(Sender: TObject);
var
  file_index: integer;
begin
  SetDialog.select_menu(5);
  if SetDialog.ModalResult = mrCancel then
    exit;
  file_index := SetDialog.FileSelection_List.ItemIndex;
  if Archive.file_names[file_index] <> '' then
    FileExportDialog.FileName := Archive.file_names[file_index]
  else
    FileExportDialog.FileName := 'file_' + inttostr(file_index) + '.bin';
  if FileExportDialog.Execute then
    Archive.export_file(file_index, FileExportDialog.FileName);
end;

procedure TMainWindow.Importfile1Click(Sender: TObject);
var
  file_index: integer;
  entry_index: integer;
  palette_changed: boolean;
begin
  SetDialog.select_menu(5);
  if SetDialog.ModalResult = mrCancel then
    exit;
  palette_changed := false;
  file_index := SetDialog.FileSelection_List.ItemIndex;
  if FileImportDialog.Execute then
  begin
    Archive.import_file(file_index, FileImportDialog.FileName);
    SetDialog.update_file_listitem(file_index);
    ShowMessage('File imported');
    // If importing a backdrop image, ask for copying its upper palette
    if (file_index >= Archive.first_backdrop_file_index) and (file_index < (Archive.first_backdrop_file_index + Archive.tileset_count)) then
    begin
      entry_index := file_index - Archive.first_backdrop_file_index;
      if Application.MessageBox('You just imported a backdrop image.'#13'Do you want to copy the upper part of palette from the PCX file'#13'to the corresponding palette file in HOCUS.DAT?', 'Copy image palette', MB_YESNO or MB_ICONQUESTION) = IDYES then
      begin
        Archive.copy_backdrop_image_palette(entry_index);
        ShowMessage('Palette copied.');
        palette_changed := true;
      end;
      if Map.levelexedata.backdrop_number = entry_index then
      begin
        LevelPropertiesDialog.update_backdrop_image(true);
        if palette_changed then
        begin
          Tileset.load_tileset_image;
          render_map;
        end;
      end;
    end;
    // If importing a backdrop image palette, reload backdrop image
    if (file_index >= Archive.first_backdrop_palette_file_index) and (file_index < (Archive.first_backdrop_palette_file_index + Archive.tileset_count)) then
    begin
      entry_index := file_index - Archive.first_backdrop_palette_file_index;
      if Map.levelexedata.backdrop_number = entry_index then
      begin
        LevelPropertiesDialog.update_backdrop_image(true);
        Tileset.load_tileset_image;
        render_map;
      end;
    end;
    // If importing a tileset image, reload current tileset
    if (file_index >= Archive.first_tileset_file_index) and (file_index < (Archive.first_tileset_file_index + Archive.tileset_count)) then
    begin
      entry_index := file_index - Archive.first_tileset_file_index;
      if Map.levelexedata.tileset_number = entry_index then
      begin
        Tileset.load_tileset_image;
        render_map;
      end;
    end;
  end;
end;

procedure TMainWindow.Undo1Click(Sender: TObject);
begin
  Map.do_undo;
  render_minimap;
  render_map;
  update_level_data;
  mouse_already_clicked := false;
end;

procedure TMainWindow.Redo1Click(Sender: TObject);
begin
  Map.do_redo;
  render_minimap;
  render_map;
  update_level_data;
  mouse_already_clicked := false;
end;

procedure TMainWindow.Copy1Click(Sender: TObject);
var
  handle: THandle;
  pointer: ^TSelectionBlock;
begin
  if not Map.loaded or not mode(mBlockMode) then
    exit;
  OpenClipboard(Application.Handle);
  EmptyClipboard;

  handle := GlobalAlloc(GMEM_DDESHARE or GMEM_MOVEABLE, SizeOf(TSelectionBlock));
  pointer := GlobalLock(handle);

  Move(cur_block, pointer^, sizeof(TSelectionBlock));

  GlobalUnLock(handle);
  SetClipboardData(clipboard_format, handle);
  CloseClipboard;
end;

procedure TMainWindow.Paste1Click(Sender: TObject);
var
  handle: THandle;
  pointer: ^TSelectionBlock;
begin
  if not Map.loaded or not Clipboard.HasFormat(clipboard_format) then
    exit;
  OpenClipboard(Application.Handle);
  handle := GetClipboardData(clipboard_format);
  pointer := GlobalLock(handle);

  RbBlockMode.Checked := true;
  cbAllLayers.Checked := pointer.all_layers;
  if not pointer.all_layers or (LayerPages.ActivePageIndex = 3) then
  begin
    LayerPages.TabIndex := pointer.layer;
    LayerPagesChange(nil);
  end;
  Move(pointer^, cur_block, sizeof(TSelectionBlock));
  draw_cursor_image;
  draw_block_image;

  GlobalUnLock(handle);
  CloseClipboard;
end;

procedure TMainWindow.Selecttileset1Click(Sender: TObject);
var
  tileset_index: integer;
begin
  SetDialog.select_menu(4);
  if SetDialog.ModalResult = mrCancel then
    exit;
  tileset_index := SetDialog.TilesetSelection_List.ItemIndex;
  Map.change_tileset(tileset_index);
  // Re-render everything
  render_map;
end;

procedure TMainWindow.Selectnext1Click(Sender: TObject);
var
  new_tileset: integer;
begin
  new_tileset := Tileset.current_tileset + 1;
  if new_tileset >= Archive.tileset_count then
    new_tileset := 0;
  Map.change_tileset(new_tileset);
  // Re-render everything
  render_map;
end;

procedure TMainWindow.Loadcustomimage1Click(Sender: TObject);
begin
  if Settings.LoadCustomImageWarn then
  begin
    Application.MessageBox('This option does NOT import the selected tileset image into game data archive (HOCUS.DAT).'#13'This option is here to quickly test your tileset image without need to import it first.'#13+'To import your custom tileset, please use "Archive - Import file" option, and provide your tileset im the game-readable PCX format.'#13#13'This message will not show again.', 'Warning', MB_OK or MB_ICONWARNING);
    Settings.LoadCustomImageWarn := false;
  end;
  if TilesetOpenDialog.Execute then
  begin
    Tileset.load_custom_image(TilesetOpenDialog.FileName);
    // Re-render everything
    render_map;
  end;
end;

procedure TMainWindow.Usepredefinedtiles1Click(Sender: TObject);
begin
  update_editing_mode;
end;

procedure TMainWindow.Setmapsize1Click(Sender: TObject);
begin
  SetDialog.select_menu(1);
end;

procedure TMainWindow.Shiftmap1Click(Sender: TObject);
begin
  SetDialog.select_menu(2);
end;

procedure TMainWindow.Levelproperties1Click(Sender: TObject);
begin
  LevelPropertiesDialog.Show;
end;

procedure TMainWindow.LaunchGame1Click(Sender: TObject);
begin
  if not check_map_can_be_tested then
    exit;
  launch_game;
end;

procedure TMainWindow.TestMapDifficultyClick(Sender: TObject);
begin
  Settings.TestMapDifficulty := (Sender as TMenuItem).Tag;
end;

procedure TMainWindow.KeyShortcuts1Click(Sender: TObject);
begin
  Application.MessageBox(
              'Space = Open preset window'#13'Shift + Space = Pattern presets'#13'Ctrl + Space = Block presets'#13'Arrows = Scroll map'#13'F1 - F4 = Select layer'#13'Tab = Toggle background/foreground layer'#13'Num +/- = Change object numbers in selected block'#13#13+
              'Num 2/4/6/8 or Shift + Arrows:'#13'Tile mode: Change selected tile'#13'Pattern mode: Rotate pattern'#13'Block mode: Move block'#13#13+
              'Shift + 1 - 8 = Change brush size'#13'Shift + E = Tile mode'#13'Shift + D = Pattern mode'#13'Shift + C = Block mode'#13'Shift + A = All layers mode'#13'Shift + S = Save pattern/block as preset'#13#13+
              'Ctrl + A = Toggle foreground and hidden layer'#13'Ctrl + B/F/H/J = Toggle specific layer'#13'Ctrl + M = Show markers'#13'Ctrl + G = Show grid'#13#13+
              '0 - 9, A - Z = Select block/pattern preset',
              'Key Shortcuts',
              MB_OK or MB_ICONINFORMATION
              );
end;

procedure TMainWindow.Mouseactions1Click(Sender: TObject);
begin
  Application.MessageBox(
              'Tile mode:'#13'Left = Paint tile'#13'Right = Erase tile'#13'Middle = Copy tile'#13'Double click = Fill area ("Bucket")'#13'Shift + click = Auto-place window edge'#13'Ctrl + Select (Left) = Fill selected area'#13'Ctrl + Select (Right) = Erase selected area'#13#13+
              'Pattern mode:'#13'Left = Paint pattern'#13'Right = Erase tile'#13'Middle = Copy single tile'#13'Double click = Fill area ("Bucket")'#13'Ctrl + Select = Fill selected area'#13'Shift + Select = Copy pattern from map'#13#13+
              'Block mode:'#13'Left = Place block'#13'Right + Move = Scroll map'#13'Middle = Select empty block'#13'Shift + Select = Copy block from map'#13#13+
              'Object mode:'#13'Left = Place object'#13'Right = Erase object'#13'Middle = Copy object'#13#13+
              'Preset selection window:'#13'Left = Select preset'#13'Right = Delete preset'#13'Middle = Show/hide keys'#13#13+
              'Hold right button while selecting to scroll map.',
              'Mouse Actions',
              MB_OK or MB_ICONINFORMATION
              );
end;

procedure TMainWindow.About1Click(Sender: TObject);
begin
  ShowMessage('Hocus Pocus Level Editor'#13#13+
              'Made by Hisymak'#13'Version 1.0'#13'Date: 2015-12-26'#13#13+
              'Game version: '+ Archive.archive_version +#13#13+
              'Special thanks to:'#13'Malvineous, Spinal, K1n9 Duk3 and MainMemory'#13'for reverse-engineering the Hocus Pocus map format'#13'and providing the information on Modding Wiki.'#13'http://www.shikadi.net/moddingwiki/Hocus_Pocus');
end;

procedure TMainWindow.MapScrollChange(Sender: TObject);
var
  pos: TPoint;
begin
  map_canvas_left := MapScrollH.Position;
  map_canvas_top := MapScrollV.Position;
  render_map;
  render_minimap_position_marker;
  // Simulate MouseMove event so that editing marker and coordinates are updated
  if mouse_over_map_canvas then
  begin
    pos := MapCanvas.ScreenToClient(Mouse.CursorPos);
    MapCanvasMouseMove(nil, cur_shift_state, pos.X, pos.Y);
  end
end;

procedure TMainWindow.MapCanvasMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  canvas_x, canvas_y: integer;
  map_x, map_y: integer;
  message_data: ^TLvlMessageData;
  i, message_index: integer;
  line, tmp_hint: string;
  Button: TMouseButton;
begin
  cur_shift_state := Shift;
  // Get tile coordinates
  canvas_x := X div 32;
  canvas_y := Y div 32;
  map_x := canvas_x + map_canvas_left;
  map_y := canvas_y + map_canvas_top;
  if map_x < 0 then
    map_x := 0;
  if map_x >= Map.width then
    map_x := Map.width - 1;
  if map_y < 0 then
    map_y := 0;
  if map_y >= Map.height then
    map_y := Map.height - 1;
  // If mouse is still inside same tile, exit (for optimization)
  if (mouse_old_x = map_x) and (mouse_old_y = map_y) then
    exit;
  mouse_already_clicked := false;
  // Write coordinates on status bar
  StatusBar.Panels[0].Text := 'x: '+inttostr(map_x)+' y: '+inttostr(map_y);
  // If mouse moved over Wizard with a message, show "hint" with with the message
  if mode(mObject) and (Map.data[map_x, map_y].objects = ivMessage) then
  begin
    Application.CancelHint;
    tmp_hint := '';
    message_index := Map.data[map_x, map_y].moreinfo;
    if message_index <> 255 then
    begin
      message_data := Addr(Map.leveldata.message_data[message_index]);
      for i := 0 to Length(message_data.Lines) - 1 do
      begin
        SetString(line, message_data.Lines[i], Min(StrLen(message_data.Lines[i]), Length(message_data.Lines[i])));
        if line <> '' then
          tmp_hint := tmp_hint + line + #13#10;
      end;
      MapCanvas.Hint := tmp_hint;
      MapCanvas.ShowHint := true;
    end else
      MapCanvas.ShowHint := false;
  end else
    MapCanvas.ShowHint := false;
  // Scroll map while holding right button
  if (ssRight in shift) and mode(mRightBtnScroll) then
  begin
    MapScrollH.Position := map_canvas_left + (mouse_old_x - map_x);
    MapScrollV.Position := map_canvas_top + (mouse_old_y - map_y);
  end else
  begin
    mouse_old_x := map_x;
    mouse_old_y := map_y;
  end;
  // Move cursor image and resize if exceeding map canvas border
  CursorImage.Left := canvas_x * 32 + MapCanvas.Left;
  CursorImage.Top := canvas_y * 32 + MapCanvas.Top;
  resize_cursor_image;
  // Move end of block selection
  if selection_started then
  begin
    selection_end_x := map_x;
    selection_end_y := map_y;
  end;
  // Redraw editing marker
  render_editing_marker;
  // If mouse button is held, paint with brush
  if ((ssLeft in Shift) or (ssRight in Shift)) and (mode(mPainting) or mode(mObject)) and not selection_started then
  begin
    if ssLeft in Shift then
      button := mbLeft
    else
      button := mbRight;
    MapCanvasMouseDown(sender,Button,Shift,x,y);
  end;
end;

procedure TMainWindow.MapCanvasMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  map_x, map_y: integer;
  i: integer;
  cursor_left: integer;
  cursor_top: integer;
  editing_marker_disabled: boolean;
  obj_type: TObjectType;
  obj_index: integer;
begin
  cur_shift_state := Shift;
  // Right button is used for map scrolling - do nothing
  if (button = mbRight) and mode(mRightBtnScroll) then
    exit;
  // Get map coordinates
  map_x := x div 32 + map_canvas_left;
  map_y := y div 32 + map_canvas_top;
  // Disable multiple clicks in the same tile
  if mouse_already_clicked and (mouse_last_button = Button) then
    exit;
  mouse_already_clicked := true;
  mouse_last_button := Button;
  editing_marker_disabled := false;

  // Select mode - Just start selection, the action will be performed after mouse button is released
  if mode(mSelecting) then
  begin
    // Start selection
    selection_started := true;
    selection_start_x := map_x;
    selection_start_y := map_y;
    selection_end_x := map_x;
    selection_end_y := map_y;
    render_editing_marker;
    exit;
  end;

  // Middle mouse button
  if Button = mbMiddle then
  begin
    // Select a tile from map
    if mode(mTileMode) then
    begin
      // Get tile index from map
      cur_tile_index := Map.data[map_x, map_y].layers[cur_layer];
      if Usepredefinedtiles1.Checked then
      begin
        // Find which predefined tile was selected
        cur_predefined_tile[cur_preset_layer] := -1;
        for i := 0 to cnt_predefined_tiles - 1 do
          if Tileset.predefined_tiles[cur_preset_layer, i] = cur_tile_index then
          begin
            cur_predefined_tile[cur_preset_layer] := i;
            break;
          end;
      end;
      update_editing_mode;
    end else
    // Select a single tile as a pattern
    if mode(mPatternMode) then
    begin
      Map.copy_pattern(map_x, map_y, 1, 1, cur_layer);
      cur_selected_preset[bpgPatternPreset, cur_preset_layer] := -1;
      draw_block_image;
    end else
    // Select single tile and switch to tile mode
    if mode(mBlockMode) then
    begin
      rbTileMode.Checked := true;
      MapCanvasMouseDown(Sender, Button, Shift, X, Y);
    end;
    // Copy object
    if mode(mObject) then
    begin
      obj_type := Map.get_object_type(Map.data[map_x, map_y].objects, obj_index);
      // Copy message
      if (obj_type = otItem) and (obj_index = ivMessage) then
      begin
        ObjectTypePages.TabIndex := 5;
        ObjectTypePagesChange(nil);
        lstMessageList.ItemIndex := Map.data[map_x, map_y].moreinfo;
        lstMessageListClick(nil);
      end else
      // Copy item
      if obj_type = otItem then
      begin
        for i := 0 to Length(item_info) - 1 do
        begin
          if item_info[i].value = obj_index then
            sbObjectClick(item_buttons[i]);
        end;
      end else
      // Copy teleport
      if obj_type = otTeleport then
      begin
        ObjectTypePages.TabIndex := 1;
        ObjectTypePagesChange(nil);
        lstTeleportList.ItemIndex := obj_index;
        lstTeleportListClick(nil);
        if Map.data[map_x, map_y].moreinfo = 0 then
          sbObjectClick(sbTeleportStart)
        else
          sbObjectClick(sbTeleportTarget);
      end else
      // Copy switch
      if obj_type = otSwitch then
      begin
        ObjectTypePages.TabIndex := 2;
        ObjectTypePagesChange(nil);
        lstSwitchGroupList.ItemIndex := obj_index;
        lstSwitchGroupListClick(nil);
        if Map.data[map_x, map_y].layers[0] = Map.leveldata.animation_info.SwitchDownTile then
          sbObjectClick(sbSwitchDown)
        else
          sbObjectClick(sbSwitchUp);
      end else
      // Copy insert wall trigger
      if obj_type = otInsertWall then
      begin
        ObjectTypePages.TabIndex := 3;
        ObjectTypePagesChange(nil);
        lstInsertTriggerList.ItemIndex := obj_index;
        lstInsertTriggerListClick(nil);
      end else
      // Copy remove wall trigger
      if obj_type = otRemoveWall then
      begin
        ObjectTypePages.TabIndex := 4;
        ObjectTypePagesChange(nil);
        lstRemoveTriggerList.ItemIndex := obj_index;
        lstRemoveTriggerListClick(nil);
      end else
      // Copy monster
      if obj_type = otMonster then
      begin
        ObjectTypePages.TabIndex := 0;
        ObjectTypePagesChange(nil);
        lstMonsterTriggerList.ItemIndex := Map.data[map_x, map_y].moreinfo;
        lstMonsterTriggerListClick(nil);
        lstMonsterTypeList.ItemIndex := obj_index;
      end else
      // Copy monster trigger
      if obj_type = otMonsterTrigger then
      begin
        ObjectTypePages.TabIndex := 0;
        ObjectTypePagesChange(nil);
        lstMonsterTriggerList.ItemIndex := obj_index;
        lstMonsterTriggerListClick(nil);
        sbObjectClick(sbMonsterTrigger);
      end;
    end;
    // Nothing to render, exit
    exit;
  end else

  // Left mouse button
  if Button = mbLeft then
  begin
    // Paint tile
    if mode(mTileMode) then
    begin
      if (cur_layer = 0) and (ssShift in Shift) then
        Map.smooth_edges(map_x, map_y, 0)
      else
        Map.paint_tile_rect(map_x, map_y, tbBrushWidth.Position, tbBrushHeight.Position, cur_layer, cur_tile_index);
    end else
    // Paint pattern
    if mode(mPatternMode) then
    begin
      Map.paint_tile_rect(map_x, map_y, tbBrushWidth.Position, tbBrushHeight.Position, cur_layer, 254);
    end else
    // Place block
    if mode(mBlockMode) then
    begin
      cursor_left := (CursorImage.Left - MapCanvas.Left) div 32 + map_canvas_left;
      cursor_top := (CursorImage.Top - MapCanvas.Top) div 32 + map_canvas_top;
      if (cursor_left <> map_x) or (cursor_top <> map_y) then
        // Enable additional clicks if cursor image was moved from mouse cursor position
        mouse_already_clicked := false;
      Map.put_block(cursor_left, cursor_top, cur_layer, cbAllLayers.Checked, Addr(cur_block));
    end else
    // Place object
    if mode(mObject) then
    begin
      if (cur_object_mode >= 0) and (cur_object_mode < Length(item_info)) then
        Map.put_item(map_x, map_y, cur_object_mode)
      else
      case cur_object_mode of
        // Start position
        99: Map.set_start_position(map_x, map_y);
        // Monster
        100: Map.put_object(map_x, map_y, otMonster, lstMonsterTypeList.ItemIndex, lstMonsterTriggerList.ItemIndex);
        // Monster trigger
        101: Map.put_object(map_x, map_y, otMonsterTrigger, lstMonsterTriggerList.ItemIndex, 255);
        // Teleport start
        200: Map.put_object(map_x, map_y, otTeleport, lstTeleportList.ItemIndex, 0);
        // Teleport target
        201: Map.put_object(map_x, map_y, otTeleport, lstTeleportList.ItemIndex, 1);
        // Switch down-state
        300: Map.put_object(map_x, map_y, otSwitch, lstSwitchGroupList.ItemIndex, 0);
        // Switch up-state
        301: Map.put_object(map_x, map_y, otSwitch, lstSwitchGroupList.ItemIndex, 1);
        // Insert wall trigger
        400: Map.put_object(map_x, map_y, otInsertWall, lstInsertTriggerList.ItemIndex, cbxInsertKeyType.ItemIndex);
        // Remove wall trigger
        500: Map.put_object(map_x, map_y, otRemoveWall, lstRemoveTriggerList.ItemIndex, cbxRemoveKeyType.ItemIndex);
        // Message
        600: Map.put_object(map_x, map_y, otItem, ivMessage, lstMessageList.ItemIndex);
      end;
    end;
  end else

  // Right mouse button
  if button = mbRight then
  begin
    // Erase tiles
    if mode(mTileMode) or mode(mPatternMode) then
    begin
      Map.paint_tile_rect(map_x, map_y, tbBrushWidth.Position, tbBrushHeight.Position, cur_layer, 255);
    end else
    // Remove object
    if mode(mObject) then
    begin
      Map.remove_object(map_x, map_y);
    end;
  end;

  // Do not update statistics and level data while painting but at end of painting
  if not mode(mPainting) then
  begin
    Map.compute_statistics;
    update_level_data;
  end;
  // Finally render changes in map
  render_minimap;
  render_map;
  if editing_marker_disabled then
    Renderer.remove_editing_marker(MapCanvas.Canvas);
end;

procedure TMainWindow.MapCanvasDblClick(Sender: TObject);
begin
  // Double click for filling area
  if mode(mPainting) then
  begin
    if rbTileMode.Checked then
      Map.fill_area_start(mouse_old_x, mouse_old_y, cur_layer, cur_tile_index)
    else if rbPatternMode.Checked then
      Map.fill_area_start(mouse_old_x, mouse_old_y, cur_layer, 254);
    Map.compute_statistics;
    render_minimap;
    render_map;
    update_level_data;
  end;
end;

procedure TMainWindow.MapCanvasMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  min_x, max_x, min_y, max_y: word;
  size_x, size_y: word;
  i: integer;
begin
  cur_shift_state := Shift;
  // Right button is used for map scrolling - do nothing
  if (button = mbRight) and mode(mRightBtnScroll) then
    exit;
  // Finish selection
  if selection_started then
  begin
    selection_started := false;
    StatusBar.Panels[1].Text := '';
    min_x := Min(selection_start_x, selection_end_x);
    max_x := Max(selection_start_x, selection_end_x);
    min_y := Min(selection_start_y, selection_end_y);
    max_y := Max(selection_start_y, selection_end_y);
    size_x := max_x - min_x + 1;
    size_y := max_y - min_y + 1;
    if mode(mTileLayer) then
    begin
      // Erase tiles
      if (mode(mTileMode) or mode(mPatternMode)) and (Button = mbRight) then
        Map.paint_tile_rect(min_x, min_y, size_x, size_y, cur_layer, 255)
      // Paint tiles
      else if mode(mTileMode) and (ssCtrl in Shift) then
        Map.paint_tile_rect(min_x, min_y, size_x, size_y, cur_layer, cur_tile_index)
      // Paint pattern
      else if mode(mPatternMode) and (ssCtrl in Shift) then
        Map.paint_tile_rect(min_x, min_y, size_x, size_y, cur_layer, 254)
      // Select pattern
      else if mode(mPatternMode) and (ssShift in Shift) then
      begin
        Map.copy_pattern(min_x, min_y, size_x, size_y, cur_layer);
        cur_copied_pattern[cur_preset_layer] := Map.pattern;
        cur_selected_preset[bpgPatternPreset, cur_preset_layer] := -1;
        btnSavePreset.Caption := 'Save pattern as preset';
        btnSavePreset.Visible := true;
        draw_block_image;
      end
      // Select and copy block
      else if mode(mBlockMode) then
      begin
        Map.copy_block(min_x, min_y, size_x, size_y, cur_layer, cbAllLayers.Checked, Addr(cur_block));
        cur_copied_block[cur_preset_layer] := cur_block;
        cur_selected_preset[bpgBlockPreset, cur_preset_layer] := -1;
        if cbAllLayers.Checked then
        begin
          cur_copied_block[1 - cur_preset_layer] := cur_block;
          cur_selected_preset[bpgBlockPreset, 1 - cur_preset_layer] := -1;
        end;
        if not cbAllLayers.Checked and (size_x <= max_block_preset_size) and (size_y <= max_block_preset_size) then
        begin
          btnSavePreset.Caption := 'Save block as preset';
          btnSavePreset.Visible := true;
        end;
        draw_cursor_image;
        draw_block_image;
        // Erase copied area
        if ssCtrl in Shift then
        begin
          for i := 0 to 2 do
            if cbAllLayers.Checked or (i = cur_layer) then
              Map.paint_tile_rect(min_x, min_y, size_x, size_y, i, 255);
        end else
          exit;
      end;
    end else
    if mode(mObject) then
    begin
      case cur_object_mode of
        302: Map.set_switch_target_area(lstSwitchGroupList.ItemIndex, min_x, min_y, max_x, max_y, cbxSwitchType.ItemIndex);
        401: Map.set_insert_target_area(lstInsertTriggerList.ItemIndex, min_x, min_y, max_x, max_y);
        501: Map.set_remove_target_area(lstRemoveTriggerList.ItemIndex, min_x, min_y, max_x, max_y);
      end;
      update_level_data;
    end;
    Map.compute_statistics;
    render_minimap;
    render_map;
  end else
  if mode(mPainting) then
  begin
    Map.compute_statistics;
    update_level_data;
  end;
end;

procedure TMainWindow.CursorImageMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  MapCanvasMouseMove(Sender, Shift, X + CursorImage.Left - MapCanvas.Left, Y + CursorImage.Top - MapCanvas.Top);
end;

procedure TMainWindow.CursorImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MapCanvasMousedown(Sender, Button, Shift, X + CursorImage.Left - MapCanvas.Left, Y + CursorImage.Top - MapCanvas.Top);
end;

procedure TMainWindow.ImageMouseLeave(Sender: TObject);
begin
  if (Sender <> MapCanvas) and (Sender <> CursorImage) then
    exit;
  if mouse_over_map_canvas then
    exit;
  StatusBar.Panels[0].Text := '';
  // Reset mouse position to a value outside of map range
  mouse_old_x := max_map_width;
  mouse_old_y := max_map_height;
  // Remove editing markers
  render_editing_marker;
end;

procedure TMainWindow.MiniMapMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not Map.loaded then
    exit;
  if (x < mmap_border_x) or (y < mmap_border_y) or (x > MiniMap.Width - mmap_border_x) or (y > MiniMap.Height - mmap_border_y) then
    exit;
  MapScrollH.Position := x - mmap_border_x - (map_canvas_width div 2);
  MapScrollV.Position := y - mmap_border_y - (map_canvas_height div 2);
end;

procedure TMainWindow.MiniMapMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if ssLeft in Shift then
    MiniMapMouseDown(Sender, mbLeft, Shift, X, Y);
end;

procedure TMainWindow.RenderSettingChange(Sender: TObject);
begin
  render_map;
end;

procedure TMainWindow.LayerPagesChange(Sender: TObject);
begin
  case LayerPages.ActivePageIndex of
    0: TileLayerControlsPanel.Parent := LrPageBackground;
    1: TileLayerControlsPanel.Parent := LrPageForeground;
    2: TileLayerControlsPanel.Parent := LrPageHidden;
  end;
  Copy1.ShortCut := IfThen(LayerPages.ActivePageIndex <> 3, 16451, 0);
  Paste1.ShortCut := IfThen(LayerPages.ActivePageIndex <> 3, 16470, 0);
  cur_layer := LayerPages.ActivePageIndex;
  cur_preset_layer := Min(cur_layer, 1);
  update_editing_mode;
  update_level_data;
end;

procedure TMainWindow.BlockImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if rbTileMode.Checked and not Usepredefinedtiles1.Checked then
  begin
    cur_tile_index := (Y div 16) * 16 + (X div 16);
    update_editing_mode;
  end else
  if rbTileMode.Checked and Usepredefinedtiles1.Checked then
  begin
    cur_predefined_tile[cur_preset_layer] := (Y div 32) * 8 + (X div 32);
    update_editing_mode;
  end else
  if rbPatternMode.Checked or rbBlockMode.Checked then
  begin
    BlockPresetDialog.Show;
  end;
end;

procedure TMainWindow.EditingModeChange(Sender: TObject);
begin
  if rbPatternMode.Checked then
    cur_preset_group := bpgPatternPreset
  else
    cur_preset_group := bpgBlockPreset;
  update_editing_mode;
  if MainWindow.Visible then
    LayerPages.SetFocus;
  btnSavePreset.Visible := False;
end;

procedure TMainWindow.tbBrushSizeChange(Sender: TObject);
begin
  lbBrushWidthVal.Caption := inttostr(tbBrushWidth.Position);
  lbBrushHeightVal.Caption := inttostr(tbBrushHeight.Position);
end;

procedure TMainWindow.btnSavePresetClick(Sender: TObject);
var
  preset_index: integer;
  tmp_preset: TBlockPreset;
  x, y: integer;
begin
  BlockPresetDialog.select_preset_to_save;
  if BlockPresetDialog.ModalResult <> mrOk then
    exit;
  preset_index := BlockPresetDialog.preset_to_save;
  if preset_index = -1 then
    exit;
  if mode(mPatternMode) then
  begin
    Tileset.save_preset(Addr(Map.pattern), bpgPatternPreset, cur_preset_layer, preset_index);
    cur_selected_preset[bpgPatternPreset, cur_preset_layer] := preset_index;
  end else
  if mode(mBlockMode) then
  begin
    tmp_preset.width := cur_block.width;
    tmp_preset.height := cur_block.height;
    for x := 0 to tmp_preset.width - 1 do
      for y := 0 to tmp_preset.height - 1 do
        tmp_preset.tiles[x, y] := cur_block.data[x, y].layers[cur_preset_layer];
    Tileset.save_preset(Addr(tmp_preset), bpgBlockPreset, cur_preset_layer, preset_index);
    cur_selected_preset[bpgBlockPreset, cur_preset_layer] := preset_index;
  end;
  BlockPresetDialog.update_presets(cur_preset_group, cur_preset_layer);
  btnSavePreset.Visible := false;
end;

procedure TMainWindow.ObjectTypePagesChange(Sender: TObject);
begin
  update_level_data;
  case ObjectTypePages.ActivePageIndex of
    0: sbObjectClick(sbMonsterMonster);
    1: sbObjectClick(sbTeleportStart);
    2: sbObjectClick(sbSwitchDown);
    3: sbObjectClick(sbInsertTrigger);
    4: sbObjectClick(sbRemoveTrigger);
    5: sbObjectClick(sbMessagePosition);
  end;
end;

procedure TMainWindow.sbObjectClick(Sender: TObject);
begin
  // Clicked on button which is already active - keep it active
  if cur_object_speedbtn = (Sender as TSpeedButton) then
  begin
    cur_object_speedbtn.Down := true;
    exit;
  end;
  // Deactivate active button
  if cur_object_speedbtn <> nil then
  begin
    cur_object_speedbtn.AllowAllUp := true;
    cur_object_speedbtn.Down := false;
  end;
  // Activate the clicked button
  cur_object_speedbtn := (Sender as TSpeedButton);
  cur_object_speedbtn.Down := true;
  cur_object_speedbtn.AllowAllUp := false;
  cur_object_mode := cur_object_speedbtn.Tag;
  lbActiveObjectMode.Caption := cur_object_speedbtn.Hint;
  render_editing_marker;
end;

procedure TMainWindow.lstMonsterTriggerListClick(Sender: TObject);
begin
  update_monster_entry(lstMonsterTriggerList.ItemIndex);
end;

procedure TMainWindow.lstTeleportListClick(Sender: TObject);
begin
  update_teleport_entry(lstTeleportList.ItemIndex);
end;

procedure TMainWindow.lstSwitchGroupListClick(Sender: TObject);
begin
  update_switch_entry(lstSwitchGroupList.ItemIndex);
end;

procedure TMainWindow.lstInsertTriggerListClick(Sender: TObject);
begin
  update_insert_entry(lstInsertTriggerList.ItemIndex);
end;

procedure TMainWindow.lstRemoveTriggerListClick(Sender: TObject);
begin
  update_remove_entry(lstRemoveTriggerList.ItemIndex);
end;

procedure TMainWindow.lstMessageListClick(Sender: TObject);
begin
  update_message_entry(lstMessageList.ItemIndex);
end;

procedure TMainWindow.clbSwitchListClickCheck(Sender: TObject);
begin
  Map.set_switch_desired_state(lstSwitchGroupList.ItemIndex, clbSwitchList.ItemIndex, IfThen(clbSwitchList.Checked[clbSwitchList.ItemIndex], 1, 0));
  render_map;
end;

procedure TMainWindow.btnMessageTextSaveClick(Sender: TObject);
begin
  Map.set_message_text(lstMessageList.ItemIndex, mMessageText.Lines);
  update_level_data;
end;

procedure TMainWindow.btnSwitchTargetClearClick(Sender: TObject);
begin
  Map.set_switch_target_area(lstSwitchGroupList.ItemIndex, 0, 0, 0, 0, 0);
  update_level_data;
  render_map;
end;

procedure TMainWindow.btnInsertTargetClearClick(Sender: TObject);
begin
  Map.set_insert_target_area(lstInsertTriggerList.ItemIndex, 65535, 65535, 65535, 65535);
  update_level_data;
  render_map;
end;

procedure TMainWindow.btnRemoveTargetClearClick(Sender: TObject);
begin
  Map.set_remove_target_area(lstRemoveTriggerList.ItemIndex, 65535, 65535, 65535, 65535);
  update_level_data;
  render_map;
end;

procedure TMainWindow.lstMonsterListDblClick(Sender: TObject);
var
  trg: ^TLvlMonsterTrigger;
  offset: word;
  pos_x, pos_y: integer;
begin
  trg := Addr(Map.leveldata.monster_triggers[lstMonsterTriggerList.ItemIndex]);
  offset := trg.Offsets[Max(lstMonsterList.ItemIndex, 0)];
  pos_x := offset mod max_map_width;
  pos_y := offset div max_map_width;
  center_map_to(pos_x, pos_y);
end;

procedure TMainWindow.lbTeleportStartPosDblClick(Sender: TObject);
var
  coords: ^TLvlTeleportCoordinates;
begin
  coords := Addr(Map.leveldata.teleport_coordinates[lstTeleportList.ItemIndex]);
  center_map_to(coords.StartOff mod max_map_width, coords.StartOff div max_map_width);
end;

procedure TMainWindow.lbTeleportTargetPosDblClick(Sender: TObject);
var
  coords: ^TLvlTeleportCoordinates;
begin
  coords := Addr(Map.leveldata.teleport_coordinates[lstTeleportList.ItemIndex]);
  center_map_to(coords.EndOff mod max_map_width, coords.EndOff div max_map_width);
end;

procedure TMainWindow.lbSwitchTargetDblClick(Sender: TObject);
var
  coords: ^TLvlSwitchCoordinates;
begin
  coords := Addr(Map.leveldata.switch_coordinates[lstSwitchGroupList.ItemIndex]);
  center_map_to((coords.UpperLeftX + coords.LowerRightX) div 2, (coords.UpperLeftY + coords.LowerRightY) div 2);
end;

procedure TMainWindow.clbSwitchListDblClick(Sender: TObject);
var
  coords: ^TLvlSwitchCoordinates;
  offset: word;
  pos_x, pos_y: integer;
begin
  coords := Addr(Map.leveldata.switch_coordinates[lstSwitchGroupList.ItemIndex]);
  offset := coords.SwitchOffsets[Max(clbSwitchList.ItemIndex, 0)];
  pos_x := offset mod max_map_width;
  pos_y := offset div max_map_width;
  center_map_to(pos_x, pos_y);
end;

procedure TMainWindow.lbInsertTargetDblClick(Sender: TObject);
var
  coords: ^TLvlTriggerCoordinates;
begin
  coords := Addr(Map.leveldata.insert_trigger_coordinates[lstInsertTriggerList.ItemIndex]);
  if coords.UpperLeftX <> 65535 then
    center_map_to((coords.UpperLeftX + coords.LowerRightX) div 2, (coords.UpperLeftY + coords.LowerRightY) div 2);
end;

procedure TMainWindow.lbRemoveTargetDblClick(Sender: TObject);
var
  coords: ^TLvlTriggerCoordinates;
begin
  coords := Addr(Map.leveldata.remove_trigger_coordinates[lstRemoveTriggerList.ItemIndex]);
  if coords.UpperLeftX <> 65535 then
    center_map_to((coords.UpperLeftX + coords.LowerRightX) div 2, (coords.UpperLeftY + coords.LowerRightY) div 2);
end;

procedure TMainWindow.lbMessagePositionDblClick(Sender: TObject);
var
  msg_data: ^TLvlMessageData;
begin
  msg_data := Addr(Map.leveldata.message_data[lstMessageList.ItemIndex]);
  if msg_data.PosX <> 65535 then
    center_map_to(msg_data.PosX, msg_data.PosY);
end;

procedure TMainWindow.resize_map_canvas;
begin
  map_canvas_width := (ClientWidth - 308) div 32;
  if map_canvas_width > Map.width then
    map_canvas_width := Map.width;
  map_canvas_height := (ClientHeight - 50) div 32;
  if map_canvas_height > Map.height then
    map_canvas_height := Map.height;
  MapCanvas.Picture.Bitmap.Width := map_canvas_width * 32;
  MapCanvas.Picture.Bitmap.Height := map_canvas_height * 32;
  MapCanvas.Width := map_canvas_width * 32;
  MapCanvas.Height := map_canvas_height * 32;
  MapScrollH.Top := map_canvas_height * 32 + 8;
  MapScrollH.Width := map_canvas_width * 32;
  MapScrollH.Visible := MapScrollH.Width > 0;
  MapScrollH.Max := Map.width - map_canvas_width;
  if Map.width = map_canvas_width then
    MapScrollH.Enabled := False
  else
    MapScrollH.Enabled := True;
  MapScrollV.Left := map_canvas_width * 32 + 8;
  MapScrollV.Height := map_canvas_height * 32;
  MapScrollV.Visible := MapScrollV.Height > 0;
  MapScrollV.Max := Map.height - map_canvas_height;
  if Map.height = map_canvas_height then
    MapScrollV.Enabled := False
  else
    MapScrollV.Enabled := True;
  mmap_border_x := (max_map_width - Map.width) div 2;
  mmap_border_y := (max_map_height - Map.height) div 2;
end;

procedure TMainWindow.render_map;
begin
  if not Map.loaded then
    exit;
  Renderer.render_map_contents(MapCanvas.Canvas, map_canvas_left, map_canvas_top, map_canvas_width, map_canvas_height, 0, 0, Addr(Map.data),
    sbBackgroundLayer.Down, sbForegroundLayer.Down, sbHiddenLayer.Down, sbObjectLayer.Down, sbShowMarkers.Down, sbShowGrid.Down,
    true);
  render_editing_marker;
end;

procedure TMainWindow.render_minimap;
begin
  if not Map.loaded then
    exit;
  Renderer.render_minimap_contents(minimap_buffer.Canvas, Addr(Map.data), Map.width, Map.height);
  render_minimap_position_marker;
end;

procedure TMainWindow.render_minimap_position_marker;
begin
  MiniMap.Canvas.CopyRect(rect(0,0,MiniMap.Width,MiniMap.Height),minimap_buffer.Canvas,rect(0,0,MiniMap.Width,MiniMap.Height));
  MiniMap.Canvas.Pen.Color:= $00FF00;
  MiniMap.Canvas.Brush.Style := bsClear;
  MiniMap.Canvas.Rectangle(mmap_border_x + map_canvas_left,mmap_border_y + map_canvas_top,mmap_border_x + map_canvas_left + map_canvas_width,mmap_border_y + map_canvas_top + map_canvas_height);
  MiniMap.Canvas.Brush.Style := bsSolid;
end;

procedure TMainWindow.render_editing_marker;
var
  min_x, min_y, max_x, max_y: integer;
  mark_color: TColor;
  mark_text: string;
  object_type: TObjectType;
begin
  if not mouse_over_map_canvas then
  begin
    Renderer.remove_editing_marker(MapCanvas.Canvas);
    CursorImage.Visible := false;
    exit;
  end;

  if Settings.DrawObjectBrush and mode(mObject) and not mode(mSelecting) then
  begin
    object_type := otItem;
    mark_text := '';
    min_x := 1;
    min_y := 1;
    // Determine the color of marker
    case cur_object_mode of
      100: object_type := otMonster;
      101: object_type := otMonsterTrigger;
      200: begin object_type := otTeleport; mark_text := 'Start'; end;
      201: begin object_type := otTeleport; mark_text := 'Targ'; end;
      300: begin object_type := otSwitch; mark_text := ''; end;
      301: begin object_type := otSwitch; mark_text := 'Up'; end;
      400: object_type := otInsertWall;
      500: object_type := otRemoveWall;
    end;
    mark_color := object_type_colors[ord(object_type)];
    // For start position it is 2x2-sized marker
    if cur_object_mode = 99 then
    begin
      mark_color := clRed;
      min_x := 2;
      min_y := 2;
    end;
    // Draw object placement marker
    Renderer.draw_editing_marker(MapCanvas.Canvas, map_canvas_left, map_canvas_top, map_canvas_width, map_canvas_height,
      Addr(Map.data), mouse_old_x, mouse_old_y, min_x, min_y, psDot, mark_color, mark_text);
  end else
  if selection_started then
  begin
    // Draw border around selected block on map
    min_x := min(selection_start_x, selection_end_x);
    max_x := max(selection_start_x, selection_end_x);
    min_y := min(selection_start_y, selection_end_y);
    max_y := max(selection_start_y, selection_end_y);
    mark_color := clMaroon;
    if mode(mObject) then
    begin
      case cur_object_mode of
        302: mark_color := object_type_colors[ord(otSwitch)];
        401: mark_color := object_type_colors[ord(otInsertWall)];
        501: mark_color := object_type_colors[ord(otRemoveWall)];
      end;
    end;
    if mode(mTileMode) or (mode(mPatternMode) and not (ssShift in cur_shift_state)) or (mode(mBlockMode) and not cbAllLayers.Checked) then
      mark_color := layer_marker_color[cur_layer];
    Renderer.draw_editing_marker(MapCanvas.Canvas, map_canvas_left, map_canvas_top, map_canvas_width, map_canvas_height,
      Addr(Map.data), min_x, min_y, max_x-min_x+1, max_y-min_y+1, psSolid, mark_color, '');
    StatusBar.Panels[1].Text := inttostr(max_x-min_x+1) + ' x ' + inttostr(max_y-min_y+1);
  end else
  if Settings.DrawPaintBrush and mode(mPainting) then
  begin
    // Draw paint brush marker
    Renderer.draw_editing_marker(MapCanvas.Canvas, map_canvas_left, map_canvas_top, map_canvas_width, map_canvas_height,
      Addr(Map.data), mouse_old_x, mouse_old_y, tbBrushWidth.Position, tbBrushHeight.Position, psDot, layer_marker_color[cur_layer], '');
  end else
  begin
    Renderer.remove_editing_marker(MapCanvas.Canvas);
  end;
  // Set cursor image visibility
  CursorImage.Visible := mode(mBlockMode) and (cur_block.width > 0) and (cur_block.height > 0) and not selection_started;
end;

procedure TMainWindow.render_tileset;
var
  i: integer;
  tile_x, tile_y: integer;
begin
  for i := 0 to Length(item_info) - 1 do
  begin
    tile_x := tileset.item_tiles[i] mod tileset_cols;
    tile_y := tileset.item_tiles[i] div tileset_cols;
    item_buttons[i].Glyph.Canvas.CopyRect(Rect(0,0,32,32), Tileset.tileimage.Canvas, Rect(tile_x*16, tile_y*16, tile_x*16+16, tile_y*16+16));
    item_buttons[i].Glyph.Canvas.Pixels[0,31] := $1;
  end;
  cur_selected_preset[0,0] := 0;
  cur_selected_preset[0,1] := 0;
  cur_selected_preset[1,0] := 0;
  cur_selected_preset[1,1] := 0;
  LevelPropertiesDialog.update_contents;
  update_editing_mode;
end;

procedure TMainWindow.update_level_data;
var
  i, j: integer;
  last_item_index: integer;
  tmp_strings: TStringList;
  sprite_set: integer;
  cnt: integer;
  str: string;
  switch_coords: ^TLvlSwitchCoordinates;
  trig_coords: ^TLvlTriggerCoordinates;
begin
  // Do not update anything if objects page is not selected
  if LayerPages.ActivePageIndex <> 3 then
    exit;
  // Update start position label
  if ufStartPos in Map.leveldata_dirtyflag then
  begin
    lbStartPosition.Caption := 'Start at: ' + inttostr(Map.leveldata.player_info.PlayerX) + ' , ' + inttostr(Map.leveldata.player_info.PlayerY);
    Map.leveldata_dirtyflag := Map.leveldata_dirtyflag - [ufStartPos];
  end;
  // Update Monsters tab
  if (ObjectTypePages.ActivePageIndex = 0) and (ufMonsters in Map.leveldata_dirtyflag) then
  begin
    tmp_strings := TStringList.Create;
    for i := 0 to Length(Map.leveldata.monster_triggers) - 1 do
    begin
      cnt := 0;
      // Compute number of monsters in the group
      for j := 0 to Length(Map.leveldata.monster_triggers[0].Offsets) - 1 do
        if Map.leveldata.monster_triggers[i].Types[j] <> 65535 then
          Inc(cnt);
      str := 'Group ' + inttostr(i);
      if cnt <> 0 then
        str := str + ' (' + inttostr(cnt) + ')';
      tmp_strings.Add(str);
    end;
    last_item_index := Max(lstMonsterTriggerList.ItemIndex, 0);
    lstMonsterTriggerList.Items := tmp_strings;
    lstMonsterTriggerList.ItemIndex := last_item_index;
    update_monster_entry(last_item_index);
    tmp_strings.Destroy;
    Map.leveldata_dirtyflag := Map.leveldata_dirtyflag - [ufMonsters];
  end;
  // Update Monster types list
  if (ObjectTypePages.ActivePageIndex = 0) and (ufMonsterTypes in Map.leveldata_dirtyflag) then
  begin
    tmp_strings := TStringList.Create;
    for i := 0 to Length(Map.leveldata.monster_info) - 1 do
    begin
      sprite_set := Map.leveldata.monster_info[i].SpriteSet;
      if sprite_set = 65535 then
        continue;
      str := inttostr(i) + ' - ' + Archive.get_monster_type_name(sprite_set);
      tmp_strings.Add(str);
    end;
    last_item_index := Max(lstMonsterTypeList.ItemIndex, 0);
    lstMonsterTypeList.Items := tmp_strings;
    lstMonsterTypeList.ItemIndex := Min(last_item_index, lstMonsterTriggerList.Count - 1);
    tmp_strings.Destroy;
    Map.leveldata_dirtyflag := Map.leveldata_dirtyflag - [ufMonsterTypes];
  end;
  // Update Teleports tab
  if (ObjectTypePages.ActivePageIndex = 1) and (ufTeleports in Map.leveldata_dirtyflag) then
  begin
    tmp_strings := TStringList.Create;
    for i := 0 to Length(Map.leveldata.teleport_coordinates) - 1 do
    begin
      cnt := IfThen(Map.leveldata.teleport_coordinates[i].StartOff <> 0, 1, 0);
      str := 'Teleport ' + inttostr(i);
      if cnt <> 0 then
        str := str + ' (*)';
      tmp_strings.Add(str);
    end;
    last_item_index := Max(lstTeleportList.ItemIndex, 0);
    lstTeleportList.Items := tmp_strings;
    lstTeleportList.ItemIndex := last_item_index;
    update_teleport_entry(last_item_index);
    tmp_strings.Destroy;
    Map.leveldata_dirtyflag := Map.leveldata_dirtyflag - [ufTeleports];
  end;
  // Update Switches tab
  if (ObjectTypePages.ActivePageIndex = 2) and (ufSwitches in Map.leveldata_dirtyflag) then
  begin
    tmp_strings := TStringList.Create;
    for i := 0 to Length(Map.leveldata.switch_coordinates) - 1 do
    begin
      switch_coords := Addr(Map.leveldata.switch_coordinates[i]);
      cnt := 0;
      // Compute number of switches in the group
      for j := 0 to Length(switch_coords.SwitchOffsets) - 1 do
        if switch_coords.SwitchOffsets[j] <> 65535 then
          Inc(cnt);
      str := 'Group ' + inttostr(i);
      if cnt <> 0 then
        str := str + ' (' + inttostr(cnt) + ')';
      if (switch_coords.UpperLeftX <> 0) or (switch_coords.UpperLeftY <> 0) then
        str := str + ' (' + chr(IfThen(switch_coords.SwitchType = 0, ord('R'), ord('I'))) + ')';
      tmp_strings.Add(str);
    end;
    last_item_index := Max(lstSwitchGroupList.ItemIndex, 0);
    lstSwitchGroupList.Items := tmp_strings;
    lstSwitchGroupList.ItemIndex := last_item_index;
    update_switch_entry(last_item_index);
    tmp_strings.Destroy;
    Map.leveldata_dirtyflag := Map.leveldata_dirtyflag - [ufSwitches];
  end;
  // Update Insert wall tab
  if (ObjectTypePages.ActivePageIndex = 3) and (ufInsertTrg in Map.leveldata_dirtyflag) then
  begin
    tmp_strings := TStringList.Create;
    for i := 0 to Length(Map.leveldata.insert_trigger_coordinates) - 1 do
    begin
      trig_coords := Addr(Map.leveldata.insert_trigger_coordinates[i]);
      cnt := IfThen((trig_coords.UpperLeftX <> 0) and (trig_coords.UpperLeftY <> 0) and (trig_coords.UpperLeftX <> 65535), 1, 0);
      str := 'Trigger ' + inttostr(i);
      if cnt <> 0 then
        case trig_coords.RequiredKey of
          0: str := str + ' (*)';
          1: str := str + ' (S)';
          2: str := str + ' (G)';
        end;
      tmp_strings.Add(str);
    end;
    last_item_index := Max(lstInsertTriggerList.ItemIndex, 0);
    lstInsertTriggerList.Items := tmp_strings;
    lstInsertTriggerList.ItemIndex := last_item_index;
    update_insert_entry(last_item_index);
    tmp_strings.Destroy;
    Map.leveldata_dirtyflag := Map.leveldata_dirtyflag - [ufInsertTrg];
  end;
  // Update Remove wall tab
  if (ObjectTypePages.ActivePageIndex = 4) and (ufRemoveTrg in Map.leveldata_dirtyflag) then
  begin
    tmp_strings := TStringList.Create;
    for i := 0 to Length(Map.leveldata.remove_trigger_coordinates) - 1 do
    begin
      trig_coords := Addr(Map.leveldata.remove_trigger_coordinates[i]);
      cnt := IfThen((trig_coords.UpperLeftX <> 0) and (trig_coords.UpperLeftY <> 0) and (trig_coords.UpperLeftX <> 65535), 1, 0);      str := 'Trigger ' + inttostr(i);
      if cnt <> 0 then
        case trig_coords.RequiredKey of
          0: str := str + ' (*)';
          1: str := str + ' (S)';
          2: str := str + ' (G)';
        end;
      tmp_strings.Add(str);
    end;
    last_item_index := Max(lstRemoveTriggerList.ItemIndex, 0);
    lstRemoveTriggerList.Items := tmp_strings;
    lstRemoveTriggerList.ItemIndex := last_item_index;
    update_remove_entry(last_item_index);
    tmp_strings.Destroy;
    Map.leveldata_dirtyflag := Map.leveldata_dirtyflag - [ufRemoveTrg];
  end;
  // Update Messages tab
  if (ObjectTypePages.ActivePageIndex = 5) and (ufMsgData in Map.leveldata_dirtyflag) then
  begin
    tmp_strings := TStringList.Create;
    for i := 0 to Length(Map.leveldata.message_data) - 1 do
    begin
      cnt := IfThen(Map.leveldata.message_data[i].PosX <> 65535, 1, 0);
      str := 'Message ' + inttostr(i);
      if cnt <> 0 then
        str := str + ' (*)';
      tmp_strings.Add(str);
    end;
    last_item_index := Max(lstMessageList.ItemIndex, 0);
    lstMessageList.Items := tmp_strings;
    lstMessageList.ItemIndex := last_item_index;
    update_message_entry(last_item_index);
    tmp_strings.Destroy;
    Map.leveldata_dirtyflag := Map.leveldata_dirtyflag - [ufMsgData];
  end;
end;

procedure TMainWindow.update_monster_entry(index: integer);
var
  i: integer;
  offset, mtype: word;
  pos_x, pos_y: integer;
begin
  lstMonsterList.Items.Clear;
  if index = -1 then
    exit;
  for i := 0 to Length(Map.leveldata.monster_triggers[0].Offsets) - 1 do
  begin
    mtype := Map.leveldata.monster_triggers[index].Types[i];
    offset := Map.leveldata.monster_triggers[index].Offsets[i];
    if mtype = 65535 then
      continue;
    pos_x := offset mod max_map_width;
    pos_y := offset div max_map_width;
    lstMonsterList.Items.Add(inttostr(i+1) + ' (' + inttostr(mtype) + ' - ' + Archive.get_monster_type_name(Map.leveldata.monster_info[mtype].SpriteSet) + ') at ' + inttostr(pos_x) + ' , ' + inttostr(pos_y));
  end;
end;

procedure TMainWindow.update_teleport_entry(index: integer);
var
  pos_x, pos_y: integer;
begin
  if index = -1 then
    exit;
  pos_x := Map.leveldata.teleport_coordinates[index].StartOff mod max_map_width;
  pos_Y := Map.leveldata.teleport_coordinates[index].StartOff div max_map_width;
  lbTeleportStartPos.Caption := 'Start at: ' + inttostr(pos_x) + ' , ' + inttostr(pos_y);
  pos_x := Map.leveldata.teleport_coordinates[index].EndOff mod max_map_width;
  pos_Y := Map.leveldata.teleport_coordinates[index].EndOff div max_map_width;
  lbTeleportTargetPos.Caption := 'Target at: ' + inttostr(pos_x) + ' , ' + inttostr(pos_y);
end;

procedure TMainWindow.update_switch_entry(index: integer);
var
  i: integer;
  switch_coords: ^TLvlSwitchCoordinates;
  offset, desired_state: word;
  pos_x, pos_y: integer;
  switch_group: integer;
  switch_state_str: string;
begin
  clbSwitchList.Clear;
  if index = -1 then
    exit;
  switch_coords := Addr(Map.leveldata.switch_coordinates[index]);
  cbxSwitchType.ItemIndex := switch_coords.SwitchType;
  for i := 0 to Length(switch_coords.SwitchOffsets) - 1 do
  begin
    desired_state := switch_coords.DesiredTiles[i];
    offset := switch_coords.SwitchOffsets[i];
    if offset = 65535 then
      continue;
    pos_x := offset mod max_map_width;
    pos_y := offset div max_map_width;
    if Map.get_object_type(Map.data[pos_x, pos_y].objects, switch_group) <> otSwitch then
      switch_state_str := 'invalid'
    else if switch_group <> index then
      switch_state_str := 'invalid'
    else if Map.data[pos_x, pos_y].layers[0] = Map.leveldata.animation_info.SwitchDownTile then
      switch_state_str := 'down'
    else if Map.data[pos_x, pos_y].layers[0] = Map.leveldata.animation_info.SwitchUpTile then
      switch_state_str := 'up'
    else
      switch_state_str := 'invalid';
    clbSwitchList.Items.Add('S ' + inttostr(i) + ' (' + switch_state_str + ') at ' + inttostr(pos_x) + ' , ' + inttostr(pos_y));
    clbSwitchList.Checked[clbSwitchList.Count - 1] := desired_state = Map.leveldata.animation_info.SwitchUpTile;
  end;
  lbSwitchTarget.Caption := 'Target area: ' + inttostr(switch_coords.UpperLeftX) + ' : ' + inttostr(switch_coords.LowerRightX) + ' , ' + inttostr(switch_coords.UpperLeftY) + ' : ' + inttostr(switch_coords.LowerRightY);
end;

procedure TMainWindow.update_insert_entry(index: integer);
var
  trig_coords: ^TLvlTriggerCoordinates;
begin
  if index = -1 then
    exit;
  trig_coords := Addr(Map.leveldata.insert_trigger_coordinates[index]);
  cbxInsertKeyType.ItemIndex := trig_coords.RequiredKey;
  lbInsertTrigger.Caption := 'Trigger tile: ' + inttostr(trig_coords.RequiredTile);
  if trig_coords.UpperLeftX = 65535 then
    lbInsertTarget.Caption := 'Target area: None'
  else
    lbInsertTarget.Caption := 'Target area: ' + inttostr(trig_coords.UpperLeftX) + ' : ' + inttostr(trig_coords.LowerRightX) + ' , ' + inttostr(trig_coords.UpperLeftY) + ' : ' + inttostr(trig_coords.LowerRightY);
end;

procedure TMainWindow.update_remove_entry(index: integer);
var
  trig_coords: ^TLvlTriggerCoordinates;
begin
  if index = -1 then
    exit;
  trig_coords := Addr(Map.leveldata.remove_trigger_coordinates[index]);
  cbxRemoveKeyType.ItemIndex := trig_coords.RequiredKey;
  lbRemoveTrigger.Caption := 'Trigger tile: ' + inttostr(trig_coords.RequiredTile);
  if trig_coords.UpperLeftX = 65535 then
    lbRemoveTarget.Caption := 'Target area: None'
  else
    lbRemoveTarget.Caption := 'Target area: ' + inttostr(trig_coords.UpperLeftX) + ' : ' + inttostr(trig_coords.LowerRightX) + ' , ' + inttostr(trig_coords.UpperLeftY) + ' : ' + inttostr(trig_coords.LowerRightY);
end;

procedure TMainWindow.update_message_entry(index: integer);
var
  msg_data: ^TLvlMessageData;
  i: integer;
  line: string;
begin
  if index = -1 then
    exit;
  msg_data := Addr(Map.leveldata.message_data[index]);
  lbMessagePosition.Caption := 'Position: ' + inttostr(msg_data.PosX) + ' , ' + inttostr(msg_data.PosY);
  mMessageText.Lines.Clear;
  for i := 0 to Length(msg_data.Lines) - 1 do
  begin
    SetString(line, msg_data.Lines[i], Min(StrLen(msg_data.Lines[i]), Length(msg_data.Lines[i])));
    if line <> '' then
      mMessageText.Lines.Add(line);
  end;
end;

procedure TMainWindow.load_map_from_archive(index: integer);
begin
  // Load map
  Map.load_map_from_archive(index);
  // Set status bar
  StatusBar.Panels[4].Text := Archive.level_names[index];
  StatusBar.Panels[3].Text := inttostr(Map.width)+' x '+inttostr(Map.height);
  set_window_titles(Archive.level_names[index]);
  LevelPropertiesDialog.update_contents;
  // Rendering
  resize_map_canvas;
  render_minimap;
  render_map;
  update_level_data;
end;

procedure TMainWindow.save_map_to_archive(index: integer);
var
  old_index: integer;
begin
  if not Map.loaded then
    exit;
  // Save map
  old_index := Map.index;
  Map.save_map_to_archive(index);
  // Update map name on status bar if map index has changed
  if Map.index <> old_index then
  begin
    StatusBar.Panels[4].Text := Archive.level_names[Map.index];
    set_window_titles(Archive.level_names[Map.index]);
  end;
end;

procedure TMainWindow.load_map_from_file(filename: String);
begin
  if not FileExists(filename) then
    exit;
  if UpperCase(Copy(filename, Length(filename)-2, 3)) <> 'HPM' then
  begin
    Application.MessageBox('Invalid file type', 'Load map error', MB_ICONERROR);
    exit;
  end;
  // Load map file
  Map.load_map_file(filename);
  // Show map filename on status bar and title
  StatusBar.Panels[4].Text := filename;
  StatusBar.Panels[3].Text := inttostr(Map.width)+' x '+inttostr(Map.height);
  set_window_titles(ExtractFileName(filename));
  LevelPropertiesDialog.update_contents;
  // Initialize settings
  Settings.get_file_paths_from_map_filename;
  // Rendering
  resize_map_canvas;
  render_minimap;
  render_map;
  update_level_data;
end;

procedure TMainWindow.save_map_to_file(filename: String);
begin
  if not Map.loaded then
    exit;
  // Save map file
  Map.save_map_file(filename);
  // Show map filename on status bar and title if map is not loaded from archive
  if Map.index = -1 then
  begin
    StatusBar.Panels[4].Text := filename;
    set_window_titles(ExtractFileName(filename));
  end;
end;

function TMainWindow.check_map_errors: boolean;
var
  errmsg: String;
begin
  errmsg := Map.check_errors;
  result := true;
  if errmsg <> '' then
  begin
    Application.MessageBox(PChar(errmsg), 'Map error', MB_ICONWARNING);
    result := false;
  end;
end;

procedure TMainWindow.set_window_titles(map_name: String);
begin
  Caption := 'Hocus Pocus Level Editor';
  LevelPropertiesDialog.Caption := 'Level properties';
  if map_name <> '' then
  begin
    Caption := Caption + ' - ' + map_name;
    LevelPropertiesDialog.Caption := LevelPropertiesDialog.Caption + ' - ' + map_name;
  end;
end;

function TMainWindow.check_map_can_be_tested: boolean;
begin
  if not Map.loaded then
  begin
    Application.MessageBox('No map to test.', 'Cannot test map', MB_ICONERROR);
    result := false;
  end else
  if Map.index = -1 then
  begin
    Application.MessageBox('Map is not saved in the game archive.', 'Cannot test map', MB_ICONERROR);
    result := false;
  end else
  if not FileExists(Settings.GameFolder + 'HOCUS.EXE') then
  begin
    Application.MessageBox(PChar('Cannot find game executable (' + Settings.GameFolder + 'HOCUS.EXE)'), 'Cannot test map', MB_ICONERROR);
    result := false;
  end else
  if not FileExists(Settings.DosboxPath) then
  begin
    Application.MessageBox(PChar('Cannot find Dosbox. Please specify full path to Dosbox in HocusEditor.ini file.'#13#13'(Note: Close the program first. HocusEditor.ini file is overwritten each time the program is closed.)'), 'Cannot test map', MB_ICONERROR);
    result := false;
  end else
  if Settings.CheckMapErrorsOnTest then
    result := check_map_errors
  else
    result := true;
end;

procedure TMainWindow.launch_game;
var
  sav_file: file of byte;
  sav_file_contents: TSavFile;
  save_name: string[7];
begin
  // Save the map
  save_map_to_archive(Map.index);
  // Modify HOCUS.SAV to allow direct testing of this map
  AssignFile(sav_file, Settings.GameFolder + 'HOCUS.SAV');
  FileMode := fmOpenReadWrite;
  Reset(sav_file);
  BlockRead(sav_file, sav_file_contents, sizeof(sav_file_contents));
  sav_file_contents.save_episode[0] := Map.index div 9;
  sav_file_contents.save_levelnum[0] := Map.index mod 9;
  sav_file_contents.save_difficulty[0] := Settings.TestMapDifficulty;
  FillChar(sav_file_contents.save_name[0,0], save_name_length, 0);
  save_name := 'TESTMAP';
  Move(save_name[1], sav_file_contents.save_name[0,0], 7);
  sav_file_contents.save_score[0] := 0;
  Seek(sav_file, 0);
  BlockWrite(sav_file, sav_file_contents, sizeof(sav_file_contents));
  // Launch game
  ShellExecuteA(0, 'open', PChar(Settings.DosboxPath), PChar('"' + Settings.GameFolder + 'HOCUS.EXE" ' + Settings.DosboxParameters), PChar(Settings.GameFolder), SW_SHOWNORMAL);
end;

function TMainWindow.mode(m: SelectedMode): boolean;
begin
  result := false;
  case m of
    mTileMode:        result := (LayerPages.TabIndex < 3) and (rbTileMode.Checked);
    mPatternMode:     result := (LayerPages.TabIndex < 3) and (rbPatternMode.Checked);
    mBlockMode:       result := (LayerPages.TabIndex < 3) and (rbBlockMode.Checked);
    mPainting:        result := (LayerPages.TabIndex < 3) and (rbTileMode.Checked or rbPatternMode.Checked) and not selection_started;
    mSelecting:       result := ((mode(mPatternMode) or mode(mBlockMode)) and (ssShift in cur_shift_state)) or
                                ((mode(mTileMode) or (mode(mPatternMode))) and (ssCtrl in cur_shift_state)) or
                                (mode(mObject) and ((cur_object_mode = 302) or (cur_object_mode = 401) or (cur_object_mode = 501)));
    mRightBtnScroll:  result := mode(mBlockMode) or (selection_started and (ssLeft in cur_shift_state)) or (mode(mObject) and mode(mSelecting));
    mTileLayer:       result := (LayerPages.TabIndex < 3);
    mObject:          result := (LayerPages.TabIndex = 3);
  end;
end;

function TMainWindow.mouse_over_map_canvas: boolean;
var
  pos: TPoint;
begin
  pos := MainWindow.ScreenToClient(Mouse.CursorPos);
  result := PtInRect(MapCanvas.BoundsRect, pos);
  result := result and not (BlockPresetDialog.Visible and PtInRect(BlockPresetDialog.BoundsRect, Mouse.CursorPos));
  result := result and not (LevelPropertiesDialog.Visible and PtInRect(LevelPropertiesDialog.BoundsRect, Mouse.CursorPos));
  result := result or selection_started;
end;

procedure TMainWindow.center_map_to(x, y: integer);
begin
  MapScrollH.Position := x - (map_canvas_width div 2);
  MapScrollV.Position := y - (map_canvas_height div 2);
end;

procedure TMainWindow.show_statistics;
begin
  StatusBar.Panels[5].Text := 'Cry: ' + inttostr(Map.stats.cnt_crystals) + '  Heal: ' + inttostr(Map.stats.cnt_healths) + '  Tre: ' + inttostr(Map.stats.cnt_treasure) +
    '  Key: ' + inttostr(Map.stats.cnt_silver_keys) + '/' + inttostr(Map.stats.cnt_gold_keys) + '  Mon: ' + inttostr(Map.stats.cnt_monsters);
  StatusBar.Panels[6].Text := 'Points: ' + inttostr(Map.stats.total_points);
end;

procedure TMainWindow.update_editing_mode;
begin
  if mode(mTileMode) and Usepredefinedtiles1.Checked then
  begin
    if cur_predefined_tile[cur_preset_layer] <> -1 then
      cur_tile_index := Tileset.predefined_tiles[cur_preset_layer, cur_predefined_tile[cur_preset_layer]];
  end else
  begin
    select_current_preset;
  end;
  draw_block_image;
  render_editing_marker;
  mouse_already_clicked := false;
  if (BlockPresetDialog <> nil) then
    BlockPresetDialog.update_presets(cur_preset_group, cur_preset_layer);
end;

procedure TMainWindow.select_current_preset;
var
  preset_index: integer;
  preset: TBlockPresetPtr;
  x, y, z: integer;
begin
  preset_index := cur_selected_preset[cur_preset_group, cur_preset_layer];
  if preset_index = -1 then
  begin
    if cur_preset_group = bpgPatternPreset then
      Map.set_pattern(Addr(cur_copied_pattern[cur_preset_layer]))
    else
      cur_block := cur_copied_block[cur_preset_layer];
    draw_cursor_image;
    exit;
  end;
  preset := Addr(Tileset.block_presets[cur_preset_group, cur_preset_layer, preset_index]);
  if cur_preset_group = bpgPatternPreset then
  begin
    Map.set_pattern(preset);
  end else
  begin
    // Reset block data
    for x:= 0 to max_block_preset_size - 1 do
      for y := 0 to max_block_preset_size - 1 do
      begin
        for z := 0 to 2 do
          cur_block.data[x,y].layers[z] := 255;
        cur_block.data[x,y].objects := empty_object;
        cur_block.data[x,y].moreinfo := 255;
      end;
    // Copy block data from the block preset
    cur_block.width := preset.width;
    cur_block.height := preset.height;
    for x:= 0 to cur_block.width - 1 do
      for y := 0 to cur_block.height - 1 do
      begin
        cur_block.data[x,y].layers[cur_preset_layer] := preset.tiles[x,y];
        // Set tiles for both foreground and hidden layers
        if cur_preset_layer = 1 then
          cur_block.data[x,y].layers[2] := preset.tiles[x,y];
      end;
  end;
  btnSavePreset.Visible := false;
  draw_cursor_image;
end;

procedure TMainWindow.resize_cursor_image;
var
  cursor_image_left: integer;
  cursor_image_top: integer;
begin
  cursor_image_left := (CursorImage.Left - MapCanvas.Left) div 32;
  cursor_image_top := (CursorImage.Top - MapCanvas.Top) div 32;
  if (cursor_image_left + cur_block.width) > map_canvas_width then
    CursorImage.Width := (map_canvas_width - cursor_image_left) * 32
  else
    CursorImage.Width := cur_block.width * 32 + 1;
  if (cursor_image_top + cur_block.height) > map_canvas_height then
    CursorImage.Height := (map_canvas_height - cursor_image_top) * 32
  else
    CursorImage.Height := cur_block.height * 32 + 1;
end;

procedure TMainWindow.draw_cursor_image;
var
  all_layers: boolean;
begin
  if not mode(mBlockMode) then
    exit;
  CursorImage.Width := cur_block.width * 32 + 1;
  CursorImage.Height := cur_block.height * 32 + 1;
  CursorImage.Picture.Bitmap.Width := cur_block.width * 32 + 1;
  CursorImage.Picture.Bitmap.Height := cur_block.height * 32 + 1;
  // Render cursor image
  all_layers := cbAllLayers.Checked;
  Renderer.render_map_contents(CursorImage.Canvas, 0, 0, cur_block.width, cur_block.height, 0, 0, Addr(cur_block.data),
    (cur_layer = 0) or all_layers, (cur_layer = 1) or all_layers, (cur_layer = 2) or all_layers, all_layers, false, false,
    false);
  CursorImage.Canvas.Pen.Color := IfThen(all_layers, clMaroon, layer_marker_color[cur_layer]);
  CursorImage.Canvas.Brush.Style := bsClear;
  CursorImage.Canvas.Rectangle(0, 0, cur_block.width * 32 + 1, cur_block.height * 32 + 1);
  resize_cursor_image;
  render_editing_marker;
end;

procedure TMainWindow.draw_block_image;
var
  i, x, y: integer;
  tile_x, tile_y: integer;
  border_x, border_y: integer;
  src_rect, dest_rect: TRect;
  str: String;
  all_layers: boolean;
begin
  BlockImage.Canvas.Brush.Style := bsSolid;
  BlockImage.Canvas.Pen.Style := psSolid;
  BlockImage.Canvas.Brush.Color := clBtnFace;
  BlockImage.Canvas.Pen.Color := clBtnFace;
  BlockImage.Canvas.Rectangle(0,0,BlockImage.Width,BlockImage.Height);
  if not Map.loaded then
    exit;
  // Tile mode - render tileset
  if mode(mTileMode) and not Usepredefinedtiles1.Checked then
  begin
    for i := 0 to cnt_tileset_tiles - 1 do
    begin
      x := i mod 16;
      y := i div 16;
      tile_x := i mod tileset_cols;
      tile_y := i div tileset_cols;
      src_rect := Rect(tile_x*16,tile_y*16,tile_x*16+16,tile_y*16+16);
      dest_rect := Rect(x*16,y*16,x*16+16,y*16+16);
      BlockImage.Canvas.CopyRect(dest_rect, Tileset.tileimage.Canvas, src_rect);
      if (i = cur_tile_index) then
      begin
        BlockImage.Canvas.Brush.Style := bsClear;
        BlockImage.Canvas.Pen.Color := clRed;
        BlockImage.Canvas.Rectangle(dest_rect);
      end;
    end;
  end else
  if mode(mTileMode) and Usepredefinedtiles1.Checked then
  begin
    for i := 0 to cnt_predefined_tiles - 1 do
    begin
      x := i mod 8;
      y := i div 8;
      tile_x := Tileset.predefined_tiles[cur_preset_layer, i] mod tileset_cols;
      tile_y := Tileset.predefined_tiles[cur_preset_layer, i] div tileset_cols;
      src_rect := Rect(tile_x*16,tile_y*16,tile_x*16+16,tile_y*16+16);
      dest_rect := Rect(x*32,y*32,x*32+32,y*32+32);
      BlockImage.Canvas.CopyRect(dest_rect, Tileset.tileimage.Canvas, src_rect);
      if (i = cur_predefined_tile[cur_preset_layer]) then
      begin
        BlockImage.Canvas.Brush.Style := bsClear;
        BlockImage.Canvas.Pen.Color := clRed;
        BlockImage.Canvas.Rectangle(dest_rect);
      end;
    end;
  end else
  // Pattern mode - render pattern
  if mode(mPatternMode) then
  begin
    border_x := (BlockImage.Width - Map.pattern.width * 32) div 2;
    border_y := (BlockImage.Height - Map.pattern.height * 32) div 2;
    for x:= 0 to Map.pattern.width-1 do
      for y := 0 to Map.pattern.height-1 do
      begin
        tile_x := Map.pattern.tiles[x,y] mod 20;
        tile_y := Map.pattern.tiles[x,y] div 20;
        src_rect := Rect(tile_x*16, tile_y*16, tile_x*16+16, tile_y*16+16);
        dest_rect := Rect(x*32+border_x, y*32+border_y, x*32+32+border_x, y*32+32+border_y);
        if Map.pattern.tiles[x,y] <> 255 then
          BlockImage.Canvas.CopyRect(dest_rect, Tileset.tileimage.Canvas, src_rect);
      end;
  end else
  // Block mode - render block
  if mode(mBlockMode) then
  begin
    str := '';
    if (cur_block.width = 0) or (cur_block.height = 0) then
      str := 'Empty block is selected.';
    if (cur_block.width > 8) or (cur_block.height > 8) then
      str := 'Block is too big to display it here.';
    if str <> '' then
    begin
      BlockImage.Canvas.TextOut((BlockImage.Width - BlockImage.Canvas.TextWidth(str)) div 2, 122, str);
      exit;
    end;
    border_x := (BlockImage.Width - cur_block.width * 32) div 2;
    border_y := (BlockImage.Height - cur_block.height * 32) div 2;
    all_layers := cbAllLayers.Checked;
    Renderer.render_map_contents(BlockImage.Canvas, 0, 0, cur_block.width, cur_block.height, border_x, border_y, Addr(cur_block.data),
      (cur_layer = 0) or all_layers, (cur_layer = 1) or all_layers, (cur_layer = 2) or all_layers, all_layers, false, false,
      false);
  end;
end;


procedure TMainWindow.set_map_size(new_width, new_height: integer);
begin
  if (Map.width = new_width) and (Map.height = new_height) then
    exit;
  Map.set_map_size(new_width, new_height);
  StatusBar.Panels[3].Text := inttostr(Map.width)+' x '+inttostr(Map.height);
  resize_map_canvas;
  render_minimap;
  render_map;
end;

procedure TMainWindow.shift_map(direction: TDirection; num_tiles: integer);
begin
  Map.shift_map(direction, num_tiles);
  render_minimap;
  render_map;
  update_level_data;
end;

end.
