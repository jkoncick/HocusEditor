unit block_preset_dialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls;

type
  TBlockPresetDialog = class(TForm)
    BlockPresetImage: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CMDialogKey(var AMessage: TCMDialogKey); message CM_DIALOGKEY;
    procedure BlockPresetImageMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    preset_group, preset_layer: integer;
    update_pending: boolean;
    render_letters: boolean;
    selecting_to_save: boolean;

  public
    preset_to_save: integer;

    procedure update_presets(group, layer: integer);
    procedure select_preset(preset_index: integer);
    procedure draw_all;
    procedure draw_block_preset(row, col: integer);
    procedure select_preset_to_save;
  end;

var
  BlockPresetDialog: TBlockPresetDialog;

implementation

uses _tileset, _settings, main;

{$R *.dfm}

procedure TBlockPresetDialog.FormCreate(Sender: TObject);
begin
  ClientWidth := 1280;
  ClientHeight := 512;
end;

procedure TBlockPresetDialog.FormShow(Sender: TObject);
begin
  if update_pending then
  begin
    update_pending := false;
    draw_all;
  end;
end;

procedure TBlockPresetDialog.FormHide(Sender: TObject);
begin
  selecting_to_save := false;
end;

procedure TBlockPresetDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
    27: begin
      if selecting_to_save then
        ModalResult := mrCancel
      else
        Close;
    end;
  end;
  // F1-F4: Change block preset group
  if (key >= 112) and (key <= 115) then
    MainWindow.FormKeyDown(Sender, Key, Shift);
  // Number/letter: Select preset
  if ((key >= ord('0')) and (key <= ord('9'))) or ((key >= ord('A')) and (key <= ord('Z'))) or (key = 186) or (key = 188) or (key = 190) or (key = 191) then
  begin
    if key = 188 then
      key := ord('<');
    if key = 190 then
      key := ord('>');
    if key = 186 then
      key := ord(':');
    if key = 191 then
      key := ord('?');
    select_preset(Tileset.block_key_to_index(key));
  end;
end;

procedure TBlockPresetDialog.CMDialogKey(var AMessage: TCMDialogKey);
begin
  if AMessage.CharCode = VK_TAB then
  begin
    update_presets((preset_group + 1) and 1, preset_layer);
    if preset_group = 1 then
      MainWindow.rbBlockMode.Checked := true
    else
      MainWindow.rbPatternMode.Checked := true;
    SetFocus;
    AMessage.Result := 1;
  end else
    inherited;
end;

procedure TBlockPresetDialog.BlockPresetImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  row, col: integer;
begin
  col := X div 128;
  row := Y div 128;
  if Button = mbRight then
  begin
    // Remove preset
    if Application.MessageBox('Do you want to delete this preset?', 'Delete preset', MB_YESNO or MB_ICONQUESTION) = IDYES then
    begin
      Tileset.remove_preset(preset_group, preset_layer, row * num_key_cols + col);
      draw_all;
    end;
  end
  else if Button = mbLeft then
  begin
    select_preset(row * num_key_cols + col);
  end
  else if Button = mbMiddle then
  begin
    render_letters := not render_letters;
    draw_all;
  end;
end;

procedure TBlockPresetDialog.update_presets(group, layer: integer);
begin
  Caption := 'Preset selection - ' + block_preset_ini_sections[group, layer];
  preset_group := group;
  preset_layer := layer;
  update_pending := not Visible;
  if Visible then
    draw_all;
end;

procedure TBlockPresetDialog.select_preset(preset_index: integer);
begin
  if selecting_to_save then
  begin
    preset_to_save := preset_index;
    ModalResult := mrOk;
  end else
  begin
    if settings.HidePresetWindow then
      Hide;
    MainWindow.cur_selected_preset[preset_group, preset_layer] := preset_index;
    MainWindow.update_editing_mode;
  end;
end;

procedure TBlockPresetDialog.draw_all;
var
  i, j: integer;
begin
  for i := 0 to num_key_rows -1 do
    for j:= 0 to num_key_cols -1 do
      draw_block_preset(i,j);
end;

procedure TBlockPresetDialog.draw_block_preset(row, col: integer);
var
  preset: ^TBlockPreset;
  scale: integer;
  size_x, size_y: integer;
  src_x, src_y: integer;
  off_x, off_y: integer;
  min_x, min_y: integer;
  src_rect, dest_rect: TRect;
  x, y: integer;
  tile_x, tile_y: integer;
begin
  preset := Addr(tileset.block_presets[preset_group, preset_layer, row * num_key_cols + col]);
  BlockPresetImage.Canvas.Pen.Color := clBtnFace;
  BlockPresetImage.Canvas.Brush.Color := clBtnFace;
  BlockPresetImage.Canvas.Rectangle(col*128, row*128, col*128+128, row*128+128);

  scale := 32;
  if (preset.width > 4) or (preset.height > 4) then
    scale := 16;
  min_x := col * 128;
  min_y := row * 128;
  size_x := preset.width * scale;
  size_y := preset.height * scale;
  off_x := ((128 - size_x) div 2) + min_x;
  off_y := ((128 - size_y) div 2) + min_y;
  for x := 0 to preset.width - 1 do
    for y := 0 to preset.height - 1 do
    begin
      if preset.tiles[x,y] = 255 then
        continue;
      tile_x := preset.tiles[x,y] mod tileset_cols;
      tile_y := preset.tiles[x,y] div tileset_cols;
      src_rect := Rect(tile_x*16, tile_y*16, tile_x*16+16, tile_y*16+16);
      dest_rect := Rect(off_x + x*scale, off_y + y*scale, off_x + x*scale + scale, off_y + y*scale + scale);
      BlockPresetImage.Canvas.CopyRect(dest_rect, Tileset.tileimage.Canvas, src_rect);
    end;
  if render_letters then
    BlockPresetImage.Canvas.TextOut(col * 128 + 4, row * 128 + 2, block_preset_keys[row,col]);

  BlockPresetImage.Canvas.Pen.Color := clBlack;
  BlockPresetImage.Canvas.MoveTo(col*128, row*128);
  BlockPresetImage.Canvas.LineTo(col*128+128, row*128);
  BlockPresetImage.Canvas.MoveTo(col*128, row*128);
  BlockPresetImage.Canvas.LineTo(col*128, row*128+128);
end;

procedure TBlockPresetDialog.select_preset_to_save;
begin
  selecting_to_save := true;
  preset_to_save := -1;
  ShowModal;
end;

end.
