unit sprite_dialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, _spritefile, _archive, ExtCtrls, Spin, Math, Buttons, pngimage;

type
  TSpriteDialog = class(TForm)
    lbxSpriteSetList: TListBox;
    SpriteImage: TImage;
    lbSpriteSetNum: TLabel;
    edSpriteName: TEdit;
    ColorDialog: TColorDialog;
    btnBackgroundColor: TButton;
    lbSpriteNameCustom: TLabel;
    btnSaveChanges: TBitBtn;
    cbShowNumbers: TCheckBox;
    btnUndoChanges: TBitBtn;
    btnExportSprites: TButton;
    gbExpImpSprites: TGroupBox;
    rbExpImpSingle: TRadioButton;
    rbExpImpAllMultFiles: TRadioButton;
    rbExpImpAllOneFile: TRadioButton;
    pnExportFormat: TPanel;
    rbExportPng: TRadioButton;
    rbExportBmp: TRadioButton;
    seExpImpSpriteNum: TSpinEdit;
    ExportDialogBmp: TSaveDialog;
    ImportDialog: TOpenDialog;
    btnImportSprites: TButton;
    lbImportTranspIndex: TLabel;
    seImportTranspIndex: TSpinEdit;
    btnEraseSprite: TButton;
    lbExportAs: TLabel;
    lbImportOptions: TLabel;
    cbImportUseUpperPal: TCheckBox;
    ExportDialogPng: TSaveDialog;
    gbSpriteSetProperties: TGroupBox;
    lbSpriteWidth: TLabel;
    lbSpriteHeight: TLabel;
    lbStandFrames: TLabel;
    lbWalkFrames: TLabel;
    lbShootDashFrames: TLabel;
    seShootDashFrameFirst: TSpinEdit;
    seWalkFrameFirst: TSpinEdit;
    seStandFrameFirst: TSpinEdit;
    seSpriteHeight: TSpinEdit;
    seSpriteWidth: TSpinEdit;
    lbTo1: TLabel;
    lbTo2: TLabel;
    lbTo3: TLabel;
    seShootDashFrameLast: TSpinEdit;
    seWalkFrameLast: TSpinEdit;
    seStandFrameLast: TSpinEdit;
    lbProjectileWidth: TLabel;
    lbProjectileHeight: TLabel;
    lbProjectileYOff: TLabel;
    lbProjectileFrames: TLabel;
    lbJumpFallFrame: TLabel;
    seJumpFrame: TSpinEdit;
    seProjectileFrameFirst: TSpinEdit;
    seProjectileYOff: TSpinEdit;
    seProjectileHeight: TSpinEdit;
    seProjectileWidth: TSpinEdit;
    lbTo4: TLabel;
    seProjectileFrameLast: TSpinEdit;
    seFallFrame: TSpinEdit;
    lbLayoutData: TLabel;
    lbPixelData: TLabel;
    lbLayoutSize: TLabel;
    lbPixelSize: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lbxSpriteSetListClick(Sender: TObject);
    procedure SpriteImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure edSpriteNameChange(Sender: TObject);
    procedure cbShowNumbersClick(Sender: TObject);
    procedure btnBackgroundColorClick(Sender: TObject);
    procedure btnSaveChangesClick(Sender: TObject);
    procedure btnUndoChangesClick(Sender: TObject);
    procedure seSpriteSizeChange(Sender: TObject);
    procedure seSpriteSetPropertyChange(Sender: TObject);
    procedure seExpImpSpriteNumChange(Sender: TObject);
    procedure btnExportSpritesClick(Sender: TObject);
    procedure btnImportSpritesClick(Sender: TObject);
    procedure btnEraseSpriteClick(Sender: TObject);

  private
    updating: boolean;
    modified: boolean;
    last_max_used_sprite_num: integer;

  public
    procedure init_sprite_list;
    procedure render_all_sprites;
    procedure load_input_image(input_filename: String; input_buffer: TBitmap);
    procedure set_modified(val: boolean);
  end;

var
  SpriteDialog: TSpriteDialog;

implementation

uses _settings, _map;

{$R *.dfm}

procedure TSpriteDialog.FormCreate(Sender: TObject);
begin
  set_modified(false);
  init_sprite_list;
end;

procedure TSpriteDialog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if modified then
  begin
    if Application.MessageBox('Save changes?', 'Sprite data modified', MB_YESNO or MB_ICONQUESTION) = idYes then
      btnSaveChangesClick(nil)
    else
      btnUndoChangesClick(nil);
  end;
end;

procedure TSpriteDialog.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key = 27 then
    Close;
  if (key = 13) and modified then
    btnSaveChangesClick(nil);
end;

procedure TSpriteDialog.lbxSpriteSetListClick(Sender: TObject);
var
  sprite_set: integer;
  entry: ^TSpriteEntry;
begin
  sprite_set := lbxSpriteSetList.ItemIndex;
  entry := Addr(SpriteFile.sprite_entries[sprite_set]);
  updating := true;
  lbSpriteSetNum.Caption := 'Sprite set ' + inttostr(sprite_set);
  edSpriteName.Text := entry.cSpriteName;
  if Settings.UseCustomMonsterNames = true then
    lbSpriteNameCustom.Caption := Archive.get_monster_type_name(sprite_set);
  seSpriteWidth.Value := entry.iWidth4 * 4;
  seSpriteHeight.Value := entry.iHeight;
  seStandFrameFirst.Value := entry.iStandFrame;
  seStandFrameLast.Value := entry.iStandFrame2;
  seWalkFrameFirst.Value := entry.iWalkFrame1;
  seWalkFrameLast.Value := entry.iWalkFrame2;
  seShootDashFrameFirst.Value := entry.iShootDashFrame1;
  seShootDashFrameLast.Value := entry.iShootDashFrame2;
  seProjectileWidth.Value := entry.iProjectileWidth4 * 4;
  seProjectileHeight.Value := entry.iProjectileHeight;
  seProjectileYOff.Value := entry.iProjectileY;
  seProjectileFrameFirst.Value := entry.iProjectileFrame;
  seProjectileFrameLast.Value := entry.iProjectileF2;
  seJumpFrame.Value := entry.iJumpFrame;
  seFallFrame.Value := entry.iFallFrame;
  updating := false;
  render_all_sprites;
end;

procedure TSpriteDialog.SpriteImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  sprite_width, sprite_height: integer;
begin
  X := X div 2;
  Y := Y div 2;
  sprite_width := seSpriteWidth.Value;
  sprite_height := seSpriteHeight.Value;
  if (Y >= (sprite_height * 2)) or (X >= sprite_width * (SpriteFile.get_max_used_sprite_num(lbxSpriteSetList.ItemIndex) + 1)) then
    exit;
  seExpImpSpriteNum.Value := X div sprite_width + (Y div sprite_height) * 20;
end;

procedure TSpriteDialog.edSpriteNameChange(Sender: TObject);
var
  sprite_set: integer;
  entry: ^TSpriteEntry;
begin
  sprite_set := lbxSpriteSetList.ItemIndex;
  if (sprite_set = -1) or updating then
    exit;
  entry := Addr(SpriteFile.sprite_entries[sprite_set]);
  StrPLCopy(entry.cSpriteName, edSpriteName.Text, High(entry.cSpriteName));
  lbxSpriteSetList.Items[sprite_set] := inttostr(sprite_set) + ' - ' + entry.cSpriteName;
  set_modified(true);
  if Settings.UseCustomMonsterNames = false then
    Map.leveldata_dirtyflag := Map.leveldata_dirtyflag + [ufMonsterTypes];
end;

procedure TSpriteDialog.cbShowNumbersClick(Sender: TObject);
begin
  render_all_sprites;
end;

procedure TSpriteDialog.btnBackgroundColorClick(Sender: TObject);
begin
  if ColorDialog.Execute then
  begin
    SpriteFile.update_transparent_color(ColorDialog.Color);
    render_all_sprites;
  end;
end;

procedure TSpriteDialog.btnSaveChangesClick(Sender: TObject);
begin
  SpriteFile.save_to_archive;
  set_modified(false);
end;

procedure TSpriteDialog.btnUndoChangesClick(Sender: TObject);
begin
  SpriteFile.load_from_archive;
  init_sprite_list;
  set_modified(false);
end;

procedure TSpriteDialog.seSpriteSizeChange(Sender: TObject);
var
  sprite_set: integer;
  entry: ^TSpriteEntry;
begin
  sprite_set := lbxSpriteSetList.ItemIndex;
  if (sprite_set = -1) or updating then
    exit;
  entry := Addr(SpriteFile.sprite_entries[sprite_set]);
  entry.iWidth4 := strToIntDef(seSpriteWidth.Text, 0) div 4;
  entry.iHeight := strToIntDef(seSpriteHeight.Text, 0);
  render_all_sprites;
  set_modified(true);
end;

procedure TSpriteDialog.seSpriteSetPropertyChange(Sender: TObject);
var
  sprite_set: integer;
  entry: ^TSpriteEntry;
  new_max_used_sprite_num: integer;
begin
  sprite_set := lbxSpriteSetList.ItemIndex;
  if (sprite_set = -1) or updating then
    exit;
  entry := Addr(SpriteFile.sprite_entries[sprite_set]);
  entry.iStandFrame := strToIntDef(seStandFrameFirst.Text, 0);
  entry.iStandFrame2 := strToIntDef(seStandFrameLast.Text, 0);
  entry.iWalkFrame1 := strToIntDef(seWalkFrameFirst.Text, 0);
  entry.iWalkFrame2 := strToIntDef(seWalkFrameLast.Text, 0);
  entry.iJumpFrame := strToIntDef(seJumpFrame.Text, 0);
  entry.iFallFrame := strToIntDef(seFallFrame.Text, 0);
  entry.iShootDashFrame1 := strToIntDef(seShootDashFrameFirst.Text, 0);
  entry.iShootDashFrame2 := strToIntDef(seShootDashFrameLast.Text, 0);
  entry.iProjectileWidth4 := strToIntDef(seProjectileWidth.Text, 0) div 4;
  entry.iProjectileHeight := strToIntDef(seProjectileHeight.Text, 0);
  entry.iProjectileY := strToIntDef(seProjectileYOff.Text, 0);
  entry.iProjectileFrame := strToIntDef(seProjectileFrameFirst.Text, 0);
  entry.iProjectileF2 := strToIntDef(seProjectileFrameLast.Text, 0);
  new_max_used_sprite_num := SpriteFile.get_max_used_sprite_num(sprite_set);
  if last_max_used_sprite_num <> new_max_used_sprite_num then
    render_all_sprites;
  set_modified(true);
end;

procedure TSpriteDialog.seExpImpSpriteNumChange(Sender: TObject);
begin
  if cbShowNumbers.Checked then
    render_all_sprites;
end;

procedure TSpriteDialog.btnExportSpritesClick(Sender: TObject);
var
  sprite_buffer: TBitmap;
  output_buffer: TBitmap;
  output_filename: String;
  file_base, file_ext: String;
  save_dialog: TSaveDialog;
  PNG: TPNGObject;
  output_canvas: TCanvas;
  i: integer;
  sprite_set, sprite_num: integer;
  entry: ^TSpriteEntry;
  width, height: integer;
  max_used_sprite: integer;
  palette_num: integer;
begin
  sprite_set := lbxSpriteSetList.ItemIndex;
  if (sprite_set = -1) then
    exit;
  entry := Addr(SpriteFile.sprite_entries[sprite_set]);
  width := entry.iWidth4 * 4;
  height := entry.iHeight;
  if rbExportPng.Checked then
  begin
    palette_num := 1;
    save_dialog := ExportDialogPng;
  end else
  begin
    palette_num := 0;
    save_dialog := ExportDialogBmp;
  end;
  if save_dialog.Execute then
    output_filename := save_dialog.FileName
  else
    exit;
  max_used_sprite := SpriteFile.get_max_used_sprite_num(sprite_set);
  output_buffer := TBitmap.Create;
  output_buffer.PixelFormat := pf8bit;
  output_buffer.Width := IfThen(rbExpImpAllOneFile.Checked, width * (max_used_sprite + 1), width);
  output_buffer.Height := IfThen(rbExpImpAllOneFile.Checked, height * 2, height);
  output_buffer.Palette := SpriteFile.get_palette_handle(palette_num);
  output_canvas := output_buffer.Canvas;
  // Export single sprite
  if rbExpImpSingle.Checked then
  begin
    sprite_num := seExpImpSpriteNum.Value;
    sprite_buffer := SpriteFile.load_sprite(sprite_set, sprite_num, palette_num);
    output_canvas.CopyRect(Rect(0,0,width,height), sprite_buffer.Canvas, Rect(0,0,width,height));
  end;
  // Export all sprites into multiple files
  if rbExpImpAllMultFiles.Checked then
  begin
    file_base := copy(output_filename, 0, strlen(PChar(output_filename)) - 4);
    file_ext := ExtractFileExt(output_filename);
    for i := 0 to max_used_sprite + 20 do
    begin
      if (i > max_used_sprite) and (i < 20) then
        continue;
      sprite_buffer := SpriteFile.load_sprite(sprite_set, i, palette_num);
      output_canvas.CopyRect(Rect(0,0,width,height), sprite_buffer.Canvas, Rect(0,0,width,height));
      output_filename := Format('%s_%.2d%s',[file_base, i, file_ext]);
      if rbExportPng.Checked then
      begin
        PNG := TPNGObject.Create;
        PNG.Assign(output_buffer);
        PNG.TransparentColor := $010101;
        PNG.SaveToFile(output_filename);
      end else
        output_buffer.SaveToFile(output_filename);
    end;
    exit;
  end;
  // Export all sprites into one file
  if rbExpImpAllOneFile.Checked then
  begin
    for i := 0 to max_used_sprite do
    begin
      sprite_buffer := SpriteFile.load_sprite(sprite_set, i, palette_num);
      output_canvas.CopyRect(Rect(i*width, 0,i*width + width, height), sprite_buffer.Canvas, Rect(0,0,width,height));
      sprite_buffer := SpriteFile.load_sprite(sprite_set, i+20, palette_num);
      output_canvas.CopyRect(Rect(i*width, height,i*width + width, height*2), sprite_buffer.Canvas, Rect(0,0,width,height));
    end;
  end;
  if rbExportPng.Checked then
  begin
    PNG := TPNGObject.Create;
    PNG.Assign(output_buffer);
    PNG.TransparentColor := $010101;
    PNG.SaveToFile(output_filename);
  end else
    output_buffer.SaveToFile(output_filename);
end;

procedure TSpriteDialog.btnImportSpritesClick(Sender: TObject);
var
  input_filename: String;
  sprite_set, sprite_num: integer;
  input_buffer: TBitmap;
  single_buffer: TBitmap;
  len: integer;
  width, height: integer;
  i: integer;
begin
  sprite_set := lbxSpriteSetList.ItemIndex;
  if (sprite_set = -1) then
    exit;
  if ImportDialog.Execute then
    input_filename := ImportDialog.FileName
  else
    exit;
  sprite_num := seExpImpSpriteNum.Value;
  input_buffer := TBitmap.Create;
  // Import single sprite
  if rbExpImpSingle.Checked then
  begin
    load_input_image(input_filename, input_buffer);
    SpriteFile.import_sprite(sprite_set, sprite_num, input_buffer, seImportTranspIndex.Value, cbImportUseUpperPal.Checked);
  end;
  // Import all sprites from multiple files
  if rbExpImpAllMultFiles.Checked then
  begin
    len := Length(input_filename);
    if not ((input_filename[len - 6] = '_') and (input_filename[len - 5] in ['0'..'3']) and (input_filename[len - 4] in ['0'..'9'])) then
    begin
      Application.MessageBox('The input file name must follow the pattern "name_XX" where XX is a two-digit sprite number.', 'Unusable filename', MB_OK or MB_ICONERROR);
      exit;
    end;
    for i := 0 to 39 do
    begin
      input_filename[len - 5] := chr(ord('0') + i div 10);
      input_filename[len - 4] := chr(ord('0') + i mod 10);
      if not FileExists(input_filename) then
        continue;
      load_input_image(input_filename, input_buffer);
      SpriteFile.import_sprite(sprite_set, i, input_buffer, seImportTranspIndex.Value, cbImportUseUpperPal.Checked);
    end;
  end;
  // Import all sprites from one file
  if rbExpImpAllOneFile.Checked then
  begin
    load_input_image(input_filename, input_buffer);
    width := seSpriteWidth.Value;
    height := seSpriteHeight.Value;
    single_buffer := TBitmap.Create;
    single_buffer.PixelFormat := input_buffer.PixelFormat;
    single_buffer.Palette := input_buffer.Palette;
    single_buffer.TransparentColor := input_buffer.TransparentColor;
    single_buffer.Width := width;
    single_buffer.Height := height;
    len := Min(input_buffer.Width div width, 20);
    for i := 0 to len - 1 do
    begin
      single_buffer.Canvas.CopyRect(Rect(0,0,width,height), input_buffer.Canvas, Rect(i*width, 0,i*width + width, height));
      SpriteFile.import_sprite(sprite_set, i, single_buffer, seImportTranspIndex.Value, cbImportUseUpperPal.Checked);
      single_buffer.Canvas.CopyRect(Rect(0,0,width,height), input_buffer.Canvas, Rect(i*width, height,i*width + width, height*2));
      SpriteFile.import_sprite(sprite_set, i+20, single_buffer, seImportTranspIndex.Value, cbImportUseUpperPal.Checked);
    end;
  end;
  set_modified(true);
  render_all_sprites;
end;

procedure TSpriteDialog.btnEraseSpriteClick(Sender: TObject);
var
  sprite_set: integer;
begin
  sprite_set := lbxSpriteSetList.ItemIndex;
  if (sprite_set = -1) then
    exit;
  SpriteFile.erase_sprite(sprite_set, seExpImpSpriteNum.Value);
  set_modified(true);
  render_all_sprites;
end;


procedure TSpriteDialog.init_sprite_list;
var
  i: integer;
  old_index: integer;
begin
  old_index := lbxSpriteSetList.ItemIndex;
  lbxSpriteSetList.Items.Clear;
  for i := 0 to SpriteFile.num_sprite_entries - 1 do
  begin
    lbxSpriteSetList.Items.Add(inttostr(i) + ' - ' + SpriteFile.sprite_entries[i].cSpriteName);
  end;
  if old_index <> -1 then
  begin
    lbxSpriteSetList.ItemIndex := old_index;
    lbxSpriteSetListClick(nil);
  end;
end;

procedure TSpriteDialog.render_all_sprites;
var
  i: integer;
  sprite_set: integer;
  entry: ^TSpriteEntry;
  width, height: integer;
  max_used_sprite: integer;
  sprite_buffer: TBitmap;
begin
  sprite_set := lbxSpriteSetList.ItemIndex;
  if sprite_set = -1 then
    exit;
  entry := Addr(SpriteFile.sprite_entries[sprite_set]);
  width := entry.iWidth4 * 4;
  height := entry.iHeight;
  max_used_sprite := SpriteFile.get_max_used_sprite_num(sprite_set);
  last_max_used_sprite_num := max_used_sprite;
  SpriteImage.Canvas.Pen.Style := psClear;
  SpriteImage.Canvas.Brush.Color := ColorDialog.Color;
  SpriteImage.Canvas.Rectangle(0,0,SpriteImage.Width+1,SpriteImage.Height+1);
  SpriteImage.Canvas.Brush.Color := clWhite;
  for i := 0 to max_used_sprite do
  begin
    sprite_buffer := SpriteFile.load_sprite(sprite_set, i, 0);
    SpriteImage.Canvas.CopyRect(Rect(i*width*2, 0,i*width*2 + width*2, height*2), sprite_buffer.Canvas, Rect(0,0,width,height));
    if cbShowNumbers.Checked then
    begin
      if (seExpImpSpriteNum.Value = i) then
        SpriteImage.Canvas.Brush.Color := clRed
      else
        SpriteImage.Canvas.Brush.Color := clWhite;
      SpriteImage.Canvas.Rectangle(i*width*2, 0, i*width*2 + 15, 14);
      SpriteImage.Canvas.TextOut(i*width*2, 0, inttostr(i));
    end;
    sprite_buffer := SpriteFile.load_sprite(sprite_set, i+20, 0);
    SpriteImage.Canvas.CopyRect(Rect(i*width*2, height*2,i*width*2 + width*2, height*4), sprite_buffer.Canvas, Rect(0,0,width,height));
    if cbShowNumbers.Checked then
    begin
      if (seExpImpSpriteNum.Value = (i + 20)) then
        SpriteImage.Canvas.Brush.Color := clRed
      else
        SpriteImage.Canvas.Brush.Color := clWhite;
      SpriteImage.Canvas.Rectangle(i*width*2, height*2, i*width*2 + 15, height*2 + 14);
      SpriteImage.Canvas.TextOut(i*width*2, height*2, inttostr(i+20));
    end;
  end;
  lbLayoutSize.Caption := inttostr(entry.iLayoutSize) + ' bytes';
  lbPixelSize.Caption := inttostr(entry.iPixelsSize) + ' bytes';
end;

procedure TSpriteDialog.load_input_image(input_filename: String; input_buffer: TBitmap);
var
  PNG: TPNGObject;
begin
  if AnsiCompareText(ExtractFileExt(input_filename), '.PNG') = 0 then
  begin
    PNG := TPngObject.Create;
    PNG.LoadFromFile(input_filename);
    if PNG.Header.ColorType = COLOR_PALETTE then
      PNG.RemoveTransparency;
    input_buffer.Assign(PNG);
    PNG.Destroy;
  end else
  begin
    input_buffer.LoadFromFile(input_filename);
  end;
end;

procedure TSpriteDialog.set_modified(val: boolean);
begin
  modified := val;
  btnSaveChanges.Enabled := val;
  btnUndoChanges.Enabled := val;
end;

end.
