unit sprite_dialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, _spritefile, _archive, ExtCtrls, Spin, Math, Buttons;

type
  TSpriteDialog = class(TForm)
    lbxSpriteSetList: TListBox;
    SpriteImage: TImage;
    lbSpriteSetNum: TLabel;
    lbSpriteWidth: TLabel;
    lbSpriteHeight: TLabel;
    lbProjectileWidth: TLabel;
    lbProjectileHeight: TLabel;
    lbProjectileYOff: TLabel;
    seSpriteWidth: TSpinEdit;
    seSpriteHeight: TSpinEdit;
    seProjectileWidth: TSpinEdit;
    seProjectileHeight: TSpinEdit;
    seProjectileYOff: TSpinEdit;
    lbStandFrames: TLabel;
    lbWalkFrames: TLabel;
    lbShootDashFrames: TLabel;
    seStandFrameFirst: TSpinEdit;
    seWalkFrameFirst: TSpinEdit;
    seShootDashFrameFirst: TSpinEdit;
    seStandFrameLast: TSpinEdit;
    seWalkFrameLast: TSpinEdit;
    seShootDashFrameLast: TSpinEdit;
    lbProjectileFrames: TLabel;
    lbJumpFallFrame: TLabel;
    seProjectileFrameFirst: TSpinEdit;
    seProjectileFrameLast: TSpinEdit;
    lbTo1: TLabel;
    lbTo2: TLabel;
    lbTo3: TLabel;
    lbTo4: TLabel;
    seJumpFrame: TSpinEdit;
    seFallFrame: TSpinEdit;
    edSpriteName: TEdit;
    ColorDialog: TColorDialog;
    btnBackgroundColor: TButton;
    lbSpriteNameCustom: TLabel;
    btnSaveChanges: TBitBtn;
    cbShowNumbers: TCheckBox;
    btnUndoChanges: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lbxSpriteSetListClick(Sender: TObject);
    procedure cbShowNumbersClick(Sender: TObject);
    procedure btnBackgroundColorClick(Sender: TObject);
    procedure btnSaveChangesClick(Sender: TObject);
    procedure btnUndoChangesClick(Sender: TObject);
    procedure seSpriteSizeChange(Sender: TObject);
    procedure seSpriteSetPropertyChange(Sender: TObject);
  private
    sprite_buffer: TBitmap;
    updating: boolean;
    modified: boolean;

    procedure render_all_sprites;
    procedure set_modified(val: boolean);
  public
    { Public declarations }
  end;

var
  SpriteDialog: TSpriteDialog;

implementation

{$R *.dfm}

procedure TSpriteDialog.FormCreate(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to SpriteFile.num_sprite_entries - 1 do
  begin
    lbxSpriteSetList.Items.Add(inttostr(i) + ' - ' + SpriteFile.sprite_entries[i].cSpriteName);
  end;
  sprite_buffer := TBitmap.Create;
  sprite_buffer.Width := 320;
  sprite_buffer.Height := 200;
  sprite_buffer.PixelFormat := pf32bit;
  set_modified(false);
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

procedure TSpriteDialog.cbShowNumbersClick(Sender: TObject);
begin
  render_all_sprites;
end;

procedure TSpriteDialog.btnBackgroundColorClick(Sender: TObject);
begin
  if ColorDialog.Execute then
    render_all_sprites;
end;

procedure TSpriteDialog.btnSaveChangesClick(Sender: TObject);
begin
  SpriteFile.save_to_archive;
  set_modified(false);
end;

procedure TSpriteDialog.btnUndoChangesClick(Sender: TObject);
begin
  SpriteFile.load_from_archive;
  lbxSpriteSetListClick(nil);
  set_modified(false);
end;

procedure TSpriteDialog.seSpriteSizeChange(Sender: TObject);
var
  sprite_set: integer;
  entry: ^TSpriteEntry;
begin
  if updating then
    exit;
  sprite_set := lbxSpriteSetList.ItemIndex;
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
begin
  if updating then
    exit;
  sprite_set := lbxSpriteSetList.ItemIndex;
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
  set_modified(true);
end;


procedure TSpriteDialog.render_all_sprites;
var
  i: integer;
  sprite_set: integer;
  entry: ^TSpriteEntry;
  width, height: integer;
  max_used_sprite: integer;
begin
  sprite_set := lbxSpriteSetList.ItemIndex;
  entry := Addr(SpriteFile.sprite_entries[sprite_set]);
  width := entry.iWidth4 * 4;
  height := entry.iHeight;
  max_used_sprite := SpriteFile.get_max_used_sprite(sprite_set);
  SpriteImage.Canvas.Pen.Style := psClear;
  SpriteImage.Canvas.Brush.Color := ColorDialog.Color;
  SpriteImage.Canvas.Rectangle(0,0,SpriteImage.Width+1,SpriteImage.Height+1);
  SpriteImage.Canvas.Brush.Color := clWhite;
  for i := 0 to max_used_sprite do
  begin
    SpriteFile.load_sprite(sprite_buffer, ColorDialog.Color, sprite_set, i);
    SpriteImage.Canvas.CopyRect(Rect(i*width*2, 0,i*width*2 + width*2, height*2), sprite_buffer.Canvas, Rect(0,0,width,height));
    if cbShowNumbers.Checked then
    begin
      SpriteImage.Canvas.Rectangle(i*width*2, 0, i*width*2 + 15, 14);
      SpriteImage.Canvas.TextOut(i*width*2, 0, inttostr(i));
    end;
    SpriteFile.load_sprite(sprite_buffer, ColorDialog.Color, sprite_set, i+20);
    SpriteImage.Canvas.CopyRect(Rect(i*width*2, height*2,i*width*2 + width*2, height*4), sprite_buffer.Canvas, Rect(0,0,width,height));
    if cbShowNumbers.Checked then
    begin
      SpriteImage.Canvas.Rectangle(i*width*2, height*2, i*width*2 + 15, height*2 + 14);
      SpriteImage.Canvas.TextOut(i*width*2, height*2, inttostr(i+20));
    end;
  end;
end;

procedure TSpriteDialog.set_modified(val: boolean);
begin
  modified := val;
  btnSaveChanges.Enabled := val;
  btnUndoChanges.Enabled := val;
end;

end.
