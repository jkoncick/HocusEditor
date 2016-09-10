unit sprite_dialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, _spritefile, ExtCtrls, Spin, Math;

type
  TSpriteDialog = class(TForm)
    lbxSpriteSetList: TListBox;
    Image1: TImage;
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
    Label1: TLabel;
    seJumpFrame: TSpinEdit;
    seFallFrame: TSpinEdit;
    edSpriteName: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lbxSpriteSetListClick(Sender: TObject);
  private
    sprite_buffer: TBitmap;

    procedure render_all_sprites;
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
end;

procedure TSpriteDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = 27 then
    Close;
end;

procedure TSpriteDialog.lbxSpriteSetListClick(Sender: TObject);
var
  sprite_set: integer;
  entry: ^TSpriteEntry;
begin
  sprite_set := lbxSpriteSetList.ItemIndex;
  entry := Addr(SpriteFile.sprite_entries[sprite_set]);
  lbSpriteSetNum.Caption := 'Sprite set ' + inttostr(sprite_set);
  edSpriteName.Text := entry.cSpriteName;
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
  render_all_sprites;
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
  max_used_sprite := 0;
  max_used_sprite := max(max_used_sprite, entry.iStandFrame2);
  max_used_sprite := max(max_used_sprite, entry.iWalkFrame2);
  max_used_sprite := max(max_used_sprite, entry.iFallFrame);
  max_used_sprite := max(max_used_sprite, entry.iShootDashFrame2);
  max_used_sprite := max(max_used_sprite, entry.iProjectileF2);
  Image1.Canvas.Pen.Style := psClear;
  Image1.Canvas.Brush.Color := $FFE0A0;
  Image1.Canvas.Rectangle(0,0,Image1.Width+1,Image1.Height+1);
  for i := 0 to max_used_sprite do
  begin
    SpriteFile.load_sprite(sprite_buffer, $FFE0A0, sprite_set, i);
    Image1.Canvas.CopyRect(Rect(i*width*2, 0,i*width*2 + width*2, height*2), sprite_buffer.Canvas, Rect(0,0,width,height));
    SpriteFile.load_sprite(sprite_buffer, $FFE0A0, sprite_set, i+20);
    Image1.Canvas.CopyRect(Rect(i*width*2, height*2,i*width*2 + width*2, height*4), sprite_buffer.Canvas, Rect(0,0,width,height));
  end;
end;

end.
