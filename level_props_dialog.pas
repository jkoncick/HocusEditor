unit level_props_dialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, ExtCtrls, Math;

type
  TLevelPropertiesDialog = class(TForm)
    gbLevelSettings: TGroupBox;
    gbAnimationSettings: TGroupBox;
    gbMonsterSettings: TGroupBox;
    lstAnimationList: TListBox;
    lstMonsterList: TListBox;
    rbAnimTypeNone: TRadioButton;
    rbAnimTypePermanent: TRadioButton;
    rbAnimTypeRandom: TRadioButton;
    lbAnimType: TLabel;
    imgAnimAnimatedTiles: TImage;
    lbAnimTiles: TLabel;
    lbAnimBlankTile: TLabel;
    imgAnimBlankTile: TImage;
    imgAnimSwitchDownTile: TImage;
    imgAnimSwitchUpTile: TImage;
    imgAnimShootableTile: TImage;
    lbAnimSwitchDownTile: TLabel;
    lbAnimSwitchUpTile: TLabel;
    lbAnimShootableTile: TLabel;
    lbAnimBlankTileIndex: TLabel;
    lbAnimSwitchDownTileIndex: TLabel;
    lbAnimSwitchUpTileIndex: TLabel;
    lbAnimShootableTileIndex: TLabel;
    lbMonsterSpriteSet: TLabel;
    cbxMonsterSpriteSet: TComboBox;
    lbMonsterHealth: TLabel;
    lbMonsterProjectileHSpeed: TLabel;
    lbMonsterProjectileVSpeed: TLabel;
    lbMonsterProjectileOffset: TLabel;
    seMonsterHealth: TSpinEdit;
    seMonsterProjectileHSpeed: TSpinEdit;
    seMonsterProjectileVSpeed: TSpinEdit;
    lbMonsterHealthInfo1: TLabel;
    cbMonsterTargetPlayer: TCheckBox;
    cbMonsterShootProjectiles: TCheckBox;
    cbMonsterWobblyProjectiles: TCheckBox;
    lbMonsterBehavior: TLabel;
    cbxMonsterBehavior: TComboBox;
    Bevel1: TBevel;
    lbLevelMonsterShootDelay: TLabel;
    seLevelMonsterShootDelay: TSpinEdit;
    seMonsterProjectileOffset: TSpinEdit;
    cbMonsterUnknown2: TCheckBox;
    cbMonsterUnknown3: TCheckBox;
    btnClearMonster: TButton;
    lbAnimFirstIndex: TLabel;
    lbAnimLastIndex: TLabel;
    seAnimFirstIndex: TSpinEdit;
    seAnimLastIndex: TSpinEdit;
    pnTilesetImage: TPanel;
    imgTilesetImage: TImage;
    imgBackdropImage: TImage;
    lbLevelParTime: TLabel;
    seLevelParTime: TSpinEdit;
    lbLevelBackdrop: TLabel;
    cbxLevelBackdrop: TComboBox;
    lbLevelMusic: TLabel;
    cbxLevelMusic: TComboBox;
    lbLevelElevatorTiles: TLabel;
    imgLevelElevatorLeft: TImage;
    lbLevelElevatorLeft: TLabel;
    imgLevelElevatorRight: TImage;
    lbLevelElevatorRight: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lstAnimationListClick(Sender: TObject);
    procedure lstMonsterListClick(Sender: TObject);
    procedure LevelPropertyChange(Sender: TObject);
    procedure MonsterPropertyChange(Sender: TObject);
    procedure btnClearMonsterClick(Sender: TObject);
    procedure seAnimFirstIndexChange(Sender: TObject);
    procedure seAnimLastIndexChange(Sender: TObject);
    procedure rbAnimTypeClick(Sender: TObject);
    procedure imgTileClick(Sender: TObject);
    procedure imgLevelElevatorMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure imgTilesetImageMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    updating: boolean;
    editing_tile: integer;
    last_backdrop_image: integer;
  public
    procedure update_contents;
    function get_animation_entry_title(index: integer): string;
    procedure update_level_properties;
    procedure update_backdrop_image(force: boolean);
    procedure update_animation_defined_tiles;
    procedure update_animation_entry;
    procedure update_monster_type;
  end;

const anim_type_mark: array[0..3] of char = ('-', 'A', 'R', '*');

var
  LevelPropertiesDialog: TLevelPropertiesDialog;

implementation

uses _archive, _map, _tileset;

{$R *.dfm}

{ TLevelPropertiesDialog }

procedure TLevelPropertiesDialog.FormCreate(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to Length(Archive.monster_type_names) - 1 do
    cbxMonsterSpriteSet.Items.Add(inttostr(i) + ' - ' + Archive.get_monster_type_name(i));
  cbxMonsterSpriteSet.ItemIndex := 0;
  for i := 0 to Length(Archive.tileset_names) - 1 do
    cbxLevelBackdrop.Items.Add(inttostr(i) + ' - ' + Archive.tileset_names[i]);
  cbxLevelBackdrop.ItemIndex := 0;
  for i := 0 to Length(Archive.music_names) - 1 do
    cbxLevelMusic.Items.Add(inttostr(i) + ' - ' + Archive.music_names[i]);
  cbxLevelMusic.ItemIndex := 0;
  imgAnimAnimatedTiles.Canvas.Pixels[0,0] := 0;
  imgAnimAnimatedTiles.Width := 0;
  last_backdrop_image := -1;
end;

procedure TLevelPropertiesDialog.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if key = 27 then
  begin
    if pnTilesetImage.Visible then
      pnTilesetImage.Visible := false
    else
      close;
  end;
end;

procedure TLevelPropertiesDialog.lstAnimationListClick(Sender: TObject);
begin
  update_animation_entry;
end;

procedure TLevelPropertiesDialog.lstMonsterListClick(Sender: TObject);
begin
  update_monster_type;
end;

procedure TLevelPropertiesDialog.LevelPropertyChange(Sender: TObject);
var
  p: ^TLvlPlayerInfo;
  e: ^TLevelExeData;
begin
  if updating then
    exit;
  p := Addr(Map.leveldata.player_info);
  e := Addr(Map.levelexedata);
  p.MonsterShootDelay := strtointdef(seLevelMonsterShootDelay.Text, 0);
  e.par_time := strtointdef(seLevelParTime.Text, 0);
  e.backdrop_number := cbxLevelBackdrop.ItemIndex;
  e.music_number := cbxLevelMusic.ItemIndex;
  update_backdrop_image(false);
end;

procedure TLevelPropertiesDialog.MonsterPropertyChange(Sender: TObject);
var
  m: ^TLvlMonsterInfo;
  ss: integer;
begin
  if updating then exit;
  m := Addr(Map.leveldata.monster_info[lstMonsterList.ItemIndex]);
  ss := cbxMonsterSpriteSet.ItemIndex;
  if m.SpriteSet <> ss then
    lstMonsterList.Items[lstMonsterList.ItemIndex] := inttostr(lstMonsterList.ItemIndex) + ' - ' + Archive.get_monster_type_name(ss);
  m.SpriteSet := ss;
  m.Health := strtointdef(seMonsterHealth.Text, 0);
  m.ProjectileHSpeed := strtointdef(seMonsterProjectileHSpeed.Text, 0);
  m.ProjectileVSpeed := strtointdef(seMonsterProjectileVSpeed.Text, 0);
  m.ProjectileOffset := strtointdef(seMonsterProjectileOffset.Text, 0);
  m.TargetPlayer := IfThen(cbMonsterTargetPlayer.Checked, 1, 0);
  m.ShootProjectiles := IfThen(cbMonsterShootProjectiles.Checked, 1, 0);
  m.WobblyProjectiles := IfThen(cbMonsterWobblyProjectiles.Checked, 1, 0);
  m.Unknown2 := IfThen(cbMonsterUnknown2.Checked, 1, 0);
  m.Unknown3 := IfThen(cbMonsterUnknown3.Checked, 1, 0);
  m.Behavior := IfThen(cbxMonsterBehavior.ItemIndex < 11, cbxMonsterBehavior.ItemIndex, 99);
  Map.leveldata_dirtyflag := Map.leveldata_dirtyflag + [ufMonsterTypes];
end;

procedure TLevelPropertiesDialog.btnClearMonsterClick(Sender: TObject);
var
  m: ^TLvlMonsterInfo;
begin
  m := Addr(Map.leveldata.monster_info[lstMonsterList.ItemIndex]);
  FillChar(m^, sizeof(m^), 0);
  m.SpriteSet := 65535;
  lstMonsterList.Items[lstMonsterList.ItemIndex] := inttostr(lstMonsterList.ItemIndex) + ' - (Unused)';
  Map.leveldata_dirtyflag := Map.leveldata_dirtyflag + [ufMonsterTypes];
  update_monster_type;
end;

procedure TLevelPropertiesDialog.seAnimFirstIndexChange(Sender: TObject);
var
  e: ^TLvlAnimationEntry;
  val, diff: integer;
begin
  if updating then
    exit;
  if seAnimFirstIndex.Text = '' then
    exit;
  e := Addr(Map.leveldata.animation_info.AnimData[lstAnimationList.ItemIndex]);
  val := strtointdef(seAnimFirstIndex.Text, 0);
  diff := val - e.FirstIndex;
  Inc(e.FirstIndex, diff);
  Inc(e.LastIndex, diff);
  update_animation_entry;
end;

procedure TLevelPropertiesDialog.seAnimLastIndexChange(Sender: TObject);
var
  e: ^TLvlAnimationEntry;
  val: byte;
begin
  if updating then
    exit;
  if seAnimLastIndex.Text = '' then
    exit;
  e := Addr(Map.leveldata.animation_info.AnimData[lstAnimationList.ItemIndex]);
  val := strtointdef(seAnimLastIndex.Text, 0);
  if val < e.FirstIndex then
    e.LastIndex := e.FirstIndex
  else
    e.LastIndex := val;
  lstAnimationList.Items[lstAnimationList.ItemIndex] := get_animation_entry_title(lstAnimationList.ItemIndex);
  update_animation_entry;
end;

procedure TLevelPropertiesDialog.rbAnimTypeClick(Sender: TObject);
var
  e: ^TLvlAnimationEntry;
begin
  if updating then
    exit;
  e := Addr(Map.leveldata.animation_info.AnimData[lstAnimationList.ItemIndex]);
  e.AnimType := (Sender as TRadioButton).Tag;
  lstAnimationList.Items[lstAnimationList.ItemIndex] := get_animation_entry_title(lstAnimationList.ItemIndex);
end;

procedure TLevelPropertiesDialog.imgTileClick(Sender: TObject);
begin
  editing_tile := (Sender as TImage).Tag;
  pnTilesetImage.Visible := true;
end;

procedure TLevelPropertiesDialog.imgLevelElevatorMouseDown(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  e: ^TLevelExeData;
begin
  if Button <> mbRight then
    exit;
  e := Addr(Map.levelexedata);
  if (Sender as TImage).Tag = 4 then
    e.elevator_tile_left := -1
  else
    e.elevator_tile_right := -1;
  update_level_properties;
end;

procedure TLevelPropertiesDialog.update_contents;
var
  i: integer;
  tmp_strings: TStringList;
  sprite_set: word;
begin
  if not Map.loaded then
    exit;
  // Update level properties
  update_level_properties;
  // Update animation properties (header)
  update_animation_defined_tiles;
  // Update animation properties (entry list)
  tmp_strings := TStringList.Create;
  lstAnimationList.Items.Clear;
  for i := 0 to Length(Map.leveldata.animation_info.AnimData) - 1 do
  begin
    tmp_strings.Add(get_animation_entry_title(i));
  end;
  lstAnimationList.Items := tmp_strings;
  lstAnimationList.ItemIndex := 0;
  update_animation_entry;
  tmp_strings.Clear;
  // Update monster properties (monster list)
  lstMonsterList.Items.Clear;
  for i := 0 to Length(Map.leveldata.monster_info) - 1 do
  begin
    sprite_set := Map.leveldata.monster_info[i].SpriteSet;
    if sprite_set = 65535 then
      tmp_strings.Add(inttostr(i) + ' - (Unused)')
    else
      tmp_strings.Add(inttostr(i) + ' - ' + Archive.get_monster_type_name(sprite_set));
  end;
  lstMonsterList.Items := tmp_strings;
  lstMonsterList.ItemIndex := 0;
  update_monster_type;
  tmp_strings.Destroy;
  imgTilesetImage.Canvas.CopyRect(Rect(0,0,320,200), Tileset.tileimage.Canvas, Rect(0,0,320,200));
end;

function TLevelPropertiesDialog.get_animation_entry_title(index: integer): string;
var
  num_anims: integer;
begin
  num_anims := Map.leveldata.animation_info.AnimData[index].LastIndex - Map.leveldata.animation_info.AnimData[index].FirstIndex + 1;
  if num_anims = 1 then
    result := 'Entry ' + inttostr(index)
  else
    result := 'Entry ' + inttostr(index) + ' (' + inttostr(num_anims) + ' ' + anim_type_mark[Min(Map.leveldata.animation_info.AnimData[index].AnimType,3)] + ')';
end;

procedure TLevelPropertiesDialog.update_level_properties;
var
  tile: smallint;
  tile_x, tile_y: integer;
begin
  updating := true;
  seLevelMonsterShootDelay.Value := Map.leveldata.player_info.MonsterShootDelay;
  seLevelParTime.Value := Map.levelexedata.par_time;
  cbxLevelBackdrop.ItemIndex := Map.levelexedata.backdrop_number;
  cbxLevelMusic.ItemIndex := Map.levelexedata.music_number;
  tile := Map.levelexedata.elevator_tile_left;
  lbLevelElevatorLeft.Caption := '(' + inttostr(tile) + ')';
  if tile = -1 then
  begin
    imgLevelElevatorLeft.Canvas.Brush.Color := clWhite;
    imgLevelElevatorLeft.Canvas.Pen.Color := clWhite;
    imgLevelElevatorLeft.Canvas.Rectangle(0,0,32,32);
  end else
  begin
    tile_x := tile mod tileset_cols;
    tile_y := tile div tileset_cols;
    imgLevelElevatorLeft.Canvas.CopyRect(Rect(0, 0, 32, 32), Tileset.tileimage.Canvas, Rect(tile_x*16, tile_y*16, tile_x*16+16, tile_y*16+16));
  end;
  tile := Map.levelexedata.elevator_tile_right;
  lbLevelElevatorRight.Caption := '(' + inttostr(tile) + ')';
  if tile = -1 then
  begin
    imgLevelElevatorRight.Canvas.Brush.Color := clWhite;
    imgLevelElevatorRight.Canvas.Pen.Color := clWhite;
    imgLevelElevatorRight.Canvas.Rectangle(0,0,32,32);
  end else
  begin
    tile_x := tile mod tileset_cols;
    tile_y := tile div tileset_cols;
    imgLevelElevatorRight.Canvas.CopyRect(Rect(0, 0, 32, 32), Tileset.tileimage.Canvas, Rect(tile_x*16, tile_y*16, tile_x*16+16, tile_y*16+16));
  end;
  updating := false;
  update_backdrop_image(false);
end;

procedure TLevelPropertiesDialog.update_backdrop_image(force: boolean);
var
  index: integer;
begin
  index := Map.levelexedata.backdrop_number;
  if (index = last_backdrop_image) and not force then
    exit;
  last_backdrop_image := index;
  Archive.load_palette(Archive.first_backdrop_palette_file_index + index, 1);
  Archive.load_pcx_image(imgBackdropImage.Picture.Bitmap, Archive.first_backdrop_file_index + index);
end;

procedure TLevelPropertiesDialog.update_animation_defined_tiles;
var
  tile: byte;
  tile_x, tile_y: integer;
begin
  tile := Map.leveldata.animation_info.BackgroundTile;
  tile_x := tile mod tileset_cols;
  tile_y := tile div tileset_cols;
  lbAnimBlankTileIndex.Caption := '(' + inttostr(tile) + ')';
  imgAnimBlankTile.Canvas.CopyRect(Rect(0, 0, 32, 32), Tileset.tileimage.Canvas, Rect(tile_x*16, tile_y*16, tile_x*16+16, tile_y*16+16));
  tile := Map.leveldata.animation_info.SwitchDownTile;
  tile_x := tile mod tileset_cols;
  tile_y := tile div tileset_cols;
  lbAnimSwitchDownTileIndex.Caption := '(' + inttostr(tile) + ')';
  imgAnimSwitchDownTile.Canvas.CopyRect(Rect(0, 0, 32, 32), Tileset.tileimage.Canvas, Rect(tile_x*16, tile_y*16, tile_x*16+16, tile_y*16+16));
  tile := Map.leveldata.animation_info.SwitchUpTile;
  tile_x := tile mod tileset_cols;
  tile_y := tile div tileset_cols;
  lbAnimSwitchUpTileIndex.Caption := '(' + inttostr(tile) + ')';
  imgAnimSwitchUpTile.Canvas.CopyRect(Rect(0, 0, 32, 32), Tileset.tileimage.Canvas, Rect(tile_x*16, tile_y*16, tile_x*16+16, tile_y*16+16));
  tile := Map.leveldata.animation_info.ShootableTile;
  tile_x := tile mod tileset_cols;
  tile_y := tile div tileset_cols;
  lbAnimShootableTileIndex.Caption := '(' + inttostr(tile) + ')';
  imgAnimShootableTile.Canvas.CopyRect(Rect(0, 0, 32, 32), Tileset.tileimage.Canvas, Rect(tile_x*16, tile_y*16, tile_x*16+16, tile_y*16+16));
end;

procedure TLevelPropertiesDialog.update_animation_entry;
var
  anim_entry: ^TLvlAnimationEntry;
  num_anims: integer;
  i: integer;
  tile_x, tile_y: integer;
begin
  updating := true;
  anim_entry := Addr(Map.leveldata.animation_info.AnimData[lstAnimationList.ItemIndex]);
  seAnimFirstIndex.Value := anim_entry.FirstIndex;
  seAnimLastIndex.Value := anim_entry.LastIndex;
  case anim_entry.AnimType of
    0: rbAnimTypeNone.Checked := true;
    1: rbAnimTypePermanent.Checked := true;
    2: rbAnimTypeRandom.Checked := true;
  end;
  num_anims := Min(anim_entry.LastIndex - anim_entry.FirstIndex + 1, 8);
  imgAnimAnimatedTiles.Width := num_anims * 32;
  for i := 0 to num_anims - 1 do
  begin
    tile_x := (anim_entry.FirstIndex + i) mod tileset_cols;
    tile_y := (anim_entry.FirstIndex + i) div tileset_cols;
    imgAnimAnimatedTiles.Canvas.CopyRect(Rect(i*32, 0, i*32+32, 32), Tileset.tileimage.Canvas, Rect(tile_x*16, tile_y*16, tile_x*16+16, tile_y*16+16));
  end;
  updating := false;
end;

procedure TLevelPropertiesDialog.update_monster_type;
var
  monster_info: ^TLvlMonsterInfo;
begin
  updating := true;
  monster_info := Addr(Map.leveldata.monster_info[lstMonsterList.ItemIndex]);
  if monster_info.SpriteSet = 65535 then
    cbxMonsterSpriteSet.ItemIndex := -1
  else
    cbxMonsterSpriteSet.ItemIndex := monster_info.SpriteSet;
  seMonsterHealth.Value := monster_info.Health;
  seMonsterProjectileHSpeed.Value := monster_info.ProjectileHSpeed;
  seMonsterProjectileVSpeed.Value := monster_info.ProjectileVSpeed;
  seMonsterProjectileOffset.Value := monster_info.ProjectileOffset;
  cbxMonsterBehavior.ItemIndex := IfThen(monster_info.Behavior <> 99, monster_info.Behavior, 11);
  cbMonsterTargetPlayer.Checked := monster_info.TargetPlayer = 1;
  cbMonsterShootProjectiles.Checked := monster_info.ShootProjectiles = 1;
  cbMonsterWobblyProjectiles.Checked := monster_info.WobblyProjectiles = 1;
  cbMonsterUnknown2.Checked := monster_info.Unknown2 = 1;
  cbMonsterUnknown3.Checked := monster_info.Unknown3 = 1;
  updating := false;
end;

procedure TLevelPropertiesDialog.imgTilesetImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  a: ^TLvlAnimationInfo;
  b: ^TLevelExeData;
  tile_index: byte;
begin
  a := Addr(Map.leveldata.animation_info);
  b := Addr(Map.levelexedata);
  tile_index := (X div 16) + (Y div 16) * tileset_cols;
  case editing_tile of
    0: a.BackgroundTile := tile_index;
    1: a.SwitchDownTile := tile_index;
    2: a.SwitchUpTile := tile_index;
    3: a.ShootableTile := tile_index;
    4: b.elevator_tile_left := tile_index;
    5: b.elevator_tile_right := tile_index;
  end;
  pnTilesetImage.Visible := false;
  update_contents;
end;

end.
