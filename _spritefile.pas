unit _spritefile;

interface

uses Windows, SysUtils, Graphics, Classes;

type
  TSpriteEntry = record
    iOffset: Cardinal;
    cSpriteName: array[0..21] of char;
    iWidth4: smallint;
    iHeight: smallint;
    iStandFrame: smallint;
    iStandFrame2: smallint;
    iWalkFrame1: smallint;
    iWalkFrame2: smallint;
    iJumpFrame: smallint;
    iFallFrame: smallint;
    iShootDashFrame1: smallint;
    iShootDashFrame2: smallint;
    iProjectileWidth4: smallint;
    iProjectileHeight: smallint;
    iProjectileY: smallint;
    iProjectileFrame: smallint;
    iProjectileF2: smallint;
    iPixelsOff: word;
    iPixelsSize: word;
    iLayoutStarts: array[0..39] of word;
    iPixelStarts: array[0..39] of word;
  end;

type
  TByteArr = array[0..0] of byte;
  TByteArrPtr = ^TByteArr;

type
  TSpriteFile = class

  private
    file_size: integer;
    file_data: array of byte;

    Pal: PLogPalette;
    transparent_color: array[0..2] of TColor;
    palette_handle: array[0..2] of hPalette;
    sprite_buffer: array [0..2] of TBitmap;
    palette_assigned: array [0..2] of boolean;

    preloaded_sprites: array of TBitmap;
    preloaded_sprites_mask: array of TBitmap;

  public
    sprite_entries: array of TSpriteEntry;
    num_sprite_entries: integer;

    procedure init;
    procedure load_from_archive;
    procedure save_to_archive;
    procedure update_palette;
    procedure update_transparent_color(new_color: TColor);
    function get_palette_handle(pal_index: integer): hPalette;
    function load_sprite(sprite_set, sprite_num, pal_index: integer): TBitmap;
    function get_preloaded_sprite(sprite_set: integer; var mask: TBitmap): TBitmap;
    function get_used_sprite_count(sprite_set: integer): integer;
  end;

var
  SpriteFile: TSpriteFile;

implementation

uses _archive, _exefile, Math;

{ TSpriteFile }

procedure TSpriteFile.init;
var
  i: integer;
begin
  GetMem( Pal, Sizeof( TLogPalette ) + Sizeof( TPaletteEntry ) * 255 );
  Pal.palversion := $300;
  Pal.palnumentries := 256;
  transparent_color[0] := $FFE0A0; // For viewing the sprites in sprite dialog
  transparent_color[1] := $010101; // For exporting sprites into png (must be different than black)
  transparent_color[2] := $000000; // For rendering the level
  for i := 0 to 2 do
  begin
    palette_handle[i] := 0;
    sprite_buffer[i] := TBitmap.Create;
    sprite_buffer[i].Width := 320;
    sprite_buffer[i].Height := 200;
    sprite_buffer[i].PixelFormat := pf8bit;
    palette_assigned[i] := false;
  end;
end;

procedure TSpriteFile.load_from_archive;
var
  sprite_file_entry: ^TFileEntry;
  first_sprite_data_offset: cardinal;
  intptr: ^cardinal;
begin
  sprite_file_entry := Addr(Archive.file_list[Archive.sprite_file_index]);
  file_size := sprite_file_entry.size;
  SetLength(file_data, file_size);
  Archive.load_data(file_data, sprite_file_entry.offset, file_size);
  intptr := Addr(file_data[0]);
  first_sprite_data_offset := intptr^;
  num_sprite_entries := first_sprite_data_offset div sizeof(TSpriteEntry);
  sprite_entries := Addr(file_data[0]);
  SetLength(preloaded_sprites, num_sprite_entries);
  SetLength(preloaded_sprites_mask, num_sprite_entries);
end;

procedure TSpriteFile.save_to_archive;
begin
  Archive.save_file(file_data, Archive.sprite_file_index, file_size);
end;

procedure TSpriteFile.update_palette;
var
  i: integer;
begin
  for i := 0 to 254 do
  begin
    Pal.palPalEntry[i].peRed := (Archive.palette[i] shr 16) and 255;
    Pal.palPalEntry[i].peGreen := (Archive.palette[i] shr 8) and 255;
    Pal.palPalEntry[i].peBlue := (Archive.palette[i] shr 0) and 255;
  end;
  // Invalidate existing palettes
  for i := 0 to 2 do
  begin
    if palette_handle[i] <> 0 then
      DeleteObject(palette_handle[i]);
    palette_handle[i] := 0;
    palette_assigned[i] := false;
  end;
  // Invalidate also preloaded sprites
  for i := 0 to num_sprite_entries - 1 do
  begin
    if preloaded_sprites[i] <> Nil then
    begin
      preloaded_sprites[i].Destroy;
      preloaded_sprites[i] := Nil;
      preloaded_sprites_mask[i].Destroy;
      preloaded_sprites_mask[i] := Nil;
    end;
  end;
end;

procedure TSpriteFile.update_transparent_color(new_color: TColor);
begin
  transparent_color[0] := new_color;
  // Invalidate existing palette
  if palette_handle[0] <> 0 then
    DeleteObject(palette_handle[0]);
  palette_handle[0] := 0;
  palette_assigned[0] := false;
end;

function TSpriteFile.get_palette_handle(pal_index: integer): hPalette;
var
  i: integer;
begin
  if palette_handle[pal_index] = 0 then
  begin
    i := 255;
    Pal.palPalEntry[i].peRed := (transparent_color[pal_index] shr 0) and 255;
    Pal.palPalEntry[i].peGreen := (transparent_color[pal_index] shr 8) and 255;
    Pal.palPalEntry[i].peBlue := (transparent_color[pal_index] shr 16) and 255;
    palette_handle[pal_index] := CreatePalette( Pal^ );
  end;
  result := palette_handle[pal_index];
end;

function TSpriteFile.load_sprite(sprite_set, sprite_num, pal_index: integer): TBitmap;
var
  i, j: integer;
  sprite_entry: ^TSpriteEntry;
  layout_off, pixel_off: Cardinal;
  layout_data: TByteArrPtr;
  pixel_data: TByteArrPtr;
  pixel_buffer: array[0..63999] of byte;
  layout_data_ptr, pixel_data_ptr, image_ptr: integer;
  layout_flag: byte;
  transparency_type: byte;
  moveby: word;
begin
  if not palette_assigned[pal_index] then
    sprite_buffer[pal_index].Palette := get_palette_handle(pal_index);
  palette_assigned[pal_index] := true;
  for i := 0 to Length(pixel_buffer) - 1 do
    pixel_buffer[i] := 255;
  sprite_entry := Addr(sprite_entries[sprite_set]);
  layout_off := sprite_entry.iLayoutStarts[sprite_num];
  pixel_off := sprite_entry.iPixelStarts[sprite_num] * 4;
  if (pixel_off >= sprite_entry.iPixelsSize) or (layout_off >= sprite_entry.iPixelsOff) then
  begin
    SetBitmapBits(sprite_buffer[pal_index].Handle, sizeof(pixel_buffer), Addr(pixel_buffer));
    Result := sprite_buffer[pal_index];
    exit;
  end;
  layout_data := Addr(file_data[sprite_entry.iOffset + layout_off]);
  pixel_data := Addr(file_data[sprite_entry.iOffset + sprite_entry.iPixelsOff + pixel_off]);
  layout_data_ptr := 0;
  pixel_data_ptr := 0;
  image_ptr := 0;
  transparency_type := 0;
  for i := 0 to sprite_entry.iPixelsOff - 1 do
  begin
    layout_flag := layout_data[layout_data_ptr];
    Inc(layout_data_ptr);
    case layout_flag of
      0: begin
          transparency_type := layout_data[layout_data_ptr];
          Inc(layout_data_ptr);
          image_ptr := 0;
        end;
      1: begin
          moveby := layout_data[layout_data_ptr] + layout_data[layout_data_ptr+1] * 256;
          Inc(layout_data_ptr, 2);
          Inc(image_ptr, moveby * 4);
        end;
      2: begin
          for j := 0 to 3 do
          begin
            if ((transparency_type shr j) and 1) = 1 then
              pixel_buffer[image_ptr + j] := pixel_data[pixel_data_ptr + j];
          end;
          Inc(image_ptr, 4);
          Inc(pixel_data_ptr, 4);
        end;
      3: begin
          break;
        end;
    end;
  end;
  SetBitmapBits(sprite_buffer[pal_index].Handle, sizeof(pixel_buffer), Addr(pixel_buffer));
  Result := sprite_buffer[pal_index];
end;

function TSpriteFile.get_preloaded_sprite(sprite_set: integer; var mask: TBitmap): TBitmap;
var
  bmp, bmp_mask, sprite_buf: TBitmap;
  width, height: integer;
  buf: array[0..63999] of byte;
  i, j: integer;
begin
  if sprite_set >= num_sprite_entries then
    sprite_set := 2;
  width := sprite_entries[sprite_set].iWidth4 * 4;
  height := sprite_entries[sprite_set].iHeight;
  if preloaded_sprites[sprite_set] <> Nil then
  begin
    result := preloaded_sprites[sprite_set];
    mask := preloaded_sprites_mask[sprite_set];
    exit;
  end;
  bmp := TBitmap.Create;
  bmp.Width := width;
  bmp.Height := height;
  bmp.PixelFormat := pf32bit;
  bmp_mask := TBitmap.Create;
  bmp_mask.Width := width;
  bmp_mask.Height := height;
  bmp_mask.PixelFormat := pf1bit;
  sprite_buf := load_sprite(sprite_set, 0, 2);
  bmp.Canvas.CopyRect(Rect(0,0,width,height), sprite_buf.Canvas, Rect(0,0,width,height));
  preloaded_sprites[sprite_set] := bmp;
  GetBitmapBits(sprite_buf.Handle, sizeof(buf), addr(buf));
  for j := 0 to 199 do
    for i := 0 to 319 do
    begin
      if buf[j * 320 + i] = 255 then
        bmp_mask.Canvas.pixels[i,j] := clWhite
      else
        bmp_mask.Canvas.pixels[i,j] := clBlack;
    end;
  preloaded_sprites_mask[sprite_set] := bmp_mask;
  mask := bmp_mask;
  result := bmp;
end;

function TSpriteFile.get_used_sprite_count(sprite_set: integer): integer;
var
  entry: ^TSpriteEntry;
  max_used_sprite: integer;
begin
  entry := Addr(sprite_entries[sprite_set]);
  max_used_sprite := 0;
  max_used_sprite := max(max_used_sprite, entry.iStandFrame2);
  max_used_sprite := max(max_used_sprite, entry.iWalkFrame2);
  max_used_sprite := max(max_used_sprite, entry.iFallFrame);
  max_used_sprite := max(max_used_sprite, entry.iShootDashFrame2);
  max_used_sprite := max(max_used_sprite, entry.iProjectileF2);
  result := max_used_sprite;
end;

end.
