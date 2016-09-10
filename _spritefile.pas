unit _spritefile;

interface

uses Windows, SysUtils, Graphics;

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

  public
    sprite_entries: array of TSpriteEntry;
    num_sprite_entries: integer;

    procedure load_from_archive;
    procedure save_to_archive;
    procedure load_sprite(dest: TBitmap; transparent_color: TColor; sprite_set, sprite_num: integer);

  end;

var
  SpriteFile: TSpriteFile;

implementation

uses _archive, _exefile;

{ TSpriteFile }

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
end;

procedure TSpriteFile.save_to_archive;
begin

end;

procedure TSpriteFile.load_sprite(dest: TBitmap; transparent_color: TColor; sprite_set, sprite_num: integer);
var
  i, j: integer;
  sprite_entry: TSpriteEntry;
  layout_off, pixel_off: Cardinal;
  layout_data: TByteArrPtr;
  pixel_data: TByteArrPtr;
  pixel_buffer: array[0..63999] of cardinal;
  layout_data_ptr, pixel_data_ptr, image_ptr: integer;
  layout_flag: byte;
  transparency_type: byte;
  moveby: word;
begin
  transparent_color := ((transparent_color and $FF) shl 16) or (transparent_color and $FF00) or ((transparent_color and $FF0000) shr 16);
  for i := 0 to Length(pixel_buffer) - 1 do
    pixel_buffer[i] := transparent_color;
  sprite_entry := (sprite_entries[sprite_set]);
  layout_off := sprite_entry.iLayoutStarts[sprite_num];
  pixel_off := sprite_entry.iPixelStarts[sprite_num] * 4;
  if (pixel_off >= sprite_entry.iPixelsSize) or
     (layout_off >= sprite_entry.iPixelsOff) or
     //((pixel_off >= (sprite_entry.iPixelStarts[20] * 4)) and (sprite_num < 20)) or
     //((layout_off >= (sprite_entry.iLayoutStarts[20])) and (sprite_num < 20)) or
     //((pixel_off <= (sprite_entry.iPixelStarts[20] * 4)) and (sprite_num > 20)) or
     //((layout_off <= (sprite_entry.iLayoutStarts[20])) and (sprite_num > 20)) or
     false then
  begin
    SetBitmapBits(dest.Handle, sizeof(pixel_buffer), Addr(pixel_buffer));
    dest.Modified := true;
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
              pixel_buffer[image_ptr + j] := Archive.palette[pixel_data[pixel_data_ptr + j]];
          end;
          Inc(image_ptr, 4);
          Inc(pixel_data_ptr, 4);
        end;
      3: begin
          break;
        end;
    end;
  end;
  SetBitmapBits(dest.Handle, sizeof(pixel_buffer), Addr(pixel_buffer));
  dest.Modified := true;
end;

end.
