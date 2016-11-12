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
    iLayoutSize: word;
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
    transparent_color: array[0..3] of TColor;
    palette_handle: array[0..3] of hPalette;
    sprite_buffer: array [0..3] of TBitmap;
    palette_assigned: array [0..3] of boolean;

    preloaded_sprites: array of TBitmap;
    preloaded_sprites_mask: array of TBitmap;

  public
    sprite_entries: array of TSpriteEntry;
    num_sprite_entries: integer;

    procedure init;
    procedure load_from_archive;
    procedure save_to_archive;
    procedure update_palette(source_pal_index: integer);
    procedure update_transparent_color(new_color: TColor);
    function get_palette_handle(pal_index: integer): hPalette;
    function load_sprite(sprite_set, sprite_num, pal_index: integer): TBitmap;
    procedure import_sprite(sprite_set, sprite_num: integer; input_image: TBitmap; transparent_index: integer; use_upper_pal: boolean);
    procedure erase_sprite(sprite_set, sprite_num: integer);
    function get_preloaded_sprite(sprite_set: integer; var mask: TBitmap): TBitmap;
    function get_max_used_sprite_num(sprite_set: integer): integer;
  private
    procedure invalidate_preloaded_sprites;
    procedure replace_sprite_blocks(sprite_set, sprite_num, layout_size, pixel_size: integer; layout_data, pixel_data: Pointer);
    procedure replace_data_block(offset, orig_size, new_size: Integer; data: Pointer);
  end;

var
  SpriteFile: TSpriteFile;

implementation

uses _archive, _exefile, Math, sprite_dialog;

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
  transparent_color[2] := $000000; // For rendering the level and importing sprites
  transparent_color[3] := $000000; // For importing sprites with lower palette only
  for i := 0 to 3 do
  begin
    palette_handle[i] := 0;
    sprite_buffer[i] := TBitmap.Create;
    sprite_buffer[i].Width := 320;
    sprite_buffer[i].Height := 200;
    sprite_buffer[i].PixelFormat := pf8bit;
    palette_assigned[i] := false;
  end;
  num_sprite_entries := 0;
end;

procedure TSpriteFile.load_from_archive;
var
  sprite_file_entry: ^TFileEntry;
  first_sprite_data_offset: cardinal;
  intptr: ^cardinal;
  i, j: integer;
begin
  invalidate_preloaded_sprites;
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
  // Remove garbage data from unused sprites, so that importing can work properly
  if sprite_entries[0].iLayoutStarts[39] <> 0 then
    exit;
  for i := 0 to num_sprite_entries - 1 do
  begin
    for j := get_max_used_sprite_num(i) + 1 to 19 do
    begin
      sprite_entries[i].iLayoutStarts[j] := sprite_entries[i].iLayoutStarts[20];
      sprite_entries[i].iPixelStarts[j] := sprite_entries[i].iPixelStarts[20];
      sprite_entries[i].iLayoutStarts[j+20] := sprite_entries[i].iLayoutSize;
      sprite_entries[i].iPixelStarts[j+20] := sprite_entries[i].iPixelsSize div 4;
    end;
  end;
end;

procedure TSpriteFile.save_to_archive;
begin
  Archive.save_file(file_data, Archive.sprite_file_index, file_size);
end;

procedure TSpriteFile.update_palette(source_pal_index: integer);
var
  i: integer;
begin
  for i := 0 to 254 do
  begin
    Pal.palPalEntry[i].peRed := (Archive.palette[i] shr 16) and 255;
    Pal.palPalEntry[i].peGreen := (Archive.palette[i] shr 8) and 255;
    Pal.palPalEntry[i].peBlue := (Archive.palette[i] shr 0) and 255;
  end;
  // Invalidate existing palettes (invalidate palette 3 only for the lower palette)
  for i := 0 to IfThen(source_pal_index = 0, 3, 2) do
  begin
    if palette_handle[i] <> 0 then
      DeleteObject(palette_handle[i]);
    palette_handle[i] := 0;
    palette_assigned[i] := false;
  end;
  // Create third palette now before upper palette is loaded
  if source_pal_index = 0 then
    get_palette_handle(3);
  // Invalidate also preloaded sprites
  invalidate_preloaded_sprites;
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
  if (pixel_off >= sprite_entry.iPixelsSize) or (layout_off >= sprite_entry.iLayoutSize) then
  begin
    SetBitmapBits(sprite_buffer[pal_index].Handle, sizeof(pixel_buffer), Addr(pixel_buffer));
    Result := sprite_buffer[pal_index];
    exit;
  end;
  layout_data := Addr(file_data[sprite_entry.iOffset + layout_off]);
  pixel_data := Addr(file_data[sprite_entry.iOffset + sprite_entry.iLayoutSize + pixel_off]);
  layout_data_ptr := 0;
  pixel_data_ptr := 0;
  image_ptr := 0;
  transparency_type := 0;
  for i := 0 to sprite_entry.iLayoutSize - 1 do
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

procedure TSpriteFile.import_sprite(sprite_set, sprite_num: integer; input_image: TBitmap; transparent_index: integer; use_upper_pal: boolean);
var
  input_buffer: array of byte;
  input_buffer_4: array of Cardinal;
  input_width, input_width_2, input_height: integer;
  source_image: TBitmap;
  source_buffer: array[0..63999] of byte;
  source_width, source_height: integer;
  source_rect: TRect;
  transparency_mask: array[0..63999] of byte;
  pixel_buffer: array[0..63999] of byte;
  pixel_size: integer;
  layout_buffer: array[0..63999] of byte;
  layout_size: integer;
  oldmask, mask, mask2, skip: integer;
  x, y, j: integer;
begin
  // Prepare transparency mask
  FillChar(transparency_mask, sizeof(transparency_mask), 0);
  input_width := input_image.Width;
  input_width_2 := (input_width + 1) and $FFFFFE;
  input_height := input_image.Height;
  source_width := Min(input_width, 320);
  source_height := Min(input_height, 200);
  if input_image.PixelFormat = pf8bit then
  begin
    SetLength(input_buffer, input_width_2 * input_height);
    GetBitmapBits(input_image.Handle, input_width_2 * input_height, input_buffer);
    if transparent_index = -1 then
      transparent_index := input_buffer[0];
    for y := 0 to source_height - 1 do
      for x := 0 to source_width - 1 do
      begin
        if input_buffer[y * input_width_2 + x] <> transparent_index then
          transparency_mask[y * 320 + x] := 1;
      end;
    SetLength(input_buffer, 0);
  end else
  begin
    input_image.PixelFormat := pf32bit;
    SetLength(input_buffer_4, input_width * input_height);
    GetBitmapBits(input_image.Handle, input_width * input_height * 4, input_buffer_4);
    for y := 0 to source_height - 1 do
      for x := 0 to source_width - 1 do
      begin
        if input_buffer_4[y * input_width + x] <> input_buffer_4[0] then
          transparency_mask[y * 320 + x] := 1;
      end;
    SetLength(input_buffer_4, 0);
  end;
  // Copy input image to source image to make it 320*200 and palettize it into game palette
  source_image := TBitmap.Create;
  source_image.PixelFormat := pf8bit;
  source_image.Width := 320;
  source_image.Height := 200;
  source_image.Palette := get_palette_handle(IfThen(use_upper_pal, 2, 3));
  source_rect := Rect(0, 0, source_width, source_height);
  source_image.Canvas.CopyRect(source_rect, input_image.Canvas, source_rect);
  GetBitmapBits(source_image.Handle, 64000, Addr(source_buffer));
  // Convert the source image into Hocus Pocus format (pixel and layout blocks)
	pixel_size := 0;	// Pixel index
	layout_size := 0;	// Layout index
	oldmask := 0;
	for mask := 15 downto 1 do
	begin
		skip := 0;
		for y := 0 to source_height - 1 do
		begin
			x := 0;
			WHILE x < 320 DO
			BEGIN
				mask2 := 0;
        for j := 0 to 3 do
				  if transparency_mask[x + y*320 + j] <> 0 then
					  mask2 := mask2 or (1 shl j);
				IF mask = mask2 THEN
				BEGIN
					IF mask <> oldmask THEN
					BEGIN
						oldmask := mask;
						layout_buffer[layout_size] := 0;
						layout_buffer[layout_size + 1] := mask;
						Inc(layout_size, 2);
					END;
					IF skip <> 0 THEN
					BEGIN
						layout_buffer[layout_size] := 1;
						layout_buffer[layout_size + 1] := skip and 255;
						layout_buffer[layout_size + 2] := skip shr 8;
						Inc(layout_size, 3);
					END;
          for j := 0 to 3 do
					  pixel_buffer[pixel_size + j] := source_buffer[x + y*320 + j];
					Inc(pixel_size, 4);
					skip := 0;
					layout_buffer[layout_size] := 2;
					Inc(layout_size, 1);
				END	ELSE
					skip := skip + 1;
				x := x + 4;
			END;
		end;
	end;
	layout_buffer[layout_size] := 3;
  Inc(layout_size, 1);
  // Copy the pixel and layout blocks into sprite file
  replace_sprite_blocks(sprite_set, sprite_num, layout_size, pixel_size, Addr(layout_buffer), Addr(pixel_buffer));
end;

procedure TSpriteFile.erase_sprite(sprite_set, sprite_num: integer);
var
  dummy_data: array[0..0] of byte;
begin
  dummy_data[0] := 3;
  replace_sprite_blocks(sprite_set, sprite_num, 1, 0, Addr(dummy_data), Addr(dummy_data));
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

function TSpriteFile.get_max_used_sprite_num(sprite_set: integer): integer;
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

procedure TSpriteFile.invalidate_preloaded_sprites;
var
  i: integer;
begin
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

procedure TSpriteFile.replace_sprite_blocks(sprite_set, sprite_num, layout_size, pixel_size: integer; layout_data, pixel_data: Pointer);
var
  sprite_entry: ^TSpriteEntry;
  old_layout_size, old_pixel_size: integer;
  layout_shift, pixel_shift: integer;
  j: integer;
begin
  sprite_entry := Addr(sprite_entries[sprite_set]);
  // Get size of the original layout and pixels blocks
  if sprite_num = 39 then
    old_layout_size := sprite_entry.iLayoutSize
  else
    old_layout_size := sprite_entry.iLayoutStarts[sprite_num + 1];
  old_layout_size := old_layout_size - sprite_entry.iLayoutStarts[sprite_num];
  if sprite_num = 39 then
    old_pixel_size := sprite_entry.iPixelsSize
  else
    old_pixel_size := sprite_entry.iPixelStarts[sprite_num + 1] * 4;
  old_pixel_size := old_pixel_size - (sprite_entry.iPixelStarts[sprite_num] * 4);
  // Replace layout block
  replace_data_block(sprite_entry.iOffset + sprite_entry.iLayoutStarts[sprite_num], old_layout_size, layout_size, layout_data);
  sprite_entry := Addr(sprite_entries[sprite_set]);
  // Adjust offsets of following layout blocks
  layout_shift := layout_size - old_layout_size;
  for j := sprite_num + 1 to 39 do
    sprite_entry.iLayoutStarts[j] := sprite_entry.iLayoutStarts[j] + layout_shift;
  sprite_entry.iLayoutSize := sprite_entry.iLayoutSize + layout_shift;
  // Replace sprite block
  replace_data_block(sprite_entry.iOffset + sprite_entry.iLayoutSize + sprite_entry.iPixelStarts[sprite_num] * 4, old_pixel_size, pixel_size, pixel_data);
  sprite_entry := Addr(sprite_entries[sprite_set]);
  // Adjust offsets of following pixel blocks
  pixel_shift := pixel_size - old_pixel_size;
  for j := sprite_num + 1 to 39 do
    sprite_entry.iPixelStarts[j] := sprite_entry.iPixelStarts[j] + pixel_shift div 4;
  sprite_entry.iPixelsSize := sprite_entry.iPixelsSize + pixel_shift;
  // Shift all offsets for following sprite sets
  for j := sprite_set + 1 to num_sprite_entries - 1 do
    sprite_entries[j].iOffset := integer(sprite_entries[j].iOffset) + layout_shift + pixel_shift;
end;

procedure TSpriteFile.replace_data_block(offset, orig_size, new_size: Integer; data: Pointer);
var
  shift_src_off, shift_dest_off, shift_count: integer;
  ptr: Pointer;
begin
  shift_src_off := offset + orig_size;
  shift_count := new_size - orig_size;
  shift_dest_off := shift_src_off + shift_count;
  if shift_count >= 0 then
    SetLength(file_data, file_size + shift_count);
  Move(file_data[shift_src_off], file_data[shift_dest_off], file_size - shift_src_off);
  if shift_count < 0 then
    SetLength(file_data, file_size + shift_count);
  file_size := file_size + shift_count;
  sprite_entries := Addr(file_data[0]);
  ptr := Addr(file_data[offset]);
  Move(data^, ptr^, new_size);
end;

end.
