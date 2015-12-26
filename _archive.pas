unit _archive;

interface

uses Forms, Windows, Graphics;

type
  TTilesetInfo = record
    name: String;
    image_offset: Cardinal;
    image_size: Cardinal;
  end;

type
  TLevelInfo = record
    name: String;
    tileset: integer;
  end;

const level_subfile_sizes: array[0..12] of Cardinal = (8, 724, 5040, 40, 506, 300, 300, 240, 8000, 14400, 14400, 14400, 28800);

type
  TArchive = class

  public
    // Archive file variables
    archive_open_permanent: boolean;
    archive_exists: boolean;
    archive_filename: String;
    archive_version: String;
    archive_file_mode: integer;
    archive_file: File of byte;

  public
    // Palette variables
    palette_file_offset: Cardinal;
    palette: array[0..255] of TColor;

    // Tileset variables
    tileset_count: integer;
    tileset_info: array of TTilesetInfo;

    // Level variables
    level_file_offset: Cardinal;
    level_count: Integer;
    level_info: array of TLevelInfo;

    // Monster types
    monster_type_names: array[0..39] of string;

  public
    procedure init;
    procedure load_config(filename: string);

    procedure open_archive(file_mode: integer; permanent: boolean);
    procedure close_archive(force: boolean);
    procedure load_data(mem: Pointer; offset, size: Cardinal);
    procedure save_data(mem: Pointer; offset, size: Cardinal);

    procedure load_palette;
    procedure load_pcx_image(target: TBitmap; offset, size: Cardinal);
    procedure load_tileset_image(target: TBitmap; index: integer);
    procedure load_level_data(mem: Pointer; level_num, subfile_num: integer);
    procedure save_level_data(mem: Pointer; level_num, subfile_num: integer);

    function get_monster_type_name(sprite_set: word): string;
    function get_first_level_by_tileset(tileset_index: integer): integer;
  end;

var
  Archive: TArchive;

implementation

{ TArchive }

uses SysUtils, Classes, IniFiles, _settings, main;

procedure TArchive.init;
var
  ini: TMemIniFile;
  tmp_strings: TStringList;
  i: integer;
  version: String;
  actual_filesize, ini_filesize: integer;
  SRec: TSearchRec;
begin
  // Check if archive exists
  archive_filename := Settings.GameFolder + 'HOCUS.DAT';
  archive_exists := FileExists(archive_filename);
  if not archive_exists then
  begin
    Application.MessageBox('Could not find game data (HOCUS.DAT) file.'#13'Please copy the editor into your Hocus Pocus game folder'#13'or specify the game folder in HocusEditor.ini file.', 'Fatal Error', MB_ICONERROR);
    exit;
  end;

  // Get HOCUS.DAT filesize
  actual_filesize := 0;
  if FindFirst(archive_filename, faAnyfile, SRec) = 0 then
  begin
    actual_filesize := SRec.Size;
    FindClose(SRec);
  end;

  // Detect game version from HOCUS.DAT filesize
  version := '';
  ini := TMemIniFile.Create(current_dir + 'config\versions.ini');
  tmp_strings := TStringList.Create;
  ini.ReadSection('Versions', tmp_strings);
  for i := 0 to tmp_strings.Count -1 do
  begin
    version := tmp_strings[i];
    ini_filesize := ini.ReadInteger('Versions', version, 0);
    if ini_filesize = actual_filesize then
    begin
      archive_version := version;
      break;
    end;
  end;
  tmp_strings.Destroy;
  if archive_version = '' then
  begin
    Application.MessageBox(PChar('Could not detect your game version - the HOCUS.DAT file size does not match any known size.'#13'Using settings for this version: '+ version +#13'The levels and graphics may be broken.'), 'Error', MB_ICONWARNING);
  end;

  // Load configuration from ini file
  load_config(current_dir + 'config\' + version + '.ini');

  // Load palette
  load_palette;
end;

procedure TArchive.load_config(filename: string);
var
  ini: TMemIniFile;
  tmp_strings: TStringList;
  decoder: TStringList;
  i, num: integer;
begin
  // Load configuration from ini file
  ini := TMemIniFile.Create(filename);
  tmp_strings := TStringList.Create;
  decoder := TStringList.Create;
  decoder.Delimiter := '.';
  // Load basic information
  palette_file_offset := ini.ReadInteger('Basic', 'Palette_Offset', 152820);
  level_file_offset := ini.ReadInteger('Basic', 'Level_Base_Offset', 2594316);
  // Load tilesets
  ini.ReadSection('Tilesets', tmp_strings);
  tileset_count := tmp_strings.Count;
  SetLength(tileset_info, tileset_count);
  for i := 0 to tileset_count -1 do
  begin
    tileset_info[i].name := tmp_strings[i];
    decoder.DelimitedText := ini.ReadString('Tilesets', tmp_strings[i], '');
    tileset_info[i].image_offset := strtoint(decoder[0]);
    tileset_info[i].image_size := strtoint(decoder[1]);
  end;
  // Load levels
  ini.ReadSection('Levels', tmp_strings);
  level_count := tmp_strings.Count;
  SetLength(level_info, level_count);
  for i := 0 to level_count -1 do
  begin
    level_info[i].name := tmp_strings[i];
    level_info[i].tileset := ini.ReadInteger('Levels', tmp_strings[i], 1) - 1;
  end;
  // Load monster types
  ini.ReadSection('Monster_Types', tmp_strings);
  for i := 0 to tmp_strings.Count -1 do
  begin
    num := strtoint(tmp_strings[i]);
    if num >= Length(monster_type_names) then
      continue;
    monster_type_names[num] := ini.ReadString('Monster_Types', tmp_strings[i], '');
  end;
  // Free memory
  ini.Destroy;
  tmp_strings.Destroy;
  decoder.Destroy;
end;

procedure TArchive.open_archive(file_mode: integer; permanent: boolean);
begin
  // Archive is already opened with same file mode - do nothing
  if archive_open_permanent and (archive_file_mode = file_mode) then
    exit;
  // Archive is already opened with different file mode - close it first
  if archive_open_permanent and (archive_file_mode < file_mode) then
    close_archive(true);
  // Open archive
  archive_open_permanent := permanent;
  archive_file_mode := file_mode;
  AssignFile(archive_file, archive_filename);
  FileMode := file_mode;
  Reset(archive_file);
end;

procedure TArchive.close_archive(force: boolean);
begin
  if archive_open_permanent and not force then
    exit;
  archive_open_permanent := false;
  close(archive_file);
end;

procedure TArchive.load_data(mem: Pointer; offset, size: Cardinal);
begin
  open_archive(fmOpenRead, false);
  Seek(archive_file, offset);
  BlockRead(archive_file, mem^, size);
  close_archive(false);
end;

procedure TArchive.save_data(mem: Pointer; offset, size: Cardinal);
begin
  open_archive(fmOpenReadWrite, false);
  Seek(archive_file, offset);
  BlockWrite(archive_file, mem^, size);
  close_archive(false);
end;

procedure TArchive.load_palette;
var
  tmp_palette: array[0..255, 0..2] of byte;
  i: integer;
begin
  load_data(Addr(tmp_palette), palette_file_offset, 768);
  for i := 0 to 255 do
    palette[i] := (tmp_palette[i,0] shl 2) + (tmp_palette[i,1] shl 10) + (tmp_palette[i,2] shl 18);
end;

procedure TArchive.load_pcx_image(target: TBitmap; offset, size: Cardinal);
var
  buffer: Array[0..65535] of byte;
  b, color: byte;
  i, j, count, x, y: Cardinal;
begin
  target.PixelFormat := pf32bit;
  target.Width := 320;
  target.Height := 200;
  load_data(Addr(buffer), offset, size);
  x := 0;
  y := 0;
  i := $80;
  while i < size do
  begin
    b := buffer[i];
    if (b and $C0) = $C0 then
    begin
      count := b and $3F;
      Inc(i);
      color := buffer[i];
    end else
    begin
      count := 1;
      color := b;
    end;
    for j := 0 to count-1 do
    begin
      target.Canvas.Pixels[x,y] := palette[color];
      Inc(x);
      if x = 320 then
      begin
        x := 0;
        inc(y);
      end;
      if y = 200 then
        exit;
    end;
    Inc(i);
  end;
end;

procedure TArchive.load_tileset_image(target: TBitmap; index: integer);
begin
  load_pcx_image(target, tileset_info[index].image_offset, tileset_info[index].image_size);
end;

procedure TArchive.load_level_data(mem: Pointer; level_num, subfile_num: integer);
var
  offset: Cardinal;
  i: integer;
begin
  offset := level_file_offset;
  for i := 0 to subfile_num - 1 do
    offset := offset + level_subfile_sizes[i] * Cardinal(level_count);
  offset := offset + level_subfile_sizes[subfile_num] * Cardinal(level_num);
  load_data(mem, offset, level_subfile_sizes[subfile_num]);
end;

procedure TArchive.save_level_data(mem: Pointer; level_num, subfile_num: integer);
var
  offset: Cardinal;
  i: integer;
begin
  offset := level_file_offset;
  for i := 0 to subfile_num - 1 do
    offset := offset + level_subfile_sizes[i] * Cardinal(level_count);
  offset := offset + level_subfile_sizes[subfile_num] * Cardinal(level_num);
  save_data(mem, offset, level_subfile_sizes[subfile_num]);
end;

function TArchive.get_monster_type_name(sprite_set: word): string;
begin
  if (sprite_set >= Length(Archive.monster_type_names)) or (monster_type_names[sprite_set] = '') then
    result := 'Sprite set ' + inttostr(sprite_set)
  else
    result := monster_type_names[sprite_set];
end;

function TArchive.get_first_level_by_tileset(tileset_index: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to level_count - 1 do
  begin
    if level_info[i].tileset = tileset_index then
    begin
      result := i;
      exit;
    end;
  end;
end;

end.
