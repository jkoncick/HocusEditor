unit _archive;

interface

uses Forms, Windows, Graphics, _exefile;

type
  TArchive = class

  public
    // Archive file variables
    archive_open_permanent: boolean;
    archive_exists: boolean;
    archive_filename: String;
    archive_filesize: Cardinal;
    archive_version: String;
    archive_file_mode: integer;
    archive_file: File of byte;

  public
    // Configuration variables
    palette_file_index: integer;
    first_level_file_index: integer;
    first_tileset_file_index: integer;
    first_backdrop_file_index: integer;
    first_backdrop_palette_file_index: integer;

    // FAT variables
    file_count: integer;
    file_list: array of TFileEntry;
    file_names: array of string;

    // Tileset names
    tileset_count: integer;
    tileset_names: array of String;

    // Level names
    level_count: Integer;
    level_names: array of String;

    // Music names
    music_count: integer;
    music_names: array of String;

    // Monster types
    monster_type_names: array[0..39] of string;

    // Palette data
    palette: array[0..255] of TColor;

  public
    procedure init;
    procedure load_config(filename: string);

    procedure open_archive(file_mode: integer; permanent: boolean);
    procedure close_archive(force: boolean);
    procedure load_data(mem: Pointer; offset, size: Cardinal);
    procedure save_data(mem: Pointer; offset, size: Cardinal);
    procedure reserve_space_for_file(file_index: integer; size: Cardinal);
    procedure export_file(file_index: integer; filename: String);
    procedure import_file(file_index: integer; filename: String);

    procedure load_palette(file_index, palette_index: integer);
    procedure load_pcx_image(target: TBitmap; file_index: integer);
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
  ini_filesize: Cardinal;
  SRec: TSearchRec;
  exe_filename: String;
  exe_filesize: Cardinal;
begin
  // Check if archive exists
  archive_filename := Settings.GameFolder + 'HOCUS.DAT';
  archive_exists := FileExists(archive_filename);
  archive_filesize := 0;
  if not archive_exists then
  begin
    Application.MessageBox('Could not find game data (HOCUS.DAT) file.'#13'Please copy the editor into your Hocus Pocus game folder'#13'or specify the game folder in HocusEditor.ini file.', 'Fatal Error', MB_ICONERROR);
    exit;
  end;
  // Get HOCUS.DAT filesize
  if FindFirst(archive_filename, faAnyfile, SRec) = 0 then
  begin
    archive_filesize := SRec.Size;
    FindClose(SRec);
  end;

  // Check if executable exists
  exe_filename := Settings.GameFolder + 'HOCUS.EXE';
  exe_filesize := 0;
  if not FileExists(exe_filename) then
  begin
    Application.MessageBox('Could not find game executable (HOCUS.EXE) file.'#13'Please copy the editor into your Hocus Pocus game folder'#13'or specify the game folder in HocusEditor.ini file.', 'Fatal Error', MB_ICONERROR);
    exit;
  end;
  // Get HOCUS.EXE filesize
  if FindFirst(exe_filename, faAnyfile, SRec) = 0 then
  begin
    exe_filesize := SRec.Size;
    FindClose(SRec);
  end;

  // Detect game version from HOCUS.EXE filesize
  version := '';
  ini := TMemIniFile.Create(current_dir + 'config\versions.ini');
  tmp_strings := TStringList.Create;
  ini.ReadSection('Versions', tmp_strings);
  for i := 0 to tmp_strings.Count -1 do
  begin
    version := tmp_strings[i];
    ini_filesize := ini.ReadInteger('Versions', version, 0);
    if ini_filesize = exe_filesize then
    begin
      archive_version := version;
      break;
    end;
  end;
  tmp_strings.Destroy;
  if archive_version = '' then
  begin
    Application.MessageBox(PChar('Could not detect your game version - the HOCUS.EXE file size does not match any size defined in versions.ini.'#13'Using settings for this version: '+ version), 'Error', MB_ICONWARNING);
  end;

  // Load configuration from ini file
  load_config(current_dir + 'config\' + version + '.ini');

  // Load data from exe file
  ExeFile.load_data(exe_filename);
  file_list := Addr(ExeFile.file_list[0]);

  // Load palette
  load_palette(palette_file_index, 0);
end;

procedure TArchive.load_config(filename: string);
var
  ini: TMemIniFile;
  tmp_strings: TStringList;
  i, num: integer;
begin
  // Load configuration from ini file
  ini := TMemIniFile.Create(filename);
  tmp_strings := TStringList.Create;
  // Load basic information
  palette_file_index := ini.ReadInteger('Basic', 'Palette_File', 7);
  first_level_file_index := ini.ReadInteger('Basic', 'First_Level_File', 131);
  first_tileset_file_index := ini.ReadInteger('Basic', 'First_Tileset_File', 105);
  first_backdrop_file_index := ini.ReadInteger('Basic', 'First_Backdrop_File', 89);
  first_backdrop_palette_file_index := ini.ReadInteger('Basic', 'First_Backdrop_Palette_File', 73);
  // Load tilesets
  ini.ReadSection('Tilesets', tmp_strings);
  tileset_count := tmp_strings.Count;
  SetLength(tileset_names, tileset_count);
  for i := 0 to tileset_count -1 do
    tileset_names[i] := tmp_strings[i];
  // Load levels
  ini.ReadSection('Levels', tmp_strings);
  level_count := tmp_strings.Count;
  SetLength(level_names, level_count);
  for i := 0 to level_count -1 do
    level_names[i] := tmp_strings[i];
  // Load music names
  ini.ReadSection('Music_Names', tmp_strings);
  music_count := tmp_strings.Count;
  SetLength(music_names, music_count);
  for i := 0 to music_count -1 do
    music_names[i] := tmp_strings[i];
  // Load monster types
  ini.ReadSection('Monster_Types', tmp_strings);
  for i := 0 to tmp_strings.Count -1 do
  begin
    num := strtoint(tmp_strings[i]);
    if num >= Length(monster_type_names) then
      continue;
    monster_type_names[num] := ini.ReadString('Monster_Types', tmp_strings[i], '');
  end;
  // Load exe config
  ExeFile.load_config(ini);
  file_count := ExeFile.file_count;

  // Load file names
  SetLength(file_names, file_count);
  ini.ReadSection('File_Names', tmp_strings);
  for i := 0 to tmp_strings.Count -1 do
  begin
    num := strtoint(tmp_strings[i]);
    if num >= file_count then
      continue;
    file_names[num] := ini.ReadString('File_Names', tmp_strings[i], '');
  end;

  // Free memory
  ini.Destroy;
  tmp_strings.Destroy;
end;

procedure TArchive.open_archive(file_mode: integer; permanent: boolean);
begin
  // Archive is already opened with same file mode - do nothing
  if archive_open_permanent and (archive_file_mode >= file_mode) then
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

procedure TArchive.reserve_space_for_file(file_index: integer; size: Cardinal);
var
  old_size: Cardinal;
  next_offset: Cardinal;
  size_diff: integer;
  already_open: boolean;
  buffer: array of byte;
  buffer_size: Cardinal;
  i: integer;
begin
  old_size := file_list[file_index].size;
  next_offset := file_list[file_index].offset + old_size;
  if size = old_size then
    exit;
  size_diff := size - old_size;
  already_open := archive_open_permanent and (archive_file_mode = fmOpenReadWrite);
  if not already_open then
    open_archive(fmOpenReadWrite, true);
  // Shift data following the file in question
  buffer_size := archive_filesize - next_offset;
  SetLength(buffer, buffer_size);
  load_data(buffer, next_offset, buffer_size);
  save_data(buffer, file_list[file_index].offset + size, buffer_size);
  // Adjust all offsets in the file list
  file_list[file_index].size := size;
  for i := file_index + 1 to file_count - 1 do
    file_list[i].offset := integer(file_list[i].offset) + size_diff;
  ExeFile.save_file_list;
  // Truncate archive if the final size is smaller than before
  archive_filesize := integer(archive_filesize) + size_diff;
  if size_diff < 0 then
  begin
    Seek(archive_file, archive_filesize);
    Truncate(archive_file);
  end;
  if not already_open then
    close_archive(true);
end;

procedure TArchive.export_file(file_index: integer; filename: String);
var
  buffer: array of byte;
  size: Cardinal;
  f: file of byte;
begin
  size := file_list[file_index].size;
  SetLength(buffer, size);
  load_data(Addr(buffer[0]), file_list[file_index].offset, size);
  AssignFile(f, filename);
  Rewrite(f);
  BlockWrite(f, buffer[0], size);
  Close(f);
end;

procedure TArchive.import_file(file_index: integer; filename: String);
var
  buffer: array of byte;
  size: Cardinal;
  f: file of byte;
begin
  AssignFile(f, filename);
  FileMode := fmOpenRead;
  Reset(f);
  size := FileSize(f);  
  SetLength(buffer, size);
  BlockRead(f, buffer[0], size);
  Close(f);
  reserve_space_for_file(file_index, size);
  save_data(Addr(buffer[0]), file_list[file_index].offset, size);
end;

procedure TArchive.load_palette(file_index, palette_index: integer);
var
  tmp_palette: array[0..127, 0..2] of byte;
  i: integer;
  offset: integer;
begin
  load_data(Addr(tmp_palette), file_list[file_index].offset, 384);
  offset := palette_index * 128;
  for i := 0 to 127 do
    palette[i + offset] := (tmp_palette[i,0] shl 2) + (tmp_palette[i,1] shl 10) + (tmp_palette[i,2] shl 18);
end;

procedure TArchive.load_pcx_image(target: TBitmap; file_index: integer);
var
  file_entry: ^TFileEntry;
  buffer: Array[0..65535] of byte;
  b, color: byte;
  i, j, count, x, y: Cardinal;
begin
  file_entry := Addr(file_list[file_index]);
  target.PixelFormat := pf32bit;
  target.Width := 320;
  target.Height := 200;
  load_data(Addr(buffer), file_entry.offset, file_entry.size);
  x := 0;
  y := 0;
  i := $80;
  while i < file_entry.size do
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
  load_pcx_image(target, first_tileset_file_index + index);
end;

procedure TArchive.load_level_data(mem: Pointer; level_num, subfile_num: integer);
var
  file_entry: ^TFileEntry;
begin
  file_entry := Addr(file_list[first_level_file_index + level_num + subfile_num * level_count]);
  load_data(mem, file_entry.offset, file_entry.size);
end;

procedure TArchive.save_level_data(mem: Pointer; level_num, subfile_num: integer);
var
  file_entry: ^TFileEntry;
begin
  file_entry := Addr(file_list[first_level_file_index + level_num + subfile_num * level_count]);
  save_data(mem, file_entry.offset, file_entry.size);
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
  // TODO
end;

end.
