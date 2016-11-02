unit _exefile;

interface

uses IniFiles, _map;

type
  TFileEntry = record
    offset: cardinal;
    size: cardinal;
  end;

type
  TExePatchHeader = packed record
    signature: array[0..11] of char;
    version: word;
    context_size: word;
    base_offset: integer;
  end;

type
  TExePatchEntry = packed record
    offset: integer;
    size: integer;
    data: array[0..0] of byte;
  end;

const patch_version = 1;

type
  TExeFile = class
  private
    // Exe file variables
    exe_filename: String;

    // Configuration variables
    fat_offset: Cardinal;
    par_times_offset: Cardinal;
    tileset_numbers_offset: Cardinal;
    backdrop_numbers_offset: Cardinal;
    music_numbers_offset: Cardinal;
    elevator_tiles_offset: Cardinal;
  public

    // FAT variables
    file_count: integer;
    file_list: array of TFileEntry;

    // Level data variables
    par_times: array[0..35] of word;
    tileset_numbers: array[0..39] of word;
    backdrop_numbers: array[0..39] of word;
    music_numbers: array[0..35] of word;
    elevator_tiles: array[0..39,0..1] of smallint;

    procedure load_config(ini: TMemIniFile);
    procedure load_data(filename: String);
    procedure save_file_list;

    procedure get_level_data(level_index: integer; var data: TLevelExeData);
    procedure save_level_data(level_index: integer; var data: TLevelExeData);

    procedure create_exe_patch(original_exe_filename, patch_filename: String);
    function apply_exe_patch(patch_filename: String): boolean;
  end;

var
  ExeFile: TExeFile;

implementation

uses Windows, Forms, SysUtils, _settings;

{ TExeFile }

procedure TExeFile.load_config(ini: TMemIniFile);
begin
  file_count := ini.ReadInteger('Basic', 'Number_Of_Files', 652);
  fat_offset := ini.ReadInteger('Exe_File', 'FAT_Offset', $1F1A4);
  par_times_offset := ini.ReadInteger('Exe_File', 'Par_Times_Offset', $20A9A);
  tileset_numbers_offset := ini.ReadInteger('Exe_File', 'Tileset_Numbers_Offset', $21ADA);
  backdrop_numbers_offset := ini.ReadInteger('Exe_File', 'Backdrop_Numbers_Offset', $21B7A);
  music_numbers_offset := ini.ReadInteger('Exe_File', 'Music_Numbers_Offset', $21BDE);
  elevator_tiles_offset := ini.ReadInteger('Exe_File', 'Elevator_Tiles_Offset', $21C26);
end;

procedure TExeFile.load_data(filename: String);
var
  f: file of Byte;
begin
  exe_filename := filename;
  SetLength(file_list, file_count);
  AssignFile(f, filename);
  FileMode := fmOpenRead;
  Reset(f);
  Seek(f, fat_offset);
  BlockRead(f, file_list[0], file_count * sizeof(TFileEntry));
  Seek(f, par_times_offset);
  BlockRead(f, par_times, sizeof(par_times));
  Seek(f, tileset_numbers_offset);
  BlockRead(f, tileset_numbers, sizeof(tileset_numbers));
  Seek(f, backdrop_numbers_offset);
  BlockRead(f, backdrop_numbers, sizeof(backdrop_numbers));
  Seek(f, music_numbers_offset);
  BlockRead(f, music_numbers, sizeof(music_numbers));
  Seek(f, elevator_tiles_offset);
  BlockRead(f, elevator_tiles, sizeof(elevator_tiles));
  Close(f);
end;

procedure TExeFile.save_file_list;
var
  f: file of Byte;
begin
  AssignFile(f, exe_filename);
  FileMode := fmOpenReadWrite;
  Reset(f);
  Seek(f, fat_offset);
  BlockWrite(f, file_list[0], file_count * sizeof(TFileEntry));
  Close(f);
end;

procedure TExeFile.get_level_data(level_index: integer; var data: TLevelExeData);
var
  level_10_index: integer;
begin
  level_10_index := level_index + (level_index div 9);
  data.par_time := par_times[level_index];
  data.tileset_number := tileset_numbers[level_10_index];
  data.backdrop_number := backdrop_numbers[level_10_index];
  data.music_number := music_numbers[level_index];
  data.elevator_tile_left := elevator_tiles[level_10_index, 0];
  data.elevator_tile_right := elevator_tiles[level_10_index, 1];
end;


procedure TExeFile.save_level_data(level_index: integer; var data: TLevelExeData);
var
  level_10_index: integer;
  f: file of Byte;
begin
  level_10_index := level_index + (level_index div 9);
  par_times[level_index] := data.par_time;
  tileset_numbers[level_10_index] := data.tileset_number;
  backdrop_numbers[level_10_index] := data.backdrop_number;
  music_numbers[level_index] := data.music_number;
  elevator_tiles[level_10_index, 0] := data.elevator_tile_left;
  elevator_tiles[level_10_index, 1] := data.elevator_tile_right;
  AssignFile(f, exe_filename);
  FileMode := fmOpenReadWrite;
  Reset(f);
  Seek(f, par_times_offset);
  BlockWrite(f, par_times, sizeof(par_times));
  Seek(f, tileset_numbers_offset);
  BlockWrite(f, tileset_numbers, sizeof(tileset_numbers));
  Seek(f, backdrop_numbers_offset);
  BlockWrite(f, backdrop_numbers, sizeof(backdrop_numbers));
  Seek(f, music_numbers_offset);
  BlockWrite(f, music_numbers, sizeof(music_numbers));
  Seek(f, elevator_tiles_offset);
  BlockWrite(f, elevator_tiles, sizeof(elevator_tiles));
  Close(f);
end;

procedure TExeFile.create_exe_patch(original_exe_filename, patch_filename: String);
var
  SRec: TSearchRec;
  orig_exe_size, exe_size: integer;
  f: file of byte;
  signature: String;
  context_size: integer;
  base_offset: integer;
  header: TExePatchHeader;
  entry: ^TExePatchEntry;
  entry_buffer: array[0..65535] of byte;
  orig_exe_buffer, new_exe_buffer: array of byte;
  off: integer;
  diff_start, diff_end: integer;
begin
  orig_exe_size := 0;
  exe_size := 0;
  context_size := Settings.ExePatchContextSize;
  base_offset := elevator_tiles_offset;
  // Compare size of both original and current exe files
  if FindFirst(exe_filename, faAnyfile, SRec) = 0 then
  begin
    exe_size := SRec.Size;
    FindClose(SRec);
  end;
  if FindFirst(original_exe_filename, faAnyfile, SRec) = 0 then
  begin
    orig_exe_size := SRec.Size;
    FindClose(SRec);
  end;
  if orig_exe_size <> exe_size then
  begin
    Application.MessageBox('The current and original exe file size differs.', 'Cannot create exe patch', MB_OK or MB_ICONERROR);
    exit;
  end;
  // Read both exe's into buffers
  SetLength(orig_exe_buffer, exe_size);
  SetLength(new_exe_buffer, exe_size);
  AssignFile(f, original_exe_filename);
  FileMode := fmOpenRead;
  Reset(f);
  BlockRead(f, orig_exe_buffer[0], exe_size);
  Close(f);
  AssignFile(f, exe_filename);
  FileMode := fmOpenRead;
  Reset(f);
  BlockRead(f, new_exe_buffer[0], exe_size);
  Close(f);
  // Copy FAT and level properties data from one exe to the other to ignore these differences
  Move(orig_exe_buffer[fat_offset], new_exe_buffer[fat_offset], file_count * sizeof(TFileEntry));
  Move(orig_exe_buffer[par_times_offset], new_exe_buffer[par_times_offset], sizeof(par_times));
  Move(orig_exe_buffer[tileset_numbers_offset], new_exe_buffer[tileset_numbers_offset], sizeof(tileset_numbers));
  Move(orig_exe_buffer[backdrop_numbers_offset], new_exe_buffer[backdrop_numbers_offset], sizeof(backdrop_numbers));
  Move(orig_exe_buffer[music_numbers_offset], new_exe_buffer[music_numbers_offset], sizeof(music_numbers));
  Move(orig_exe_buffer[elevator_tiles_offset], new_exe_buffer[elevator_tiles_offset], sizeof(elevator_tiles));
  // Fill and write header
  signature := 'HocusPatch';
  StrPLCopy(header.signature, signature, High(header.signature));
  header.version := patch_version;
  header.context_size := context_size;
  header.base_offset := base_offset;
  AssignFile(f, patch_filename);
  Rewrite(f);
  BlockWrite(f, header, sizeof(header));
  // Compare both exe files and write difference entries into patch
  entry := Addr(entry_buffer[0]);
  diff_start := -1;
  diff_end := -1;
  for off := 0 to exe_size - 1 do
  begin
    if orig_exe_buffer[off] <> new_exe_buffer[off] then
    begin
      // Bytes in both exe's are different
      if diff_start = -1 then
        diff_start := off;
      diff_end := off;
    end
    else if diff_start <> -1 then
    begin
      // Bytes in both exe's are same, but there was a difference recently
      if (off - diff_end) > (context_size * 2) then
      begin
        // Finalize the difference
        entry.offset := diff_start;
        entry.size := diff_end - diff_start + 1;
        Move(orig_exe_buffer[diff_start - context_size], entry.data[0], entry.size + context_size * 2);
        Move(new_exe_buffer[diff_start], entry.data[entry.size + context_size * 2], entry.size);
        BlockWrite(f, entry_buffer, sizeof(TExePatchEntry) + entry.size * 2 + context_size * 2 - 1);
        diff_start := -1;
        diff_end := -1;
      end;
    end;
  end;
  Close(f);
end;

function TExeFile.apply_exe_patch(patch_filename: String): boolean;
var
  f: file of byte;
  context_size: integer;
  offset_difference: integer;
  header: ^TExePatchHeader;
  entry: ^TExePatchEntry;
  patch_size, exe_size: integer;
  patch_buffer, exe_buffer: array of byte;
  next_entry_offset: integer;
  compare_start, compare_pos, trynextpos: integer;
  same, found: boolean;
begin
  result := false;
  // Read patch
  AssignFile(f, patch_filename);
  FileMode := fmOpenRead;
  Reset(f);
  patch_size := FileSize(f);
  SetLength(patch_buffer, patch_size);
  BlockRead(f, patch_buffer[0], patch_size);
  Close(f);
  // Read exe
  AssignFile(f, exe_filename);
  FileMode := fmOpenRead;
  Reset(f);
  exe_size := FileSize(f);
  SetLength(exe_buffer, exe_size);
  BlockRead(f, exe_buffer[0], exe_size);
  Close(f);
  // Read patch header
  header := Addr(patch_buffer[0]);
  if header.signature <> 'HocusPatch' then
  begin
    Application.MessageBox(PChar('Invalid file format. Not a HocusPatch.'), 'Error during applying exe patch', MB_OK or MB_ICONERROR);
    exit;
  end;
  if header.version <> patch_version then
  begin
    Application.MessageBox(PChar('Unsupported patch version. Actual: ' + inttostr(header.version) + ', Supported: ' + inttostr(patch_version)), 'Error during applying exe patch', MB_OK or MB_ICONERROR);
    exit;
  end;
  context_size := header.context_size;
  offset_difference := integer(elevator_tiles_offset) - header.base_offset;
  // Read patch entries and apply them
  next_entry_offset := sizeof(TExePatchHeader);
  while next_entry_offset < patch_size do
  begin
    entry := Addr(patch_buffer[next_entry_offset]);
    Inc(next_entry_offset, sizeof(TExePatchEntry) + entry.size * 2 + context_size * 2 - 1);
    // Compare the difference section
    trynextpos := 0;
    found := false;
    while (not found) and (trynextpos <> Settings.ExePatchLookDistance) do
    begin
      same := true;
      compare_start := entry.offset + offset_difference + trynextpos;
      for compare_pos := 0 to entry.size + context_size * 2 - 1 do
      begin
        if entry.data[compare_pos] <> exe_buffer[compare_start - context_size + compare_pos] then
        begin
          same := false;
          break;
        end;
      end;
      if same then
      begin
        Move(entry.data[entry.size + context_size * 2], exe_buffer[compare_start], entry.size);
        found := true;
      end else
      begin
        // Not found on this position, try next position
        // only if HOCUS.EXE is different - offset_difference = 0
        if offset_difference = 0 then
          break;
        if trynextpos >= 0 then
          trynextpos := (trynextpos + 1) * -1
        else
          trynextpos := trynextpos * -1;
      end;
    end;
    if not found then
      Application.MessageBox(PChar('Could not find the data from original exe at offset ' + inttohex(entry.offset, 5) + ' of size ' + inttostr(entry.size) + ' bytes in the target exe.'), 'Error during applying exe patch', MB_OK or MB_ICONWARNING);
  end;
  AssignFile(f, exe_filename);
  Rewrite(f);
  BlockWrite(f, exe_buffer[0], exe_size);
  Close(f);
  result := true;
end;

end.
