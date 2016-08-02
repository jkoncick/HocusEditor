unit _exefile;

interface

uses IniFiles, _map;

type
  TFileEntry = record
    offset: cardinal;
    size: cardinal;
  end;

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
  end;

var
  ExeFile: TExeFile;

implementation

uses SysUtils;

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

end.
