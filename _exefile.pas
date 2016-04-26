unit _exefile;

interface

uses IniFiles;

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

  public
    // FAT variables
    file_count: integer;
    fat_offset: Cardinal;
    file_list: array of TFileEntry;

    procedure load_config(ini: TMemIniFile);
    procedure load_data(filename: String);
    procedure save_file_list;
  end;

var
  ExeFile: TExeFile;

implementation

uses SysUtils;

{ TExeFile }

procedure TExeFile.load_config(ini: TMemIniFile);
begin
  file_count := ini.ReadInteger('Basic', 'Number_Of_Files', 652);
  fat_offset := ini.ReadInteger('Basic', 'FAT_Exe_Offset', $1F1A4);
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

end.
