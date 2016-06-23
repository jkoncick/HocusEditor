unit _settings;

interface

uses
  IniFiles;

type
  TSettings = class

  public
    // Preferences
    PreserveGUISettings: boolean;
    HidePresetWindow: boolean;
    CheckMapErrorsOnSave: boolean;
    CheckMapErrorsOnTest: boolean;
    AlwaysAskOnQuit: boolean;
    DrawObjectBrush: boolean;
    DrawPaintBrush: boolean;
    TestMapDifficulty: integer;
    LoadCustomImageWarn: boolean;
    DosboxParameters: String;

    // File paths
    GameFolder: String;
    DosboxPath: String;

  private
    tmp_ini: TMemIniFile;

  public
    procedure load_precreate_editor_settings;
    procedure load_postcreate_editor_settings;
    procedure save_editor_settings;
    procedure get_file_paths_from_map_filename;

  end;

var
  Settings: TSettings;

implementation

uses
  SysUtils, main, block_preset_dialog, set_dialog, level_props_dialog, _map;

procedure TSettings.load_precreate_editor_settings;
var
  ini: TMemIniFile;
begin
  ini := TMemIniFile.Create(current_dir + 'HocusEditor.ini');
  tmp_ini := ini;
  // Load preferences
  PreserveGUISettings := ini.ReadBool('Preferences', 'PreserveGUISettings', true);
  HidePresetWindow := ini.ReadBool('Preferences', 'HidePresetWindow', true);
  CheckMapErrorsOnSave := ini.ReadBool('Preferences', 'CheckMapErrorsOnSave', true);
  CheckMapErrorsOnTest := ini.ReadBool('Preferences', 'CheckMapErrorsOnTest', true);
  AlwaysAskOnQuit := ini.ReadBool('Preferences', 'AlwaysAskOnQuit', true);
  DrawObjectBrush := ini.ReadBool('Preferences', 'DrawObjectBrush', true);
  DrawPaintBrush := ini.ReadBool('Preferences', 'DrawPaintBrush', true);
  TestMapDifficulty := ini.ReadInteger('Preferences', 'TestMapDifficulty', 1);
  LoadCustomImageWarn := ini.ReadBool('Preferences', 'LoadCustomImageWarn', true);
  DosboxParameters := ini.ReadString('Preferences','DosboxParameters', '');
  // Load file paths
  GameFolder := ini.ReadString('Paths','GameFolder', current_dir);
  if GameFolder[Length(GameFolder)] <> '\' then
    GameFolder := GameFolder + '\';
  DosboxPath := ini.ReadString('Paths','DosboxPath', current_dir + '..\dosbox\dosbox.exe');
  // Load MainWindow GUI setings
  if not PreserveGUISettings then
    exit;
  MainWindow.Left := ini.ReadInteger('GUI','MainWindow.Left',MainWindow.Left);
  MainWindow.Top := ini.ReadInteger('GUI','MainWindow.Top',MainWindow.Top);
  MainWindow.Width := ini.ReadInteger('GUI','MainWindow.Width',MainWindow.Width);
  MainWindow.Height := ini.ReadInteger('GUI','MainWindow.Height',MainWindow.Height);
  MainWindow.cbAllLayers.Checked := ini.ReadBool('GUI','MainWindow.cbAllLayers.Checked',MainWindow.cbAllLayers.Checked);
  MainWindow.Usepredefinedtiles1.Checked := ini.ReadBool('GUI','MainWindow.Usepredefinedtiles1.Checked',MainWindow.Usepredefinedtiles1.Checked);
end;

procedure TSettings.load_postcreate_editor_settings;
var
  ini: TMemIniFile;
begin
  ini := tmp_ini;
  // Load GUI settings for all other dialogs
  if not PreserveGUISettings then
  begin
    ini.Destroy;
    exit;
  end;
  BlockPresetDialog.Left := ini.ReadInteger('GUI','BlockPresetDialog.Left',BlockPresetDialog.Left);
  BlockPresetDialog.Top := ini.ReadInteger('GUI','BlockPresetDialog.Top',BlockPresetDialog.Top);
  SetDialog.Left := ini.ReadInteger('GUI','SetDialog.Left',SetDialog.Left);
  SetDialog.Top := ini.ReadInteger('GUI','SetDialog.Top',SetDialog.Top);
  LevelPropertiesDialog.Left := ini.ReadInteger('GUI','LevelPropertiesDialog.Left',SetDialog.Left);
  LevelPropertiesDialog.Top := ini.ReadInteger('GUI','LevelPropertiesDialog.Top',SetDialog.Top);
  ini.Destroy;
end;


procedure TSettings.save_editor_settings;
var
  ini: TMemIniFile;
begin
  ini := TMemIniFile.Create(current_dir + 'HocusEditor.ini');
  // Save preferences
  ini.WriteBool('Preferences', 'PreserveGUISettings', PreserveGUISettings);
  ini.WriteBool('Preferences', 'HidePresetWindow', HidePresetWindow);
  ini.WriteBool('Preferences', 'CheckMapErrorsOnSave', CheckMapErrorsOnSave);
  ini.WriteBool('Preferences', 'CheckMapErrorsOnTest', CheckMapErrorsOnTest);
  ini.WriteBool('Preferences', 'AlwaysAskOnQuit', AlwaysAskOnQuit);
  ini.WriteBool('Preferences', 'DrawObjectBrush', DrawObjectBrush);
  ini.WriteBool('Preferences', 'DrawPaintBrush', DrawPaintBrush);
  ini.WriteInteger('Preferences', 'TestMapDifficulty', TestMapDifficulty);
  ini.WriteBool('Preferences', 'LoadCustomImageWarn', LoadCustomImageWarn);
  ini.WriteString('Preferences','DosboxParameters',DosboxParameters);
  // Save file paths
  ini.WriteString('Paths','GameFolder',GameFolder);
  ini.WriteString('Paths','DosboxPath',DosboxPath);
  // Save GUI settings
  ini.WriteInteger('GUI','MainWindow.Left',MainWindow.Left);
  ini.WriteInteger('GUI','MainWindow.Top',MainWindow.Top);
  ini.WriteInteger('GUI','MainWindow.Width',MainWindow.Width);
  ini.WriteInteger('GUI','MainWindow.Height',MainWindow.Height);
  ini.WriteBool('GUI','MainWindow.cbAllLayers.Checked',MainWindow.cbAllLayers.Checked);
  ini.WriteBool('GUI','MainWindow.Usepredefinedtiles1.Checked',MainWindow.Usepredefinedtiles1.Checked);
  ini.WriteInteger('GUI','BlockPresetDialog.Left',BlockPresetDialog.Left);
  ini.WriteInteger('GUI','BlockPresetDialog.Top',BlockPresetDialog.Top);
  ini.WriteInteger('GUI','SetDialog.Left',SetDialog.Left);
  ini.WriteInteger('GUI','SetDialog.Top',SetDialog.Top);
  ini.WriteInteger('GUI','LevelPropertiesDialog.Left',LevelPropertiesDialog.Left);
  ini.WriteInteger('GUI','LevelPropertiesDialog.Top',LevelPropertiesDialog.Top);
  ini.UpdateFile;
  ini.Destroy;
end;

procedure TSettings.get_file_paths_from_map_filename;
begin
  // Get Game path and game executable from map filename
  {if (GameExecutable = '') or (not FileExists(GameExecutable)) then
  begin
    GamePath := ExtractFilePath(ExcludeTrailingPathDelimiter(ExtractFilePath(Map.filename)));
    GameExecutable := GamePath + 'HOCUS.EXE';
  end;}
end;

end.
