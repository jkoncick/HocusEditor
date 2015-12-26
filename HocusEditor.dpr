program HocusEditor;

uses
  Forms,
  main in 'main.pas' {MainWindow},
  set_dialog in 'set_dialog.pas' {SetDialog},
  block_preset_dialog in 'block_preset_dialog.pas' {BlockPresetDialog},
  _renderer in '_renderer.pas',
  _map in '_map.pas',
  _tileset in '_tileset.pas',
  _settings in '_settings.pas',
  _archive in '_archive.pas',
  level_props_dialog in 'level_props_dialog.pas' {LevelPropertiesDialog},
  _savegame in '_savegame.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'HocusEditor';
  Renderer := TRenderer.Create;
  Map := TMap.Create;
  Tileset := TTileset.Create;
  Settings := TSettings.Create;
  Archive := TArchive.Create;
  Application.CreateForm(TMainWindow, MainWindow);
  Application.CreateForm(TSetDialog, SetDialog);
  Application.CreateForm(TBlockPresetDialog, BlockPresetDialog);
  Application.CreateForm(TLevelPropertiesDialog, LevelPropertiesDialog);
  // All GUI settings must be loaded after all dialogs are created.
  Settings.load_postcreate_editor_settings;
  // Initialize set_dialog
  SetDialog.init;
  // Load map given as first parameter
  if ParamCount > 0 then
    MainWindow.load_map_from_file(ParamStr(1));
  Application.Run;
end.
