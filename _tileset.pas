unit _tileset;

interface

uses Graphics, Menus;

// Tileset constants
const tileset_cols = 20;
const tileset_rows = 12;
const cnt_tileset_tiles = tileset_rows * tileset_cols;
const cnt_predefined_tiles = 64;
const cnt_edge_tiles = 9;
const cnt_item_tiles = 18;
const cnt_block_preset_keys = 40; // 0-9, A-Z...
const max_block_presets = cnt_block_preset_keys;
const max_block_preset_size = 8;
const block_preset_ini_sections: array[0..1, 0..1] of String =
  (('Patterns_Background','Patterns_Foreground'),('Blocks_Background','Blocks_Foreground'));
const predefined_tiles_ini_entries: array[0..1] of String = ('Background','Foreground');

const bpgPatternPreset = 0;
const bpgBlockPreset = 1;

const num_key_rows = 4;
const num_key_cols = 10;
const block_preset_keys: array[0..num_key_rows-1, 0..num_key_cols-1] of char =
  (
    ('1', '2', '3', '4', '5', '6', '7', '8', '9', '0'),
    ('Q', 'W', 'E', 'R', 'T', 'Y', 'U', 'I', 'O', 'P'),
    ('A', 'S', 'D', 'F', 'G', 'H', 'J', 'K', 'L', ':'),
    ('Z', 'X', 'C', 'V', 'B', 'N', 'M', '<', '>', '?')
  );

// Edge tiles
const etLeft = 0;
const etTop = 1;
const etRight = 2;
const etBottom = 3;
const etTopLeft = 4;
const etTopRight = 5;
const etBottomLeft = 6;
const etBottomRight = 7;

// Tileset type definitions
type
  TBlockPreset = record
    width: word;
    height: word;
    tiles: array[0..max_block_preset_size-1, 0..max_block_preset_size-1] of byte;
  end;
  TBlockPresetPtr = ^TBlockPreset;

const empty_block_preset: TBlockPreset = (width: 0; height: 0;);

// Tileset class
type
  TTileset = class

  private
    config_changed: boolean;

  public
    tileimage_filename: String;
    tileset_config_filename: String;
    tileimage: TBitmap;
    current_tileset: integer;

    // Tileset configuration
    // First index = pattern/block presets, Second index = background/foreground presets
    predefined_tiles: array[0..1, 0..cnt_predefined_tiles-1] of byte;
    edge_tiles: array[0..cnt_edge_tiles-1] of byte;
    item_tiles: array[0..cnt_item_tiles-1] of byte;

    block_presets: array[0..1, 0..1, 0..max_block_presets-1] of TBlockPreset;
    block_preset_cnt: array[0..1, 0..1] of integer;

  public
    procedure init;
    procedure change_tileset(index: integer);
    procedure next_tileset;
    procedure use_custom_image(filename: String);

    procedure load_config;
    procedure save_config;

    function add_preset(preset: TBlockPresetPtr; group, layer: integer): integer;
    procedure remove_preset(group, layer, preset_num: integer);

    function block_key_to_index(key: word): integer;

  end;

var
  Tileset: TTileset;

implementation

uses Windows, Forms, SysUtils, Math, main, block_preset_dialog, _settings, IniFiles, Classes, Dialogs, _archive;

procedure TTileset.init;
begin
  tileimage := Graphics.TBitmap.Create;
  tileimage.PixelFormat := pf32bit;
  tileimage.Width := 320;
  tileimage.Height := 200;
  current_tileset := -1;
end;

procedure TTileset.change_tileset(index: integer);
begin
  if index = current_tileset then
    exit;
  if index >= Archive.tileset_count then
    exit;
  // Save configuration of previous tileset
  save_config;
  // Change to new tileset
  current_tileset := index;
  MainWindow.StatusBar.Panels[2].Text := Archive.tileset_info[current_tileset].name;
  // Load tileset configuration if it is different
  load_config;
  // Load tileset image
  Archive.load_tileset_image(tileimage, index);
  tileimage_filename := '';
  MainWindow.render_tileset;
end;


procedure TTileset.next_tileset;
var
  new_tileset: integer;
begin
  new_tileset := current_tileset + 1;
  if new_tileset >= Archive.tileset_count then
    new_tileset := 0;
  change_tileset(new_tileset);
end;

procedure TTileset.use_custom_image(filename: String);
begin
  current_tileset := -1;
  MainWindow.StatusBar.Panels[2].Text := 'Custom image';
  tileimage.LoadFromFile(tileimage_filename);
  tileimage_filename := filename;
  MainWindow.render_tileset;
end;

procedure TTileset.load_config;
var
  filename: String;
  ini: TMemIniFile;
  tmp_strings: TStringList;
  decoder, decoder2: TStringList;
  i, j, preset_type, preset_layer: integer;
  preset: ^TBlockPreset;
  width, height: integer;
begin
  // Reset all configuration first
  FillChar(predefined_tiles, sizeof(predefined_tiles), 0);
  FillChar(edge_tiles, sizeof(edge_tiles), 0);
  FillChar(item_tiles, sizeof(item_tiles), 0);
  FillChar(block_presets, sizeof(block_presets), 0);
  FillChar(block_preset_cnt, sizeof(block_preset_cnt), 0);
  // Try to open configuration ini file
  filename := current_dir+'/tilesets/'+Archive.tileset_info[current_tileset].name+'.ini';
  tileset_config_filename := filename;
  if not FileExists(filename) then
    exit;
  ini := TMemIniFile.Create(filename);
  tmp_strings := TStringList.Create;
  decoder := TStringList.Create;
  decoder2 := TStringList.Create;
  decoder.Delimiter := ';';
  decoder2.Delimiter := '.';
  // Load predefined tiles
  for preset_layer := 0 to 1 do
  begin
    decoder2.DelimitedText := ini.ReadString('Tiles', predefined_tiles_ini_entries[preset_layer], '');
    for i := 0 to cnt_predefined_tiles - 1 do
    begin
      if i < decoder2.Count then
        predefined_tiles[preset_layer, i] := strtoint(decoder2[i])
      else
        predefined_tiles[preset_layer, i] := 0;
    end;
  end;
  // Load edge tiles
  decoder2.DelimitedText := ini.ReadString('Tiles', 'Window_Edge', '');
  for i := 0 to cnt_edge_tiles - 1 do
  begin
    if i < decoder2.Count then
      edge_tiles[i] := strtoint(decoder2[i])
    else
      edge_tiles[i] := 0;
  end;
  // Load object tiles
  decoder2.DelimitedText := ini.ReadString('Tiles', 'Items', '');
  for i := 0 to cnt_item_tiles - 1 do
  begin
    if i < decoder2.Count then
      item_tiles[i] := strtoint(decoder2[i])
    else
      item_tiles[i] := 0;
  end;
  // Load block presets
  for preset_type := 0 to 1 do
    for preset_layer := 0 to 1 do
    begin
      ini.ReadSection(block_preset_ini_sections[preset_type, preset_layer], tmp_strings);
      block_preset_cnt[preset_type, preset_layer] := Min(tmp_strings.Count, max_block_presets);
      for i := 0 to block_preset_cnt[preset_type, preset_layer] - 1 do
      begin
        decoder2.DelimitedText := ini.ReadString(block_preset_ini_sections[preset_type, preset_layer], tmp_strings[i], '');
        if decoder2.Count < 2 then
          continue;
        width := strtoint(decoder2[0]);
        height := strtoint(decoder2[1]);
        if (width > max_block_preset_size) or (height > max_block_preset_size) then
          continue;
        preset := Addr(block_presets[preset_type, preset_layer, i]);
        preset.width := width;
        preset.height := height;
        for j := 2 to decoder2.Count - 1 do
          preset.tiles[(j - 2) mod width, (j - 2) div width] := strtoint(decoder2[j]);
      end;
    end;

  ini.Destroy;
  tmp_strings.Destroy;
  decoder.Destroy;
  decoder2.Destroy;
end;

procedure TTileset.save_config;
var
  filename: String;
  ini: TMemIniFile;
  encoder, encoder2: TStringList;
  i, j, k, preset_type, preset_layer: integer;
  preset: ^TBlockPreset;
begin
  if not config_changed then
    exit;
  if current_tileset = -1 then
    exit;
  filename := tileset_config_filename;
  ini := TMemIniFile.Create(filename);
  encoder := TStringList.Create;
  encoder2 := TStringList.Create;
  encoder.Delimiter := ';';
  encoder2.Delimiter := '.';
  // Save block presets
  for preset_type := 0 to 1 do
    for preset_layer := 0 to 1 do
    begin
      ini.EraseSection(block_preset_ini_sections[preset_type, preset_layer]);
      for i := 0 to block_preset_cnt[preset_type, preset_layer] - 1 do
      begin
        encoder2.Clear;
        preset := Addr(block_presets[preset_type, preset_layer, i]);
        encoder2.Add(inttostr(preset.width));
        encoder2.Add(inttostr(preset.height));
        for k := 0 to preset.height - 1 do
          for j := 0 to preset.width - 1 do
            encoder2.Add(inttostr(preset.tiles[j, k]));
        ini.WriteString(block_preset_ini_sections[preset_type, preset_layer], inttostr(i + 1), encoder2.delimitedText);
      end;
    end;

  config_changed := false;
  ini.UpdateFile;
  ini.Destroy;
  encoder.Destroy;
  encoder2.Destroy;
end;

function TTileset.add_preset(preset: TBlockPresetPtr; group, layer: integer): integer;
begin
  if block_preset_cnt[group, layer] = max_block_presets then
  begin
    result := -1;
    exit;
  end;
  block_presets[group, layer, block_preset_cnt[group, layer]] := preset^;
  result := block_preset_cnt[group, layer];
  Inc(block_preset_cnt[group, layer]);
  config_changed := true;
end;

procedure TTileset.remove_preset(group, layer, preset_num: integer);
var
  i: integer;
begin
  for i := preset_num to max_block_presets - 2 do
    block_presets[group, layer, i] := block_presets[group, layer, i+1];
  block_presets[group, layer, max_block_presets-1] := empty_block_preset;
  Dec(block_preset_cnt[group, layer]);
  config_changed := true;
end;

function TTileset.block_key_to_index(key: word): integer;
var
  row, col: integer;
begin
  result := -1;
  for row := 0 to num_key_rows - 1 do
    for col := 0 to num_key_cols - 1 do
      if ord(block_preset_keys[row, col]) = key then
      begin
        result := row * num_key_cols + col;
        break;
      end;
end;

end.
