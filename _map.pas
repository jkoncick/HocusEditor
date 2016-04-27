unit _map;

interface

uses Classes, _tileset;

const max_map_width = 240;
const max_map_height = 60;
const max_undo_steps = 32767;

// ============================================================================
// Object types and constants
// ============================================================================
const empty_object = $7530;

type
  TDirection = (drLeft, drUp, drRight, drDown);

type
  TObjectType = (otNone, otItem, otTeleport, otSwitch, otInsertWall, otRemoveWall, otMonster, otMonsterTrigger);
const first_object_type_value : array[0..8] of word =
  ($0, $0, $17, $21, $38, $51, $6A, $74, $16E);

// Item types
const itRuby = 0;       ivRuby = 0;
const itDiamond = 1;    ivDiamond = 1;
const itGoblet = 2;     ivGoblet = 2;
const itCrown= 3;       ivCrown = 3;
const itCrystal = 4;    ivCrystal = 5;
const itHealing = 5;    ivHealing = 4;
const itZapper = 6;     ivZapper = 7;
const itWhitePot = 7;   ivWhitePot = 10;
const itGoldPot = 8;    ivGoldPot = 9;
const itGreyPot = 9;    ivGreyPot = 16;
const itSilverKey = 10; ivSilverKey = 12;
const itGoldKey = 11;   ivGoldKey = 13;
const itSpikes = 12;    ivSpikes = 6;
const itLava = 13;      ivLava = 14;
const itMessage = 14;   ivMessage = 15;
const itTeleport = 15;
const itSilverKeyhole = 16;
const itGoldKeyhole = 17;

type
  TItemInfo = record
    name: String;
    value: word;
    score: word;
  end;

const item_info: array[0..13] of TItemInfo =
  (
    (name: 'Ruby (100 points)';    value: ivRuby;     score: 100;),
    (name: 'Diamond (250 points)'; value: ivDiamond;  score: 250;),
    (name: 'Goblet (500 points)';  value: ivGoblet;   score: 500;),
    (name: 'Crown (1000 points)';  value: ivCrown;    score: 1000;),
    (name: 'Magic Crystal';        value: ivCrystal),
    (name: 'Healing Potion';       value: ivHealing),
    (name: 'Extra Firepower';      value: ivZapper),
    (name: 'Rapid Fire';           value: ivWhitePot),
    (name: 'Super Jump';           value: ivGoldPot),
    (name: 'Laser Shots';          value: ivGreyPot),
    (name: 'Silver Key';           value: ivSilverKey),
    (name: 'Gold Key';             value: ivGoldKey),
    (name: 'Spike tile';           value: ivSpikes),
    (name: 'Lava tile';            value: ivLava)
  );

// ============================================================================
// Tile-related type definitions
// ============================================================================
type
  TMapTile = record
    layers: array[0..2] of byte;
    moreinfo: byte; // Additional layer for the editor purposes (monster group etc.)
    objects: word;
  end;

type
  TMapData = array[0..max_map_width-1, 0..max_map_height-1] of TMapTile;
  TMapDataPtr = ^TMapData;

const empty_tile: TMapTile = (layers:(255, 255, 255); moreinfo: 255; objects: empty_object;);

// ============================================================================
// Level-info type definitions
// ============================================================================
type
  TLvlPlayerInfo = record
    Null: word;
    PlayerX: word;
    PlayerY: word;
    MonsterShootDelay: word;
  end;

type
  TLvlAnimationEntry = record
    FirstIndex: byte;
    LastIndex: byte;
    AnimType: byte;
  end;

type
  TLvlAnimationInfo = record
    BackgroundTile: byte;
    SwitchDownTile: byte;
    SwitchUpTile: byte;
    ShootableTile: byte;
    AnimData: array[0..239] of TLvlAnimationEntry;
  end;

type
  TLvlMessageData = record
    PosX: word;
    PosY: word;
    Lines: array[0..9, 0..49] of char;
  end;

type
  TLvlTeleportCoordinates = record
    StartOff: word;
    EndOff: word;
  end;

type
  TLvlSwitchCoordinates = record
    SwitchType: word;
    SwitchOffsets: array[0..3] of word;
    DesiredTiles: array[0..3] of byte;
    UpperLeftX: word;
    UpperLeftY: word;
    LowerRightX: word;
    LowerRightY: word;
  end;

type
  TLvlTriggerCoordinates = record
    RequiredKey: word;
    RequiredTile: word;
    UpperLeftX: word;
    UpperLeftY: word;
    LowerRightX: word;
    LowerRightY: word;
  end;

type
  TLvlMonsterInfo = record
    SpriteSet: word;
    Health: word;
    ProjectileHSpeed: word;
    ProjectileVSpeed: word;
    ProjectileOffset: word;
    TargetPlayer: word;
    Unknown2: word;
    ShootProjectiles: word;
    Unknown3: word;
    WobblyProjectiles: word;
    Unknown4: word;
    Behavior: word;
  end;

type
  TLvlMonsterTrigger = record
    Types: array[0..7] of word;
    Offsets: array[0..7] of word;
  end;

type
  TLevelData = record
    player_info: TLvlPlayerInfo;
    animation_info: TLvlAnimationInfo;
    message_data: array[0..9] of TLvlMessageData;
    teleport_coordinates: array[0..9] of TLvlTeleportCoordinates;
    switch_coordinates: array[0..22] of TLvlSwitchCoordinates;
    insert_trigger_coordinates: array[0..24] of TLvlTriggerCoordinates;
    remove_trigger_coordinates: array[0..24] of TLvlTriggerCoordinates;
    monster_info: array[0..9] of TLvlMonsterInfo;
    monster_triggers: array[0..249] of TLvlMonsterTrigger;
  end;

type
  TLevelDataUpdateFlags = set of (ufStartPos, ufMonsters, ufMonsterTypes, ufTeleports, ufSwitches, ufInsertTrg, ufRemoveTrg, ufMsgData);

const all_level_data_update_flags: TLevelDataUpdateFlags = [ufStartPos, ufMonsters, ufMonsterTypes, ufTeleports, ufSwitches, ufInsertTrg, ufRemoveTrg, ufMsgData];

// ============================================================================
// Other type definitions
// ============================================================================
type
  TLevelExeData = record
    par_time: word;
    tileset_number: word;
    backdrop_number: word;
    music_number: word;
    elevator_tile_left: smallInt;
    elevator_tile_right: smallInt;
  end;

type
  TSelectionBlock = record
    width: word;
    height: word;
    layer: word;
    all_layers: boolean;
    data: TMapData;
  end;
  TSelectionBlockPtr = ^TSelectionBlock;

type
  TUndoEntry = record
    x, y: word;
    data: TMapTile;
    is_first: boolean;
  end;

type
  TMapStats = record
    cnt_crystals: integer;
    cnt_healths: integer;
    cnt_treasure: integer;
    cnt_silver_keys: integer;
    cnt_gold_keys: integer;
    cnt_monsters: integer;
    total_points: integer;
  end;

// ============================================================================
// Map class
// ============================================================================
type
  TMap = class

  private
    // Map variables
    map_loaded: boolean;
    map_index: integer;
    map_data: TMapData;
    map_width: word;
    map_height: word;

    // Level variables
    level_data: TLevelData;
    level_data_update_flags: TLevelDataUpdateFlags;
    level_exe_data: TLevelExeData;

    // Statistics variables
    map_stats: TMapStats;

    // Undo variables
    undo_history: array[0..max_undo_steps] of TUndoEntry;
    undo_start: integer;
    undo_max: integer;
    undo_pos: integer;
    undo_block_start: boolean;

    // Pattern variables
    cur_pattern: TBlockPreset;

    // Fill area variables
    tmp_layer: integer;
    tmp_tile_index: byte;

  public
    Property loaded: boolean read map_loaded;
    Property index: Integer read map_index;
    Property data: TMapData read map_data;
    Property leveldata: TLevelData read level_data;
    Property leveldata_dirtyflag: TLevelDataUpdateFlags read level_data_update_flags write level_data_update_flags;
    Property levelexedata: TLevelExeData read level_exe_data;
    Property width: word read map_width;
    Property height: word read map_height;
    Property stats: TMapStats read map_stats;
    Property pattern: TBlockPreset read cur_pattern;

    // Basic map manipulation procedures
  private
    procedure modify_map_tile(x,y: integer; new_tile: TMapTile);
    procedure paint_tile(x,y: integer; layer: integer; tile_index: byte);
  public
    procedure paint_tile_rect(x, y, width, height, layer: integer; tile_index: byte);

    procedure set_pattern(new_pattern: TBlockPresetPtr);
    procedure copy_pattern(x, y, width, height, layer: integer);
    procedure rotate_pattern(direction: TDirection);

    procedure copy_block(x, y, width, height, layer: integer; all_layers: boolean; block: TSelectionBlockPtr);
    procedure put_block(x, y, layer: integer; all_layers: boolean; block: TSelectionBlockPtr);
    procedure adjust_objects_in_block(step: integer; block: TSelectionBlockPtr);

    // Object manipulation methods
  public
    function get_object_type(value: word; var index: integer): TObjectType;
    procedure put_item(x, y: integer; item_type: integer);
    procedure put_object(x, y: integer; obj_type: TObjectType; index, moreinfo: integer);
    procedure remove_object(x, y: integer);
  private
    procedure remove_visual_object(x, y: integer; var new_tile: TMapTile);
    procedure unregister_object(x, y: integer);
    function register_object(x, y: integer; var new_tile: TMapTile): boolean;

    // Level data modification methods
  public
    procedure set_start_position(x, y: integer);
    procedure set_switch_target_area(group, X1, Y1, X2, Y2, switch_type: integer);
    procedure set_insert_target_area(trig_index, X1, Y1, X2, Y2: integer);
    procedure set_remove_target_area(trig_index, X1, Y1, X2, Y2: integer);
    procedure set_switch_desired_state(group, switchnum, state: integer);
    procedure set_message_text(msgnum: integer; text: TStrings);

    // Fill area procedures
  public
    procedure fill_area_start(x, y: integer; layer: integer; tile_index: byte);
  private
    procedure fill_area_step(x,y: integer; area_type: integer);

    // Undo & Redo procedures
  public
    procedure do_undo;
    procedure do_redo;
  private
    procedure reset_undo_history;

    // Procedures related to auto-smoothing edges
  private
    function check_edge_tile(x,y: integer; exact: boolean): boolean;
    procedure put_edge_tile(var xpos, ypos: integer; moveoff_x, moveoff_y: integer; edge_tile: integer);
  public
    procedure smooth_edges(x, y: integer; paint_tile_group: integer);

    // Miscellaneous procedures
    procedure compute_statistics;
    function check_errors: String;

    // Load & Save procedures
    procedure new_map(tileset_index: integer);
    procedure load_map_from_archive(index: integer);
    procedure save_map_to_archive(index: integer);
    procedure load_map_file(filename: String);
    procedure save_map_file(filename: String);

    // Map actions
    procedure set_map_size(new_width, new_height: integer);
    procedure shift_map(direction: TDirection; num_tiles: integer);

  end;

var
  Map: TMap;


implementation

uses Windows, Forms, SysUtils, Math, _renderer, _settings, _archive, _exefile, main;


// Modify map tile and save old values into undo history.
// Map data should not be modified outside this or undo/redo methods.
procedure TMap.modify_map_tile(x,y: integer; new_tile: TMapTile);
begin
  // Unregister old and register new object first
  if (map_data[x,y].objects <> new_tile.objects) or (map_data[x,y].moreinfo <> new_tile.moreinfo) then
  begin
    if not register_object(x, y, new_tile) then
      exit;
    unregister_object(x, y);
  end;
  // Save old map tile into undo history
  undo_history[undo_pos].x := x;
  undo_history[undo_pos].y := y;
  undo_history[undo_pos].data := map_data[x,y];
  undo_history[undo_pos].is_first := undo_block_start;
  undo_block_start := false;
  undo_pos := (undo_pos + 1) and max_undo_steps;
  if undo_start = undo_pos then
    undo_start := (undo_start + 1) and max_undo_steps;
  undo_max := undo_pos;
  MainWindow.Undo1.Enabled := true;
  MainWindow.Redo1.Enabled := false;
  // Modify the actual tile
  map_data[x,y] := new_tile;
  Renderer.invalidate_map_tile(x, y);
end;

procedure TMap.paint_tile(x, y, layer: integer; tile_index: byte);
var
  new_tile: TMapTile;
begin
  new_tile := map_data[x,y];
  // Replace reserved value of 254 by a tile from current pattern
  if tile_index = 254 then
  begin
    if (cur_pattern.width = 0) or (cur_pattern.height = 0) then
      exit;
    tile_index := cur_pattern.tiles[x mod cur_pattern.width, y mod cur_pattern.height];
  end;
  new_tile.layers[layer] := tile_index;
  // Modifying background layer erases objects
  if (layer = 0) and (new_tile.objects <> empty_object) then
  begin
    new_tile.objects := empty_object;
    new_tile.moreinfo := 255;
  end;
  // Modifying foreground layer modifies also hidden layer
  if (layer = 1) then
    new_tile.layers[2] := tile_index;
  modify_map_tile(x, y, new_tile);
end;

procedure TMap.paint_tile_rect(x, y, width, height, layer: integer; tile_index: byte);
var
  xx, yy: integer;
begin
  undo_block_start := true;
  Renderer.invalidate_init;
  for xx := x to x + width - 1 do
    for yy := y to y + height - 1 do
    begin
      if (xx >= map_width) or (xx < 0) or (yy >= map_height) or (yy < 0) then
        continue;
      paint_tile(xx, yy, layer, tile_index);
    end;
end;

procedure TMap.set_pattern(new_pattern: TBlockPresetPtr);
begin
  cur_pattern := new_pattern^;
end;

procedure TMap.copy_pattern(x, y, width, height, layer: integer);
var
  xx, yy: integer;
  mod_x, mod_y: integer;
  tile: byte;
begin
  cur_pattern.width := Min(width, max_block_preset_size);
  cur_pattern.height := Min(height, max_block_preset_size);
  mod_x := x mod cur_pattern.width;
  mod_y := y mod cur_pattern.height;
  for xx := 0 to cur_pattern.width - 1 do
    for yy := 0 to cur_pattern.height - 1 do
    begin
      if (x + xx < map_width) and (y + yy < map_height) then
      begin
        tile := map_data[x + xx, y + yy].layers[layer];
      end else
      begin
        tile := 255;
      end;
      cur_pattern.tiles[(xx + mod_x) mod cur_pattern.width, (yy + mod_y) mod cur_pattern.height] := tile;
    end;
end;

procedure TMap.rotate_pattern(direction: TDirection);
var
  x, y: integer;
  tmp_tile: byte;
begin
  case direction of
    drLeft:
      for y := 0 to cur_pattern.height-1 do
      begin
        tmp_tile := cur_pattern.tiles[0, y];
        for x := 0 to cur_pattern.width-2 do
          cur_pattern.tiles[x, y] := cur_pattern.tiles[x+1, y];
        cur_pattern.tiles[cur_pattern.width-1, y] := tmp_tile;
      end;
    drUp:
      for x := 0 to cur_pattern.width-1 do
      begin
        tmp_tile := cur_pattern.tiles[x, 0];
        for y := 0 to cur_pattern.height-2 do
          cur_pattern.tiles[x, y] := cur_pattern.tiles[x, y+1];
        cur_pattern.tiles[x, cur_pattern.height-1] := tmp_tile;
      end;
    drRight:
      for y := 0 to cur_pattern.height-1 do
      begin
        tmp_tile := cur_pattern.tiles[cur_pattern.width-1, y];
        for x := cur_pattern.width-1 downto 1 do
          cur_pattern.tiles[x, y] := cur_pattern.tiles[x-1, y];
        cur_pattern.tiles[0, y] := tmp_tile;
      end;
    drDown:
      for x := 0 to cur_pattern.width-1 do
      begin
        tmp_tile := cur_pattern.tiles[x, cur_pattern.height-1];
        for y := cur_pattern.height-1 downto 1 do
          cur_pattern.tiles[x, y] := cur_pattern.tiles[x, y-1];
        cur_pattern.tiles[x, 0] := tmp_tile;
      end;
  end;
end;

procedure TMap.copy_block(x, y, width, height, layer: integer; all_layers: boolean; block: TSelectionBlockPtr);
var
  xx, yy: integer;
  map_tile: TMapTile;
begin
  block.width := width;
  block.height := height;
  block.layer := layer;
  block.all_layers := all_layers;
  for xx := 0 to width - 1 do
    for yy := 0 to height - 1 do
    begin
      map_tile := empty_tile;
      if (x + xx < map_width) and (y + yy < map_height) then
      begin
        map_tile := map_data[x + xx, y + yy];
      end;
      block.data[xx,yy] := map_tile;
    end;
end;

procedure TMap.put_block(x, y, layer: integer; all_layers: boolean; block: TSelectionBlockPtr);
var
  xx, yy: integer;
begin
  undo_block_start := true;
  Renderer.invalidate_init;
  for xx := 0 to block.width - 1 do
    for yy := 0 to block.height - 1 do
      if (x + xx < map_width) and (x + xx >= 0) and (y + yy < map_height) and (y + yy >= 0) then
      begin
        if all_layers then
        begin
          modify_map_tile(x + xx, y + yy, block.data[xx, yy])
        end else
          paint_tile(x + xx, y + yy, layer, block.data[xx, yy].layers[layer]);
      end;
end;

procedure TMap.adjust_objects_in_block(step: integer; block: TSelectionBlockPtr);
var
  x, y: integer;
  obj: word;
  obj_type: TObjectType;
  obj_index: integer;
begin
  for x := 0 to block.width - 1 do
    for y := 0 to block.height - 1 do
    begin
      obj := block.data[x,y].objects;
      obj_type := get_object_type(obj, obj_index);
      if obj_type = otMonster then
        block.data[x,y].moreinfo := Min(Max(block.data[x,y].moreinfo + step, 0), Length(level_data.monster_triggers) - 1);
      if obj_type = otMonsterTrigger then
        block.data[x,y].objects := Min(Max(obj_index + step, 0), Length(level_data.monster_triggers) - 1) + first_object_type_value[ord(otMonsterTrigger)];
      if obj_type = otSwitch then
        block.data[x,y].objects := Min(Max(obj_index + step, 0), Length(level_data.switch_coordinates) - 1) + first_object_type_value[ord(otSwitch)];
    end;
end;

function TMap.get_object_type(value: word; var index: integer): TObjectType;
var
  i: integer;
begin
  result := otNone;
  index := value;
  for i := ord(otItem) to ord(otMonsterTrigger) do
  begin
    if value < first_object_type_value[i+1] then
    begin
      index := value - first_object_type_value[i];
      result := TObjectType(i);
      exit;
    end;
  end;
end;

procedure TMap.put_item(x, y, item_type: integer);
var
  new_tile: TMapTile;
begin
  undo_block_start := true;
  Renderer.invalidate_init;
  if (item_type < 0) or (item_type >= Length(item_info)) then
    exit;
  new_tile := map_data[x, y];
  new_tile.objects := item_info[item_type].value;
  new_tile.moreinfo := 255;
  // Auto-set background tile to respective item tile
  if item_type < itSpikes then
    new_tile.layers[0] := Tileset.item_tiles[item_type];
  // Modify tile
  modify_map_tile(x, y, new_tile);
end;

procedure TMap.put_object(x, y: integer; obj_type: TObjectType; index, moreinfo: integer);
var
  new_tile: TMapTile;
begin
  undo_block_start := true;
  Renderer.invalidate_init;
  new_tile := map_data[x, y];
  new_tile.objects := first_object_type_value[ord(obj_type)] + index;
  new_tile.moreinfo := moreinfo;
  // Auto-set background tile to respective tile:
  remove_visual_object(x, y, new_tile);
  case obj_type of
    otTeleport:
      // Teleport potion
      if moreinfo = 0 then new_tile.layers[0] := Tileset.item_tiles[itTeleport];
    otSwitch:
      begin
        // Down/Up switch
        new_tile.layers[0] := IfThen(moreinfo = 0, level_data.animation_info.SwitchDownTile, level_data.animation_info.SwitchUpTile);
        // Desired state is opposite from the initial state
        new_tile.moreinfo := IfThen(moreinfo = 0, 1, 0);
      end;
    otInsertWall:
      // Silver/Gold keyhole
      if moreinfo = 1 then new_tile.layers[0] := Tileset.item_tiles[itSilverKeyhole] else
      if moreinfo = 2 then new_tile.layers[0] := Tileset.item_tiles[itGoldKeyhole];
    otRemoveWall:
      // Silver/Gold keyhole
      if moreinfo = 1 then new_tile.layers[0] := Tileset.item_tiles[itSilverKeyhole] else
      if moreinfo = 2 then new_tile.layers[0] := Tileset.item_tiles[itGoldKeyhole];
  end;
  // Modify tile
  modify_map_tile(x, y, new_tile);
end;

procedure TMap.remove_object(x, y: integer);
var
  new_tile: TMapTile;
begin
  undo_block_start := true;
  Renderer.invalidate_init;
  new_tile := map_data[x, y];
  new_tile.objects := empty_object;
  new_tile.moreinfo := 255;
  // Auto-set background tile to blank tile
  remove_visual_object(x, y, new_tile);
  // Modify tile
  modify_map_tile(x, y, new_tile);
end;

procedure TMap.remove_visual_object(x, y: integer; var new_tile: TMapTile);
var
  obj: word;
  moreinfo: byte;
  obj_type: TObjectType;
  index: integer;
begin
  obj := map_data[x, y].objects;
  moreinfo := map_data[x, y].moreinfo;
  obj_type := get_object_type(obj, index);
  if ((obj_type = otItem) and (obj <> ivSpikes) and (obj <> ivLava) and (obj <> ivMessage))
    or ((obj_type = otTeleport) and (moreinfo = 0))
    or (obj_type = otSwitch)
    or ((obj_type = otInsertWall) and (moreinfo > 0))
    or ((obj_type = otRemoveWall) and (moreinfo > 0))
  then
    new_tile.layers[0] := level_data.animation_info.BackgroundTile;
end;

procedure TMap.unregister_object(x, y: integer);
var
  obj: word;
  moreinfo: byte;
  obj_type: TObjectType;
  obj_index: integer;
  i, j: integer;
  len: integer;
begin
  obj := map_data[x, y].objects;
  moreinfo := map_data[x, y].moreinfo;
  obj_type := get_object_type(obj, obj_index);
  case obj_type of
    otMonster:
      begin
        len := Length(level_data.monster_triggers[moreinfo].Offsets);
        for i := 0 to len - 1 do
        begin
          if (x = level_data.monster_triggers[moreinfo].Offsets[i] mod max_map_width) and
            (y = level_data.monster_triggers[moreinfo].Offsets[i] div max_map_width) then
          begin
            for j := i to len - 2 do
            begin
              level_data.monster_triggers[moreinfo].Offsets[j] := level_data.monster_triggers[moreinfo].Offsets[j+1];
              level_data.monster_triggers[moreinfo].Types[j] := level_data.monster_triggers[moreinfo].Types[j+1];
            end;
            level_data.monster_triggers[moreinfo].Offsets[len - 1] := 0;
            level_data.monster_triggers[moreinfo].Types[len - 1] := 65535;
            level_data_update_flags := level_data_update_flags + [ufMonsters];
            break;
          end;
        end;
      end;
    otTeleport:
      begin
        if moreinfo = 0 then
          level_data.teleport_coordinates[obj_index].StartOff := 0
        else
          level_data.teleport_coordinates[obj_index].EndOff := 0;
        level_data_update_flags := level_data_update_flags + [ufTeleports];
      end;
    otSwitch:
      begin
        len := Length(level_data.switch_coordinates[obj_index].SwitchOffsets);
        for i := 0 to len - 1 do
        begin
          if (x = level_data.switch_coordinates[obj_index].SwitchOffsets[i] mod max_map_width) and
            (y = level_data.switch_coordinates[obj_index].SwitchOffsets[i] div max_map_width) then
          begin
            for j := i to len - 2 do
            begin
              level_data.switch_coordinates[obj_index].SwitchOffsets[j] := level_data.switch_coordinates[obj_index].SwitchOffsets[j+1];
              level_data.switch_coordinates[obj_index].DesiredTiles[j] := level_data.switch_coordinates[obj_index].DesiredTiles[j+1];
            end;
            level_data.switch_coordinates[obj_index].SwitchOffsets[len - 1] := 65535;
            level_data.switch_coordinates[obj_index].DesiredTiles[len - 1] := 0;
            level_data_update_flags := level_data_update_flags + [ufSwitches];
            break;
          end;
        end;
      end;
    otItem:
      // Message
      if obj_index = ivMessage then
      begin
        if (level_data.message_data[moreinfo].PosX = x) and (level_data.message_data[moreinfo].PosY = y) then
        begin
          level_data.message_data[moreinfo].PosX := 65535;
          level_data.message_data[moreinfo].PosY := 65535;
          level_data_update_flags := level_data_update_flags + [ufMsgData];
        end;
      end;
  end;
end;

function TMap.register_object(x, y: integer; var new_tile: TMapTile): boolean;
var
  obj: word;
  moreinfo: byte;
  obj_type: TObjectType;
  obj_index: integer;
  pos_x, pos_y: integer;
  i: integer;
  free_slot: integer;
begin
  result := true;
  obj := new_tile.objects;
  moreinfo := new_tile.moreinfo;
  obj_type := get_object_type(obj, obj_index);
  case obj_type of
    otMonster:
      begin
        // Find free monster slot
        free_slot := -1;
        for i := 0 to Length(level_data.monster_triggers[moreinfo].Types) - 1 do
        begin
          if level_data.monster_triggers[moreinfo].Types[i] = 65535 then
          begin
            free_slot := i;
            break;
          end;
        end;
        if free_slot <> -1 then
        begin
          // Use that slot
          level_data.monster_triggers[moreinfo].Offsets[free_slot] := y * max_map_width + x;
          level_data.monster_triggers[moreinfo].Types[free_slot] := obj_index;
          level_data_update_flags := level_data_update_flags + [ufMonsters];
        end else
        begin
          // No free slot found, disable monster insertion
          result := false;
        end;
      end;
    otTeleport:
      begin
        // Remove old teleport first, then set coordinates of the new teleport
        if moreinfo = 0 then
        begin
          pos_x := level_data.teleport_coordinates[obj_index].StartOff mod max_map_width;
          pos_y := level_data.teleport_coordinates[obj_index].StartOff div max_map_width;
          if (pos_x <> 0) or (pos_y <> 0) then
            remove_object(pos_x, pos_y);
          level_data.teleport_coordinates[obj_index].StartOff := y * max_map_width + x;
        end else
        begin
          pos_x := level_data.teleport_coordinates[obj_index].EndOff mod max_map_width;
          pos_y := level_data.teleport_coordinates[obj_index].EndOff div max_map_width;
          if (pos_x <> 0) or (pos_y <> 0) then
            remove_object(pos_x, pos_y);
          level_data.teleport_coordinates[obj_index].EndOff := y * max_map_width + x;
        end;
        level_data_update_flags := level_data_update_flags + [ufTeleports];
      end;
    otSwitch:
      begin
        // Find free switch slot
        free_slot := -1;
        for i := 0 to Length(level_data.switch_coordinates[obj_index].SwitchOffsets) - 1 do
        begin
          if level_data.switch_coordinates[obj_index].SwitchOffsets[i] = 65535 then
          begin
            free_slot := i;
            break;
          end;
        end;
        if free_slot <> -1 then
        begin
          // Use that slot
          level_data.switch_coordinates[obj_index].SwitchOffsets[free_slot] := y * max_map_width + x;
          level_data.switch_coordinates[obj_index].DesiredTiles[free_slot] := IfThen(moreinfo = 0, level_data.animation_info.SwitchDownTile, level_data.animation_info.SwitchUpTile);
          level_data_update_flags := level_data_update_flags + [ufSwitches];
        end else
        begin
          // No free slot found, disable switch insertion
          result := false;
        end;
      end;
    otInsertWall:
      begin
        level_data.insert_trigger_coordinates[obj_index].RequiredKey := moreinfo;
        level_data.insert_trigger_coordinates[obj_index].RequiredTile := new_tile.layers[0];
        level_data_update_flags := level_data_update_flags + [ufInsertTrg];
      end;
    otRemoveWall:
      begin
        level_data.remove_trigger_coordinates[obj_index].RequiredKey := moreinfo;
        level_data.remove_trigger_coordinates[obj_index].RequiredTile := new_tile.layers[0];
        level_data_update_flags := level_data_update_flags + [ufRemoveTrg];
      end;
    otItem:
      // Message
      if obj_index = ivMessage then
      begin
        if level_data.message_data[moreinfo].PosX = 65535 then
        begin
          level_data.message_data[moreinfo].PosX := x;
          level_data.message_data[moreinfo].PosY := y;
          level_data_update_flags := level_data_update_flags + [ufMsgData];
        end else
        begin
          // Allow placing additional message tile only one tile to the left or right from the message position
          if ((level_data.message_data[moreinfo].PosX <> (x+1)) and (level_data.message_data[moreinfo].PosX <> (x-1))) or
            (level_data.message_data[moreinfo].PosY <> y) then
            result := false;
        end;
      end;
  end;
end;

procedure TMap.set_start_position(x, y: integer);
begin
  Renderer.invalidate_init;
  Renderer.invalidate_map_tile(level_data.player_info.PlayerX, level_data.player_info.PlayerY);
  Renderer.invalidate_map_tile(level_data.player_info.PlayerX+1, level_data.player_info.PlayerY+1);
  level_data.player_info.PlayerX := x;
  level_data.player_info.PlayerY := y;
  level_data_update_flags := level_data_update_flags + [ufStartPos];
  Renderer.invalidate_map_tile(level_data.player_info.PlayerX, level_data.player_info.PlayerY);
end;

procedure TMap.set_switch_target_area(group, X1, Y1, X2, Y2, switch_type: integer);
var
  switch_coords: ^TLvlSwitchCoordinates;
begin
  switch_coords := Addr(level_data.switch_coordinates[group]);
  Renderer.invalidate_init;
  // Invalidate old area so that the old marker is redrawn
  if (switch_coords.UpperLeftX <> 0) or (switch_coords.UpperLeftY <> 0) then
  begin
    Renderer.invalidate_map_tile(Max(switch_coords.UpperLeftX-1, 0), Max(switch_coords.UpperLeftY-1, 0));
    Renderer.invalidate_map_tile(Min(switch_coords.LowerRightX+2, map_width - 1), switch_coords.LowerRightY);
  end;
  // Update target area and switch type
  switch_coords.SwitchType := switch_type;
  switch_coords.UpperLeftX := X1;
  switch_coords.UpperLeftY := Y1;
  switch_coords.LowerRightX := X2;
  switch_coords.LowerRightY := Y2;
  level_data_update_flags := level_data_update_flags + [ufSwitches];
  // Invalidate a tile so that new marker is drawn
  Renderer.invalidate_map_tile(X1, Y1);
end;

procedure TMap.set_insert_target_area(trig_index, X1, Y1, X2, Y2: integer);
var
  trig_coords: ^TLvlTriggerCoordinates;
begin
  trig_coords := Addr(level_data.insert_trigger_coordinates[trig_index]);
  Renderer.invalidate_init;
  // Invalidate old area so that the old marker is redrawn
  if ((trig_coords.UpperLeftX <> 0) or (trig_coords.UpperLeftY <> 0)) and (trig_coords.UpperLeftX <> 65535) then
  begin
    Renderer.invalidate_map_tile(Max(trig_coords.UpperLeftX-1, 0), Max(trig_coords.UpperLeftY-1, 0));
    Renderer.invalidate_map_tile(Min(trig_coords.LowerRightX+2, map_width - 1), trig_coords.LowerRightY);
  end;
  // Update target area
  trig_coords.UpperLeftX := X1;
  trig_coords.UpperLeftY := Y1;
  trig_coords.LowerRightX := X2;
  trig_coords.LowerRightY := Y2;
  level_data_update_flags := level_data_update_flags + [ufInsertTrg];
  // Invalidate a tile so that new marker is drawn
  Renderer.invalidate_map_tile(X1, Y1);
end;

procedure TMap.set_remove_target_area(trig_index, X1, Y1, X2, Y2: integer);
var
  trig_coords: ^TLvlTriggerCoordinates;
begin
  trig_coords := Addr(level_data.remove_trigger_coordinates[trig_index]);
  Renderer.invalidate_init;
  // Invalidate old area so that the old marker is redrawn
  if ((trig_coords.UpperLeftX <> 0) or (trig_coords.UpperLeftY <> 0)) and (trig_coords.UpperLeftX <> 65535) then
  begin
    Renderer.invalidate_map_tile(Max(trig_coords.UpperLeftX-1, 0), Max(trig_coords.UpperLeftY-1, 0));
    Renderer.invalidate_map_tile(Min(trig_coords.LowerRightX+2, map_width - 1), trig_coords.LowerRightY);
  end;
  // Update target area
  trig_coords.UpperLeftX := X1;
  trig_coords.UpperLeftY := Y1;
  trig_coords.LowerRightX := X2;
  trig_coords.LowerRightY := Y2;
  level_data_update_flags := level_data_update_flags + [ufRemoveTrg];
  // Invalidate a tile so that new marker is drawn
  Renderer.invalidate_map_tile(X1, Y1);
end;

procedure TMap.set_switch_desired_state(group, switchnum, state: integer);
var
  tile_x, tile_y: integer;
begin
  Renderer.invalidate_init;
  level_data.switch_coordinates[group].DesiredTiles[switchnum] := IfThen(state = 0, level_data.animation_info.SwitchDownTile, level_data.animation_info.SwitchUpTile);
  tile_x := level_data.switch_coordinates[group].SwitchOffsets[switchnum] mod max_map_width;
  tile_y := level_data.switch_coordinates[group].SwitchOffsets[switchnum] div max_map_width;
  map_data[tile_x, tile_y].moreinfo := state;
  Renderer.invalidate_map_tile(tile_x, tile_y);
  level_data_update_flags := level_data_update_flags + [ufSwitches];
end;

procedure TMap.set_message_text(msgnum: integer; text: TStrings);
var
  i: integer;
  msg_data: ^TLvlMessageData;
begin
  msg_data := Addr(level_data.message_data[msgnum]);
  FillChar(msg_data.Lines, sizeof(msg_data.Lines), 0);
  for i := 0 to Min(text.Count, Length(msg_data.Lines)) - 1 do
    Move(PChar(text[i])^, msg_data.Lines[i], Min(Length(text[i]), Length(msg_data.Lines[i])));
  level_data_update_flags := level_data_update_flags + [ufMsgData];
end;

procedure TMap.fill_area_start(x, y: integer; layer: integer; tile_index: byte);
var
  tmp_pos: integer;
begin
  // Undo the action which was made by first click
  tmp_pos := undo_pos;
  repeat
    tmp_pos := (tmp_pos - 1) and max_undo_steps
  until undo_history[tmp_pos].is_first;
  if (undo_history[tmp_pos].x = x) and (undo_history[tmp_pos].y = y) then
    do_undo;
  // Fill area
  undo_block_start := true;
  Renderer.invalidate_init;
  tmp_layer := layer;
  tmp_tile_index := tile_index;
  fill_area_step(x, y, map_data[x,y].layers[layer]);
end;

procedure TMap.fill_area_step(x, y: integer; area_type: integer);
begin
  //if Tileset.get_fill_area_type(map_data[x,y], map_data[x,y]) <> area_type then
  //  exit;
  if map_data[x,y].layers[tmp_layer] <> area_type then
    exit;
  paint_tile(x, y, tmp_layer, tmp_tile_index);
  if map_data[x,y].layers[tmp_layer] = area_type then
    exit;
  //if Tileset.get_fill_area_type(map_data[x,y], map_data[x,y]) = area_type then
  //  exit;
  if x > 0 then
    fill_area_step(x-1, y, area_type);
  if x < (map_width - 1) then
    fill_area_step(x+1, y, area_type);
  if y > 0 then
    fill_area_step(x, y-1, area_type);
  if y < (map_height - 1) then
    fill_area_step(x, y+1, area_type);
end;


procedure TMap.do_undo;
var
  tmp_data: TMapTile;
  x, y: word;
begin
  if undo_pos = undo_start then
    exit;
  repeat
    undo_pos := (undo_pos - 1) and max_undo_steps;
    x := undo_history[undo_pos].x;
    y := undo_history[undo_pos].y;
    tmp_data := map_data[x, y];
    unregister_object(x, y);
    map_data[x, y] := undo_history[undo_pos].data;
    register_object(x, y, map_data[x, y]);
    undo_history[undo_pos].data := tmp_data;
    Renderer.invalidate_map_tile(x, y);
  until undo_history[undo_pos].is_first or (undo_pos = undo_start);
  if undo_pos = undo_start then
    MainWindow.Undo1.Enabled := false;
  MainWindow.Redo1.Enabled := true;
  compute_statistics;
end;

procedure TMap.do_redo;
var
  tmp_data: TMapTile;
  x, y: word;
begin
  if undo_pos = undo_max then
    exit;
  repeat
    x := undo_history[undo_pos].x;
    y := undo_history[undo_pos].y;
    tmp_data := map_data[x, y];
    unregister_object(x, y);
    map_data[x, y] := undo_history[undo_pos].data;
    register_object(x, y, map_data[x, y]);
    undo_history[undo_pos].data := tmp_data;
    Renderer.invalidate_map_tile(x, y);
    undo_pos := (undo_pos + 1) and max_undo_steps;
  until undo_history[undo_pos].is_first or (undo_pos = undo_max);
  if undo_pos = undo_max then
    MainWindow.Redo1.Enabled := false;
  MainWindow.Undo1.Enabled := true;
  compute_statistics;
end;

procedure TMap.reset_undo_history;
begin
  MainWindow.Undo1.Enabled := false;
  MainWindow.Redo1.Enabled := false;
  undo_start := 0;
  undo_max := 0;
  undo_pos := 0;
end;


function TMap.check_edge_tile(x, y: integer; exact: boolean): boolean;
begin
  if exact then
    result := map.map_data[x, y].layers[0] = level_data.animation_info.BackgroundTile
  else
    result := map.map_data[x, y].layers[0] <> 255;
end;

procedure TMap.put_edge_tile(var xpos, ypos: integer; moveoff_x, moveoff_y, edge_tile: integer);
var
  tile_index: byte;
begin
  tile_index := Tileset.edge_tiles[edge_tile];
  // We cannot place the edge tile immediately physically into map,
  // because it would interfere with checks for tiles around the following tile.
  // Instead, we exploit undo feature for this purpose: we store all changes
  // into history and in the end we apply the changes (like doing redo)
  undo_history[undo_max].x := xpos;
  undo_history[undo_max].y := ypos;
  undo_history[undo_max].data := map_data[xpos, ypos];
  undo_history[undo_max].data.layers[0] := tile_index;
  undo_history[undo_max].is_first := false;
  undo_max := (undo_max + 1) and max_undo_steps;
  // Finally move to next position (anticlockwise direction)
  xpos := xpos + moveoff_x;
  ypos := ypos + moveoff_y;
end;

procedure TMap.smooth_edges(x, y: integer; paint_tile_group: integer);
var
  start_x, start_y: integer;
  sum: integer;
  steps: integer;
begin
  start_x := x;
  start_y := y;
  Renderer.invalidate_init;
  undo_max := undo_pos;
  steps := 0;
  // Start smoothing edge from starting point (where user shift-clicked)
  while check_edge_tile(x, y, true) do
  begin
    // Check for all 8 tiles around current tile to determine the direction of edge
    sum := 0;
    if check_edge_tile(x,   y-1, false) then sum := sum + 1;   // 16 1 32
    if check_edge_tile(x-1, y  , false) then sum := sum + 2;   //  2 X 4
    if check_edge_tile(x+1, y  , false) then sum := sum + 4;   // 64 8 128
    if check_edge_tile(x  , y+1, false) then sum := sum + 8;
    if check_edge_tile(x-1, y-1, false) then sum := sum + 16;
    if check_edge_tile(x+1, y-1, false) then sum := sum + 32;
    if check_edge_tile(x-1, y+1, false) then sum := sum + 64;
    if check_edge_tile(x+1, y+1, false) then sum := sum + 128;
    // Transform current tile into edge tile and move to following tile
    case (sum and 15) of
       7: begin // down
            put_edge_tile(x,y,1,0,etTop);
        end;
      11: begin // right
            put_edge_tile(x,y,0,-1,etLeft);
        end;
      14: begin // up
            put_edge_tile(x,y,-1,0,etBottom);
        end;
      13: begin // left
            put_edge_tile(x,y,0,1,etRight);
        end;
       3: begin // down-right corner
            put_edge_tile(x,y,0,-1,etTopLeft);
        end;
      10: begin // up-right corner
            put_edge_tile(x,y,-1,0,etBottomLeft);
        end;
      12: begin // up-left corner
            put_edge_tile(x,y,0,1,etBottomRight);
        end;
       5: begin // down-left corner
            put_edge_tile(x,y,1,0,etTopRight);
        end;
      15: begin // inner curves
        case sum of
          239: put_edge_tile(x,y,-1,0,etBottomRight); // down-right curve
          191: put_edge_tile(x,y,0,1,etTopRight);  // up-right curve
          127: put_edge_tile(x,y,1,0,etTopLeft);  // up-left curve
          223: put_edge_tile(x,y,0,-1,etBottomLeft); // down-left curve
          else break; // Invalid combination - end
        end;
        end;
      else break; // Invalid combination - end
    end;
    // End if we got back into starting point
    if (x = start_x) and (y = start_y) then
      break;
    // End if we got outside the map
    if (x < 0) or (y < 0) or (x >= map_width) or (y >= map_height) then
      break;
    // Sometimes the algorithm may end up in infinite loop. This is to prevent it.
    inc(steps);
    if steps > 1000 then
      break;
  end;
  undo_history[undo_pos].is_first := true;
  // Finally put smoothed edges on map - apply all changes we stored into undo history
  do_redo;
end;


procedure TMap.compute_statistics;
var
  x, y: integer;
  obj: word;
begin
  // Process map object layer and count statistics of several types of objects
  map_stats.cnt_crystals := 0;
  map_stats.cnt_healths := 0;
  map_stats.cnt_treasure := 0;
  map_stats.cnt_silver_keys := 0;
  map_stats.cnt_gold_keys := 0;
  map_stats.cnt_monsters := 0;
  map_stats.total_points := 0;
  for y := 0 to map_height - 1 do
    for x := 0 to map_width -1 do
    begin
      obj := map_data[x,y].objects;
      case obj of
        ivRuby:    begin Inc(map_stats.cnt_treasure); Inc(map_stats.total_points, item_info[itRuby].score); end;
        ivDiamond: begin Inc(map_stats.cnt_treasure); Inc(map_stats.total_points, item_info[itDiamond].score); end;
        ivGoblet:  begin Inc(map_stats.cnt_treasure); Inc(map_stats.total_points, item_info[itGoblet].score); end;
        ivCrown:   begin Inc(map_stats.cnt_treasure); Inc(map_stats.total_points, item_info[itCrown].score); end;
        ivHealing: Inc(map_stats.cnt_healths);
        ivCrystal: Inc(map_stats.cnt_crystals);
        ivSilverKey: Inc(map_stats.cnt_silver_keys);
        ivGoldKey: Inc(map_stats.cnt_gold_keys);
      end;
      if (obj >= $6A) and (obj <= $73) then
        Inc(map_stats.cnt_monsters);
    end;
  MainWindow.show_statistics;
end;

function TMap.check_errors: String;
begin
  //if map_stats.cnt_crystals = 0 then
  //begin
  //  result := 'You must place at least one Magic Crystal.';
  //  exit;
  //end;
  result := '';
end;

procedure TMap.new_map(tileset_index: integer);
var
  x, y: integer;
  i, j: integer;
  level_index: integer;
begin
  // Initialize map layers
  map_width := max_map_width;
  map_height := max_map_height;
  for x := 0 to map_width - 1 do
    for y := 0 to map_height - 1 do
      map_data[x,y] := empty_tile;
  // Initialize level data
  FillChar(level_data, sizeof(level_data), 0);
  level_data.player_info.MonsterShootDelay := 45; 
  for i := 0 to Length(level_data.monster_triggers) - 1 do
    for j := 0 to Length(level_data.monster_triggers[i].Types) - 1 do
      level_data.monster_triggers[i].Types[j] := 65535;
  for i := 0 to Length(level_data.switch_coordinates) - 1 do
    for j := 0 to Length(level_data.switch_coordinates[i].SwitchOffsets) - 1 do
      level_data.switch_coordinates[i].SwitchOffsets[j] := 65535;
  for i := 0 to Length(level_data.insert_trigger_coordinates) - 1 do
  begin
    level_data.insert_trigger_coordinates[i].UpperLeftX := 65535;
    level_data.insert_trigger_coordinates[i].UpperLeftY := 65535;
    level_data.insert_trigger_coordinates[i].LowerRightX := 65535;
    level_data.insert_trigger_coordinates[i].LowerRightY := 65535;
  end;
  for i := 0 to Length(level_data.remove_trigger_coordinates) - 1 do
  begin
    level_data.remove_trigger_coordinates[i].UpperLeftX := 65535;
    level_data.remove_trigger_coordinates[i].UpperLeftY := 65535;
    level_data.remove_trigger_coordinates[i].LowerRightX := 65535;
    level_data.remove_trigger_coordinates[i].LowerRightY := 65535;
  end;
  for i := 0 to Length(level_data.message_data) - 1 do
  begin
    level_data.message_data[i].PosX := 65535;
    level_data.message_data[i].PosY := 65535;
  end;
  // Load animation and monster info from the first level using selected tileset
  level_index := Archive.get_first_level_by_tileset(tileset_index);
  if level_index <> -1 then
  begin
    Archive.load_level_data(Addr(level_data.animation_info), level_index, 1);
    Archive.load_level_data(Addr(level_data.monster_info), level_index, 7);
  end;
  // Finalize it
  map_loaded := true;
  map_index := -1;
  reset_undo_history;
  compute_statistics;
  level_data_update_flags := all_level_data_update_flags;
  // Change tileset respectively
  Tileset.change_tileset(level_exe_data.tileset_number);
end;

procedure TMap.load_map_from_archive(index: integer);
var
  layer_buffer: array[0..max_map_height-1, 0..max_map_width-1] of byte;
  objects_buffer: array[0..max_map_height-1, 0..max_map_width-1] of word;
  x, y, i, j: integer;
  obj_type: TObjectType;
  obj_index: integer;
  switch_coords: ^TLvlSwitchCoordinates;
begin
  map_width := max_map_width;
  map_height := max_map_height;
  Archive.open_archive(fmOpenRead, true);
  // Load background layer
  Archive.load_level_data(Addr(layer_buffer), index, 9);
  for x := 0 to max_map_width - 1 do
    for y := 0 to max_map_height - 1 do
      map_data[x, y].layers[0] := layer_buffer[y, x];
  // Load foreground layer
  Archive.load_level_data(Addr(layer_buffer), index, 10);
  for x := 0 to max_map_width - 1 do
    for y := 0 to max_map_height - 1 do
      map_data[x, y].layers[1] := layer_buffer[y, x];
  // Load hidden layer
  Archive.load_level_data(Addr(layer_buffer), index, 11);
  for x := 0 to max_map_width - 1 do
    for y := 0 to max_map_height - 1 do
      map_data[x, y].layers[2] := layer_buffer[y, x];
  // Load objects layer
  Archive.load_level_data(Addr(objects_buffer), index, 12);
  for x := 0 to max_map_width - 1 do
    for y := 0 to max_map_height - 1 do
      map_data[x, y].objects := objects_buffer[y, x];
  // Load level data
  Archive.load_level_data(Addr(level_data.player_info), index, 0);
  Archive.load_level_data(Addr(level_data.animation_info), index, 1);
  Archive.load_level_data(Addr(level_data.message_data), index, 2);
  Archive.load_level_data(Addr(level_data.teleport_coordinates), index, 3);
  Archive.load_level_data(Addr(level_data.switch_coordinates), index, 4);
  Archive.load_level_data(Addr(level_data.insert_trigger_coordinates), index, 5);
  Archive.load_level_data(Addr(level_data.remove_trigger_coordinates), index, 6);
  Archive.load_level_data(Addr(level_data.monster_info), index, 7);
  Archive.load_level_data(Addr(level_data.monster_triggers), index, 8);
  Archive.close_archive(true);
  // Load level data from exe
  ExeFile.get_level_data(index, level_exe_data);
  // Fill moreinfo layer
  for x := 0 to max_map_width - 1 do
    for y := 0 to max_map_height - 1 do
    begin
      map_data[x, y].moreinfo := 255;
      obj_type := get_object_type(map_data[x, y].objects, obj_index);
      // Teleport start/destination
      if obj_type = otTeleport then
        map_data[x, y].moreinfo := IfThen(
          (level_data.teleport_coordinates[obj_index].StartOff mod max_map_width = x) and
          (level_data.teleport_coordinates[obj_index].StartOff div max_map_width = y),
          0, 1);
      // Switches
      if obj_type = otSwitch then
      begin
        switch_coords := Addr(level_data.switch_coordinates[obj_index]);
        for i := 0 to Length(switch_coords.SwitchOffsets) - 1 do
        begin
          if (switch_coords.SwitchOffsets[i] mod max_map_width = x) and (switch_coords.SwitchOffsets[i] div max_map_width = y) then
          begin
            map_data[x, y].moreinfo := IfThen(switch_coords.DesiredTiles[i] = level_data.animation_info.SwitchDownTile, 0, 1);
            break;
          end;
        end;
      end;
      // Insert wall triggers
      if obj_type = otInsertWall then
        map_data[x, y].moreinfo := level_data.insert_trigger_coordinates[obj_index].RequiredKey;
      // Remove wall triggers
      if obj_type = otRemoveWall then
        map_data[x, y].moreinfo := level_data.remove_trigger_coordinates[obj_index].RequiredKey;
    end;
  // Monster groups
  for i := 0 to Length(level_data.monster_triggers) - 1 do
    for j := 0 to Length(level_data.monster_triggers[i].Offsets) do
    begin
      if level_data.monster_triggers[i].Types[j] = 65535 then
        continue;
      x := level_data.monster_triggers[i].Offsets[j] mod max_map_width;
      y := level_data.monster_triggers[i].Offsets[j] div max_map_width;
      map_data[x, y].moreinfo := i;
    end;
  // Message indexes
  for i := 0 to Length(level_data.message_data) - 1 do
  begin
    if level_data.message_data[i].PosX = 65535 then
      continue;
    x := level_data.message_data[i].PosX;
    y := level_data.message_data[i].PosY;
    if (map_data[x, y].objects = ivMessage) then
      map_data[x, y].moreinfo := i;
    if (x+1 < max_map_width) and (map_data[x+1, y].objects = ivMessage) then
      map_data[x+1, y].moreinfo := i;
    if (x-1 >= 0) and (map_data[x-1, y].objects = ivMessage) then
      map_data[x-1, y].moreinfo := i;
  end;
  // Finalize it
  map_loaded := true;
  map_index := index;
  reset_undo_history;
  compute_statistics;
  level_data_update_flags := all_level_data_update_flags;
  // Change tileset respectively
  Tileset.change_tileset(level_exe_data.tileset_number);
end;

procedure TMap.save_map_to_archive(index: integer);
var
  layer_buffer: array[0..max_map_height-1, 0..max_map_width-1] of byte;
  objects_buffer: array[0..max_map_height-1, 0..max_map_width-1] of word;
  x, y: integer;
begin
  Archive.open_archive(fmOpenReadWrite, true);
  // Save background layer
  for x := 0 to max_map_width - 1 do
    for y := 0 to max_map_height - 1 do
      layer_buffer[y, x] := map_data[x, y].layers[0];
  Archive.save_level_data(Addr(layer_buffer), index, 9);
  // Save foreground layer
  for x := 0 to max_map_width - 1 do
    for y := 0 to max_map_height - 1 do
      layer_buffer[y, x] := map_data[x, y].layers[1];
  Archive.save_level_data(Addr(layer_buffer), index, 10);
  // Save hidden layer
  for x := 0 to max_map_width - 1 do
    for y := 0 to max_map_height - 1 do
      layer_buffer[y, x] := map_data[x, y].layers[2];
  Archive.save_level_data(Addr(layer_buffer), index, 11);
  // Save objects layer
  for x := 0 to max_map_width - 1 do
    for y := 0 to max_map_height - 1 do
      objects_buffer[y, x] := map_data[x, y].objects;
  Archive.save_level_data(Addr(objects_buffer), index, 12);
  // Save level data
  Archive.save_level_data(Addr(level_data.player_info), index, 0);
  Archive.save_level_data(Addr(level_data.animation_info), index, 1);
  Archive.save_level_data(Addr(level_data.message_data), index, 2);
  Archive.save_level_data(Addr(level_data.teleport_coordinates), index, 3);
  Archive.save_level_data(Addr(level_data.switch_coordinates), index, 4);
  Archive.save_level_data(Addr(level_data.insert_trigger_coordinates), index, 5);
  Archive.save_level_data(Addr(level_data.remove_trigger_coordinates), index, 6);
  Archive.save_level_data(Addr(level_data.monster_info), index, 7);
  Archive.save_level_data(Addr(level_data.monster_triggers), index, 8);
  Archive.close_archive(true);
  // Save level data to exe
  Exefile.save_level_data(index, level_exe_data);
  map_index := index;
end;

procedure TMap.load_map_file(filename: String);
var
  map_file: file of byte;
  header: array[0..3] of byte;
begin
  // Read map file
  AssignFile(map_file, filename);
  FileMode := fmOpenRead;
  Reset(map_file);
  BlockRead(map_file, header, sizeof(header));
  BlockRead(map_file, level_exe_data, sizeof(level_exe_data));
  BlockRead(map_file, level_data, sizeof(level_data));
  BlockRead(map_file, map_data, sizeof(map_data));
  CloseFile(map_file);
  map_width := max_map_width;
  map_height := max_map_height;
  // Finalize it
  map_loaded := true;
  map_index := -1;
  reset_undo_history;
  compute_statistics;
  level_data_update_flags := all_level_data_update_flags;
  // Change tileset respectively
  Tileset.change_tileset(level_exe_data.tileset_number);
end;

procedure TMap.save_map_file(filename: String);
var
  map_file: file of byte;
  header: array[0..3] of byte;
begin
  AssignFile(map_file, filename);
  ReWrite(map_file);
  header[0] := ord('H');
  header[1] := ord('P');
  header[2] := ord('M');
  header[3] := 0;
  BlockWrite(map_file, header, sizeof(header));
  BlockWrite(map_file, level_exe_data, sizeof(level_exe_data));
  BlockWrite(map_file, level_data, sizeof(level_data));
  BlockWrite(map_file, map_data, sizeof(map_data));
  CloseFile(map_file);
end;


procedure TMap.set_map_size(new_width, new_height: integer);
var
  i, j: integer;
begin
  if (map_width = new_width) and (map_height = new_height) then
    exit;
  if (new_width <> max_map_width) or (new_height <> max_map_height) then
    exit;
  // Fill additional area with empty tiles
  for i := 0 to new_height - 1 do
    for j := 0 to new_width - 1 do
      if (i >= map_height) or (j >= map_width) then
      begin
        map_data[j,i] := empty_tile;
      end;
  // Set new map size
  map_width := new_width;
  map_height := new_height;
  // Finalize it
  reset_undo_history;
  compute_statistics;
end;

procedure TMap.shift_map(direction: TDirection; num_tiles: integer);
var
  x, y: integer;
  src_x, src_y: integer;
  move_x, move_y: integer;
  i: integer;
  switch_coords: ^TLvlSwitchCoordinates;
  trig_coords: ^TLvlTriggerCoordinates;
begin
  move_x := 0;
  move_y := 0;
  // Shift map tiles
  case direction of
    drLeft:  begin // Left
          for y := 0 to map_height - 1 do
            for x := 0 to map_width - 1 do
            begin
              src_x := x + num_tiles;
              move_x := num_tiles * -1;
              unregister_object(x, y);
              if (src_x < map_width) then
                map_data[x,y] := map_data[src_x,y]
              else
                map_data[x,y] := empty_tile;
            end;
        end;
    drUp:  begin // Up
          for y := 0 to map_height - 1 do
            for x := 0 to map_width - 1 do
            begin
              src_y := y + num_tiles;
              move_y := num_tiles * -1;
              unregister_object(x, y);
              if (src_y < map_height) then
                map_data[x,y] := map_data[x,src_y]
              else
                map_data[x,y] := empty_tile;
            end;
        end;
    drRight:  begin // Right
          for y := map_height - 1 downto 0 do
            for x := map_width - 1 downto 0 do
            begin
              src_x := x - num_tiles;
              move_x := num_tiles;
              unregister_object(x, y);
              if (src_x >= 0) then
                map_data[x,y] := map_data[src_x,y]
              else
                map_data[x,y] := empty_tile;
            end;
        end;
    drDown:  begin // Down
          for y := map_height - 1 downto 0 do
            for x := map_width - 1 downto 0 do
            begin
              src_y := y - num_tiles;
              move_y := num_tiles;
              unregister_object(x, y);
              if (src_y >= 0) then
                map_data[x,y] := map_data[x,src_y]
              else
                map_data[x,y] := empty_tile;
            end;
        end;
  end;
  // Re-register all objects
  for y := 0 to map_height - 1 do
    for x := 0 to map_width - 1 do
      register_object(x, y, map_data[x,y]);
  // Shift switch and trigger target areas
  for i := 0 to Length(level_data.switch_coordinates) - 1 do
  begin
    switch_coords := Addr(level_data.switch_coordinates[i]);
    if (switch_coords.UpperLeftX = 0) and (switch_coords.UpperLeftY = 0) then
      continue;
    switch_coords.UpperLeftX := Max(Min(switch_coords.UpperLeftX + move_x, max_map_width - 1), 0);
    switch_coords.UpperLeftY := Max(Min(switch_coords.UpperLeftY + move_y, max_map_height - 1), 0);
    switch_coords.LowerRightX := Max(Min(switch_coords.LowerRightX + move_x, max_map_width - 1), 0);
    switch_coords.LowerRightY := Max(Min(switch_coords.LowerRightY + move_y, max_map_height - 1), 0);
  end;
  for i := 0 to Length(level_data.insert_trigger_coordinates) - 1 do
  begin
    trig_coords := Addr(level_data.insert_trigger_coordinates[i]);
    if ((trig_coords.UpperLeftX = 0) and (trig_coords.UpperLeftY = 0)) or (trig_coords.UpperLeftX = 65535) then
      continue;
    trig_coords.UpperLeftX := Max(Min(trig_coords.UpperLeftX + move_x, max_map_width - 1), 0);
    trig_coords.UpperLeftY := Max(Min(trig_coords.UpperLeftY + move_y, max_map_height - 1), 0);
    trig_coords.LowerRightX := Max(Min(trig_coords.LowerRightX + move_x, max_map_width - 1), 0);
    trig_coords.LowerRightY := Max(Min(trig_coords.LowerRightY + move_y, max_map_height - 1), 0);
  end;
  for i := 0 to Length(level_data.remove_trigger_coordinates) - 1 do
  begin
    trig_coords := Addr(level_data.remove_trigger_coordinates[i]);
    if ((trig_coords.UpperLeftX = 0) and (trig_coords.UpperLeftY = 0)) or (trig_coords.UpperLeftX = 65535) then
      continue;
    trig_coords.UpperLeftX := Max(Min(trig_coords.UpperLeftX + move_x, max_map_width - 1), 0);
    trig_coords.UpperLeftY := Max(Min(trig_coords.UpperLeftY + move_y, max_map_height - 1), 0);
    trig_coords.LowerRightX := Max(Min(trig_coords.LowerRightX + move_x, max_map_width - 1), 0);
    trig_coords.LowerRightY := Max(Min(trig_coords.LowerRightY + move_y, max_map_height - 1), 0);
  end;
  // Shift starting position
  level_data.player_info.PlayerX := Max(Min(level_data.player_info.PlayerX + move_x, max_map_width - 2), 0);
  level_data.player_info.PlayerY := Max(Min(level_data.player_info.PlayerY + move_y, max_map_height - 2), 0);
  // Finalize it
  reset_undo_history;
  compute_statistics;
  level_data_update_flags := all_level_data_update_flags;
end;

end.
