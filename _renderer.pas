unit _renderer;

interface

uses Windows, Graphics, Types, _map;

type EditingMarkerType = (emBrush, emSingleObject, emSelectionArea);

const object_type_colors: array[0..7] of TColor = ($000000, $40A000, $FF80C0, $00FFFF, $FFC040, $4040FF, $0000C0, $C000C0);
const object_type_marktext: array[0..7] of String = ('U', 'It ', 'Tp', 'Sw', 'Iw', 'Rw', 'M ', 'T ');

type
  TRenderer = class

  private
    // Differential rendering variables
    diffrender_old_left: word;
    diffrender_old_top: word;

    // Invalidation variables
    invalidated: boolean;
    inv_nothing: boolean;
    inv_rect: TRect;

    // Editing markers rendering variables
    bkup_bitmap: TBitmap;
    bkup_rect: TRect;
    bkup_valid: boolean;

  public
    procedure init;

    procedure load_or_create_mask(graph: TBitmap; mask: TBitmap; filename: String);

    procedure invalidate_init;
    procedure invalidate_map_tile(x, y: word);

    procedure render_map_contents(cnv_target: TCanvas; cnv_left, cnv_top, cnv_width, cnv_height: word; cnv_off_left, cnv_off_top: integer;
      data: TMapDataPtr;
      o_bglr, o_fglr, o_hdnlr, o_objlr, o_show_markers, o_draw_sprites, o_show_grid,
      o_rendering_optimization: boolean);
    procedure render_minimap_contents(cnv_target: TCanvas; data: TMapDataPtr; data_width, data_height: word);

    procedure remove_editing_marker(cnv_target: TCanvas);
    procedure draw_editing_marker(cnv_target: TCanvas; cnv_left, cnv_top, cnv_width, cnv_height: word;
      data: TMapDataPtr; mark_x, mark_y, mark_width, mark_height: word; mark_style: TPenStyle; mark_color: TColor; mark_text: string);

  end;

var
  Renderer: TRenderer;

implementation

uses SysUtils, Math, main, _tileset, _archive, _spritefile;

procedure TRenderer.init;
begin
  // Init backup image
  bkup_bitmap := TBitmap.Create;
  bkup_bitmap.PixelFormat := pf32bit;
  bkup_bitmap.Width := 128;
  bkup_bitmap.Height := 128;
end;

procedure TRenderer.load_or_create_mask(graph, mask: TBitmap; filename: String);
var
  x, y: integer;
  black: TColor;
begin
  mask.PixelFormat := pf1bit;
  if FileExists(filename) then
    mask.LoadFromFile(filename)
  else begin
    mask.Width := graph.Width;
    mask.Height := graph.Height;
    black := graph.Canvas.Pixels[0,0];
    for y := 0 to graph.Height - 1 do
      for x := 0 to graph.Width - 1 do
      begin
        if graph.Canvas.Pixels[x,y] <> black then
          mask.Canvas.Pixels[x,y] := clBlack;
      end;
    mask.SaveToFile(filename);
  end;
end;

procedure TRenderer.invalidate_init;
begin
  if not invalidated then
    inv_nothing := true;
end;

procedure TRenderer.invalidate_map_tile(x, y: word);
begin
  if not invalidated then
  begin
    invalidated := true;
    inv_rect := Rect(x, y, x, y);
  end else
  begin
    inv_rect.Left := Min(inv_rect.Left, x);
    inv_rect.Top := Min(inv_rect.Top, y);
    inv_rect.Right := Max(inv_rect.Right, x);
    inv_rect.Bottom := Max(inv_rect.Bottom, y);
  end;
  inv_nothing := false;
end;

procedure TRenderer.render_map_contents(cnv_target: TCanvas; cnv_left, cnv_top, cnv_width, cnv_height: word; cnv_off_left, cnv_off_top: integer;
  data: TMapDataPtr;
  o_bglr, o_fglr, o_hdnlr, o_objlr, o_show_markers, o_draw_sprites, o_show_grid,
  o_rendering_optimization: boolean);
var
  min_x, min_y, max_x, max_y: integer;
  shift_count: word;
  x, y: integer;
  xx, yy: integer;
  i: integer;
  actual_x, actual_y: integer;
  map_tile: ^TMapTile;
  tile_index, tile_x, tile_y: byte;
  hidden_layer: boolean;
  dest_rect: TRect;
  src_rect: TRect;
  obj: word;
  mark_color: TColor;
  mark_text: String;
  obj_type: TObjectType;
  index: integer;
  switch_coords: ^TLvlSwitchCoordinates;
  trig_coords: ^TLvlTriggerCoordinates;
  sprite, sprite_mask: TBitmap;
  sprite_set: integer;
begin
  if not Map.loaded then
    exit;
  min_x := 0;
  min_y := 0;
  max_x := cnv_width - 1;
  max_y := cnv_height - 1;
  cnv_target.Pen.Color := clBlack;
  cnv_target.Brush.Color := clBlack;
  //cnv_target.Rectangle(0,0,cnv_width*32, cnv_height*32);
  // Rendering optimization
  if o_rendering_optimization then
  begin
    remove_editing_marker(cnv_target);
    // Horizontal scroll
    if (cnv_left <> diffrender_old_left) and (abs(cnv_left - diffrender_old_left) < cnv_width)  then
    begin
      shift_count := abs(cnv_left - diffrender_old_left);
      if cnv_left < diffrender_old_left then
      begin
        // Scrolling left
        max_x := shift_count - 1;
        dest_rect := rect(shift_count*32,0,cnv_width*32,cnv_height*32);
        src_rect := rect(0,0,cnv_width*32-shift_count*32,cnv_height*32);
      end else
      begin
        // Scrolling right
        min_x := max_x - shift_count + 1;
        src_rect := rect(shift_count*32,0,cnv_width*32,cnv_height*32);
        dest_rect := rect(0,0,cnv_width*32-shift_count*32,cnv_height*32);
      end;
      // Shifting part of map canvas
      cnv_target.CopyRect(dest_rect,cnv_target,src_rect);
    end else
    // Vertical scroll
    if (cnv_top <> diffrender_old_top) and (abs(cnv_top - diffrender_old_top) < cnv_height)  then
    begin
      shift_count := abs(cnv_top - diffrender_old_top);
      if cnv_top < diffrender_old_top then
      begin
        // Scrolling up
        max_y := shift_count - 1;
        dest_rect := rect(0,shift_count*32,cnv_width*32,cnv_height*32);
        src_rect := rect(0,0,cnv_width*32,cnv_height*32-shift_count*32);
      end else
      begin
        // Scrolling down
        min_y := max_y - shift_count + 1;
        src_rect := rect(0,shift_count*32,cnv_width*32,cnv_height*32);
        dest_rect := rect(0,0,cnv_width*32,cnv_height*32-shift_count*32);
      end;
      // Shifting part of map canvas
      cnv_target.CopyRect(dest_rect,cnv_target,src_rect);
    end else
    // Invalidated area
    if invalidated then
    begin
      min_x := Min(Max(inv_rect.Left - cnv_left, 0), cnv_width);
      max_x := Min(Max(inv_rect.Right - cnv_left, -1), cnv_width - 1);
      min_y := Min(Max(inv_rect.Top - cnv_top, 0), cnv_height);
      max_y := Min(Max(inv_rect.Bottom - cnv_top, -1), cnv_height - 1);
      // Nothing to render
      if (min_x > max_x) or (min_y > max_y) then
      begin
        invalidated := false;
        exit;
      end;
    end else
    if inv_nothing then
    begin
      // Nothing to render
      inv_nothing := false;
      exit;
    end;
    diffrender_old_left := cnv_left;
    diffrender_old_top := cnv_top;
    invalidated := false;
    inv_nothing := false;
  end;
  // Draw map layers
  cnv_target.Pen.Width := 1;
  for y:= min_y to max_y do
  begin
    for x:= min_x to max_x do
    begin
      map_tile := Addr(data[x + cnv_left, y + cnv_top]);
      tile_index := 255;
      hidden_layer := false;
      // Background layer
      if o_bglr then
        tile_index := map_tile.layers[0];
      // Hidden layer
      if o_hdnlr and (map_tile.layers[2] <> 255) then
      begin
        tile_index := map_tile.layers[2];
        hidden_layer := true;
      end;
      // Foreground layer
      if o_fglr and (map_tile.layers[1] <> 255) then
      begin
        tile_index := map_tile.layers[1];
        hidden_layer := false;
      end;
      dest_rect := Rect(cnv_off_left + x*32, cnv_off_top + y*32, cnv_off_left + x*32+32, cnv_off_top + y*32+32);
      if tile_index = 255 then
      begin
        // Empty tile
        cnv_target.Pen.Style := psSolid;
        cnv_target.Brush.Style := bsSolid;
        cnv_target.Pen.Color := $FFE0A0;
        cnv_target.Brush.Color := $FFE0A0;
        cnv_target.Rectangle(dest_rect);
      end else
      begin
        // Nonempty tile
        tile_x := tile_index mod tileset_cols;
        tile_y := tile_index div tileset_cols;
        src_rect := Rect(tile_x * 16, tile_y * 16, tile_x * 16 + 16, tile_y * 16 + 16);
        cnv_target.CopyRect(dest_rect, Tileset.tileimage.Canvas, src_rect);
      end;
      if hidden_layer then
      begin
        // Tile is in hidden layer
        cnv_target.Pen.Style := psClear;
        cnv_target.Brush.Style := bsBDiagonal;
        cnv_target.Brush.Color := $D0D0D0;
        cnv_target.Rectangle(dest_rect);
      end;
      // Object layer
      if (map_tile.objects = empty_object) or (not o_objlr) then
        continue;
      obj := map_tile.objects;
      obj_type := otNone;
      // Spikes
      if obj = ivSpikes then
      begin
        mark_color := $A0A0A0;
        mark_text := 'Sp';
      end else
      // Lava
      if obj = ivLava then
      begin
        mark_color := $A0A0A0;
        mark_text := 'Lav';
      end else
      // Message
      if obj = ivMessage then
      begin
        mark_color := $A0A0A0;
        mark_text := 'Msg';
      end else
      begin
        obj_type := Map.get_object_type(obj, index);
        mark_color := object_type_colors[ord(obj_type)];
        mark_text := object_type_marktext[ord(obj_type)] + inttostr(index);
      end;
      cnv_target.Pen.Style := psSolid;
      cnv_target.Brush.Style := bsClear;
      cnv_target.Pen.Color := mark_color;
      cnv_target.Rectangle(dest_rect);
      cnv_target.Font.Color := mark_color;
      cnv_target.TextOut(dest_rect.Left+3, dest_rect.Top+3, mark_text);
      mark_text := '';
      if ((obj_type = otMonster) or (obj = ivMessage)) and (map_tile.moreinfo <> 255) then
        mark_text := '(' + inttostr(map_tile.moreinfo) + ')'
      else if (obj_type = otSwitch) and (map_tile.moreinfo = 0) then
        mark_text := '(D)'
      else if (obj_type = otSwitch) and (map_tile.moreinfo = 1) then
        mark_text := '(U)'
      else if (obj_type = otTeleport) and (map_tile.moreinfo = 0) then
        mark_text := 'Start'
      else if (obj_type = otTeleport) and (map_tile.moreinfo = 1) then
        mark_text := 'Targ';
      if mark_text <> '' then
        cnv_target.TextOut(dest_rect.Left+3, dest_rect.Top+15, mark_text);
    end;
  end;
  // Draw sprites
  if o_draw_sprites then
  begin
    cnv_target.Pen.Width := 1;
    for y:= max(min_y - 4, 0 - cnv_top) to max_y do
    begin
      for x:= max(min_x - 4, 0 - cnv_left) to max_x do
      begin
        map_tile := Addr(data[x + cnv_left, y + cnv_top]);
        obj := map_tile.objects;
        obj_type := Map.get_object_type(obj, index);
        sprite_set := -1;
        // There is a monster
        if obj_type = otMonster then
          sprite_set := Map.leveldata.monster_info[index].SpriteSet;
        // There is player start
        if ((x + cnv_left) = Map.leveldata.player_info.PlayerX) and ((y + cnv_top) = Map.leveldata.player_info.PlayerY) then
          sprite_set := 0;
        // Draw sprite
        if sprite_set <> -1 then
        begin
          sprite := SpriteFile.get_preloaded_sprite(sprite_set, sprite_mask);
          dest_rect := Rect(cnv_off_left + x*32, cnv_off_top + y*32, cnv_off_left + x*32+sprite.Width*2, cnv_off_top + y*32+sprite.Height*2);
          src_rect := Rect(0,0,sprite.Width,sprite.Height);
          cnv_target.CopyMode := cmSrcAnd;
          cnv_target.CopyRect(dest_rect, sprite_mask.Canvas, src_rect);
          cnv_target.CopyMode := cmSrcPaint;
          cnv_target.CopyRect(dest_rect, sprite.Canvas, src_rect);
          cnv_target.CopyMode := cmSrcCopy;
        end;
      end;
    end;
  end;
  cnv_target.Pen.Width := 1;
  cnv_target.Pen.Style := psSolid;
  cnv_target.Brush.Style := bsClear;
  cnv_target.Font.Color := clBlack;
  // Draw grid
  if o_show_grid then
  begin
    cnv_target.Pen.Width := 2;
    for x:= 0 to cnv_width do
    begin
      if (x + cnv_left) and 1 = 1 then
        //cnv_target.Pen.Color := clYellow
        continue
      else
        cnv_target.Pen.Color := clGray;
      cnv_target.MoveTo(x*32,0);
      cnv_target.LineTo(x*32,cnv_height*32);
    end;
    for y:= 0 to cnv_height do
    begin
      if (y + cnv_top) and 1 = 1 then
        //cnv_target.Pen.Color := clYellow
        continue
      else
        cnv_target.Pen.Color := clGray;
      cnv_target.MoveTo(0,y*32);
      cnv_target.LineTo(cnv_width*32,y*32);
    end;
  end;
  if not o_show_markers then
    exit;
  // Draw starting position
  cnv_target.Pen.Style := psSolid;
  cnv_target.Pen.Color := clRed;
  cnv_target.Pen.Width := 1;
  cnv_target.Brush.Style := bsClear;
  dest_rect := Rect((Map.leveldata.player_info.PlayerX - cnv_left)*32, (Map.leveldata.player_info.PlayerY - cnv_top)*32,
    (Map.leveldata.player_info.PlayerX - cnv_left)*32+64, (Map.leveldata.player_info.PlayerY - cnv_top)*32+64);
  cnv_target.Rectangle(dest_rect);
  cnv_target.Font.Color := clRed;
  cnv_target.TextOut(dest_rect.Left + 20, dest_rect.Top + 26, 'Start');
  // Draw switch target areas
  cnv_target.Pen.Style := psSolid;
  cnv_target.Pen.Width := 2;
  cnv_target.Brush.Style := bsClear;
  for i := 0 to Length(Map.leveldata.switch_coordinates) - 1 do
  begin
    switch_coords := Addr(Map.leveldata.switch_coordinates[i]);
    if (switch_coords.UpperLeftX = 0) and (switch_coords.UpperLeftY = 0) then
      continue;
    dest_rect := Rect((switch_coords.UpperLeftX - cnv_left)*32, (switch_coords.UpperLeftY - cnv_top)*32, (switch_coords.LowerRightX - cnv_left)*32+32, (switch_coords.LowerRightY - cnv_top)*32+32);
    if switch_coords.SwitchType = 0 then
    begin
      mark_color := $00FFFF;
      mark_text := 'removal';
    end else
    begin
      mark_color := $00FFFF;
      mark_text := 'insertion';
    end;
    cnv_target.Pen.Color := mark_color;
    cnv_target.Rectangle(dest_rect);
    cnv_target.Font.Color := mark_color;
    cnv_target.TextOut(dest_rect.Left + 4, dest_rect.Top + 4, 'Switch ' + mark_text + ' ' + inttostr(i));
  end;
  // Draw insert trigger target areas
  for i := 0 to Length(Map.leveldata.insert_trigger_coordinates) - 1 do
  begin
    trig_coords := Addr(Map.leveldata.insert_trigger_coordinates[i]);
    if ((trig_coords.UpperLeftX = 0) and (trig_coords.UpperLeftY = 0)) or (trig_coords.UpperLeftX = 65535) then
      continue;
    dest_rect := Rect((trig_coords.UpperLeftX - cnv_left)*32, (trig_coords.UpperLeftY - cnv_top)*32, (trig_coords.LowerRightX - cnv_left)*32+32, (trig_coords.LowerRightY - cnv_top)*32+32);
    cnv_target.Pen.Color := $FFC040;
    cnv_target.Rectangle(dest_rect);
    cnv_target.Font.Color := $FFC040;
    cnv_target.TextOut(dest_rect.Left + 4, dest_rect.Top + 4, 'Trigger insertion ' + inttostr(i));
  end;
  // Draw remove trigger target areas
  for i := 0 to Length(Map.leveldata.remove_trigger_coordinates) - 1 do
  begin
    trig_coords := Addr(Map.leveldata.remove_trigger_coordinates[i]);
    if ((trig_coords.UpperLeftX = 0) and (trig_coords.UpperLeftY = 0)) or (trig_coords.UpperLeftX = 65535) then
      continue;
    dest_rect := Rect((trig_coords.UpperLeftX - cnv_left)*32, (trig_coords.UpperLeftY - cnv_top)*32, (trig_coords.LowerRightX - cnv_left)*32+32, (trig_coords.LowerRightY - cnv_top)*32+32);
    cnv_target.Pen.Color := $4040FF;
    cnv_target.Rectangle(dest_rect);
    cnv_target.Font.Color := $4040FF;
    cnv_target.TextOut(dest_rect.Left + 4, dest_rect.Top + 4, 'Trigger removal ' + inttostr(i));
  end;
end;

procedure TRenderer.render_minimap_contents(cnv_target: TCanvas; data: TMapDataPtr; data_width, data_height: word);
var
  min_x, min_y, max_x, max_y: integer;
  x, y: integer;
  map_tile: ^TMapTile;
  tile_color: TColor;
  border_x, border_y: integer;
  handle: HDC;
begin
  min_x := 0;
  min_y := 0;
  max_x := data_width - 1;
  max_y := data_height - 1;
  if inv_nothing then
  begin
    // Nothing to render
    exit;
  end else
  if invalidated then
  begin
    // Render only invalidated area
    min_x := inv_rect.Left;
    max_x := inv_rect.Right;
    min_y := inv_rect.Top;
    max_y := inv_rect.Bottom;
  end else
  begin
    // Render whole minimap
    cnv_target.Brush.Color := ClBtnFace;
    cnv_target.Pen.Color := ClBtnFace;
    cnv_target.Rectangle(0,0,max_map_width,max_map_height);
  end;
  border_x := (max_map_width - data_width) div 2;
  border_y := (max_map_height - data_height) div 2;
  // Rendering contents
  handle := cnv_target.Handle;
  for y:= min_y to max_y do
    for x:= min_x to max_x do
    begin
      // Get color according to full/empty tile in particular layer
      map_tile := Addr(data[x, y]);
      tile_color := $FFE0A0;
      if map_tile.layers[0] <> 255 then
        tile_color := $C0A060;
      if map_tile.layers[2] <> 255 then
        tile_color := $705070;
      if map_tile.layers[1] <> 255 then
        tile_color := $705010;
      // Get color according to item
      case map_tile.objects of
        ivRuby:    tile_color := $00FFFF;
        ivDiamond: tile_color := $00FFFF;
        ivGoblet:  tile_color := $00FFFF;
        ivCrown:   tile_color := $00FFFF;
        ivHealing: tile_color := $00D000;
        ivCrystal: tile_color := $FF00FF;
        ivSilverKey: tile_color := $2020D0;
        ivGoldKey: tile_color := $2020D0;
        ivSpikes:  tile_color := $C0C0C0;
        ivLava:    tile_color := $006CC0;
      end;
      SetPixel(handle, x+border_x, y+border_y, tile_color);
    end;
end;

procedure TRenderer.remove_editing_marker(cnv_target: TCanvas);
var
  src_rect: TRect;
begin
  if not bkup_valid then
    exit;
  src_rect := Rect(0, 0, bkup_rect.Right - bkup_rect.Left, bkup_rect.Bottom - bkup_rect.Top);
  cnv_target.CopyRect(bkup_rect, bkup_bitmap.Canvas, src_rect);
  bkup_valid := false;
end;

procedure TRenderer.draw_editing_marker(cnv_target: TCanvas; cnv_left, cnv_top, cnv_width, cnv_height: word;
  data: TMapDataPtr; mark_x, mark_y, mark_width, mark_height: word; mark_style: TPenStyle; mark_color: TColor; mark_text: string);
var
  dest_rect: TRect;
  bkup_width, bkup_height: integer;
begin
  // Restore old backup
  remove_editing_marker(cnv_target);
  // Make new backup
  bkup_rect := Rect(Max(mark_x * 32 - cnv_left * 32, 0), Max(mark_y * 32 - cnv_top * 32, 0), Min((mark_x + mark_width - cnv_left) * 32 + 1, cnv_width * 32), Min((mark_y + mark_height - cnv_top) * 32 + 1, cnv_height * 32));
  bkup_width := bkup_rect.Right - bkup_rect.Left;
  bkup_height := bkup_rect.Bottom - bkup_rect.Top;
  dest_rect := Rect(0, 0, bkup_width, bkup_height);
  bkup_bitmap.Width := Max(bkup_bitmap.Width, bkup_width);
  bkup_bitmap.Height := Max(bkup_bitmap.Height, bkup_height);
  bkup_bitmap.Canvas.CopyRect(dest_rect, cnv_target, bkup_rect);
  bkup_valid := true;
  // Draw actual_marker
  cnv_target.Brush.Style := bsClear;
  cnv_target.Pen.Style := mark_style;
  cnv_target.Pen.Color := mark_color;
  cnv_target.Pen.Width := 1;
  dest_rect := Rect((mark_x - cnv_left)*32, (mark_y - cnv_top)*32, (mark_x + mark_width - cnv_left)*32, (mark_y + mark_height - cnv_top)*32);
  cnv_target.Rectangle(dest_rect);
  cnv_target.Font.Color := mark_color;
  if mark_text <> '' then
    cnv_target.TextOut(dest_rect.Left + 3, dest_rect.Top + 3, mark_text);
  cnv_target.Pen.Style := psSolid;
end;

end.
