Hocus Pocus level editor
------------------------
Version 1.2 (2016-11-12)
Made by Hisymak (kozten@seznam.cz)
Source available at https://github.com/jkoncick/HocusEditor


1. Feature set
2. Installation
3. Modding tips
   - How the layers work and what is hidden layer
   - How to use pattern mode, block mode and presets
   - How to place monsters
   - How to create switch puzzles
   - How to copy and paste level areas
   - How to use Test Map feature
   - How to easily draw borders around "windows" in background layer
   - How to use new monster types in a level
   - How the animated tiles work
   - How to replace graphics, music, sounds and texts with your own
   - How to distribute your mod as a patch


1. Feature set
--------------
- Open/save maps directly from/to HOCUS.DAT file or import/export from/to a file
- Read tilesets directly from HOCUS.DAT
- Advanced map editing modes and features, allowing you make maps easily and fast:
  - Three editing modes: Basic Tile mode, Pattern mode, Block mode
  - Predefined Pattern and Block presets for each tileset 
  - Ability to add your own presets
  - Undo&Redo feature
  - Copy&Paste feature (works also between different maps)
  - Shift map feature
  - Auto-draw window borders feature
- Full support for editing all special objects and events (teleports, switches, locks)
- Editing level properties, animation and monster data
- Show/hide specific map layers, show grid feature
- Minimap feature (providing schematic overview of the map)
- Save map image feature
- Simple statistics (number of crystals, healing potions, points etc. in a map)
- Test map feature (game is directly launched in Dosbox)
- Many keyboard shortcuts and special mouse actions for Ctrl/Shift key combinations
New from 1.1:
- Export and import any file from/to HOCUS.DAT (graphics, music, sounds...)
- Edit level properties stored inside HOCUS.EXE (par times, elevator tiles, music...)
- Apply mod patch feature (easy distribution of your mods)
- Created document "Graphics replacement guide"
New from 1.2:
- Created tool for viewing, exporting and importing sprites
- Created tool for viewing, exporting and importing menu and UI graphics
- The document "Graphics replacement guide" was updated accordingly
- Sprites are drawn on map (can be turned on and off)
- The "Save map image" now saves into .png instead of extremely huge .bmp file
- Added exe patching feature
- Added support for beta version
- Added "Compute VRAM usage" function


2. Installation
---------------
- Unzip all the files into your Hocus Pocus game folder.
- HocusEditor.ini file is created after opening and closing the program for the first 
  time. There you can adjust some preferences and set path for Dosbox 
  (must include dosbox.exe filename).
- The editor detects your game version by HOCUS.EXE filesize.
  Supported versions are shareware (both 1.0 and 1.1), registered (only 1.1) and beta.


3. Modding tips
---------------
- If you want to understand the level format and how the special objects and events
  work (switches, locks etc.) it is greatly recommended to look at the original levels 
  and study them a bit. Alternatively you can look at the format specification if you 
  are really interested (http://www.shikadi.net/moddingwiki/Hocus_Pocus_Map_Format)
- The editor provides many keyboard shortcuts to make things easier and faster, 
  but most importantly, you need to use specific key+mouse combinations (Ctrl, Shift)
  to achieve basic editing tasks. The editor provides built-in help which tells you
  the most of keyboard shortcuts and combinations for each editing mode.
- On the Objects tab, select any subtab to edit a special object. You can double-click
  on a group or object number in the list to move the view directly on that object.
  This way you can easily navigate to any special object on a map.


How the layers work and what is hidden layer
--------------------------------------------
There are four layers: background, foreground, hidden and object. The background layer
contains non-solid background tiles and the foreground layer contains solid tiles 
(walls). Each layer may contain "empty tile". Parallax-scrolling backdrop can be 
seen if there is empty tile in all layers.
The game engine lets you use any tile from tileset in any layer, however it has sense
only to use specific tiles in a specific layer (otherwise you can make fake passable
walls or invisible impassable walls, as the game treats any tile in background layer
as passable and any tile in foreground layer as impassable).
To make things easier for you, the editor lets you place only specific tiles into the
respective layer. To override this, turn off "Use predefined tiles" option and you
will be able to place all tiles from the tileset.

The purpose of the hidden layer is to store tiles which are not initially visible, 
but are "built" during the gameplay as a result of player's action (flipping a switch, 
solving a switch puzzle, entering a specific place). 
Example: In E1L5 lower-left part, there is 4-switch puzzle. After solving it, large
stairs are built which let you continue up. The tiles from the hidden layer are marked
with gray diagonal stripes, you can clearly see them.
In the original levels, all tiles in the foreground layer are also present in the
hidden layer. Therefore the editor places tiles into both foreground and hidden layer 
automatically. To place a tile into hidden layer only, switch to the "Hidden" tab.


How to use pattern mode, block mode and presets
-----------------------------------------------
Pattern mode makes painting of repetitive patterns really easy - best is to try 
yourself and see. You can select a pattern directly from a map (by holding Shift and 
selecting an area) or use one of predefined patterns (click into pattern image on
the right side or press Space). Maximum pattern size is 8x8.  
You can "rotate" the pattern with Num2/4/6/8 keys or Shift+Arrow keys.

In block mode you can place rectangular blocks of tiles. You can select a block from 
map or use a predefined block. You can also copy and paste big parts of map, block
size is unlimited (but maximum block preset size is 8x8). 
You can either select a block from the current layer only, or select a block from 
all layers at once (presets can be used only for a single layer).
You can also move the block with Num2/4/6/8 keys or Shift+Arrow keys, then place it by
left-clicking on mouse. This can be very useful if you select really big block which
does not fit into your screen, or if you want to move block beyond left or top border.

The editor comes with already predefined pattern and block presets for each tileset.
You can create your own presets or delete existing presets. You can have up to 40
presets for backfround and foreground, patterns and blocks = 160 total.
To save your preset, select a block or pattern, click "Save as preset" button and
select the slot you want to use. To delete a preset, right-click on it. 
By pressing specific keys on keyboard (1,2,3,4,Q,W,E,R) you can quickly select a 
particular preset. To see keys click with the middle button in preset selection window.


How to place monsters
---------------------
As you can see, the monsters never appear in the level statically, but are always
spawned when you enter a specific area and de-spawned if they get off-screen.
All monsters in a level are divided into groups, each group can contain up to 8 
monsters. All monsters of a single group are spawned together.
To place monsters into your level, you must actually place two types of objects -
monster spawn points and monster trigger points. If a player enters a tile with
a trigger point, then monsters will spawn at their spawn points (and remain spawned as
long as they are kept on-screen or the player still touches trigger points).
In the editor switch to "Objects" tab and "Monsters" sub-tab and select a group in the
list. Then click either "Place monster" or "Place trigger" button. To switch between 
these two modes you can press Tab. 


How to create switch puzzles
----------------------------
Hocus Pocus features switch puzzles - a group of up to 4 switches, which need to be
flipped to a specific state in order to trigger an action in a level (remove tiles,
build tiles). A group can also contain only a single switch. Each switch in a group 
has its initial state and target state.
In the editor switch to "Objects" tab and "Switches" sub-tab and select a group in the
list. Click either "Down-state" or "Up-state" button to place a switch in specific
initial state. The target state is automatically set to the opposite of initial state.
To change the target state, check or uncheck the checkboxes in the switch list.
Also select the action (Insert wall, Remove wall) and target area.


How to copy and paste level areas
---------------------------------
To copy and paste larger areas you use Block mode with "All layers mode" turned on.
In this mode you can select tiles from all layers at once (including objects).
To transfer the area between different levels you can run two instances of the program.
In one instance, select the area you want to copy and press Ctrl+C. This saves the area
into clipboard. In the second instance press Ctrl+V and this retrieves the area from
clipboard. Of course both levels must use the same tileset.
There can be a situation when you want to copy an area with monsters. These monsters
belong to one group. When you paste that area, you want the pasted monsters to belong
to a new group. This can be achieved by pressing Num+ or Num-, which increases or
decreases the group number of monsters (and also switches) in the selected area.
If you paste an area and the resulting number of monsters in a single group would 
exceed the maximum of 8, then the monsters won't be placed.


How to use Test Map feature
---------------------------
Test Map feature allows you to quickly test your most recent modifications in-game.
It can be used only if the map is saved inside game archive (HOCUS.DAT) and path
to Dosbox is properly configured.
When clicking "Test map", the current level is saved (!!) and the game is launched 
in Dosbox. A special saved game is created in the first slot - it is called "TESTMAP" 
and loading it transfers you directly into the level you were editing.


How to easily draw borders around "windows" in background layer
---------------------------------------------------------------
There are usually "holes" in the background layer and the parallax-scrolling backdrop
can be seen through them. Let's call these holes "windows".
The windows usually have a "border" made of light-shaded tiles or "frame" tiles.

To make things easier for you, the editor can automatically place the border tiles
around a window automatically with just one click: 
First make a rectangular hole in background. Then, in tile mode, Shift-click on any 
background tile around the window. The background tiles around the window will turn 
into correct border tiles.


How to use new monster types in a level
---------------------------------------
Go to "Level properties" window, "Monster settings" section. There is list of monster
types which can be used in a level. Up to 10 monster types can be used in a level, 
but usually no more than three or four are used in the original levels.
To add a new monster type, select the next free slot, choose a sprite set and then 
configure all monster properties (usually you would copy the properties from a 
different level where that monster type is used).
Note: Due to static limitation of the amount of memory to hold graphical sprite data, 
you cannot add many monster types into a level (it also depends on the size of tileset).
Read more details in Hocus Pocus graphics replacement guide.
If you see graphical glitches in the sprites of the "highest" monster types, you
reached the limit and must remove some monster types.


How the animated tiles work
---------------------------
For each tile in the tileset there is animation information. For most tiles there is 
no animation, but there can be two types animations: permanent and random animation. 
Permanent animation is used on lava, spikes and a magic crystal. Random animation is 
used on treasure, keys and keyholes (and twinkling icicles in polar levels). 
Go to "Level properties" window, "animation settings" section.
For each tile within the animation range you simply set animation type, the first and 
last tile index of the range (the best is to look at how it is set in existing levels).


How to replace graphics, music, sounds and texts with your own
--------------------------------------------------------------
You can export and import any file from/to the game archive (HOCUS.DAT). The editor
exports and imports raw data, that means, you must import files which are in the game's
native format.
The game uses MID format for music, so replacing music is really easy. But remember
that file size is limited to 32kB. If you import a bigger file, it will rewrite some
memory which will result in broken font. Also remember that Adlib/Sound Blaster music
uses OPL chip which can play up to 9 notes at once, so if your midi is too much
polyphonic it will result in lost notes. 
The sounds are stored in VOC format which is a known format and there exist tools to 
play and create sounds in this format.
PCX format is used for tilesets, backdrops and fullscreen images. There are many image 
editors that can save into this format, but you will run into problems with palettes. 
There exists a whole document called "Hocus Pocus graphics replacement guide" where 
everything is described in detail.
The sprites are stored in a custom format and HocusEditor contains a built-in tool to
edit them. The file that contains all sprites is "SPRITES.SPR".
Texts (story, ordering info) are stored in PAG files. HocusSharp, a modding tool set
for Hocus Pocus, contains an editor for PAG files called HocusText. In order to 
edit these text files you need to export them, modify them with HocusText and import
them back. HocusText requires FONT.MSK for text preview, you need to export it too.
Other text strings are hard-coded into HOCUS.EXE. You can create exe patch to store 
changes in either text strings or just any data or code in the game executable.


How to distribute your mod as a patch
-------------------------------------
If you want to distribute your mod, you do not need to post the whole HOCUS.DAT and
HOCUS.EXE files, but you can post only the changes you made (new levels and new files)
and let the players apply that changes on their side.

The "patch" you will make will actually be a folder containing these types of files:
1. Exported levels files in .hpm format (use File - Export map to do this)
2. Files from HOCUS.DAT that will replace the original files (.mid, .pcx, etc...)
3. Exe patch file (.pat)
4. An .ini file containing references to all the above files:

[Levels]
E1L1=mylevel.hpm
E1L2=anotherlevel.hpm

[Files]
TITLE.MID=mytitlemusic.mid
TILES01.PCX=mysupertileset.pcx

[ExePatch]
patch=hocus.pat


The example should be easy to understand, on the "left" side there is level number or
file name in HOCUS.DAT to be replaced, and on the "right" side there is name of your 
file which will replace that level or file. There can be up to one exe patch file. 
You will pack all these files into a zip file which you will distribute.
The end-user will download the zip and unpack everything from it into a folder, then he
will start HocusEditor, select "Tools -> Apply mod patch" option and select the .ini
file you made. The editor will automatically replace all the game files, apply exe 
patch (if one is provided) and your mod will be ready to play.
Note that this method of patching will let you create a mod that is independent on the
game version. It will be applicable on either full or demo version, in case you replace
only the files or levels that are available in all game versions.
