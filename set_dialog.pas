unit set_dialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, ExtCtrls;

type
  TSetDialog = class(TForm)
    SetMapSize_Menu: TPanel;
    SetMapSize_LbWidth: TLabel;
    SetMapSize_LbHeight: TLabel;
    SetMapSize_Width: TSpinEdit;
    SetMapSize_Height: TSpinEdit;
    BtnOK: TButton;
    BtnCancel: TButton;
    ShiftMap_Menu: TPanel;
    ShiftMap_RbUp: TRadioButton;
    ShiftMap_RbDown: TRadioButton;
    ShiftMap_RbLeft: TRadioButton;
    ShiftMap_RbRight: TRadioButton;
    ShiftMap_NumTiles: TSpinEdit;
    ShiftMap_LbNumTiles: TLabel;
    LevelSelection_Menu: TPanel;
    LevelSelection_List: TListBox;
    TilesetSelection_Menu: TPanel;
    TilesetSelection_List: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);  
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure ShiftMap_SelectDirection(Sender: TObject);
  private
    { Private declarations }
    current_menu: integer;
    shift_map_direction: integer;
  public
    { Public declarations }
    procedure init;
    procedure select_menu(menu: integer);
  end;

var
  SetDialog: TSetDialog;

implementation

uses
  main, _map, _settings, _archive;

{$R *.dfm}

{ TSetDialog }

procedure TSetDialog.FormCreate(Sender: TObject);
begin
  SetMapSize_Width.Value := max_map_width;
  SetMapSize_Height.Value := max_map_height;
end;

procedure TSetDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = 13 then
    BtnOKClick(nil);
  if key = 27 then
    BtnCancelClick(nil);
end;

procedure TSetDialog.init;
var
  i: integer;
begin
  for i := 0 to Archive.level_count - 1 do
    LevelSelection_List.Items.Add(Archive.level_info[i].name);
  LevelSelection_List.ItemIndex := 0;
  for i := 0 to Archive.tileset_count - 1 do
    TilesetSelection_List.Items.Add(Archive.tileset_info[i].name);
  TilesetSelection_List.ItemIndex := 0;
end;

procedure TSetDialog.select_menu(menu: integer);
begin
  SetMapSize_Menu.Visible := False;
  ShiftMap_Menu.Visible := False;
  LevelSelection_Menu.Visible := False;
  TilesetSelection_Menu.Visible := False;
  Show;
  current_menu := menu;
  case menu of
    1:  begin
          Caption := 'Set map size';
          SetMapSize_Width.Value := Map.width;
          SetMapSize_Height.Value := Map.height;
          SetMapSize_Menu.Visible := True;
          //SetMapSize_Width.SetFocus;
        end;
    2:  begin
          ShiftMap_Menu.Visible := True;
          Caption := 'Shift map';
          ShiftMap_NumTiles.SetFocus;
        end;
    3:  begin
          LevelSelection_Menu.Visible := True;
          Caption := 'Select level';
          LevelSelection_List.SetFocus;
        end;
    4:  begin
          TilesetSelection_Menu.Visible := True;
          Caption := 'Select Tileset';
          TilesetSelection_List.SetFocus;
        end;
  end;
  Hide;
  ShowModal;
end;

procedure TSetDialog.BtnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TSetDialog.BtnOKClick(Sender: TObject);
begin
  case current_menu of
    1:  begin
          if ((SetMapSize_Width.Value mod 2) = 1) or ((SetMapSize_Height.Value mod 2) = 1) then
            ShowMessage('Map size must be even.')
          else
          begin
            ModalResult := mrOk;
            MainWindow.set_map_size(SetMapSize_Width.Value,SetMapSize_Height.Value);
          end;
        end;
    2:  begin
          if shift_map_direction > 0 then
          begin
            ModalResult := mrOk;
            MainWindow.shift_map(TDirection(shift_map_direction-1),ShiftMap_NumTiles.Value);
          end;
        end;
    3:  begin
          ModalResult := mrOk;
        end;
    4:  begin
          ModalResult := mrOk;
        end;
  end;
end;

procedure TSetDialog.ShiftMap_SelectDirection(Sender: TObject);
begin
  shift_map_direction := (Sender as TRadioButton).Tag;
end;

end.
