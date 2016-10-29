unit misc_graphics_dialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, pngimage;

type
  TMiscGraphicsDialog = class(TForm)
    lbxImageFileList: TListBox;
    GraphicsImage: TImage;
    btnExportImage: TButton;
    btnImportImage: TButton;
    rbExportPng: TRadioButton;
    rbExportBmp: TRadioButton;
    cbImportUseUpperPal: TCheckBox;
    ExportDialogPng: TSaveDialog;
    ExportDialogBmp: TSaveDialog;
    ImportDialog: TOpenDialog;
    lbImageSize: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lbxImageFileListClick(Sender: TObject);
    procedure btnExportImageClick(Sender: TObject);
    procedure btnImportImageClick(Sender: TObject);
  private
    file_numbers: array of integer;
    Pal1: PLogPalette;
    Pal2: PLogPalette;
    image_buffer: TBitmap;

    procedure load_image_file(file_num: Integer);
    procedure save_image_file(file_num: Integer; input_buffer: TBitmap; use_upper_pal: boolean);
    procedure render_image;
  public
    { Public declarations }
  end;

var
  MiscGraphicsDialog: TMiscGraphicsDialog;

implementation

uses StrUtils, _archive, _exefile, sprite_dialog;

{$R *.dfm}

procedure TMiscGraphicsDialog.FormCreate(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to Archive.file_count - 1 do
  begin
    if AnsiEndsStr('.IMG', Archive.file_names[i]) then
    begin
      lbxImageFileList.Items.Add(Archive.file_names[i]);
      SetLength(file_numbers, Length(file_numbers) + 1);
      file_numbers[High(file_numbers)] := i;
    end;
  end;
  // Create first palette (lower palette only)
  GetMem( Pal1, Sizeof( TLogPalette ) + Sizeof( TPaletteEntry ) * 255 );
  Pal1.palversion := $300;
  Pal1.palnumentries := 256;
  for i := 0 to 254 do
  begin
    Pal1.palPalEntry[i].peRed := (Archive.palette[i] shr 16) and 255;
    Pal1.palPalEntry[i].peGreen := (Archive.palette[i] shr 8) and 255;
    Pal1.palPalEntry[i].peBlue := (Archive.palette[i] shr 0) and 255;
  end;
  // Create second palette (also upper palette)
  Archive.load_palette(Archive.palette_file_index + 1, 1);
  GetMem( Pal2, Sizeof( TLogPalette ) + Sizeof( TPaletteEntry ) * 255 );
  Pal2.palversion := $300;
  Pal2.palnumentries := 256;
  for i := 0 to 254 do
  begin
    Pal2.palPalEntry[i].peRed := (Archive.palette[i] shr 16) and 255;
    Pal2.palPalEntry[i].peGreen := (Archive.palette[i] shr 8) and 255;
    Pal2.palPalEntry[i].peBlue := (Archive.palette[i] shr 0) and 255;
  end;
  image_buffer := TBitmap.Create;
  image_buffer.PixelFormat := pf8bit;
end;

procedure TMiscGraphicsDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = 27 then
    Close;
end;

procedure TMiscGraphicsDialog.lbxImageFileListClick(Sender: TObject);
begin
  load_image_file(file_numbers[lbxImageFileList.ItemIndex]);
  render_image;
end;

procedure TMiscGraphicsDialog.btnExportImageClick(Sender: TObject);
var
  save_dialog: TSaveDialog;
  output_filename: String;
  PNG: TPNGObject;
begin
  if lbxImageFileList.ItemIndex = -1 then
    exit;
  if rbExportPng.Checked then
  begin
    save_dialog := ExportDialogPng;
  end else
  begin
    save_dialog := ExportDialogBmp;
  end;
  if save_dialog.Execute then
    output_filename := save_dialog.FileName
  else
    exit;
  if rbExportPng.Checked then
  begin
    PNG := TPNGObject.Create;
    PNG.Assign(image_buffer);
    PNG.SaveToFile(output_filename);
  end else
    image_buffer.SaveToFile(output_filename);
end;

procedure TMiscGraphicsDialog.btnImportImageClick(Sender: TObject);
var
  input_filename: String;
  input_buffer: TBitmap;
begin
  if lbxImageFileList.ItemIndex = -1 then
    exit;
  if ImportDialog.Execute then
    input_filename := ImportDialog.FileName
  else
    exit;
  input_buffer := TBitmap.Create;
  SpriteDialog.load_input_image(input_filename, input_buffer);
  if (input_buffer.Width mod 4) <> 0 then
  begin
    Application.MessageBox('Image width must be multiple of 4.', 'Cannot import image', MB_OK or MB_ICONERROR);
    exit;
  end;
  save_image_file(file_numbers[lbxImageFileList.ItemIndex], input_buffer, cbImportUseUpperPal.Checked);
  render_image;
end;

procedure TMiscGraphicsDialog.load_image_file(file_num: Integer);
var
  file_entry: ^TFileEntry;
  file_buffer: array of byte;
  pixel_buffer: array of byte;
  width, height, plane_size: integer;
  i, j: integer;
begin
  file_entry := Addr(Archive.file_list[file_num]);
  SetLength(file_buffer, file_entry.size);
  Archive.load_data(file_buffer, file_entry.offset, file_entry.size);
  width := file_buffer[0] + (file_buffer[1] * 256);
  height := file_buffer[2] + (file_buffer[3] * 256);
  plane_size := width * height;
  width := width * 4;
  SetLength(pixel_buffer, width * height);
  for i := 0 to plane_size - 1 do
  begin
    for j := 0 to 3 do
      pixel_buffer[i * 4 + j] := file_buffer[i + j * plane_size + 4];
  end;
  image_buffer.Palette := CreatePalette( Pal2^ );
  image_buffer.Width := width;
  image_buffer.Height := height;
  SetBitmapBits(image_buffer.Handle, width * height, pixel_buffer);
end;

procedure TMiscGraphicsDialog.save_image_file(file_num: Integer; input_buffer: TBitmap; use_upper_pal: boolean);
var
  width, height: integer;
  plane_size: integer;
  file_size: integer;
  file_buffer: array of byte;
  pixel_buffer: array of byte;
  i, j: integer;
begin
  width := input_buffer.Width;
  height := input_buffer.Height;
  plane_size := (width * height) div 4;
  image_buffer.Width := width;
  image_buffer.Height := height;
  if use_upper_pal then
    image_buffer.Palette := CreatePalette( Pal2^ )
  else
    image_buffer.Palette := CreatePalette( Pal1^ );
  image_buffer.Canvas.CopyRect(Rect(0,0,width,height), input_buffer.Canvas, Rect(0,0,width,height));
  SetLength(pixel_buffer, width * height);
  GetBitmapBits(image_buffer.Handle, width * height, pixel_buffer);
  if use_upper_pal then
    for i := 0 to (width * height) - 1 do
      if pixel_buffer[i] = 0 then
        pixel_buffer[i] := 128;
  file_size := width * height + 4;
  SetLength(file_buffer, file_size);
  file_buffer[0] := (width shr 2) and 255;
  file_buffer[1] := (width shr 10) and 255;
  file_buffer[2] := height and 255;
  file_buffer[3] := (height shr 8) and 255;
  for i := 0 to plane_size - 1 do
  begin
    for j := 0 to 3 do
      file_buffer[i + j * plane_size + 4] := pixel_buffer[i * 4 + j];
  end;
  Archive.save_file(file_buffer, file_num, file_size);
end;

procedure TMiscGraphicsDialog.render_image;
var
  width, height: integer;
begin
  width := image_buffer.Width;
  height := image_buffer.Height;
  GraphicsImage.Canvas.Pen.Color := clWhite;
  GraphicsImage.Canvas.Rectangle(0, 0, GraphicsImage.Width, GraphicsImage.Height);
  GraphicsImage.Canvas.CopyRect(Rect(0, 0, width * 2, height * 2), image_buffer.Canvas, Rect(0, 0, width, height));
  lbImageSize.Caption := inttostr(width) + ' x ' + inttostr(height);
end;

end.
