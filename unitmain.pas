unit unitMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, types, LCLType, Menus;

type

  { TFormMain }

  TFormMain = class(TForm)
    ListBox: TListBox;
    MenuItem1: TMenuItem;
    MenuExport: TMenuItem;
    MenuRefresh: TMenuItem;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    procedure ButtonCheckClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBoxDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuExportClick(Sender: TObject);
    procedure MenuRefreshClick(Sender: TObject);
  private
    { private declarations }
    paths: TStringList;
  public
    { public declarations }
    procedure GetIt;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.ButtonCheckClick(Sender: TObject);
var
  I: integer;
  s: string;
begin
  s := GetEnvironmentVariable('PATH');
  //  paths.Clear;
  paths.StrictDelimiter := True;
  paths.Delimiter := ';';
  paths.DelimitedText := s;
  ListBox.Items := paths;
  ListBox.Refresh;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  paths := TStringList.Create;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  paths.Free;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  GetIt;
end;

procedure TFormMain.GetIt;
begin
  paths.Clear;
  paths.StrictDelimiter := True;
  paths.Delimiter := PathSeparator;
  paths.DelimitedText := GetEnvironmentVariable('PATH');
  ListBox.Clear;
  ListBox.Items := paths;
  ListBox.Refresh;
  ListBox.SetFocus;
end;

procedure TFormMain.ListBoxDrawItem(Control: TWinControl; Index: integer;
  ARect: TRect; State: TOwnerDrawState);
var
  s: string;
begin
  s := ListBox.Items[Index];

  with (Control as TListBox).Canvas do
  begin
    if Odd(Index) and not (odSelected in State) then
      Brush.Color := $F0F0F0;

    if not DirectoryExistsUTF8(s) then
      font.Color := clRed
    else
      font.Color := clDefault;
    FillRect(ARect);
    TextOut(ARect.Left, ARect.Top, (Control as TListBox).Items[Index]);
  end;
end;

procedure TFormMain.MenuItem1Click(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.MenuExportClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
    begin
      paths.SaveToFile(SaveDialog1.FileName);
    end;
end;

procedure TFormMain.MenuRefreshClick(Sender: TObject);
begin
  GetIt;
end;

end.
