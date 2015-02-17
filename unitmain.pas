{
Copyright 2015 George Litos

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
}

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
    MenuExit: TMenuItem;
    MenuExport: TMenuItem;
    MenuCopy: TMenuItem;
    MenuItem1: TMenuItem;
    MenuAbout: TMenuItem;
    MenuRefresh: TMenuItem;
    Popup: TPopupMenu;
    SaveDialog: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBoxDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure MenuCopyClick(Sender: TObject);
    procedure MenuExitClick(Sender: TObject);
    procedure MenuExportClick(Sender: TObject);
    procedure MenuAboutClick(Sender: TObject);
    procedure MenuRefreshClick(Sender: TObject);
  private
    { private declarations }
    paths: TStringList;
    function countString(s: string): integer;
  public
    { public declarations }
    procedure GetIt;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }
uses Clipbrd;

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
  // for testing only
  {paths.Append(UpperCase(paths[0]));
  paths.Append(paths[2]);}
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
      Font.Color := clRed
    else if countString(s) > 1 then
      Font.Color := clBlue
    else
      Font.Color := clDefault;

    FillRect(ARect);
    TextOut(ARect.Left, ARect.Top, (Control as TListBox).Items[Index]);
  end;
end;

procedure TFormMain.MenuCopyClick(Sender: TObject);
begin
  if ListBox.ItemIndex <> -1 then
    Clipboard.AsText := ListBox.GetSelectedText;
end;

procedure TFormMain.MenuExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.MenuExportClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    paths.SaveToFile(SaveDialog.FileName);
end;

procedure TFormMain.MenuAboutClick(Sender: TObject);
begin
  ShowMessage('checkPATH by George Litos, made with Lazarus.' +
    sLineBreak + 'Display path enviroment directories in a list, ' +
    'non existing directories are colored red, duplicates are blue.' +
    sLineBreak + sLineBreak + 'https://github.com/glls/checkPATH');
end;

procedure TFormMain.MenuRefreshClick(Sender: TObject);
begin
  GetIt;
end;

function TFormMain.countString(s: string): integer;
var
  i: integer;
  c: integer;
begin
  c := 0;
  for i := 0 to paths.Count - 1 do
  begin
{$IFDEF MSWINDOWS}
    if (UpperCase(s) = UpperCase(paths.Strings[i])) then
{$ELSE}
    if (s = paths.Strings[i]) then
{$ENDIF}
        Inc(c);
  end;
  Result := c;
end;

end.
