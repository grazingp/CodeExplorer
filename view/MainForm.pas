unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  SynEdit, SynHighlighterPas, Clipbrd, Menus, LCLType, StdCtrls,
  EggStrUtils, EggEnvUtils, EggFileUtils, EggPathUtils,
  EditorFrame;

type

  { TFMainForm }

  TFMainForm = class(TForm)
    FEditor_pages: TPageControl;
    Panel1: TPanel;
    FEditor_pnl: TPanel;
  private
		procedure OpenFile(AFileName: String);
  public
		constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  FMainForm: TFMainForm;

implementation

{$R *.lfm}

procedure TFMainForm.OpenFile(AFileName: String);
var
  sheet: TTabSheet;
  editor: TFEditorFrame;
begin
	sheet := FEditor_pages.AddTabSheet();
  sheet.Caption := EggExtractFileNameOnly(AFileName);
  editor := TFEditorFrame.Create(sheet);
  editor.Parent := sheet;
  editor.Align := alClient;
  editor.OpenFile(AFileName);
end;

constructor TFMainForm.Create(TheOwner: TComponent);
var
  fileName: String;
begin
  inherited Create(TheOwner);

  fileName := EggParamKey('file');
  if not EggStrEmpty(fileName) then
  begin
		if EggFileExists(fileName) then
    begin
      OpenFile(fileName);
    end;
  end;
end;

destructor TFMainForm.Destroy;
begin
  inherited Destroy;
end;

end.

