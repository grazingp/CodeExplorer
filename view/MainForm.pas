unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  SynEdit, SynHighlighterPas, Clipbrd, Menus, LCLType, StdCtrls,
  EggStrUtils, EggEnvUtils, EggFileUtils, EggPathUtils,
  EditorFrame, ProjectInspectorFrame;

type

  { TFMainForm }

  TFMainForm = class(TForm)
    FEditor_pages: TPageControl;
    Panel1: TPanel;
    FEditor_pnl: TPanel;
    FProjectInspector_pnl: TPanel;
    Splitter1: TSplitter;
  private
    FProjectInspectorFrame: TFProjectInspectorFrame;
    procedure DoProjectSelectedFile(ASender: TObject; AFileName: String);
  procedure OpenFile(const AFileName: String);
    procedure OpenProjectFile(const AFileName: String);
    function GetEditor(ASheet: TTabSheet): TFEditorFrame;
  public
		constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  FMainForm: TFMainForm;

implementation

{$R *.lfm}

procedure TFMainForm.OpenFile(const AFileName: String);
var
  sheet: TTabSheet;
  editor: TFEditorFrame;
  i: Integer;
  ext: String;
begin
	ext := ExtractFileExt(AFileName);
  if AnsiCompareFileName(ext, '.lpk') = 0 then
  begin
    OpenProjectFile(AFileName);
    exit;
  end;
  for i := 0 to Pred(FEditor_pages.PageCount) do
  begin
  	sheet := FEditor_pages.Pages[i];
    if AnsiCompareFileName(GetEditor(sheet).FileName, AFileName) = 0 then
    begin
    	FEditor_pages.PageIndex := i;
      exit;
    end;
  end;

  sheet := FEditor_pages.AddTabSheet();
  sheet.Caption := EggExtractFileNameOnly(AFileName);
  editor := TFEditorFrame.Create(sheet);
  editor.Parent := sheet;
  editor.Align := alClient;
  editor.OpenFile(AFileName);
  FEditor_pages.ActivePage := sheet;
end;

procedure TFMainForm.DoProjectSelectedFile(ASender: TObject; AFileName: String);
begin
  OpenFile(AFileName);
end;

procedure TFMainForm.OpenProjectFile(const AFileName: String);
begin
  FProjectInspectorFrame.OpenFile(AFileName);
end;

function TFMainForm.GetEditor(ASheet: TTabSheet): TFEditorFrame;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Pred(ASheet.ControlCount) do
  begin
    if ASheet.Controls[i] is TFEditorFrame then
    begin
			Result := ASheet.Controls[i] as TFEditorFrame;
      exit;
    end;
  end;
end;

constructor TFMainForm.Create(TheOwner: TComponent);
var
  fileName: String;
begin
  inherited Create(TheOwner);

  FProjectInspectorFrame := TFProjectInspectorFrame.Create(FProjectInspector_pnl);
  FProjectInspectorFrame.Parent := FProjectInspector_pnl;
  FProjectInspectorFrame.Align := alClient;
  FProjectInspectorFrame.OnSelectedFile := @DoProjectSelectedFile;

  fileName := EggParamKey('file');
  if not EggStrEmpty(fileName) then
  begin
		if EggFileExists(fileName) then
    begin
      OpenFile(fileName);
    end;
  end;
  fileName := EggParamKey('proj');
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
  FreeAndNil(FProjectInspectorFrame);

  inherited Destroy;
end;

end.

