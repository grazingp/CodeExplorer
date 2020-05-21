unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  SynEdit, SynHighlighterPas, Clipbrd, Menus, LCLType, StdCtrls,
  EggStrUtils, EggEnvUtils, EggFileUtils, EggPathUtils,
  CodeTree, CodeParser, CodeBinder, CodeWriter;

type

  { TFMainForm }

  TFMainForm = class(TForm)
    FContent_ed: TSynEdit;
    FFunction_pop: TPopupMenu;
    FEditor_imgs: TImageList;
    FTree_imgs: TImageList;
    FEditor_book: TNotebook;
    FFunction_page: TPage;
    FTreeBar_imgs: TImageList;
    Label1: TLabel;
    FClassComment_ed: TMemo;
    MenuItem1: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    FClass_page: TPage;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    FClass_pop: TPopupMenu;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    Splitter1: TSplitter;
    FCode_ed: TSynEdit;
    Splitter2: TSplitter;
    SynFreePascalSyn1: TSynFreePascalSyn;
    ToolBar1: TToolBar;
    FPasteEditor_btn: TToolButton;
    FCode_tree: TTreeView;
    ToolBar2: TToolBar;
    FCodeParse_btn: TToolButton;
    FCodeWrite_btn: TToolButton;
    procedure FCodeParse_btnClick(Sender: TObject);
    procedure FCodeWrite_btnClick(Sender: TObject);
    procedure FCode_treeKeyUp(Sender: TObject;
      var Key: Word; Shift: TShiftState);
    procedure FCode_treeSelectionChanged(Sender: TObject);
    procedure FPasteEditor_btnClick(Sender: TObject);
  private
    FFileName: String;
    FCodeTree: TCodeTree;
    procedure DoChangeClassComment(Sender: TObject);
  procedure ParseCode(const ACode: String);
    procedure OpenFile(AFileName: String);
    procedure BindWindowTitle();
  public
		constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  FMainForm: TFMainForm;

implementation

{$R *.lfm}

const
  EDITOR_FUNCTION_PAGE: Integer = 0;
  EDITOR_CLASS_PAGE: Integer = 1;

{ TFMainForm }
procedure TFMainForm.FPasteEditor_btnClick(Sender: TObject);
begin
  //
  FCode_ed.Text := Clipboard.AsText;
end;

procedure TFMainForm.ParseCode(const ACode: String);
var
  p: TCodeParser;
  b: TCodeBinder;
begin
  p := nil;
  try
		p := TCodeParser.Create();
    p.Parse(ACode, FCodeTree);

    b := TCodeBinder.Create;
    b.Bind(FCodeTree, FCode_tree);
	finally
  	FreeAndNil(p);
  	FreeAndNil(b);
	end;
end;

procedure TFMainForm.DoChangeClassComment(Sender: TObject);
var
  node: TTreeNode;
  element: TCodeElement;
  cls: TCodeClass;
begin
  node := FCode_tree.Selected;
  if Assigned(node) then
  begin
    if not Assigned(node.Data) then exit;
    element := TCodeElement(node.Data);
    if element is TCodeClass then
    begin
      cls := element as TCodeClass;
      cls.Comment := FClassComment_ed.Text;
    end;
  end;
end;

procedure TFMainForm.OpenFile(AFileName: String);
var
  s: String;
begin
  if not EggFileExists(AFileName) then exit;
  FFileName := AFileName;

  EggFileToStr(AFileName, s);
  FCode_ed.Text := s;
  ParseCode(s);
end;

procedure TFMainForm.BindWindowTitle();
var
  nm: String;
begin
  nm := 'Code Explorer';
  if not EggStrEmpty(FFileName) then
  begin
  	nm += ' - ' + FFileName;
  end;

  Self.Caption := nm;
end;

constructor TFMainForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FFileName := EggParamKey('file');

  FCodeTree := TCodeTree.Create;

  if not EggStrEmpty(FFileName) then
  begin
		if EggFileExists(FFileName) then
    begin
      OpenFile(FFileName);
    end;
  end;

  FCode_ed.Font.Name := 'Ricty Diminished Discord';
  //FCode_ed.Font.Name := 'Meiryo';
  FCode_ed.Font.Quality := fqAntialiased;
  FContent_ed.Font.Assign(FCode_ed.Font);

  FContent_ed.Keystrokes.Assign(FCode_ed.Keystrokes);

  BindWindowTitle();
end;

destructor TFMainForm.Destroy;
begin
  FreeAndNil(FCodeTree);

  inherited Destroy;
end;

procedure TFMainForm.FCodeParse_btnClick(Sender: TObject);
var
  code: String;
begin
  code := FCode_ed.Text;
	ParseCode(code);
end;

procedure TFMainForm.FCodeWrite_btnClick(Sender: TObject);
var
  wri: TCodeWriter;
  src: String;
begin
  wri := nil;
  try
    wri := TCodeWriter.Create();
    wri.Write(FCodeTree, src);

    src := 'unit ' + EggExtractFileNameOnly(FFileName) + ';' + #10#10 + src;
    Clipboard.AsText := src;
  finally
    FreeAndNil(wri);
  end;
end;

procedure TFMainForm.FCode_treeKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  node: TTreeNode;
  x, y: Integer;
  p: TPoint;
begin
  if Key = VK_SPACE then
  begin
    if Assigned(FCode_tree.PopupMenu) then
    begin
      node := FCode_tree.Selected;
      if not Assigned(node) then exit;

      x := node.DisplayTextLeft() + 40;
      y := node.Bottom() + 5;
      p := FCode_tree.ClientToScreen(Point(x, y));
    	FCode_tree.PopupMenu.PopUp(p.x, p.y);
    end;
  end;
end;

procedure TFMainForm.FCode_treeSelectionChanged(Sender: TObject);
var
  node: TTreeNode;
  element: TCodeElement;
  func: TCodeFunction;
  cls: TCodeClass;
  pageIndex: Integer;
begin
  pageIndex := -1;
  node := FCode_tree.Selected;
  if Assigned(node) and Assigned(node.Data) then
  begin
    element := TCodeElement(node.Data);
    if element is TCodeClass then
    begin
      cls := element as TCodeClass;
      FCode_tree.PopupMenu := FClass_pop;
      FClassComment_ed.OnChange := nil;
      FClassComment_ed.Text := cls.Comment;
      FClassComment_ed.OnChange := @DoChangeClassComment;
      pageIndex := EDITOR_CLASS_PAGE;
    end
    else if element is TCodeFunction then
    begin
      func := element as TCodeFunction;
      FContent_ed.Text := func.Content;
      FCode_tree.PopupMenu := FFunction_pop;
      pageIndex := EDITOR_FUNCTION_PAGE;
    end;
  end;
  FEditor_book.PageIndex := pageIndex;
end;

end.

