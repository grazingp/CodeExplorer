unit EditorFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  SynEdit, SynHighlighterPas, Clipbrd, Menus, LCLType, StdCtrls,
  EggStrUtils, EggEnvUtils, EggFileUtils, EggPathUtils,
  CodeTree, CodeParser, CodeBinder, CodeWriter;

type

  { TFEditorFrame }

  TFEditorFrame = class(TFrame)
    FAddArgument_mitem: TMenuItem;
    FAddArgvParameter_mitem: TMenuItem;
    FAddFunction_mitem: TMenuItem;
    FAddMember_mitem: TMenuItem;
    FAddPrivate_mitem: TMenuItem;
    FAddProperty_mitem: TMenuItem;
    FAddProtected_mitem: TMenuItem;
    FAddPublic_mitem: TMenuItem;
    FAddPublished_mitem: TMenuItem;
    FArgument_pop: TPopupMenu;
    FClassBaseClass_cmb: TComboBox;
    FClassComment_ed: TMemo;
    FClassName_ed: TEdit;
    FClassProperty_page: TPage;
    FClassScope_pop: TPopupMenu;
    FClass_page: TPage;
    FClass_pop: TPopupMenu;
    FCodeParse_btn: TToolButton;
    FCodeWrite_btn: TToolButton;
    FCode_ed: TSynEdit;
    FWriteCode_ed: TSynEdit;
    FCode_tree: TTreeView;
    FDeleteMember_mitem: TMenuItem;
    FDeleteProperty_mitem: TMenuItem;
    FDeleteSpecialize_mitem: TMenuItem;
    FEditor_book: TNotebook;
    FEditor_imgs: TImageList;
    FFunctionComment_ed: TMemo;
    FFunctionContent_ed: TSynEdit;
    FFunctionDelete_mitem: TMenuItem;
    FFunctionName_ed: TEdit;
    FFunctionResult_cmb: TComboBox;
    FFunction_page: TPage;
    FFunction_pop: TPopupMenu;
    FMemberComment_ed: TMemo;
    FParameterComment_ed: TMemo;
    FMemberName_ed: TEdit;
    FConstMemberComment_ed: TMemo;
    FParameterDefaultValue_ed: TEdit;
    FConstMemberDefaultValue_ed: TEdit;
    FConstMemberType_cmb: TComboBox;
    FConstMemberName_ed: TEdit;
    FParameterType_cmb: TComboBox;
    FParameterName_ed: TEdit;
    FMemberType_cmb: TComboBox;
    FParameterMemberType_cmb: TComboBox;
    FMember_page: TPage;
    FMember_pop: TPopupMenu;
    FPasteEditor_btn: TToolButton;
    FPropertyComment_ed: TMemo;
    FPropertyGetter_cmb: TComboBox;
    FPropertyName_ed: TEdit;
    FPropertySetter_cmb: TComboBox;
    FPropertyType_cmb: TComboBox;
    FProperty_pop: TPopupMenu;
    FSpecializeBaseClass_cmb: TComboBox;
    FSpecializeComment_ed: TMemo;
    FSpecializeName_ed: TEdit;
    FSpecializeTypeClass_cmb: TComboBox;
    FSpecialize_page: TPage;
    FSpecialize_pop: TPopupMenu;
    FTreeBar_imgs: TImageList;
    FTree_imgs: TImageList;
    FWrite_imgs: TImageList;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label27: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    MenuItem1: TMenuItem;
    FParameter_page: TPage;
    FDeleteParameter_mitem: TMenuItem;
    FConstMember_page: TPage;
    FDeleteConstMember_mitem: TMenuItem;
    FSource_pnl: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel14: TPanel;
    FCode_pnl: TPanel;
    FWrite_pnl: TPanel;
    Panel17: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    FParameter_pop: TPopupMenu;
    FConstMember_pop: TPopupMenu;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    SynFreePascalSyn1: TSynFreePascalSyn;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    ToolBar3: TToolBar;
    FAccept_btn: TToolButton;
    ToolButton1: TToolButton;
    FCancel_btn: TToolButton;
    FSaveCode_btn: TToolButton;
    procedure FAccept_btnClick(Sender: TObject);
    procedure FAddArgument_mitemClick(Sender: TObject);
    procedure FAddArgvParameter_mitemClick(Sender: TObject);
    procedure FAddFunction_mitemClick(Sender: TObject);
    procedure FAddMember_mitemClick(Sender: TObject);
    procedure FAddPrivate_mitemClick(Sender: TObject);
    procedure FAddProperty_mitemClick(Sender: TObject);
    procedure FAddProtected_mitemClick(Sender: TObject);
    procedure FAddPublic_mitemClick(Sender: TObject);
    procedure FAddPublished_mitemClick(Sender: TObject);
    procedure FCancel_btnClick(Sender: TObject);
    procedure FCodeParse_btnClick(Sender: TObject);
    procedure FCodeWrite_btnClick(Sender: TObject);
    procedure FCode_treeDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure FCode_treeDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
    var Accept: Boolean);
    procedure FCode_treeEnter(Sender: TObject);
    procedure FCode_treeKeyUp(Sender: TObject;
      var Key: Word; Shift: TShiftState);
    procedure FCode_treeMouseEnter(Sender: TObject);
    procedure FCode_treeSelectionChanged(Sender: TObject);
    procedure FDeleteConstMember_mitemClick(Sender: TObject);
    procedure FDeleteMember_mitemClick(Sender: TObject);
    procedure FDeleteParameter_mitemClick(Sender: TObject);
    procedure FDeleteProperty_mitemClick(Sender: TObject);
    procedure FDeleteSpecialize_mitemClick(Sender: TObject);
    procedure FFunctionDelete_mitemClick(Sender: TObject);
    procedure FPasteEditor_btnClick(Sender: TObject);
    procedure FSaveCode_btnClick(Sender: TObject);
  private
    FFileName: String;
    FCodeTree: TCodeTree;

    procedure DoChangeClassBaseClass(Sender: TObject);
    procedure DoChangeClassName(Sender: TObject);
    procedure DoChangeConstMemberComment(Sender: TObject);
    procedure DoChangeConstMemberDefaultValue(Sender: TObject);
    procedure DoChangeConstMemberName(Sender: TObject);
    procedure DoChangeConstMemberType(Sender: TObject);
    procedure DoChangeParameterComment(Sender: TObject);
    procedure DoChangeParameterDefaultValue(Sender: TObject);
    procedure DoChangeParameterMemberType(Sender: TObject);
    procedure DoChangeParameterName(Sender: TObject);
    procedure DoChangeParameterType(Sender: TObject);
    procedure DoChangeSpecializeBaseClass(Sender: TObject);
    procedure DoChangeSpecializeName(Sender: TObject);
    procedure DoChangeSpecializeTypeClass(Sender: TObject);
    procedure RebindElemnt();
    procedure BindElement(AElement: TCodeElement);
    procedure DeleteElement(AElementClass: TClass);
    procedure AppendArgument();
    procedure AppendArgvParameter(AArgv: TCodeFunctionArgv);
    procedure DoChangeClassComment(Sender: TObject);
    procedure DoChangeClassPropertyComment(Sender: TObject);
    procedure DoChangeClassPropertyGetter(Sender: TObject);
    procedure DoChangeClassPropertyName(Sender: TObject);
    procedure DoChangeClassPropertySetter(Sender: TObject);
    procedure DoChangeClassPropetyType(Sender: TObject);
    procedure DoChangeFunctionComment(Sender: TObject);
    procedure DoChangeFunctionContent(Sender: TObject);
    procedure DoChangeFunctionName(Sender: TObject);
    procedure DoChangeFunctionResultType(Sender: TObject);
    procedure DoChangeMemberComment(Sender: TObject);
    procedure DoChangeMemberName(Sender: TObject);
    procedure DoChangeMemberType(Sender: TObject);
    function GetInsertMark(ASrc, ADest: TTreeNode): TTreeViewInsertMarkType;
  	procedure ParseCode(const ACode: String);
    function AddScope(AScope: String): TTreeNode;
    procedure AddFunction();
    procedure AddClassMember();
    procedure AddClassProperty();
  public
		constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure OpenFile(const AFileName: String);
    property FileName: String read FFileName;
  end;

implementation

{$R *.lfm}

const
  EDITOR_FUNCTION_PAGE: Integer = 0;
  EDITOR_CLASS_PAGE: Integer = 1;
  EDITOR_CLASS_PROPERTY_PAGE = 2;
  EDITOR_MEMBER_PAGE = 3;
  EDITOR_SPECIALIZE_PAGE = 4;
  EDITOR_PARAMETER_PAGE = 5;
  EDITOR_CONST_MEMBER_PAGE = 6;

  CODE_WIDTH = 384;

{ TFMainForm }
procedure TFEditorFrame.FPasteEditor_btnClick(Sender: TObject);
begin
  //
  FCode_ed.Text := Clipboard.AsText;
end;

procedure TFEditorFrame.FSaveCode_btnClick(Sender: TObject);
var
  src: String;
begin
  src := FCode_ed.Text;
  if not EggStrEmpty(FFileName) then
  begin
		EggStrToFile(FFileName, src);
  end;
end;

procedure TFEditorFrame.RebindElemnt();
var
  node: TTreeNode;
  element: TCodeElement;
begin
  node := FCode_tree.Selected;
  if Assigned(node) then
  begin
    element := TCodeElement(node.Data);
    BindElement(element);
  end;
end;

procedure TFEditorFrame.DoChangeSpecializeName(Sender: TObject);
var
  node: TTreeNode;
  element: TCodeElement;
  sp: TCodeSpecialize;
begin
  node := FCode_tree.Selected;
  if Assigned(node) then
  begin
    if not Assigned(node.Data) then exit;
    element := TCodeElement(node.Data);
    if element is TCodeSpecialize then
    begin
      sp := element as TCodeSpecialize;
      sp.Name := FSpecializeName_ed.Text;
    end;
  end;
end;

procedure TFEditorFrame.DoChangeSpecializeTypeClass(Sender: TObject);
var
  node: TTreeNode;
  element: TCodeElement;
  sp: TCodeSpecialize;
begin
  node := FCode_tree.Selected;
  if Assigned(node) then
  begin
    if not Assigned(node.Data) then exit;
    element := TCodeElement(node.Data);
    if element is TCodeSpecialize then
    begin
      sp := element as TCodeSpecialize;
      sp.TypeClass := FSpecializeTypeClass_cmb.Text;
    end;
  end;
end;

procedure TFEditorFrame.DoChangeSpecializeBaseClass(Sender: TObject);
var
  node: TTreeNode;
  element: TCodeElement;
  sp: TCodeSpecialize;
begin
  node := FCode_tree.Selected;
  if Assigned(node) then
  begin
    if not Assigned(node.Data) then exit;
    element := TCodeElement(node.Data);
    if element is TCodeSpecialize then
    begin
      sp := element as TCodeSpecialize;
      sp.BaseClass := FSpecializeBaseClass_cmb.Text;
    end;
  end;
end;

procedure TFEditorFrame.DoChangeClassName(Sender: TObject);
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
      cls.Name := FClassName_ed.Text;
    end;
  end;
end;

procedure TFEditorFrame.DoChangeConstMemberComment(Sender: TObject);
var
  node: TTreeNode;
  element: TCodeElement;
  mem: TCodeConstMember;
begin
  node := FCode_tree.Selected;
  if Assigned(node) then
  begin
    if not Assigned(node.Data) then exit;
    element := TCodeElement(node.Data);
    if element is TCodeConstMember then
    begin
      mem := element as TCodeConstMember;
      mem.Comment := FConstMemberComment_ed.Text;
    end;
  end;
end;

procedure TFEditorFrame.DoChangeConstMemberDefaultValue(Sender: TObject);
var
  node: TTreeNode;
  element: TCodeElement;
  mem: TCodeConstMember;
begin
  node := FCode_tree.Selected;
  if Assigned(node) then
  begin
    if not Assigned(node.Data) then exit;
    element := TCodeElement(node.Data);
    if element is TCodeConstMember then
    begin
      mem := element as TCodeConstMember;
      mem.DefaultValue := FConstMemberDefaultValue_ed.Text;
    end;
  end;
end;

procedure TFEditorFrame.DoChangeConstMemberName(Sender: TObject);
var
  node: TTreeNode;
  element: TCodeElement;
  mem: TCodeConstMember;
begin
  node := FCode_tree.Selected;
  if Assigned(node) then
  begin
    if not Assigned(node.Data) then exit;
    element := TCodeElement(node.Data);
    if element is TCodeConstMember then
    begin
      mem := element as TCodeConstMember;
      mem.Name := FConstMemberName_ed.Text;
    end;
  end;
end;

procedure TFEditorFrame.DoChangeConstMemberType(Sender: TObject);
var
  node: TTreeNode;
  element: TCodeElement;
  mem: TCodeConstMember;
begin
  node := FCode_tree.Selected;
  if Assigned(node) then
  begin
    if not Assigned(node.Data) then exit;
    element := TCodeElement(node.Data);
    if element is TCodeConstMember then
    begin
      mem := element as TCodeConstMember;
      mem.MemberType := FConstMemberType_cmb.Text;
    end;
  end;
end;

procedure TFEditorFrame.DoChangeParameterComment(Sender: TObject);
var
  node: TTreeNode;
  element: TCodeElement;
  param: TCodeFunctionParameter;
begin
  node := FCode_tree.Selected;
  if Assigned(node) then
  begin
    if not Assigned(node.Data) then exit;
    element := TCodeElement(node.Data);
    if element is TCodeFunctionParameter then
    begin
      param := element as TCodeFunctionParameter;
      param.Comment := FParameterComment_ed.Text;
    end;
  end;
end;

procedure TFEditorFrame.DoChangeParameterDefaultValue(Sender: TObject);
var
  node: TTreeNode;
  element: TCodeElement;
  param: TCodeFunctionParameter;
begin
  node := FCode_tree.Selected;
  if Assigned(node) then
  begin
    if not Assigned(node.Data) then exit;
    element := TCodeElement(node.Data);
    if element is TCodeFunctionParameter then
    begin
      param := element as TCodeFunctionParameter;
      param.DefaultValue := FParameterDefaultValue_ed.Text;
    end;
  end;
end;

procedure TFEditorFrame.DoChangeParameterMemberType(Sender: TObject);
var
  node: TTreeNode;
  element: TCodeElement;
  param: TCodeFunctionParameter;
begin
  node := FCode_tree.Selected;
  if Assigned(node) then
  begin
    if not Assigned(node.Data) then exit;
    element := TCodeElement(node.Data);
    if element is TCodeFunctionParameter then
    begin
      param := element as TCodeFunctionParameter;
      param.MemberType := FParameterMemberType_cmb.Text;
    end;
  end;
end;

procedure TFEditorFrame.DoChangeParameterName(Sender: TObject);
var
  node: TTreeNode;
  element: TCodeElement;
  param: TCodeFunctionParameter;
begin
  node := FCode_tree.Selected;
  if Assigned(node) then
  begin
    if not Assigned(node.Data) then exit;
    element := TCodeElement(node.Data);
    if element is TCodeFunctionParameter then
    begin
      param := element as TCodeFunctionParameter;
      param.Name := FParameterName_ed.Text;
    end;
  end;
end;

procedure TFEditorFrame.DoChangeParameterType(Sender: TObject);
var
  node: TTreeNode;
  element: TCodeElement;
  param: TCodeFunctionParameter;
begin
  node := FCode_tree.Selected;
  if Assigned(node) then
  begin
    if not Assigned(node.Data) then exit;
    element := TCodeElement(node.Data);
    if element is TCodeFunctionParameter then
    begin
      param := element as TCodeFunctionParameter;
      param.ParameterType := FParameterType_cmb.Text;
    end;
  end;
end;

procedure TFEditorFrame.DoChangeClassBaseClass(Sender: TObject);
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
      cls.BaseClass := FClassBaseClass_cmb.Text;
    end;
  end;
end;

procedure TFEditorFrame.BindElement(AElement: TCodeElement);
var
  node: TTreeNode;
  b: TCodeBinder;
begin
  b := nil;
  try
    b := TCodeBinder.Create;
    node := FCode_tree.Items.FindNodeWithData(AElement);
    b.BindElement(FCode_tree, node, AElement);
  finally
    FreeAndNil(b);
  end;
end;

procedure TFEditorFrame.DeleteElement(AElementClass: TClass);
var
  node, nextNode: TTreeNode;
  element: TCodeElement;
  func: TCodeFunction;
begin
  node := FCode_tree.Selected;
  if Assigned(node) then
  begin
    if not Assigned(node.Data) then exit;
    element := TCodeElement(node.Data);
    if element.InheritsFrom(AElementClass) then
    begin
      nextNode := node.GetPrevSibling();
      if not Assigned(nextNode) then
      begin
        nextNode := node.GetNextSibling();
      end;
      if not Assigned(nextNode) then
      begin
        nextNode := node.Parent;
      end;
      FCodeTree.RemoveElement(element);
      FCode_tree.Items.Delete(node);
      FCode_tree.Selected := nextNode;
    end;
  end;
end;

procedure TFEditorFrame.AppendArgument();
var
  node: TTreeNode;
  element: TCodeElement;
  func: TCodeFunction;
  arg: TCodeFunctionArgv;
begin
  node := FCode_tree.Selected;
  if Assigned(node) then
  begin
    if not Assigned(node.Data) then exit;
    element := TCodeElement(node.Data);
    if element is TCodeFunction then
    begin
      func := element as TCodeFunction;
      arg := func.AppendArgument();
      AppendArgvParameter(arg);
    end;
  end;
end;

procedure TFEditorFrame.AppendArgvParameter(AArgv: TCodeFunctionArgv);
var
  param: TCodeFunctionParameter;
  node: TTreeNode;
begin
  param := AArgv.AppendParameter();
  param.Name := 'Parameter';
  BindElement(AArgv.FunctionElement);
  node := FCode_tree.Items.FindNodeWithData(param);
  FCode_tree.Selected := node;
end;

procedure TFEditorFrame.ParseCode(const ACode: String);
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

procedure TFEditorFrame.DoChangeClassComment(Sender: TObject);
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

procedure TFEditorFrame.DoChangeClassPropertyComment(Sender: TObject);
var
  node: TTreeNode;
  element: TCodeElement;
  prop: TCodeClassProperty;
begin
  node := FCode_tree.Selected;
  if Assigned(node) then
  begin
    if not Assigned(node.Data) then exit;
    element := TCodeElement(node.Data);
    if element is TCodeClassProperty then
    begin
      prop := element as TCodeClassProperty;
      prop.Comment := FPropertyComment_ed.Text;
    end;
  end;
end;

procedure TFEditorFrame.DoChangeClassPropertyGetter(Sender: TObject);
var
  node: TTreeNode;
  element: TCodeElement;
  prop: TCodeClassProperty;
begin
  node := FCode_tree.Selected;
  if Assigned(node) then
  begin
    if not Assigned(node.Data) then exit;
    element := TCodeElement(node.Data);
    if element is TCodeClassProperty then
    begin
      prop := element as TCodeClassProperty;
      prop.Getter := FPropertyGetter_cmb.Text;
    end;
  end;
end;

procedure TFEditorFrame.DoChangeClassPropertyName(Sender: TObject);
var
  node: TTreeNode;
  element: TCodeElement;
  prop: TCodeClassProperty;
begin
  node := FCode_tree.Selected;
  if Assigned(node) then
  begin
    if not Assigned(node.Data) then exit;
    element := TCodeElement(node.Data);
    if element is TCodeClassProperty then
    begin
      prop := element as TCodeClassProperty;
      prop.Name := FPropertyName_ed.Text;
    end;
  end;
end;

procedure TFEditorFrame.DoChangeClassPropertySetter(Sender: TObject);
var
  node: TTreeNode;
  element: TCodeElement;
  prop: TCodeClassProperty;
begin
  node := FCode_tree.Selected;
  if Assigned(node) then
  begin
    if not Assigned(node.Data) then exit;
    element := TCodeElement(node.Data);
    if element is TCodeClassProperty then
    begin
      prop := element as TCodeClassProperty;
      prop.Setter := FPropertySetter_cmb.Text;
    end;
  end;
end;

procedure TFEditorFrame.DoChangeClassPropetyType(Sender: TObject);
var
  node: TTreeNode;
  element: TCodeElement;
  prop: TCodeClassProperty;
begin
  node := FCode_tree.Selected;
  if Assigned(node) then
  begin
    if not Assigned(node.Data) then exit;
    element := TCodeElement(node.Data);
    if element is TCodeClassProperty then
    begin
      prop := element as TCodeClassProperty;
      prop.PropertyType := FPropertyType_cmb.Text;
    end;
  end;
end;

procedure TFEditorFrame.DoChangeFunctionComment(Sender: TObject);
var
  node: TTreeNode;
  element: TCodeElement;
  func: TCodeFunction;
begin
  node := FCode_tree.Selected;
  if Assigned(node) then
  begin
    if not Assigned(node.Data) then exit;
    element := TCodeElement(node.Data);
    if element is TCodeFunction then
    begin
      func := element as TCodeFunction;
      func.Comment := FFunctionComment_ed.Text;
    end;
  end;
end;

procedure TFEditorFrame.DoChangeFunctionContent(Sender: TObject);
var
  node: TTreeNode;
  element: TCodeElement;
  func: TCodeFunction;
begin
  node := FCode_tree.Selected;
  if Assigned(node) then
  begin
    if not Assigned(node.Data) then exit;
    element := TCodeElement(node.Data);
    if element is TCodeFunction then
    begin
      func := element as TCodeFunction;
      func.Content := FFunctionContent_ed.Text;
    end;
  end;
end;

procedure TFEditorFrame.DoChangeFunctionName(Sender: TObject);
var
  node: TTreeNode;
  element: TCodeElement;
  func: TCodeFunction;
begin
  node := FCode_tree.Selected;
  if Assigned(node) then
  begin
    if not Assigned(node.Data) then exit;
    element := TCodeElement(node.Data);
    if element is TCodeFunction then
    begin
      func := element as TCodeFunction;
      func.Name := FFunctionName_ed.Text;
    end;
  end;
end;

procedure TFEditorFrame.DoChangeFunctionResultType(Sender: TObject);
var
  node: TTreeNode;
  element: TCodeElement;
  func: TCodeFunction;
begin
  node := FCode_tree.Selected;
  if Assigned(node) then
  begin
    if not Assigned(node.Data) then exit;
    element := TCodeElement(node.Data);
    if element is TCodeFunction then
    begin
      func := element as TCodeFunction;
      func.ResultType := FFunctionResult_cmb.Text;
    end;
  end;
end;

procedure TFEditorFrame.DoChangeMemberComment(Sender: TObject);
var
  node: TTreeNode;
  element: TCodeElement;
  mem: TCodeMember;
begin
  node := FCode_tree.Selected;
  if Assigned(node) then
  begin
    if not Assigned(node.Data) then exit;
    element := TCodeElement(node.Data);
    if element is TCodeMember then
    begin
      mem := element as TCodeMember;
      mem.Comment := FMemberComment_ed.Text;
    end;
  end;
end;

procedure TFEditorFrame.DoChangeMemberName(Sender: TObject);
var
  node: TTreeNode;
  element: TCodeElement;
  mem: TCodeMember;
begin
  node := FCode_tree.Selected;
  if Assigned(node) then
  begin
    if not Assigned(node.Data) then exit;
    element := TCodeElement(node.Data);
    if element is TCodeMember then
    begin
      mem := element as TCodeMember;
      mem.Name := FMemberName_ed.Text;
    end;
  end;
end;

procedure TFEditorFrame.DoChangeMemberType(Sender: TObject);
var
  node: TTreeNode;
  element: TCodeElement;
  mem: TCodeMember;
begin
  node := FCode_tree.Selected;
  if Assigned(node) then
  begin
    if not Assigned(node.Data) then exit;
    element := TCodeElement(node.Data);
    if element is TCodeMember then
    begin
      mem := element as TCodeMember;
      mem.MemberType := FMemberType_cmb.Text;
    end;
  end;
end;

function TFEditorFrame.GetInsertMark(ASrc, ADest: TTreeNode): TTreeViewInsertMarkType;
var
  srcEle: TCodeElement;
  destParent: TTreeNode;
begin
  Result := tvimNone;
  srcEle := TCodeElement(ASrc.Data);
  if (srcEle is TCodeClassMember)
    or (srcEle is TCodeClassFunction)
    or (srcEle is TCodeClassProperty)
  then begin
  	destParent := ADest.Parent;
    if not Assigned(destParent) then exit;
    if TCodeElement(destParent.Data) is TCodeClassScope then
    begin
      Result := tvimAsPrevSibling;
      exit;
    end;
  end;
end;

procedure TFEditorFrame.OpenFile(const AFileName: String);
var
  s: String;
begin
  if not EggFileExists(AFileName) then exit;
  FFileName := AFileName;

  EggFileToStr(AFileName, s);
  FCode_ed.Text := s;
  ParseCode(s);
end;

function TFEditorFrame.AddScope(AScope: String): TTreeNode;
var
  node: TTreeNode;
  element: TCodeElement;
  cls: TCodeClass;
  scope: TCodeClassScope;
begin
  Result := nil;
  node := FCode_tree.Selected;
  if not Assigned(node) then exit;
  element := TCodeElement(node.Data);
  if element is TCodeClass then
  begin
  	cls := element as TCodeClass;
    scope := cls.AddScope(AScope);
    Result := FCode_tree.Items.AddChildObject(node, AScope, scope);
    FCode_tree.Selected := Result;
  end;
end;

procedure TFEditorFrame.AddFunction();
var
  node, funcNode: TTreeNode;
  element: TCodeElement;
  scope: TCodeClassScope;
  func: TCodeClassFunction;
begin
  node := FCode_tree.Selected;
  if not Assigned(node) then exit;
  element := TCodeElement(node.Data);
  if element is TCodeClassScope then
  begin
  	scope := element as TCodeClassScope;
    func := scope.AddFunction();
    func.Name := 'Function';
    funcNode := FCode_tree.Items.AddChildObject(node, func.Name, func);
    FCode_tree.Selected := funcNode;
  end;
end;

procedure TFEditorFrame.AddClassMember();
var
  node, memNode: TTreeNode;
  element: TCodeElement;
  scope: TCodeClassScope;
  mem: TCodeClassMember;
begin
  node := FCode_tree.Selected;
  if not Assigned(node) then exit;
  element := TCodeElement(node.Data);
  if element is TCodeClassScope then
  begin
  	scope := element as TCodeClassScope;
    mem := scope.AddClassMember();
    mem.Name := 'Member';
    memNode := FCode_tree.Items.AddChildObject(node, mem.Name, mem);
    FCode_tree.Selected := memNode;
  end;
end;

procedure TFEditorFrame.AddClassProperty();
var
  node, propNode: TTreeNode;
  element: TCodeElement;
  scope: TCodeClassScope;
  prop: TCodeClassProperty;
begin
  node := FCode_tree.Selected;
  if not Assigned(node) then exit;
  element := TCodeElement(node.Data);
  if element is TCodeClassScope then
  begin
  	scope := element as TCodeClassScope;
    prop := scope.AddClassProperty();
    prop.Name := 'Member';
    propNode := FCode_tree.Items.AddChildObject(node, prop.Name, prop);
    FCode_tree.Selected := propNode;
  end;
end;

constructor TFEditorFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FFileName := '';

  FCodeTree := TCodeTree.Create;
  FEditor_book.PageIndex := -1;

  FCode_ed.Font.Name := 'Ricty Diminished Discord';
  //FCode_ed.Font.Name := 'Meiryo';
  FCode_ed.Font.Quality := fqAntialiased;
  FFunctionContent_ed.Font.Assign(FCode_ed.Font);

  FFunctionContent_ed.Keystrokes.Assign(FCode_ed.Keystrokes);

  FCode_pnl.Width := CODE_WIDTH;
  FWrite_pnl.Visible := False;
  FWrite_pnl.Width := CODE_WIDTH;
end;

destructor TFEditorFrame.Destroy;
begin
  FreeAndNil(FCodeTree);

  inherited Destroy;
end;

procedure TFEditorFrame.FCodeParse_btnClick(Sender: TObject);
var
  code: String;
begin
  code := FCode_ed.Text;
  ParseCode(code);
end;

procedure TFEditorFrame.FAddPrivate_mitemClick(Sender: TObject);
begin
  AddScope('private').StateIndex := DefaultClassPrivateImageIndex;
end;

procedure TFEditorFrame.FAddProperty_mitemClick(Sender: TObject);
begin
  AddClassProperty();
end;

procedure TFEditorFrame.FAddFunction_mitemClick(Sender: TObject);
begin
  AddFunction();
end;

procedure TFEditorFrame.FAddArgument_mitemClick(Sender: TObject);
begin
  AppendArgument();
end;

procedure TFEditorFrame.FAccept_btnClick(Sender: TObject);
begin
  FWrite_pnl.Visible := False;
  FCode_ed.Text := FWriteCode_ed.Text;
  FCode_pnl.Width := CODE_WIDTH;
end;

procedure TFEditorFrame.FAddArgvParameter_mitemClick(Sender: TObject);
var
  node: TTreeNode;
  element: TCodeElement;
  arg: TCodeFunctionArgv;
begin
  node := FCode_tree.Selected;
  if Assigned(node) then
  begin
    if not Assigned(node.Data) then exit;
    element := TCodeElement(node.Data);
    if element is TCodeFunctionArgv then
    begin
      arg := element as TCodeFunctionArgv;
      AppendArgvParameter(arg);
    end;
  end;
end;

procedure TFEditorFrame.FAddMember_mitemClick(Sender: TObject);
begin
  AddClassMember();
end;

procedure TFEditorFrame.FAddProtected_mitemClick(Sender: TObject);
begin
  AddScope('protected').StateIndex := DefaultClassProtectedImageIndex;
end;

procedure TFEditorFrame.FAddPublic_mitemClick(Sender: TObject);
begin
  AddScope('public').StateIndex := DefaultClassPublicImageIndex;
end;

procedure TFEditorFrame.FAddPublished_mitemClick(Sender: TObject);
begin
  AddScope('published').StateIndex := DefaultClassPublishedImageIndex;
end;

procedure TFEditorFrame.FCancel_btnClick(Sender: TObject);
begin
	FWriteCode_ed.Text := '';
  FWrite_pnl.Visible := False;
  FCode_pnl.Width := CODE_WIDTH;
end;

procedure TFEditorFrame.FCodeWrite_btnClick(Sender: TObject);
var
  wri: TCodeWriter;
  src: String;
begin
  wri := nil;
  try
    wri := TCodeWriter.Create();
    wri.Write(FCodeTree, src);

    if not EggStrEqual(#10, LineEnding) then
    begin
      src := StringReplace(src, #10, LineEnding, [rfReplaceAll]);
    end;

    //Clipboard.AsText := src;
		FWriteCode_ed.Text := src;

    FCode_pnl.Width := CODE_WIDTH * 2;
    FWrite_pnl.Width := CODE_WIDTH;
    FWrite_pnl.Visible := True;
  finally
    FreeAndNil(wri);
  end;
end;

procedure TFEditorFrame.FCode_treeDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  src, dst: TTreeNode;
  mark: TTreeViewInsertMarkType;
  srcEle: TCodeElement;
begin
  FCode_tree.InsertMarkType := tvimNone;
  src := FCode_tree.Selected;
  if not Assigned(src) then exit;
  dst := FCode_tree.GetNodeAt(X,Y);
  mark := GetInsertMark(src, dst);
  srcEle := TCodeElement(src.Data);
  if mark = tvimNone then
  begin
    exit;
  end
  else if mark = tvimAsPrevSibling then
  begin
    src.MoveTo(dst, naInsert);
    srcEle.MoveTo(TCodeElement(dst.Data), emInsert);
  end;
end;

procedure TFEditorFrame.FCode_treeDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
var
  src, dst: TTreeNode;
  mark: TTreeViewInsertMarkType;
begin
  Accept := False;
  FCode_tree.InsertMarkType := tvimNone;
  src := FCode_tree.Selected;
  if not Assigned(src) then exit;

  dst := FCode_tree.GetNodeAt(X,Y);
  if (not Assigned(dst)) or (src = dst) then
  begin
    exit;
  end;

  mark := GetInsertMark(src, dst);
  if mark = tvimNone then
  begin
    exit;
  end;
  FCode_tree.SetInsertMark(dst, mark);
  Accept := True;
end;

procedure TFEditorFrame.FCode_treeEnter(Sender: TObject);
begin
  RebindElemnt();
end;

procedure TFEditorFrame.FCode_treeKeyUp(Sender: TObject;
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

procedure TFEditorFrame.FCode_treeMouseEnter(Sender: TObject);
begin
  if not FCode_tree.Focused then
  begin
    RebindElemnt();
  end;
end;

procedure TFEditorFrame.FCode_treeSelectionChanged(Sender: TObject);
var
  node: TTreeNode;
  element: TCodeElement;
  func: TCodeFunction;
  cls: TCodeClass;
  pageIndex: Integer;
  prop: TCodeClassProperty;
  mem: TCodeMember;
  sp: TCodeSpecialize;
  param: TCodeFunctionParameter;
  constMem: TCodeConstMember;
begin
  pageIndex := -1;
  node := FCode_tree.Selected;
  if Assigned(node) and Assigned(node.Data) then
  begin
    element := TCodeElement(node.Data);
    if element is TCodeClass then
    begin
      cls := element as TCodeClass;

      FClassName_ed.OnChange := nil;
      FClassName_ed.Text := cls.Name;
      FClassName_ed.OnChange := @DoChangeClassName;
      FClassBaseClass_cmb.OnChange := nil;
      FClassBaseClass_cmb.Text := cls.BaseClass;
      FClassBaseClass_cmb.OnChange := @DoChangeClassBaseClass;
      FClassComment_ed.OnChange := nil;
      FClassComment_ed.Text := cls.Comment;
      FClassComment_ed.OnChange := @DoChangeClassComment;

      FCode_tree.PopupMenu := FClass_pop;
      pageIndex := EDITOR_CLASS_PAGE;
    end
    else if element is TCodeClassScope then
    begin
      FCode_tree.PopupMenu := FClassScope_pop;
    end
    else if element is TCodeConstMember then
    begin
      constMem := element as TCodeConstMember;
      FConstMemberName_ed.OnChange := nil;
      FConstMemberName_ed.Text := constMem.Name;
      FConstMemberName_ed.OnChange := @DoChangeConstMemberName;
      FConstMemberType_cmb.OnChange := nil;
      FConstMemberType_cmb.Text := constMem.MemberType;
      FConstMemberType_cmb.OnChange := @DoChangeConstMemberType;
      FConstMemberDefaultValue_ed.OnChange := nil;
      FConstMemberDefaultValue_ed.Text := constMem.DefaultValue;
      FConstMemberDefaultValue_ed.OnChange := @DoChangeConstMemberDefaultValue;
      FConstMemberComment_ed.OnChange := nil;
      FConstMemberComment_ed.Text := param.Comment;
      FConstMemberComment_ed.OnChange := @DoChangeConstMemberComment;

      FCode_tree.PopupMenu := FConstMember_pop;
      pageIndex := EDITOR_CONST_MEMBER_PAGE;
    end
    else if element is TCodeFunction then
    begin
      func := element as TCodeFunction;
      FFunctionName_ed.OnChange := nil;
      FFunctionName_ed.Text := func.Name;
      FFunctionName_ed.OnChange := @DoChangeFunctionName;
      FFunctionResult_cmb.OnChange := nil;
      FFunctionResult_cmb.Text := func.ResultType;
      FFunctionResult_cmb.OnChange := @DoChangeFunctionResultType;
      FFunctionComment_ed.OnChange := nil;
      FFunctionComment_ed.Text := func.Comment;
      FFunctionComment_ed.OnChange := @DoChangeFunctionComment;
      FFunctionContent_ed.OnChange := nil;
      FFunctionContent_ed.Text := func.Content;
      FFunctionContent_ed.OnChange := @DoChangeFunctionContent;
      FCode_tree.PopupMenu := FFunction_pop;
      pageIndex := EDITOR_FUNCTION_PAGE;
    end
    else if element is TCodeFunctionArgv then
    begin
      FCode_tree.PopupMenu := FArgument_pop;
    end
    else if element is TCodeClassProperty then
    begin
      prop := element as TCodeClassProperty;
  		FPropertyName_ed.OnChange := nil;
      FPropertyName_ed.Text := prop.Name;
      FPropertyName_ed.OnChange := @DoChangeClassPropertyName;
      FPropertyType_cmb.OnChange := nil;
      FPropertyType_cmb.Text := prop.PropertyType;
      FPropertyType_cmb.OnChange := @DoChangeClassPropetyType;

      FPropertyGetter_cmb.OnChange := nil;
      FPropertyGetter_cmb.Text := prop.Getter;
      FPropertyGetter_cmb.OnChange := @DoChangeClassPropertyGetter;
      FPropertySetter_cmb.OnChange := nil;
      FPropertySetter_cmb.Text := prop.Setter;
      FPropertySetter_cmb.OnChange := @DoChangeClassPropertySetter;

      FPropertyComment_ed.OnChange := nil;
      FPropertyComment_ed.Text := prop.Comment;
      FPropertyComment_ed.OnChange := @DoChangeClassPropertyComment;
      FCode_tree.PopupMenu := FProperty_pop;
      pageIndex := EDITOR_CLASS_PROPERTY_PAGE;
    end
    else if element is TCodeFunctionParameter then
    begin
      param := element as TCodeFunctionParameter;
      FParameterName_ed.OnChange := nil;
      FParameterName_ed.Text := param.Name;
      FParameterName_ed.OnChange := @DoChangeParameterName;
      FParameterMemberType_cmb.OnChange := nil;
      FParameterMemberType_cmb.Text := param.MemberType;
      FParameterMemberType_cmb.OnChange := @DoChangeParameterMemberType;
      FParameterType_cmb.OnChange := nil;
      FParameterType_cmb.Text := param.ParameterType;
      FParameterType_cmb.OnChange := @DoChangeParameterType;
      FParameterDefaultValue_ed.OnChange := nil;
      FParameterDefaultValue_ed.Text := param.DefaultValue;
      FParameterDefaultValue_ed.OnChange := @DoChangeParameterDefaultValue;
      FParameterComment_ed.OnChange := nil;
      FParameterComment_ed.Text := param.Comment;
      FParameterComment_ed.OnChange := @DoChangeParameterComment;

      FCode_tree.PopupMenu := FParameter_pop;
      pageIndex := EDITOR_PARAMETER_PAGE;
    end
    else if element is TCodeMember then
    begin
      mem := element as TCodeMember;
      FMemberName_ed.OnChange := nil;
      FMemberName_ed.Text := mem.Name;
      FMemberName_ed.OnChange := @DoChangeMemberName;
      FMemberType_cmb.OnChange := nil;
      FMemberType_cmb.Text := mem.MemberType;
      FMemberType_cmb.OnChange := @DoChangeMemberType;
      FMemberComment_ed.OnChange := nil;
      FMemberComment_ed.Text := mem.Comment;
      FMemberComment_ed.OnChange := @DoChangeMemberComment;

      FCode_tree.PopupMenu := FMember_pop;
      pageIndex := EDITOR_MEMBER_PAGE;
    end
    else if element is TCodeSpecialize then
    begin
      sp := element as TCodeSpecialize;
      FSpecializeName_ed.OnChange := nil;
      FSpecializeName_ed.Text := sp.Name;
      FSpecializeName_ed.OnChange := @DoChangeSpecializeName;
      FSpecializeBaseClass_cmb.OnChange := nil;
      FSpecializeBaseClass_cmb.Text := sp.BaseClass;
      FSpecializeBaseClass_cmb.OnChange := @DoChangeSpecializeBaseClass;
      FSpecializeTypeClass_cmb.OnChange := nil;
      FSpecializeTypeClass_cmb.Text := sp.TypeClass;
      FSpecializeTypeClass_cmb.OnChange := @DoChangeSpecializeTypeClass;

      FCode_tree.PopupMenu := FSpecialize_pop;
      pageIndex := EDITOR_SPECIALIZE_PAGE
    end
    ;
  end;
  FEditor_book.PageIndex := pageIndex;
end;

procedure TFEditorFrame.FDeleteConstMember_mitemClick(Sender: TObject);
begin
  DeleteElement(TCodeConstMember.ClassType);
end;

procedure TFEditorFrame.FDeleteMember_mitemClick(Sender: TObject);
begin
  DeleteElement(TCodeMember.ClassType);
end;

procedure TFEditorFrame.FDeleteParameter_mitemClick(Sender: TObject);
begin
  DeleteElement(TCodeFunctionParameter.ClassType);
end;

procedure TFEditorFrame.FDeleteProperty_mitemClick(Sender: TObject);
begin
  DeleteElement(TCodeClassProperty.ClassType);
end;

procedure TFEditorFrame.FDeleteSpecialize_mitemClick(Sender: TObject);
begin
  DeleteElement(TCodeSpecialize.ClassType);
end;

procedure TFEditorFrame.FFunctionDelete_mitemClick(Sender: TObject);
begin
  DeleteElement(TCodeFunction.ClassType);
end;

end.

