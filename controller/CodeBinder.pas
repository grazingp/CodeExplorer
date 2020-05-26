unit CodeBinder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls,
  EggStrUtils,
  CodeTree;

const
	DefaultFolderImageIndex = 0;
  DefaultClassImageIndex = 1;
  DefaultClassPrivateImageIndex = 2;
  DefaultClassProtectedImageIndex = 3;
  DefaultClassPublicImageIndex = 4;
  DefaultClassPublishedImageIndex = 5;
  DefaultMemberImageIndex = 6;
  DefaultClassPropertyImageIndex = 7;
  DefaultFunctionImageIndex = 8;
  DefaultUnitImageIndex = 9;
  DefaultFunctionArgvImageIndex = 10;
  DefaultSpecializeImageIndex = 11;

type

  { TCodeBinder }

  TCodeBinder = class(TObject)
  private
    FTreeView: TTreeView;
    FFolderImageIndex: Integer;
    FClassImageIndex: Integer;
    FClassPrivateImageIndex: Integer;
    FClassProtectedImageIndex: Integer;
    FClassPublicImageIndex: Integer;
    FClassPublishedImageIndex: Integer;
    FMemberImageIndex: Integer;
    FClassPropertyImageIndex: Integer;
    FFunctionImageIndex: Integer;
    FUnitImageIndex: Integer;
		FFunctionArgvImageIndex: Integer;
    FSpecializeImageIndex: Integer;

    procedure BindUnit(ACodeTree: TCodeTree; AUnitNode: TTreeNode);
    procedure BindUses(ACodeTree: TCodeTree; AUsesNode: TTreeNode);
    procedure BindType(ACodeTree: TCodeTree; ATypeNode: TTreeNode);
    procedure BindClassScope(AClassScope: TCodeClassScope; AScopeNode: TTreeNode);
    procedure BindMember(AMember: TCodeMember; AMemberNode: TTreeNode);
    procedure BindClassProperty(AClassProperty: TCodeClassProperty; APropNode: TTreeNode);
    procedure BindFunction(AFunction: TCodeFunction; AFuncNode: TTreeNode);
  	procedure BindSpecialize(ASpecialize: TCodeSpecialize; ASpecializeNode: TTreeNode);
    function GetCaption(AElement: TCodeElement): String;
  public
    constructor Create;
    procedure Bind(ACodeTree: TCodeTree; ATreeView: TTreeView);
    procedure BindElement(ATreeView: TTreeView; ANode: TTreeNode; AElement: TCodeElement);

    property ClassPropertyImageIndex: Integer read FClassPropertyImageIndex write FClassPropertyImageIndex;
    property ClassPrivateImageIndex: Integer read FClassPrivateImageIndex write FClassPrivateImageIndex;
    property ClassProtectedImageIndex: Integer read FClassProtectedImageIndex write FClassProtectedImageIndex;
    property ClassPublicImageIndex: Integer read FClassPublicImageIndex write FClassPublicImageIndex;
    property ClassPublishedImageIndex: Integer read FClassPublishedImageIndex write FClassPublishedImageIndex;
  end;

implementation

{ TCodeBinder }

procedure TCodeBinder.BindUnit(ACodeTree: TCodeTree; AUnitNode: TTreeNode);
var
  i: Integer;
  item: TCodeElement;
begin
	for i := 0 to Pred(ACodeTree.Root.Children.Count) do
 	begin
		item := ACodeTree.Root.Children[i];
    if item is TCodeUnit then
    begin
      AUnitNode.Text := item.Code;
      AUnitNode.Data := item;
      break;
    end;
  end;
end;

procedure TCodeBinder.BindUses(ACodeTree: TCodeTree; AUsesNode: TTreeNode);
var
  i, j: Integer;
  item: TCodeElement;
  usesUnit: TCodeUsesUnit;
begin
	for i := 0 to Pred(ACodeTree.Root.Children.Count) do
 	begin
		item := ACodeTree.Root.Children[i];
    if item is TCodeUses then
    begin
			for j := 0 to Pred(item.Children.Count) do
      begin
      	usesUnit := item.Children[j] as TCodeUsesUnit;
        FTreeView.Items.AddChildObject(AUsesNode, usesUnit.Code, usesUnit);
      end;
    end;
  end;
end;

procedure TCodeBinder.BindType(ACodeTree: TCodeTree; ATypeNode: TTreeNode);
var
  i, j: Integer;
  item: TCodeElement;
  classElement: TCodeClass;
  classNode, scopeNode, funcNode, specializeNode: TTreeNode;
  element: TCodeElement;
begin
	for i := 0 to Pred(ACodeTree.Root.Children.Count) do
 	begin
		item := ACodeTree.Root.Children[i];
    if item is TCodeClass then
    begin
      classElement := item as TCodeClass;
    	classNode := FTreeView.Items.AddChildObject(ATypeNode, GetCaption(classElement), classElement);
      classNode.StateIndex := FClassImageIndex;

      for j := 0 to Pred(classElement.Children.Count) do
      begin
				element := classElement.Children[j];
        if element is TCodeClassScope then
        begin
        	scopeNode := FTreeView.Items.AddChildObject(classNode, element.Code, element);
          if EggStrEqualSame(element.Code, 'private') then
          begin
          	scopeNode.StateIndex := FClassPrivateImageIndex;
          end
          else if EggStrEqualSame(element.Code, 'protected') then
          begin
          	scopeNode.StateIndex := FClassProtectedImageIndex;
          end
          else if EggStrEqualSame(element.Code, 'public') then
          begin
          	scopeNode.StateIndex := FClassPublicImageIndex;
          end
          else if EggStrEqualSame(element.Code, 'published') then
          begin
          	scopeNode.StateIndex := FClassPublishedImageIndex;
          end
          ;
          BindClassScope(element as TCodeClassScope, scopeNode);
          scopeNode.Expand(False);
        end;
      end;
      classNode.Expand(False);
    end
    else if item is TCodeSpecialize then
    begin
      specializeNode := FTreeView.Items.AddChild(ATypeNode, '');
    	BindSpecialize(item as TCodeSpecialize, specializeNode);
    end
    else if item is TCodeFunction then
    begin
      funcNode := FTreeView.Items.AddChild(ATypeNode, '');
    	BindFunction(item as TCodeFunction, funcNode);
    end;
  end;
end;

procedure TCodeBinder.BindClassScope(AClassScope: TCodeClassScope; AScopeNode: TTreeNode);
var
  i: Integer;
  element: TCodeElement;
  funcNode, propNode, memNode: TTreeNode;
begin
	for i := 0 to Pred(AClassScope.Children.Count) do
  begin
		element := AClassScope.Children[i];
    if element is TCodeMember then
    begin
    	memNode := FTreeView.Items.AddChild(AScopeNode, '');
    	BindMember(element as TCodeMember, memNode);
    end
    else if element is TCodeClassProperty then
    begin
    	propNode := FTreeView.Items.AddChild(AScopeNode, '');
    	BindClassProperty(element as TCodeClassProperty, propNode);
    end
    else if element is TCodeFunction then
    begin
      funcNode := FTreeView.Items.AddChild(AScopeNode, '');
    	BindFunction(element as TCodeFunction, funcNode);
    end
    ;
  end;
end;

procedure TCodeBinder.BindMember(AMember: TCodeMember; AMemberNode: TTreeNode);
begin
	AMemberNode.Text := AMember.Name + ' : ' + AMember.MemberType;
  AMemberNode.Data := AMember;
  AMemberNode.StateIndex := FMemberImageIndex;
end;

procedure TCodeBinder.BindClassProperty(AClassProperty: TCodeClassProperty; APropNode: TTreeNode);
begin
  APropNode.Text := AClassProperty.Name + ' : ' + AClassProperty.PropertyType;
  APropNode.Data := AClassProperty;
  APropNode.StateIndex := FClassPropertyImageIndex;
end;

procedure TCodeBinder.BindFunction(AFunction: TCodeFunction; AFuncNode: TTreeNode);
var
  i, j: Integer;
  element: TCodeElement;
  arg: TCodeFunctionArgv;
  mem: TCodeMember;
  nm, args: String;
  argvNode, memNode: TTreeNode;
  oldExpanded: Boolean;
begin
  oldExpanded := AFuncNode.Expanded;
  AFuncNode.DeleteChildren();
  nm := AFunction.Name;

  for i := 0 to Pred(AFunction.Children.Count) do
  begin
		element := AFunction.Children[i];
    if element is TCodeFunctionArgv then
    begin
      argvNode := FTreeView.Items.AddChildObject(AFuncNode, 'Argument', element);
      argvNode.StateIndex := FFunctionArgvImageIndex;

      arg := element as TCodeFunctionArgv;
      args := '';
      for j := 0 to Pred(arg.Children.Count) do
      begin
        if j = 0 then
        begin
        	args += '(';
        end
        else
        begin
        	args += ', ';
        end;
        mem := arg.Children[j] as TCodeMember;
        args += mem.Name + ' : ' + mem.MemberType;

      	memNode := FTreeView.Items.AddChild(argvNode, '');
        BindMember(mem, memNode);

        if j = Pred(arg.Children.Count) then
        begin
        	args += ')';
        end;
      end;
    end;
  end;
  nm += args;
  if not EggStrEmpty(AFunction.ResultType) then
  begin
  	nm += ' : ' + AFunction.ResultType;
  end;

  AFuncNode.Text := nm;
  AFuncNode.StateIndex := FFunctionImageIndex;
  AFuncNode.Data := AFunction;
  if oldExpanded then
  begin
  	AFuncNode.Expand(True);
  end;
end;

procedure TCodeBinder.BindSpecialize(ASpecialize: TCodeSpecialize; ASpecializeNode: TTreeNode);
begin
  ASpecializeNode.Text := ASpecialize.Name + ' : ' + ASpecialize.BaseClass + '<' + ASpecialize.TypeClass + '>';
  ASpecializeNode.Data := ASpecialize;
  ASpecializeNode.StateIndex := FSpecializeImageIndex;
end;

function TCodeBinder.GetCaption(AElement: TCodeElement): String;
var
  cls: TCodeClass;
begin
  Result := '';
  if AElement is TCodeClass then
  begin
		cls := AElement as TCodeClass;
    Result := cls.Name + ' : ' + cls.BaseClass
  end;
end;

constructor TCodeBinder.Create;
begin
	FFolderImageIndex := DefaultFolderImageIndex;
  FClassImageIndex := DefaultClassImageIndex;
  FClassPrivateImageIndex := DefaultClassPrivateImageIndex;
  FClassProtectedImageIndex := DefaultClassProtectedImageIndex;
  FClassPublicImageIndex := DefaultClassPublicImageIndex;
  FClassPublishedImageIndex := DefaultClassPublishedImageIndex;
  FMemberImageIndex := DefaultMemberImageIndex;
  FClassPropertyImageIndex := DefaultClassPropertyImageIndex;
  FFunctionImageIndex := DefaultFunctionImageIndex;
  FUnitImageIndex := DefaultUnitImageIndex;
  FFunctionArgvImageIndex := DefaultFunctionArgvImageIndex;
  FSpecializeImageIndex := DefaultSpecializeImageIndex;
end;

procedure TCodeBinder.Bind(ACodeTree: TCodeTree; ATreeView: TTreeView);
var
  usesNode, typeNode, unitNode: TTreeNode;
begin
  FTreeView := ATreeView;
	ATreeView.Items.Clear;

  unitNode := ATreeView.Items.Add(nil, 'Unit');
  unitNode.StateIndex := FUnitImageIndex;
  BindUnit(ACodeTree, unitNode);

  usesNode := ATreeView.Items.Add(nil, 'Uses');
  usesNode.StateIndex := FFolderImageIndex;
  BindUses(ACodeTree, usesNode);

  typeNode := ATreeView.Items.Add(nil, 'Type');
  typeNode.StateIndex := FFolderImageIndex;

  BindType(ACodeTree, typeNode);
  typeNode.Expand(False);
end;

procedure TCodeBinder.BindElement(ATreeView: TTreeView; ANode: TTreeNode; AElement: TCodeElement);
begin
  FTreeView := ATreeView;
  if AElement is TCodeFunction then
  begin
  	BindFunction(AElement as TCodeFunction, ANode);
  end
  else if AElement is TCodeMember then
  begin
    BindMember(AElement as TCodeMember, ANode);
  end
  else if AElement is TCodeClassProperty then
  begin
    BindClassProperty(AElement as TCodeClassProperty, ANode);
  end
  else if AElement is TCodeSpecialize then
  begin
  	BindSpecialize(AElement as TCodeSpecialize, ANode);
  end
  else if AElement is TCodeClass then
  begin
    ANode.Text := GetCaption(AElement);
  end
  ;
end;

end.

