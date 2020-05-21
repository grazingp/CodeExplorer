unit CodeBinder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls,
  EggStrUtils,
  CodeTree;

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
    FClassMemberImageIndex: Integer;
    FClassPropertyImageIndex: Integer;
    FClassMethodImageIndex: Integer;
    FUnitImageIndex: Integer;

    procedure BindUnit(ACodeTree: TCodeTree; AUnitNode: TTreeNode);
    procedure BindUses(ACodeTree: TCodeTree; AUsesNode: TTreeNode);
    procedure BindType(ACodeTree: TCodeTree; ATypeNode: TTreeNode);
    procedure BindClassScope(AClassScope: TCodeClassScope; AScopeNode: TTreeNode);
    procedure BindClassMember(AMember: TCodeMember; AScopeNode: TTreeNode);
    procedure BindClassProperty(AClassProperty: TCodeClassProperty; AScopeNode: TTreeNode);
    procedure BindFunction(AFunction: TCodeFunction; AScopeNode: TTreeNode);
  public
    constructor Create;
    procedure Bind(ACodeTree: TCodeTree; ATreeView: TTreeView);
    property ClassPropertyImageIndex: Integer read FClassPropertyImageIndex write FClassPropertyImageIndex;
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
  classNode, scopeNode: TTreeNode;
  element: TCodeElement;
begin
	for i := 0 to Pred(ACodeTree.Root.Children.Count) do
 	begin
		item := ACodeTree.Root.Children[i];
    if item is TCodeClass then
    begin
      classElement := item as TCodeClass;
    	classNode := FTreeView.Items.AddChildObject(ATypeNode, classElement.Name + ' : ' + classElement.BaseClass, classElement);
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
        end;
      end;
    end
    else if item is TCodeFunction then
    begin
    	BindFunction(item as TCodeFunction, ATypeNode);
    end;
  end;
end;

procedure TCodeBinder.BindClassScope(AClassScope: TCodeClassScope; AScopeNode: TTreeNode);
var
  i: Integer;
  element: TCodeElement;
begin
	for i := 0 to Pred(AClassScope.Children.Count) do
  begin
		element := AClassScope.Children[i];
    if element is TCodeMember then
    begin
    	BindClassMember(element as TCodeMember, AScopeNode);
    end
    else if element is TCodeClassProperty then
    begin
    	BindClassProperty(element as TCodeClassProperty, AScopeNode);
    end
    else if element is TCodeFunction then
    begin
    	BindFunction(element as TCodeFunction, AScopeNode);
    end
    ;
  end;
end;

procedure TCodeBinder.BindClassMember(AMember: TCodeMember; AScopeNode: TTreeNode);
var
  node: TTreeNode;
begin
	node := FTreeView.Items.AddChildObject(AScopeNode, AMember.Name + ' : ' + AMember.MemberType, AMember);
  node.StateIndex := FClassMemberImageIndex;
end;

procedure TCodeBinder.BindClassProperty(AClassProperty: TCodeClassProperty; AScopeNode: TTreeNode);
var
  node: TTreeNode;
begin
	node := FTreeView.Items.AddChildObject(AScopeNode, AClassProperty.Name + ' : ' + AClassProperty.PropertyType, AClassProperty);
  node.StateIndex := FClassPropertyImageIndex;
end;

procedure TCodeBinder.BindFunction(AFunction: TCodeFunction; AScopeNode: TTreeNode);
var
  i, j: Integer;
  element: TCodeElement;
  arg: TCodeFunctionArgv;
  mem: TCodeMember;
  nm, args: String;
  node: TTreeNode;
begin
  nm := AFunction.Name;

  for i := 0 to Pred(AFunction.Children.Count) do
  begin
		element := AFunction.Children[i];
    if element is TCodeFunctionArgv then
    begin
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

  node := FTreeView.Items.AddChildObject(AScopeNode, nm, AFunction);
  node.StateIndex := FClassMethodImageIndex;
end;

constructor TCodeBinder.Create;
begin
	FFolderImageIndex := 0;
  FClassImageIndex := 1;
  FClassPrivateImageIndex := 2;
  FClassProtectedImageIndex := 3;
  FClassPublicImageIndex := 4;
  FClassPublishedImageIndex := 5;
  FClassMemberImageIndex := 6;
  FClassPropertyImageIndex := 7;
  FClassMethodImageIndex := 8;
  FUnitImageIndex := 9;
end;

procedure TCodeBinder.Bind(ACodeTree: TCodeTree; ATreeView: TTreeView);
var
  usesNode, typeNode, unitNode: TTreeNode;
begin
  FTreeView := ATreeView;
	ATreeView.Items.Clear;

  unitNode := ATreeView.Items.Add(nil, 'Uses');
  unitNode.StateIndex := FUnitImageIndex;
  BindUnit(ACodeTree, unitNode);

  usesNode := ATreeView.Items.Add(nil, 'Uses');
  usesNode.StateIndex := FFolderImageIndex;
  BindUses(ACodeTree, usesNode);

  typeNode := ATreeView.Items.Add(nil, 'Type');
  typeNode.StateIndex := FFolderImageIndex;

  BindType(ACodeTree, typeNode);
  typeNode.Expand(True);
end;

end.

