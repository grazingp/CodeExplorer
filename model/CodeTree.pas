unit CodeTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl,
  EggStrUtils;

type
  TCodeElementList = class;
  TCodeClassScope = class;
  TCodeClassFunction = class;
  TCodeClassMember = class;
  TCodeClassProperty = class;
  TCodeFunction = class;

  TCodeElementMove = (emInsert);

  { TCodeElement }
  TCodeElement = class(TPersistent)
  private
    FParentList: TCodeElementList;
    FCode: String;
    FChildren: TCodeElementList;
  	function TrimValue(const AValue: String): String;
  protected
    function GetCode: String; virtual;
  public
    constructor Create(AParentList: TCodeElementList);
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    procedure Clear();
    procedure MoveTo(ADest: TCodeElement; AMove: TCodeElementMove);
    property Code: String read GetCode write FCode;
    property Children: TCodeElementList read FChildren;
  end;

  TCodeElementClass = class of TCodeElement;

  { TCodeUnit }

  TCodeUnit = class(TCodeElement)
  private
    function GetName: String;
    procedure SetName(AValue: String);
  public
  	property Name: String read GetName write SetName;
  end;

  TCodeConst = class(TCodeElement)
  end;

  { TCodeComment }
  TCodeComment = class(TCodeElement)
  private
    FPrefix: String;
    FSuffix: String;
  public
  	property Prefix: String read FPrefix write FPrefix;
  	property Suffix: String read FSuffix write FSuffix;
  end;

  TCodeUses = class(TCodeElement)
  end;

  TCodeUsesUnit = class(TCodeElement)
  end;

  { TCodeSpecialize }

  TCodeSpecialize = class(TCodeElement)
  private
    FBaseClass: String;
    FTypeClass: String;
    function GetName: String;
    procedure SetName(AValue: String);
  public
  	property Name: String read GetName write SetName;
    property BaseClass: String read FBaseClass write FBaseClass;
    property TypeClass: String read FTypeClass write FTypeClass;
  end;

  { TCodeClass }
  TCodeClass = class(TCodeElement)
  private
    FBaseClass: String;
    FComment: String;
    function GetName: String;
    procedure SetBaseClass(AValue: String);
    procedure SetName(AValue: String);
  public
    function AddScope(AScope: String): TCodeClassScope;
    property Name: String read GetName write SetName;
    property BaseClass: String read FBaseClass write SetBaseClass;
    property Comment: String read FComment write FComment;
  end;

  { TCodeClassScope }

  TCodeClassScope = class(TCodeElement)
  public
  	function AddFunction(): TCodeClassFunction;
    function AddClassMember(): TCodeClassMember;
    function AddClassProperty(): TCodeClassProperty;
  end;

  TCodeClassPrivate = class(TCodeClassScope)
  end;

  TCodeClassProtected = class(TCodeClassScope)
  end;

  TCodeClassPublic = class(TCodeClassScope)
  end;

  TCodeClassPublished = class(TCodeClassScope)
  end;

  { TCodeMember }
  TCodeMember = class(TCodeElement)
  private
    FComment: String;
    FMemberType: String;
    function GetName: String;
    procedure SetComment(AValue: String);
    procedure SetMemberType(AValue: String);
    procedure SetName(AValue: String);
  public
    function Equals(ASource: TObject): boolean; override;
    procedure Assign(ASource: TPersistent); override;
  	property Name: String read GetName write SetName;
    property MemberType: String read FMemberType write SetMemberType;
    property Comment: String read FComment write SetComment;
  end;

  TCodeClassMember = class(TCodeMember)
  end;

  { TCodeClassProperty }
  TCodeClassProperty = class(TCodeElement)
  private
    FComment: String;
    FGetter: String;
    FPropertyType: String;
    FSetter: String;
    function GetName: String;
    procedure SetComment(AValue: String);
    procedure SetPropertyType(AValue: String);
    procedure SetName(AValue: String);
  public
    constructor Create(AParentList: TCodeElementList);
    property Name: String read GetName write SetName;
    property PropertyType: String read FPropertyType write SetPropertyType;
    property Getter: String read FGetter write FGetter;
    property Setter: String read FSetter write FSetter;
    property Comment: String read FComment write SetComment;
  end;

  { TCodeFunctionArgv }
  TCodeFunctionArgv = class(TCodeElement)
  private
    function GetFunctionElement: TCodeFunction;
  public
    function Equals(ASource: TObject): boolean; override;
    function AppendMember(): TCodeMember;
    property FunctionElement: TCodeFunction read GetFunctionElement;
  end;

  { TCodeFunction }
  TCodeFunction = class(TCodeElement)
  private
    FComment: String;
    FContent: String;
    FResultType: String;
    function GetFunctionArgv: TCodeFunctionArgv;
    function GetName: String;
    procedure SetArgument(AValue: TCodeFunctionArgv);
    procedure SetComment(AValue: String);
    procedure SetContent(AValue: String);
    procedure SetName(AValue: String);
    procedure SetResultType(AValue: String);
  public
    function AppendArgument(): TCodeFunctionArgv;
    property Name: String read GetName write SetName;
    property ResultType: String read FResultType write SetResultType;
    property Argument: TCodeFunctionArgv read GetFunctionArgv write SetArgument;
    property Comment: String read FComment write SetComment;
    property Content: String read FContent write SetContent;
  end;

  TCodeClassFunction = class(TCodeFunction)
  end;

  TCodeElements = specialize TFPGObjectList<TCodeElement>;

  { TCodeElementList }

  TCodeElementList = class(TCodeElements)
  private
    FOwner: TCodeElement;
    procedure SetOwner(AValue: TCodeElement);
  public
		function AppendComment(): TCodeComment;
		function AppendClass(): TCodeClass;
		function AppendMember(): TCodeMember;
    function Append(AClass: TCodeElementClass): TCodeElement;
    property Owner: TCodeElement read FOwner write SetOwner;
  end;

  { TCodeTree }
  TCodeTree = class(TObject)
  private
    FRoot: TCodeElement;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear();
    procedure RemoveElement(AElement: TCodeElement);
    property Root: TCodeElement read FRoot;
  end;


implementation

{ TCodeSpecialize }

function TCodeSpecialize.GetName: String;
begin
  Result := inherited Code;
end;

procedure TCodeSpecialize.SetName(AValue: String);
begin
	inherited Code := AValue;
end;

{ TCodeClassScope }

function TCodeClassScope.AddFunction(): TCodeClassFunction;
begin
  Result := Children.Append(TCodeClassFunction) as TCodeClassFunction;
end;

function TCodeClassScope.AddClassMember(): TCodeClassMember;
begin
  Result := Children.Append(TCodeClassMember) as TCodeClassMember;
end;

function TCodeClassScope.AddClassProperty(): TCodeClassProperty;
begin
  Result := Children.Append(TCodeClassProperty) as TCodeClassProperty;
end;

{ TCodeUnit }

function TCodeUnit.GetName: String;
begin
	Result := inherited Code;
end;

procedure TCodeUnit.SetName(AValue: String);
begin
	inherited Code := AValue;
end;

function TCodeFunctionArgv.GetFunctionElement: TCodeFunction;
begin
  Result := FParentList.Owner as TCodeFunction;
end;

{ TCodeFunctionArgv }
function TCodeFunctionArgv.Equals(ASource: TObject): boolean;
var
  i: Integer;
  mem: TCodeMember;
  src: TCodeFunctionArgv;
begin
  if not Assigned(ASource) then
  begin
  	if Children.Count = 0 then
    begin
      Result := True;
    end
    else
    begin
      Result := False;
    end;
    exit;
  end;

  if ASource is TCodeFunctionArgv then
  begin
    src := ASource as TCodeFunctionArgv;
    if Children.Count <> src.Children.Count then
    begin
      Result := False;
      exit;
    end;
    Result := True;
    for i := 0 to Pred(Children.Count) do
    begin
			if Children[i] is TCodeMember then
      begin
				mem := Children[i] as TCodeMember;
        if not mem.Equals(src.Children[i]) then
        begin
          Result := False;
          exit;
        end;
      end;
    end;
    exit;
  end;
  Result := inherited Equals(ASource);
end;

function TCodeFunctionArgv.AppendMember(): TCodeMember;
begin
  Result := Children.Append(TCodeMember) as TCodeMember;
end;

{ TCodeFunction }

procedure TCodeFunction.SetResultType(AValue: String);
begin
  AValue := TrimValue(AValue);
  if FResultType = AValue then Exit;
  FResultType := AValue;
end;

function TCodeFunction.AppendArgument(): TCodeFunctionArgv;
begin
  Result := GetFunctionArgv();
  if Assigned(Result) then exit;

  Result := Children.Append(TCodeFunctionArgv) as TCodeFunctionArgv;
end;

function TCodeFunction.GetName: String;
begin
  Result := Code;
end;

procedure TCodeFunction.SetArgument(AValue: TCodeFunctionArgv);
var
  arg: TCodeFunctionArgv;
begin
  arg := GetFunctionArgv();

  if not Assigned(AValue) then
  begin
    if Assigned(arg) then
    begin
      Children.Remove(arg);
    end;
    exit;
  end;
  if not Assigned(arg) then
  begin
    arg := Children.Append(TCodeFunctionArgv) as TCodeFunctionArgv;
  end;
  arg.Assign(AValue);
end;

procedure TCodeFunction.SetComment(AValue: String);
begin
  AValue := TrimValue(AValue);
  if FComment = AValue then Exit;
  FComment := AValue;
end;

procedure TCodeFunction.SetContent(AValue: String);
begin
  AValue := TrimValue(AValue);
  if FContent = AValue then Exit;
  FContent := AValue;
end;

function TCodeFunction.GetFunctionArgv: TCodeFunctionArgv;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Pred(Children.Count) do
  begin
  	if Children[i] is TCodeFunctionArgv then
    begin
      Result := Children[i] as TCodeFunctionArgv;
      exit;
    end;
  end;
end;

procedure TCodeFunction.SetName(AValue: String);
begin
	Code := AValue;
end;

{ TCodeClassProperty }

function TCodeClassProperty.GetName: String;
begin
  Result := Code;
end;

procedure TCodeClassProperty.SetComment(AValue: String);
begin
  if FComment = AValue then Exit;
  FComment := AValue;
end;

procedure TCodeClassProperty.SetPropertyType(AValue: String);
begin
  if FPropertyType = AValue then Exit;
  FPropertyType := AValue;
end;

procedure TCodeClassProperty.SetName(AValue: String);
begin
	Code := AValue;
end;

constructor TCodeClassProperty.Create(AParentList: TCodeElementList);
begin
  inherited Create(AParentList);

  FGetter := '';
  FPropertyType := '';
  FSetter := '';
  FComment := '';
end;

{ TCodeMember }

function TCodeMember.GetName: String;
begin
  Result := Code;
end;

procedure TCodeMember.SetComment(AValue: String);
begin
  if FComment = AValue then Exit;
  FComment := AValue;
end;

procedure TCodeMember.SetMemberType(AValue: String);
begin
  if FMemberType = AValue then Exit;
  FMemberType := AValue;
end;

procedure TCodeMember.SetName(AValue: String);
begin
	Code := AValue;
end;

function TCodeMember.Equals(ASource: TObject): boolean;
var
  src: TCodeMember;
begin
  if ASource is TCodeMember then
  begin
    src := ASource as TCodeMember;
    Result := False;
    if not EggStrEqualHeadSame(Name, src.Name) then exit;
    if not EggStrEqualHeadSame(FMemberType, src.FMemberType) then exit;
    Result := True;
    exit;
  end;
  Result := inherited Equals(ASource);
end;

procedure TCodeMember.Assign(ASource: TPersistent);
var
  src: TCodeMember;
begin
  if ASource is TCodeMember then
  begin
    src := ASource as TCodeMember;
    Name := src.Name;
    FMemberType := src.FMemberType;
    exit;
  end;
  inherited Assign(ASource);
end;

{ TCodeClass }

function TCodeClass.GetName: String;
begin
  Result := Code;
end;

procedure TCodeClass.SetBaseClass(AValue: String);
begin
  if FBaseClass = AValue then Exit;
  FBaseClass := AValue;
end;

procedure TCodeClass.SetName(AValue: String);
begin
  inherited Code := AValue;
end;

function TCodeClass.AddScope(AScope: String): TCodeClassScope;
var
  scope: TCodeElement;
begin
  Result := nil;
  scope := nil;
  if EggStrEqualSame(AScope, 'private') then
  begin
    scope := Children.Append(TCodeClassPrivate);
  end
  else if EggStrEqualSame(AScope, 'protected') then
  begin
    scope := Children.Append(TCodeClassProtected);
  end
  else if EggStrEqualSame(AScope, 'public') then
  begin
    scope := Children.Append(TCodeClassPublic);
  end
  else if EggStrEqualSame(AScope, 'published') then
  begin
    scope := Children.Append(TCodeClassPublished);
  end
  ;
  if Assigned(scope) then
  begin
    scope.Code := AScope;
    Result := scope as TCodeClassScope;
  end;
end;

{ TCodeElementList }

procedure TCodeElementList.SetOwner(AValue: TCodeElement);
begin
  if FOwner = AValue then Exit;
  FOwner := AValue;
end;

function TCodeElementList.AppendComment(): TCodeComment;
begin
  Result := TCodeComment.Create(Self);
  Self.Add(Result);
end;

function TCodeElementList.AppendClass(): TCodeClass;
begin
  Result := TCodeClass.Create(Self);
  Self.Add(Result);
end;

function TCodeElementList.AppendMember(): TCodeMember;
begin
  Result := TCodeMember.Create(Self);
  Self.Add(Result);
end;

function TCodeElementList.Append(AClass: TCodeElementClass): TCodeElement;
begin
	Result := AClass.Create(Self);
  Self.Add(Result);
end;

{ TCodeElement }

function TCodeElement.TrimValue(const AValue: String): String;
begin
  Result := EggReplaceLine(AValue);
  Result := Trim(Result);
end;

function TCodeElement.GetCode: String;
begin
	Result := FCode;
end;

constructor TCodeElement.Create(AParentList: TCodeElementList);
begin
  FParentList := AParentList;
  FChildren := TCodeElementList.Create(True);
  FChildren.Owner := Self;
end;

destructor TCodeElement.Destroy;
begin
  FreeAndNil(FChildren);
  inherited Destroy;
end;

procedure TCodeElement.Assign(ASource: TPersistent);
var
  i: Integer;
  cname: String;
  src, item, srcItem: TCodeElement;
begin
  if ASource is TCodeElement then
  begin
    src := ASource as TCodeElement;
    FCode := src.FCode;
    Children.Clear();
    for i := 0 to Pred(src.Children.Count) do
    begin
      srcItem := src.Children[i];
      cname := srcItem.ClassName;
    	item := Children.Append(TCodeElementClass(GetClass(cname)));
      item.Assign(srcItem);
    end;
  	exit;
  end;
  inherited Assign(ASource);
end;

procedure TCodeElement.Clear();
begin
  FCode := '';
  FChildren.Clear();
end;

procedure TCodeElement.MoveTo(ADest: TCodeElement; AMove: TCodeElementMove);
var
  bk: Boolean;
  idx: Integer;
begin
	if AMove = emInsert then
  begin
    bk := FParentList.FreeObjects;
    FParentList.FreeObjects := False;
		FParentList.Remove(Self);
    FParentList.FreeObjects := bk;
    FParentList := ADest.FParentList;
    idx := FParentList.IndexOf(ADest);
    FParentList.Insert(idx, Self);
  end;
end;

{ TCodeTree }

constructor TCodeTree.Create;
begin
  FRoot := TCodeElement.Create(nil);

end;

destructor TCodeTree.Destroy;
begin
  FreeAndNil(FRoot);

  inherited Destroy;
end;

procedure TCodeTree.Clear();
begin
	FRoot.Clear();
end;

procedure TCodeTree.RemoveElement(AElement: TCodeElement);
var
  list: TCodeElementList;
begin
	list := AElement.FParentList;
  list.Remove(AElement);
end;

end.

