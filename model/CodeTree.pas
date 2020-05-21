unit CodeTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl,
  EggStrUtils;

type
  TCodeElementList = class;

  { TCodeElement }
  TCodeElement = class(TPersistent)
  private
    FParentList: TCodeElementList;
    FCode: String;
    FChildren: TCodeElementList;
  protected
    function GetCode: String; virtual;
  public
    constructor Create(AParentList: TCodeElementList);
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    procedure Clear();
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

  { TCodeClass }
  TCodeClass = class(TCodeElement)
  private
    FBaseClass: String;
    FComment: String;
    function GetName: String;
    procedure SetBaseClass(AValue: String);
  public
    property Name: String read GetName;
    property BaseClass: String read FBaseClass write SetBaseClass;
    property Comment: String read FComment write FComment;
  end;

  TCodeClassScope = class(TCodeElement)
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
    FMemberType: String;
    function GetName: String;
    procedure SetMemberType(AValue: String);
    procedure SetName(AValue: String);
  public
    function Equals(ASource: TObject): boolean; override;
    procedure Assign(ASource: TPersistent); override;
  	property Name: String read GetName write SetName;
    property MemberType: String read FMemberType write SetMemberType;
  end;

  { TCodeClassProperty }
  TCodeClassProperty = class(TCodeElement)
  private
    FGetter: String;
    FPropertyType: String;
    FSetter: String;
    function GetName: String;
    procedure SetPropertyType(AValue: String);
    procedure SetName(AValue: String);
  public
    constructor Create(AParentList: TCodeElementList);
    property Name: String read GetName write SetName;
    property PropertyType: String read FPropertyType write SetPropertyType;
    property Getter: String read FGetter write FGetter;
    property Setter: String read FSetter write FSetter;
  end;

  { TCodeFunctionArgv }
  TCodeFunctionArgv = class(TCodeElement)
  public
    function Equals(ASource: TObject): boolean; override;
  end;

  { TCodeFunction }
  TCodeFunction = class(TCodeElement)
  private
    FContent: String;
    FResultType: String;
    function GetFunctionArgv: TCodeFunctionArgv;
    function GetName: String;
    procedure SetArgument(AValue: TCodeFunctionArgv);
    procedure SetContent(AValue: String);
    procedure SetName(AValue: String);
    procedure SetResultType(AValue: String);
  public
  	property Name: String read GetName write SetName;
    property ResultType: String read FResultType write SetResultType;
    property Argument: TCodeFunctionArgv read GetFunctionArgv write SetArgument;
    property Content: String read FContent write SetContent;
  end;

  TCodeElements = specialize TFPGObjectList<TCodeElement>;

  { TCodeElementList }

  TCodeElementList = class(TCodeElements)
  public
		function AppendComment(): TCodeComment;
		function AppendClass(): TCodeClass;
		function AppendMember(): TCodeMember;
    function Append(AClass: TCodeElementClass): TCodeElement;
  end;

  { TCodeTree }
  TCodeTree = class(TObject)
  private
    FRoot: TCodeElement;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear();
    property Root: TCodeElement read FRoot;
  end;


implementation

{ TCodeUnit }

function TCodeUnit.GetName: String;
begin
	Result := inherited Code;
end;

procedure TCodeUnit.SetName(AValue: String);
begin
	inherited Code := AValue;
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

{ TCodeFunction }

procedure TCodeFunction.SetResultType(AValue: String);
begin
  if FResultType = AValue then Exit;
  FResultType := AValue;
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

procedure TCodeFunction.SetContent(AValue: String);
begin
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
end;

{ TCodeMember }

function TCodeMember.GetName: String;
begin
  Result := Code;
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

{ TCodeElementList }

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

function TCodeElement.GetCode: String;
begin
	Result := FCode;
end;

constructor TCodeElement.Create(AParentList: TCodeElementList);
begin
  FParentList := AParentList;
  FChildren := TCodeElementList.Create(True);
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

end.

