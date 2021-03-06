unit CodeParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  EggStrUtils,
  CodeTree;

type
  TCodeParserSection = (csUnit, csInterface, csUses, csImplementation);

  { TCodeParser }
  TCodeParser = class(TObject)
  private
    FCodeTree: TCodeTree;
    FCodeSection: TCodeParserSection;
    FIndex: Integer;
    FTokens: TDynStringArray;
    FKeywords: TDynStringArray;
    FSpaces: TDynStringArray;

    procedure ParseElement(AElement: TCodeElement);
		procedure ParseConst(AConst: TCodeConst);
    procedure ParseUses(ACodeUses: TCodeUses);
    procedure ParseComment(AElement: TCodeComment; AStart, AEnd: String);
		procedure ParseSpecialize(ASpecialize: TCodeSpecialize);
    procedure ParseEnum(AEnum: TCodeEnum);
    procedure ParseSet(ASet: TCodeSet);
  	procedure ParseClass(AElement: TCodeClass);
    procedure ParseClassProperty(AProp: TCodeClassProperty);
    procedure ParseFunction(AFunc: TCodeFunction);
    procedure ParseFunctionResultType(var AResultType: String);
    procedure ParseFunctionArgv(AFuncArgv: TCodeFunctionArgv);
    procedure ParseFunctionImpl();
		function NextToken(const AKeyword: String; ANextIndex: Integer): Integer;
		function GetNextToken(const AKeyword: String; AIndex: Integer): Integer;
    function GetNextWord(AIndex: Integer; var AWord: String): Integer;
    function NextWord(var AWord: String; ANextIndex: Integer): Integer;
    function GetPriorWord(AIndex: Integer; var AWord: String): Integer;
    function GetNextNotSpace(AIndex: Integer; var AToken: String): Integer;
    function GetNextValue(AIndex: Integer; var AValue: String): Integer;
    function FindFunctionName(AParent: TCodeElement; AFuncName: String; AArgv: TCodeFunctionArgv): TCodeFunction;
    function FindClassFunctionName(const AClassName, AFuncName: String; AArgv: TCodeFunctionArgv): TCodeFunction;
    procedure InsertionComment(AElement: TCodeElement);
  public
    constructor Create;
    procedure Parse(const ACode: String; ACodeTree: TCodeTree);
  end;

implementation

{ TCodeParser }

procedure TCodeParser.ParseElement(AElement: TCodeElement);
var
  c, nm, typ, next: String;
  func: TCodeFunction;
  comm: TCodeComment;
begin
  while FIndex < Length(FTokens) do
  begin
    c := FTokens[FIndex];
    // comment
    if EggStrEqualHead(c, '//') then
    begin
      comm := TCodeComment.Create(AElement.Children);
      ParseComment(comm, '//', #10);
      if FCodeSection = csImplementation then
      begin
        // implementation 内のコメントは破棄します。
        FreeAndNil(comm);
      end
      else
      begin
        AElement.Children.Add(comm);
      end;
      continue;
    end
    else if EggStrEqualHead(c, '{') then
    begin
      comm := TCodeComment.Create(AElement.Children);
      ParseComment(comm, '{', '}');
      if FCodeSection = csImplementation then
      begin
        // implementation 内のコメントは破棄します。
        FreeAndNil(comm);
      end
      else
      begin
        AElement.Children.Add(comm);
      end;
      continue;
    end
    else if EggStrEqualSame(c, 'unit') then
    begin
      Inc(FIndex);
      NextWord(nm, 0);
      AElement.Children.Append(TCodeUnit).Code := nm;
      NextToken(';', 1);
			continue;
    end
    else if EggStrEqualSame(c, 'const') then
    begin
      Inc(FIndex);
			ParseConst(AElement.Children.Append(TCodeConst) as TCodeConst);
			continue;
    end
    else if EggStrEqualSame(c, 'type') then
    begin
    end
    else if EggStrEqualSame(c, 'interface') then
    begin
      FCodeSection := csInterface;
    end
    else if EggStrEqualSame(c, 'implementation') then
    begin
      FCodeSection := csImplementation;
    end
    else if EggStrEqualSame(c, 'uses') then
    begin
      FCodeSection := csUses;
      ParseUses(AElement.Children.Append(TCodeUses) as TCodeUses);
			continue;
    end
    else if EggStrEqual(c, '(') then
    begin
			ParseEnum(AElement.Children.Append(TCodeEnum) as TCodeEnum);
    end
    else if EggStrEqualSame(c, 'set') then
    begin
			ParseSet(AElement.Children.Append(TCodeSet) as TCodeSet);
    end
    else if EggStrEqualSame(c, 'specialize') then
    begin
			ParseSpecialize(AElement.Children.Append(TCodeSpecialize) as TCodeSpecialize);
      continue;
    end
    else if EggStrEqualSame(c, 'class') then
    begin
      GetNextNotSpace(FIndex + 1, next);
      if EggStrEqualSame(next, ';') then
      begin
        NextToken(';', 1);
  			continue;
      end;
      ParseClass(AElement.Children.AppendClass());
			continue;
    end
    else if EggStrEqualSame(c, 'procedure') or EggStrEqualSame(c, 'function') then
    begin
      if FCodeSection = csImplementation then
      begin
        Inc(FIndex);
        ParseFunctionImpl();
      end
      else
      begin
        Inc(FIndex);
  			NextWord(nm, 1);
        func := AElement.Children.Append(TCodeFunction) as TCodeFunction;
        func.Name := nm;

        if EggStrEqualSame(c, 'function') then
        begin
          ParseFunctionResultType(typ);
          func.ResultType := typ;
        end;
        ParseFunction(func);
        continue;
      end;
    end
    ;
    Inc(FIndex);
  end;
end;

procedure TCodeParser.ParseConst(AConst: TCodeConst);
var
  nm, typ, eq, colon, val: String;
  p: Integer;
  mem: TCodeConstMember;
begin
	while FIndex < Length(FTokens) do
 	begin
		p := GetNextNotSpace(FIndex, nm);
    if EggStrEqualSame(nm, 'type') then
    begin
			break;
  	end;
		p := GetNextNotSpace(p + 1, typ);
    if EggStrEqual(typ, '=') then
    begin
      typ := '';
    end
    else
    begin
  		p := GetNextNotSpace(p + 1, typ);
      p := GetNextNotSpace(p + 1, eq);
			if not EggStrEqual(eq, '=') then
      begin
        break;
      end;
    end;
    p := GetNextValue(p + 1, val);
    FIndex := GetNextNotSpace(p + 1, colon);

    mem := AConst.Children.Append(TCodeConstMember) as TCodeConstMember;
    mem.Name := nm;
    mem.MemberType := typ;
    mem.DefaultValue := val;

    Inc(FIndex);
  end;
end;

procedure TCodeParser.ParseUses(ACodeUses: TCodeUses);
var
  usesUnit: TCodeUsesUnit;
  c: String;
begin
  Inc(FIndex);
  while FIndex < Length(FTokens) do
  begin
    c := FTokens[FIndex];
    if EggStrEqual(c, ';') then
    begin
      Inc(FIndex);
      break;
    end
    else if EggArrayIndex(FKeywords, c) >= 0 then
    begin
    end
    else
    begin
    	usesUnit := ACodeUses.Children.Append(TCodeUsesUnit) as TCodeUsesUnit;
      usesUnit.Code := c;
    end;
    Inc(FIndex);
	end;
end;

procedure TCodeParser.ParseComment(AElement: TCodeComment; AStart, AEnd: String);
var
  code, c: String;
begin
  AElement.Prefix := AStart;
  AElement.Suffix := AEnd;
  code := '';
  Inc(FIndex);
  while FIndex < Length(FTokens) do
  begin
    c := FTokens[FIndex];
    // comment END
    if c = AEnd then
    begin
      Inc(FIndex);
			break;
    end;
    code += c;
    Inc(FIndex);
  end;
  AElement.Code := Trim(code);
end;

procedure TCodeParser.ParseSpecialize(ASpecialize: TCodeSpecialize);
var
  b, t, nm: String;
begin
  GetPriorWord(FIndex - 1, nm);
	ASpecialize.Name := nm;
  Inc(FIndex);
  NextWord(b, 1);
  ASpecialize.BaseClass := b;
  NextWord(t, 1);
  ASpecialize.TypeClass := t;
	NextToken(';', 1);
end;

procedure TCodeParser.ParseEnum(AEnum: TCodeEnum);
var
  i: Integer;
  c, nm: String;
  attr: TCodeEnumAttr;
begin
  GetPriorWord(FIndex - 1, nm);
  AEnum.Name := nm;

  i := FIndex + 1;
  while i < Length(FTokens) do
  begin
    i := GetNextNotSpace(i, c);
    if EggStrEqual(c, ')') then
    begin
      FIndex := i + 1;
      break;
    end
    else if EggStrEqual(c, ',') then
    begin
      i := GetNextNotSpace(i + 1, c);
		end;
    attr := AEnum.Children.Append(TCodeEnumAttr) as TCodeEnumAttr;
    attr.Name := c;
    Inc(i);
  end;
end;

procedure TCodeParser.ParseSet(ASet: TCodeSet);
var
  nm, enum: String;
begin
	GetPriorWord(FIndex - 1, nm);
  FIndex := GetNextToken('of', FIndex);
	GetNextWord(FIndex + 1, enum);
  ASet.Name := nm;
  ASet.Enum := enum;
end;

procedure TCodeParser.ParseClass(AElement: TCodeClass);
	procedure DefaultScope(var scope: TCodeElement);
  begin
    if not Assigned(scope) then
    begin
    	scope := AElement.Children.Append(TCodeClassPrivate);
      scope.Code := 'private';
    end;
  end;
var
  c, typ, nm: String;
  scope: TCodeElement;
  member: TCodeClassMember;
  prop: TCodeClassProperty;
  func: TCodeClassFunction;
begin
  GetPriorWord(FIndex - 1, c);
	AElement.Code := c;
  GetNextWord(FIndex + 1, nm);
  AElement.BaseClass := nm;
  scope := nil;
  Inc(FIndex);
  while FIndex < Length(FTokens) do
  begin
    c := FTokens[FIndex];
    // comment
    if EggStrEqualHead(c, '//') then
    begin
      ParseComment(AElement.Children.AppendComment(), '//', #10);
      continue;
    end
    else if EggStrEqualHead(c, '{') then
    begin
      ParseComment(AElement.Children.AppendComment(), '{', '}');
      continue;
    end
    else if EggStrEqualSame(c, 'private') then
    begin
			scope := AElement.Children.Append(TCodeClassPrivate);
      scope.Code := LowerCase(c);
    end
    else if EggStrEqualSame(c, 'protected') then
    begin
			scope := AElement.Children.Append(TCodeClassProtected);
      scope.Code := LowerCase(c);
    end
    else if EggStrEqualSame(c, 'public') then
    begin
			scope := AElement.Children.Append(TCodeClassPublic);
      scope.Code := LowerCase(c);
    end
    else if EggStrEqualSame(c, 'published') then
    begin
			scope := AElement.Children.Append(TCodeClassPublished);
      scope.Code := LowerCase(c);
    end
    else if EggStrEqualSame(c, 'end') then
    begin
			NextToken(';', 1);
      break;
    end
    else if EggStrEqualSame(c, 'procedure') or EggStrEqualSame(c, 'function') then
    begin
      DefaultScope(scope);
      Inc(FIndex);
			NextWord(nm, 1);
      func := scope.Children.Append(TCodeClassFunction) as TCodeClassFunction;
      func.Name := nm;

      if EggStrEqualSame(c, 'function') then
      begin
        ParseFunctionResultType(typ);
      	func.ResultType := typ;
      end;
      ParseFunction(func);
      continue;
    end
    else if EggStrEqualSame(c, 'property') then
    begin
      DefaultScope(scope);
      prop := scope.Children.Append(TCodeClassProperty) as TCodeClassProperty;
      ParseClassProperty(prop);
      continue;
    end
    else if EggStrEqual(c, ':') then
    begin
      // Class Member
      DefaultScope(scope);
			GetNextWord(FIndex + 1, typ);
			GetPriorWord(FIndex - 1, nm);
      member := scope.Children.Append(TCodeClassMember) as TCodeClassMember;
      member.Name := nm;
      member.MemberType := typ;
      NextToken(';', 1);
      continue;
    end
    ;
    Inc(FIndex);
  end;
end;

procedure TCodeParser.ParseClassProperty(AProp: TCodeClassProperty);
var
  typ, nm: String;
  idx: Integer;
begin
  NextToken(':', 0);
	GetNextWord(FIndex + 1, typ);
	GetPriorWord(FIndex - 1, nm);
  AProp.Name := nm;
  AProp.PropertyType := typ;

  idx := GetNextToken('read', FIndex + 1);
  if idx >= 0 then
  begin
    GetNextWord(idx + 1, nm);
		AProp.Getter := nm
  end;

  idx := GetNextToken('write', FIndex + 1);
  if idx >= 0 then
  begin
    GetNextWord(idx + 1, nm);
		AProp.Setter := nm;
  end;

  NextToken(';', 1);
end;

procedure TCodeParser.ParseFunction(AFunc: TCodeFunction);
var
  c: String;
  arg: TCodeFunctionArgv;
begin
  while FIndex < Length(FTokens) do
  begin
    c := FTokens[FIndex];
		if EggStrEqual(c, '(') then
    begin
      Inc(FIndex);
      arg := AFunc.Children.Append(TCodeFunctionArgv) as TCodeFunctionArgv;
      ParseFunctionArgv(arg);
    	continue;
    end
    else if EggStrEqual(c, ';') then
    begin
      Inc(FIndex);
      break;
    end
    ;
    Inc(FIndex);
  end;
end;

procedure TCodeParser.ParseFunctionResultType(var AResultType: String);
var
  p: Integer;
begin
	AResultType := '';
  p := GetNextToken(':', FIndex);
  if p >= 0 then
  begin
    GetNextWord(p, AResultType);
  end;
end;

procedure TCodeParser.ParseFunctionArgv(AFuncArgv: TCodeFunctionArgv);
var
  c, typ, nm, paramType, eq, def: String;
  param: TCodeFunctionParameter;
  p: Integer;
begin
  while FIndex < Length(FTokens) do
  begin
    c := FTokens[FIndex];
    if EggStrEqual(c, ')') then
    begin
			Inc(FIndex);
    	break;
    end
    else if EggStrEqual(c, ':') then
    begin
			p := GetPriorWord(FIndex, nm);
      GetPriorWord(p - 1, paramType);
      FIndex := GetNextWord(FIndex, typ);
      def := '';
      p := GetNextNotSpace(FIndex + 1, eq);
      if (p >= 0) and EggStrEqual(eq, '=') then
      begin
				GetNextWord(p + 1, def);
      end;
      param := AFuncArgv.AppendParameter();
      param.Name := nm;
      param.MemberType := typ;
      if EggStrEqualSame(paramType, 'var')
      	or EggStrEqualSame(paramType, 'out')
        or EggStrEqualSame(paramType, 'const')
      then begin
				param.ParameterType := paramType;
      end;
      param.DefaultValue := def;
    end;
    Inc(FIndex);
  end;
end;

procedure TCodeParser.ParseFunctionImpl();
var
  c, cname, dot, funcName, content: String;
  func: TCodeFunction;
  arg: TCodeFunctionArgv;
  beginEnd: Integer;
begin
	// Class.FunctionName
  // FunctionName()
  NextWord(cname, 1);
  GetNextNotSpace(FIndex, dot);
  if EggStrEqual(dot, '.') then
  begin
    Inc(FIndex);
    NextWord(funcName, 1);
  end
  else
  begin
    funcName := cname;
    cname := '';
  end;
  if not Assigned(func) then exit;

  arg := nil;
  try
    while FIndex < Length(FTokens) do
    begin
      c := FTokens[FIndex];
  		if EggStrEqual(c, '(') then
      begin
        Inc(FIndex);
        arg := TCodeFunctionArgv.Create(nil);
        ParseFunctionArgv(arg);
      	continue;
      end
      else if EggStrEqual(c, ';') then
      begin
        Inc(FIndex);
        break;
      end
      ;
      Inc(FIndex);
    end;

    if not EggStrEmpty(cname) then
    begin
			func := FindClassFunctionName(cname, funcName, arg);
    end
    else
    begin
			func := FindFunctionName(FCodeTree.Root, funcName, arg);
    end;

    if not Assigned(func) then
    begin
			func := FCodeTree.Root.Children.Append(TCodeFunction) as TCodeFunction;
      func.Argument := arg;
    end;

    content := '';
    beginEnd := 0;
    while FIndex < Length(FTokens) do
    begin
      c := FTokens[FIndex];
      if EggStrEqualHeadSame(c, 'begin') then
      begin
      	Inc(beginEnd);
      end
      else if EggStrEqualHeadSame(c, 'end') then
      begin
      	Dec(beginEnd);
        if beginEnd <= 0 then
        begin
          content += c;
          NextToken(';', 1);
					break;
        end;
      end
      ;
      content += c;
      Inc(FIndex);
    end;
    func.Content := content;
  finally
    FreeAndNil(arg);
  end;
end;

function TCodeParser.NextToken(
  const AKeyword: String; ANextIndex: Integer): Integer;
var
  c: String;
begin
  Result := -1;
  Inc(FIndex);
  while FIndex < Length(FTokens) do
  begin
    c := FTokens[FIndex];
    if EggStrEqualSame(c, AKeyword) then
    begin
      Result := FIndex;
      FIndex += ANextIndex;
      break;
    end;
    Inc(FIndex);
  end;
end;

function TCodeParser.GetNextToken(
  const AKeyword: String; AIndex: Integer): Integer;
var
  c: String;
begin
  Result := -1;
  while AIndex < Length(FTokens) do
  begin
    c := FTokens[AIndex];
    if EggStrEqualSame(c, AKeyword) then
    begin
      Result := AIndex;
      break;
    end;
    Inc(AIndex);
  end;
end;

function TCodeParser.GetNextWord(AIndex: Integer; var AWord: String): Integer;
var
  i: Integer;
  c: String;
begin
  AWord := '';
  Result := -1;
  for i := AIndex to Pred(Length(FTokens)) do
  begin
		c := FTokens[i];
    if EggArrayIndex(FKeywords, c) >= 0 then
    begin
    end
    else
    begin
      AWord := c;
      Result := i;
      exit;
    end;
  end;
end;

function TCodeParser.NextWord(var AWord: String; ANextIndex: Integer): Integer;
var
  i: Integer;
  c: String;
begin
  Result := -1;
  AWord := '';
  for i := FIndex to Pred(Length(FTokens)) do
  begin
		c := FTokens[i];
    if EggArrayIndex(FKeywords, c) >= 0 then
    begin
    end
    else
    begin
      FIndex := i;
      Result := FIndex;
      FIndex += ANextIndex;
      AWord := c;
      exit;
    end;
  end;
end;

function TCodeParser.GetPriorWord(AIndex: Integer; var AWord: String): Integer;
var
  i: Integer;
  c: String;
begin
  AWord := '';
  Result := -1;
	for i := AIndex downto 0 do
  begin
		c := FTokens[i];
    if EggArrayIndex(FKeywords, c) >= 0 then
    begin
    end
    else
    begin
      AWord := c;
      Result := i;
      exit;
    end;
  end;
end;

function TCodeParser.GetNextNotSpace(AIndex: Integer;
  var AToken: String): Integer;
var
  i: Integer;
  c: String;
begin
  AToken := '';
  Result := -1;
  for i := AIndex to Pred(Length(FTokens)) do
  begin
		c := FTokens[i];
    if EggArrayIndex(FSpaces, c) >= 0 then
    begin
    end
    else
    begin
      AToken := c;
      Result := i;
      exit;
    end;
  end;
end;

function TCodeParser.GetNextValue(AIndex: Integer; var AValue: String): Integer;
var
  i: Integer;
  c: String;
begin
	AValue := '';
  Result := -1;
  for i := AIndex to Pred(Length(FTokens)) do
  begin
		c := FTokens[i];
    if EggArrayIndex(FSpaces, c) >= 0 then
    begin
    end
    else if EggStrEqual(c, '''') then
    begin
      if (i + 2) < Length(FTokens) then
      begin
        AValue := FTokens[i + 1];
        Result := i + 2;
        exit;
      end;
    end
    else
    begin
      AValue := c;
      Result := i;
      exit;
    end;
  end;
end;

function TCodeParser.FindFunctionName(AParent: TCodeElement; AFuncName: String; AArgv: TCodeFunctionArgv): TCodeFunction;
	function EqualArgument(A, B: TCodeFunctionArgv): Boolean;
	begin
		if not Assigned(A) then
    begin
      if not Assigned(B) then
      begin
				Result := True;
      end
      else
      begin
				Result := B.Equals(A);
      end;
      exit;
    end;
    Result := A.Equals(B);
  end;
var
  i: Integer;
  item: TCodeElement;
  func: TCodeFunction;
begin
	Result := nil;
  for i := 0 to Pred(AParent.Children.Count) do
  begin
		item := AParent.Children[i];
    if item is TCodeFunction then
    begin
    	func := item as TCodeFunction;
      if EggStrEqualSame(func.Name, AFuncName) then
      begin
        if EqualArgument(func.Argument, AArgv) then
        begin
        	Result := func;
          exit;
        end;
      end;
    end;
  end;
end;

function TCodeParser.FindClassFunctionName(
  const AClassName, AFuncName: String; AArgv: TCodeFunctionArgv): TCodeFunction;
var
  i, j: Integer;
  item: TCodeElement;
  classElement: TCodeClass;
  scope: TCodeClassScope;
begin
	Result := nil;
	for i := 0 to Pred(FCodeTree.Root.Children.Count) do
 	begin
		item := FCodeTree.Root.Children[i];
    if item is TCodeClass then
    begin
      classElement := item as TCodeClass;
      if not EggStrEqualSame(classElement.Name, AClassName) then continue;
      for j := 0 to Pred(classElement.Children.Count) do
      begin
      	if classElement.Children[j] is TCodeClassScope then
        begin
					scope := classElement.Children[j] as TCodeClassScope;
          Result := FindFunctionName(scope, AFuncName, AArgv);
          if Assigned(Result) then
          begin
            exit;
          end;
        end;
      end;
    end;
  end;
end;

procedure TCodeParser.InsertionComment(AElement: TCodeElement);
var
  i, classIndex: Integer;
  item: TCodeElement;
  cls: TCodeClass;
begin
  for i := Pred(AElement.Children.Count) downto 0 do
  begin
		item := AElement.Children[i];
    if item is TCodeComment then
    begin
      classIndex := i + 1;
      if classIndex < AElement.Children.Count then
			begin
      	if AElement.Children[classIndex] is TCodeClass then
        begin
					cls := AElement.Children[classIndex] as TCodeClass;
          cls.Comment := item.Code;
          AElement.Children.Delete(i);
        end;
      end;
    end;
  end;
end;

constructor TCodeParser.Create;
begin
  FCodeSection := csUnit;
  FKeywords := EggArrayOf(' ', #10, #9, ':=', '=', ';', ':', '.', ',', '{', '}', '(', ')', '<', '>', '''');
  FSpaces := EggArrayOf(' ', #10, #9);
end;

procedure TCodeParser.Parse(const ACode: String; ACodeTree: TCodeTree);
var
  code: String;
begin
  FCodeTree := ACodeTree;
  ACodeTree.Clear();
  code := EggReplaceLine(ACode);
  FTokens := EggStrToTokens(code, FKeywords);
  FIndex := 0;
  ParseElement(ACodeTree.Root);
  InsertionComment(ACodeTree.Root);
end;

end.

