unit CodeWriter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  EggStrUtils,
  CodeTree;

type
  TCodeWriterSection = (cwsNone, cwsInterface, cwsType);

  { TCodeWriter }

  TCodeWriter = class(TObject)
  private
    FCodeTree: TCodeTree;
    FUnitCode: String;
    FUsesCode: String;
    FInterfaceCode: String;
    FImplementationCode: String;
    FTypeCode: String;
    FSection: TCodeWriterSection;

    function IndentCode(AComment: String; AIndent: Integer): String;
    procedure WriteElement(AElement: TCodeElement);
    procedure WriteUnit(AUnit: TCodeUnit);
    procedure WriteComment(AComment: TCodeComment);
    procedure WriteUses(AUses: TCodeUses);
    procedure WriteSpecialize(ASpecialize: TCodeSpecialize);
    procedure WriteClass(AClass: TCodeClass);
    procedure WriteClassScope(const AClassName: String; AClassScape: TCodeClassScope; var ACode: String);
    procedure WriteMember(AMember: TCodeMember; var ACode: String);
    procedure WriteFunction(const AClassName: String; AFunc: TCodeFunction; var AFuncName, AContent: String);
    procedure WriteFunctionArgument(AFuncArgv: TCodeFunctionArgv; var ACode: String);
    procedure WriteClassProperty(AProp: TCodeClassProperty; var ACode: String);
  public
    constructor Create;
		procedure Write(ACodeTree: TCodeTree; out ACode: String);
  end;

implementation

{ TCodeWriter }

function TCodeWriter.IndentCode(AComment: String; AIndent: Integer): String;
var
  lines: TDynStringArray;
  i: Integer;
begin
  lines := EggStrToStrings(AComment, #10);

  if Length(lines) <= 1 then
  begin
    Result := AComment;
    exit;
  end;

  Result := lines[0];
  for i := 1 to Pred(Length(lines)) do
  begin
		Result += #10 + EggStringOfChar(' ', AIndent) + lines[i];
  end;
end;

procedure TCodeWriter.WriteElement(AElement: TCodeElement);
var
  i: Integer;
  func, content: String;
begin
  if AElement is TCodeUnit then
  begin
  	WriteUnit(AElement as TCodeUnit);
    exit;
  end
  else if AElement is TCodeComment then
  begin
  	WriteComment(AElement as TCodeComment);
    exit;
  end
  else if AElement is TCodeUses then
  begin
  	WriteUses(AElement as TCodeUses);
    exit;
  end
  else if AElement is TCodeSpecialize then
  begin
    WriteSpecialize(AElement as TCodeSpecialize);
    exit;
  end
  else if AElement is TCodeClass then
  begin
  	WriteClass(AElement as TCodeClass);
    exit;
  end
  else if AElement is TCodeFunction then
  begin
  	WriteFunction('', AElement as TCodeFunction, func, content);
    FInterfaceCode += func + #10#10;
    FImplementationCode += content + #10#10;
    exit;
  end;

  for i := 0 to Pred(AElement.Children.Count) do
  begin
		WriteElement(AElement.Children[i]);
  end;
end;

procedure TCodeWriter.WriteUnit(AUnit: TCodeUnit);
begin
	FUnitCode := 'unit ' + AUnit.Name + ';' + #10;
end;

procedure TCodeWriter.WriteComment(AComment: TCodeComment);
var
  code: String;
begin
	code := AComment.Prefix + AComment.Code + AComment.Suffix;

  if FSection = cwsNone then
  begin
    FUsesCode += code;
  end
  else
  begin
    FInterfaceCode += '  ' + code + #10;
  end;
end;

procedure TCodeWriter.WriteUses(AUses: TCodeUses);
var
  code: String;
  i: Integer;
  usesUnit: TCodeUsesUnit;
begin
	code := 'uses' + #10 + '  ';
  for i := 0 to Pred(AUses.Children.Count) do
  begin
    if AUses.Children[i] is TCodeUsesUnit then
    begin
    	usesUnit := AUses.Children[i] as TCodeUsesUnit;
      if i > 0 then
      begin
        code += ', ';
      end;
      code += usesUnit.Code;
    end;
  end;
  code += ';' + #10;

  if FSection = cwsNone then
  begin
  	FSection := cwsInterface;
    code := #10#10 + 'interface' + #10#10 + code;
  end;

  FUsesCode += code;
end;

procedure TCodeWriter.WriteSpecialize(ASpecialize: TCodeSpecialize);
var
  code: String;
begin
  code := '  ' + ASpecialize.Name + ' = specialize ' + ASpecialize.BaseClass + '<' + ASpecialize.TypeClass + '>;' + #10#10;
  FInterfaceCode += code;
end;

procedure TCodeWriter.WriteClass(AClass: TCodeClass);
var
  code, comment: String;
  i: Integer;
begin
  code := '';
  if FSection = cwsInterface then
  begin
    FSection := cwsType;
    FTypeCode += 'type' + #10;
  end;

  FTypeCode += '  ' + AClass.Name + ' = class;' + #10;

  if EggStrEmpty(AClass.Comment) then
  begin
    code += '  { ' + AClass.Name + ' }' + #10;
  end
  else
  begin
    comment := IndentCode(AClass.Comment, 4);
    code += '  { ' + comment;
    if (Pos(#10, comment) > 0) then
    begin
      if not EggStrEqual(RightStr(comment, 1), #10) then
      begin
        code += #10;
      end;
      code += '  }' + #10;
    end
    else
    begin
      code += ' }' + #10;
    end;
  end;
  code += '  ' + AClass.Name + ' = class(' + AClass.BaseClass + ')' + #10;

  for i := 0 to Pred(AClass.Children.Count) do
  begin
    if AClass.Children[i] is TCodeClassScope then
    begin
    	code += '  ' + AClass.Children[i].Code + #10;
      WriteClassScope(AClass.Name, AClass.Children[i] as TCodeClassScope, code);
    end;
  end;

  code += '  end;' + #10#10;

  FInterfaceCode += code;
end;

procedure TCodeWriter.WriteClassScope(
  const AClassName: String; AClassScape: TCodeClassScope; var ACode: String);
var
  i: Integer;
  indent, mem, func, content: String;
  item: TCodeElement;
begin
  indent := '    ';
  for i := 0 to Pred(AClassScape.Children.Count) do
	begin
    item := AClassScape.Children[i];
		if item is TCodeMember then
    begin
    	WriteMember(item as TCodeMember, mem);
      ACode += indent + mem + ';' + #10;
    end
    else if item is TCodeClassProperty then
    begin
    	WriteClassProperty(item as TCodeClassProperty, mem);
      ACode += indent + mem + #10;
    end
    else if item is TCodeFunction then
    begin
      WriteFunction(AClassName, item as TCodeFunction, func, content);
      ACode += indent + IndentCode(func, 2) + #10;
      FImplementationCode += content + #10 + #10;
    end;
  end;
end;

procedure TCodeWriter.WriteMember(AMember: TCodeMember; var ACode: String);
begin
	ACode := AMember.Name + ': ' + AMember.MemberType;
end;

procedure TCodeWriter.WriteFunction(
  const AClassName: String; AFunc: TCodeFunction;
  var AFuncName, AContent: String);
var
  arg: TCodeFunctionArgv;
  args, comm: String;
begin
	arg := AFunc.Argument;
  args := '';
  AFuncName := '';
  AContent := '';
  if Assigned(arg) then
  begin
		WriteFunctionArgument(arg, args);
  end;
  if not EggStrEmpty(AFunc.Comment) then
  begin
		if Pos(#10, AFunc.Comment) > 0 then
    begin
      comm := '{ ' + IndentCode(AFunc.Comment, 2) + #10 + '}' + #10;
    end
    else
    begin
    	comm := '// ' + AFunc.Comment + #10;
    end;
    AFuncName += IndentCode(comm, 2);
  end;
  if EggStrEmpty(AFunc.ResultType) then
  begin
  	AFuncName += 'procedure ' + AFunc.Name + args + ';';
  	AContent += 'procedure ';
    if not EggStrEmpty(AClassName) then
    begin
	    AContent += AClassName + '.';
    end;
    AContent += AFunc.Name + args + ';';
    AContent += AFunc.Content + ';';
  end
  else
  begin
  	AFuncName += 'function ' + AFunc.Name + args + ': ' + AFunc.ResultType + ';';
  	AContent += 'function ';
    if not EggStrEmpty(AClassName) then
    begin
      AContent += AClassName + '.';
    end;
    AContent += AFunc.Name + args + ': ' + AFunc.ResultType + ';' + #10;
    AContent += AFunc.Content + ';';
  end;
end;

procedure TCodeWriter.WriteFunctionArgument(AFuncArgv: TCodeFunctionArgv;
  var ACode: String);
var
  i: Integer;
  item: TCodeElement;
  mem: TCodeMember;
begin
  ACode := '';
	for i := 0 to Pred(AFuncArgv.Children.Count) do
  begin
  	item := AFuncArgv.Children[i];
    if item is TCodeMember then
    begin
    	mem := item as TCodeMember;
      if not EggStrEmpty(ACode) then
      begin
        ACode += '; ';
      end;
      ACode += mem.Name + ': ' + mem.MemberType;
    end;
  end;
  ACode := '(' + Acode + ')'
end;

procedure TCodeWriter.WriteClassProperty(AProp: TCodeClassProperty;
  var ACode: String);
begin
	ACode := 'property ' + AProp.Name + ': ' + AProp.PropertyType;
  if not EggStrEmpty(AProp.Getter) then
  begin
  	ACode += ' read ' + AProp.Getter;
  end;
  if not EggStrEmpty(AProp.Setter) then
  begin
  	ACode += ' write ' + AProp.Setter;
  end;
  ACode += ';'
end;

constructor TCodeWriter.Create;
begin
  FSection := cwsNone;
end;

procedure TCodeWriter.Write(ACodeTree: TCodeTree; out ACode: String);
var
  i: Integer;
begin
	FCodeTree := ACodeTree;
  FUnitCode := '';
  FInterfaceCode := '';
  FImplementationCode := '';
  FTypeCode := '';
  FUsesCode := '';

  for i := 0 to Pred(FCodeTree.Root.Children.Count) do
  begin
  	WriteElement(FCodeTree.Root.Children[i]);
  end;

  ACode := FUnitCode + #10
  			+ FUsesCode + #10
  			+ FTypeCode + #10
  			+ FInterfaceCode
        + 'implementation' + #10#10
  			+ FImplementationCode
			  + 'end.' + #10;
end;

end.

