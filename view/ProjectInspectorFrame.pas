unit ProjectInspectorFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, ComCtrls,
  Laz2_DOM, EggXMLUtils,
  EggStrUtils, EggFileUtils, EggPathUtils;

type
  TProjSelectedFileEvent = procedure(ASender: TObject; AFileName: String) of object;

  { TProjFileInfo }
  TProjFileInfo = class(TObject)
  private
    FFileName: String;
    procedure SetFileName(AValue: String);
 public
    property FileName: String read FFileName write SetFileName;
  end;

  { TFProjectInspectorFrame }
  TFProjectInspectorFrame = class(TFrame)
    FProject_imgs: TImageList;
    Panel1: TPanel;
    FProject_tree: TTreeView;
    procedure FProject_treeDblClick(Sender: TObject);
  private
    FFileName: String;
    FOnSelectedFile: TProjSelectedFileEvent;

    procedure OpenLPK(const AFileName: String);
    procedure SetOnSelectedFile(AValue: TProjSelectedFileEvent);
    procedure DoOnSelectedFile(AFileName: String);
  public
		constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure OpenFile(const AFileName: String);
    property OnSelectedFile: TProjSelectedFileEvent read FOnSelectedFile write SetOnSelectedFile;
  end;

implementation

{$R *.lfm}

const
	DefaultFolderImageIndex = 0;
	DefaultFileImageIndex = 1;

{ TProjFileInfo }

procedure TProjFileInfo.SetFileName(AValue: String);
begin
  if FFileName = AValue then Exit;
  FFileName := AValue;
end;

{ TFProjectInspectorFrame }

constructor TFProjectInspectorFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FOnSelectedFile := nil;
end;

destructor TFProjectInspectorFrame.Destroy;
begin
  inherited Destroy;
end;

procedure TFProjectInspectorFrame.OpenFile(const AFileName: String);
var
  ext: String;
begin
  if not EggFileExists(AFileName) then exit;

	ext := ExtractFileExt(AFileName);
  if AnsiCompareFileName(ext, '.lpk') = 0 then
  begin
  	OpenLPK(AFileName);
  end;
end;

procedure TFProjectInspectorFrame.FProject_treeDblClick(Sender: TObject);
var
  node: TTreeNode;
  info: TProjFileInfo;
begin
  node := FProject_tree.Selected;
  if not Assigned(node) then exit;
  if not Assigned(node.Data) then exit;

  info := TProjFileInfo(node.Data);
  DoOnSelectedFile(info.FileName);
end;

procedure TFProjectInspectorFrame.OpenLPK(const AFileName: String);
var
  xdoc: TXMLDocument;
  packageNode, filesNode, itemNode, fileNameNode: TDOMElement;
  itemNodes: TEggDOMElementArray;
  node, fileNode: TTreeNode;
  nm, path: String;
  i: Integer;
  info: TProjFileInfo;
begin
  FFileName := AFileName;
  path := EggExtractFilePath(AFileName);

  FProject_tree.Items.FreeAllNodeData();
  FProject_tree.Items.Clear();

  node := FProject_tree.Items.AddChild(nil, EggExtractFileNameOnly(AFileName));
  node.StateIndex := DefaultFolderImageIndex;
  xdoc := nil;
  try
		EggReadXMLFile(xdoc, AFileName);
    packageNode := EggXMLFindNode(xdoc.DocumentElement, 'Package');
    filesNode := EggXMLFindNode(packageNode, 'Files');
    itemNodes := EggXMLChildNodes(filesNode);
		for i := 0 to Pred(Length(itemNodes)) do
    begin
			itemNode := itemNodes[i];
      fileNameNode := EggXMLFindNode(itemNode, 'Filename');
      nm := EggXMLAttrRead(fileNameNode, 'Value', '');
      info := TProjFileInfo.Create();
      info.FileName := path + nm;
      info.FileName := StringReplace(info.FileName, '/', PathDelim, [rfReplaceAll]);
      fileNode := FProject_tree.Items.AddChildObject(node, nm, info);
      fileNode.StateIndex := DefaultFileImageIndex;
    end;

    node.Expand(True);
  finally
    FreeAndNil(xdoc);
  end;
end;

procedure TFProjectInspectorFrame.SetOnSelectedFile(AValue: TProjSelectedFileEvent);
begin
  if FOnSelectedFile = AValue then Exit;
  FOnSelectedFile := AValue;
end;

procedure TFProjectInspectorFrame.DoOnSelectedFile(AFileName: String);
begin
	if Assigned(FOnSelectedFile) then
  begin
  	FOnSelectedFile(Self, AFileName);
  end;
end;

end.

