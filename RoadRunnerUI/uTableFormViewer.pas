unit uTableFormViewer;

// This unit deals with the inplace table panel on the main window
// It creates a viewer that can be registered with the controller

interface

Uses SysUtils,
     Classes,
     System.Types,
     FMX.Controls,
     FMX.Forms,
     FMX.Types,
     FMX.Memo,
     FMX.StdCtrls,
     FMX.Layouts,
     FMX.Colors,
     FMX.Dialogs,
     FMX.Objects,
     System.UIConsts,
     System.UITypes,
     uCommonTypes,
     uViewerTypes,
     uViewer,
     uRRTypes,
     uController;
type

  TTableFormViewer = class (TViewer)
    private
      layout: TLayout;
      dataMemoBackgroundColor : TAlphaColor;
      dataMemoFontColor : TAlphaColor;
      cboDataMemoBackgroundColor : TColorComboBox;
      cboDataMemoFontColor : TColorComboBox;
      saveCSVDialog : TSaveDialog;
      function  createTableHeader: TStringList;
      procedure fillDataGrid(header: TStringList; data: T2DMatrix);
      function  getNumberOfSelectedColumns: integer;
      function  extractColumnIndexes: TIntegerArray;
      procedure OnButtonClick (sender: TObject);
      procedure cboBackgroundColorChange (sender : TObject);
      procedure cboFontColorChange (sender : TObject);
      procedure dataMemoApplyStyleLookup(Sender: TObject);
    public
      simulationData : T2Dmatrix;
      dataMemo : TMemo;

      viewerPackage : TViewerPackage;

      procedure   ViewerUpdate (Sender : TObject; viewerPackage : TViewerPackage);
      procedure   ViewerClear (Sender : TObject);
      procedure   saveAsCSV (fileName : string);
      constructor Create (controller : TController; parentLayout : TLayout);
  end;

implementation

uses ufMain;

constructor TTableFormViewer.Create (controller : TController; parentLayout : TLayout);
var bottomLayout : TLayout;
    baseLayout : TLayout;
    btn : TButton;
    lb1, lb2 : TLabel;
begin
  inherited Create;

  OnViewerChange := ViewerUpdate;  // gets fired if there is new data
  OnViewerClear := ViewerClear;
  self.layout := parentLayout;  // keep a local copy
  controller.RegisterViewer(self);

  saveCSVDialog := TSaveDialog.Create(nil);
  saveCSVDialog.DefaultExt := '.csv';
  saveCSVDialog.Filter := 'CSV File|*.csv|Any files|*.*';

  baseLayout := TLayout.Create(parentLayout.parent);
  baseLayout.Parent := parentLayout;
  baseLayout.Align := TAlignLayout.Client;

  bottomLayout := TLayout.Create(baseLayout);
  bottomLayout.Parent := baseLayout;
  bottomLayout.Height := 64;
  bottomLayout.Align := TAlignLayout.Bottom;

  dataMemo := TMemo.Create (baseLayout);
  dataMemo.Parent := baseLayout;
  dataMemo.Align := TAlignLayout.Client;
  dataMemo.StyledSettings := [TStyledSetting.Style]; // allows us to change the font color
  dataMemo.OnApplyStyleLookup := dataMemoApplyStyleLookup;
  dataMemo.TextSettings.FontColor := claRed;

  btn := TButton.Create(bottomLayout);
  btn.parent := bottomLayout;
  btn.Text := 'Save as CSV File';
  btn.Width := 136;
  btn.Height := 28;
  btn.Anchors :=   [TAnchorKind.akLeft, TAnchorKind.akTop];
  btn.Position.X := 8;
  btn.Position.Y := (bottomLayout.Height / 2) - btn.Height/2;
  btn.OnClick := OnButtonClick;

  lb1 := TLabel.Create (bottomLayout);
  lb1.parent := bottomLayout;
  lb1.width := 172;
  lb1.Text := 'Background Color';
  lb1.Position.X := 168;
  lb1.Position.Y := 8;

  lb2 := TLabel.Create (bottomLayout);
  lb2.parent := bottomLayout;
  lb2.width := 172;
  lb2.Text := 'Font Color';
  lb2.Position.X := 363;
  lb2.Position.Y := 8;

  cboDataMemoBackgroundColor := TColorComboBox.Create(bottomLayout);
  cboDataMemoBackgroundColor.parent := bottomLayout;
  cboDataMemoBackgroundColor.Width := 160;
  cboDataMemoBackgroundColor.Position.X := 168;
  cboDataMemoBackgroundColor.Position.Y := 35;
  cboDataMemoBackgroundColor.DropDownCount := 16;
  dataMemoBackgroundColor := $FF1C3451; // claMidnightblue;// claDarkslategray;//  claDarkslateblue;
  cboDataMemoBackgroundColor.color := dataMemoBackgroundColor;
  cboDataMemoBackgroundColor.OnChange := cboBackgroundColorChange;

  cboDataMemoFontColor := TColorComboBox.Create(bottomLayout);
  cboDataMemoFontColor.parent := bottomLayout;
  cboDataMemoFontColor.Width := 160;
  cboDataMemoFontColor.Position.X := 360;
  cboDataMemoFontColor.Position.Y := 35;
  cboDataMemoFontColor.DropDownCount := 16;
  dataMemoFontColor := claSnow;
  cboDataMemoFontColor.OnChange := cboFontColorChange;
end;


procedure TTableFormViewer.cboBackgroundColorChange (sender : TObject);
begin
  dataMemoBackgroundColor := cboDataMemoBackgroundColor.color;
  dataMemo.NeedStyleLookup;
  dataMemo.ApplyStyleLookup;
end;


procedure TTableFormViewer.cboFontColorChange (sender : TObject);
begin
  dataMemoFontColor := cboDataMemoFontColor.color;
  dataMemo.NeedStyleLookup;
  dataMemo.ApplyStyleLookup;
end;


procedure TTableFormViewer.OnButtonClick (sender: TObject);
begin
  if saveCSVDialog.Execute then
     saveAsCSV (saveCSVDialog.FileName);
end;


procedure TTableFormViewer.dataMemoApplyStyleLookup(Sender: TObject);
var
  Obj: TFmxObject;
  Rectangle1: TRectangle;
begin
  Obj :=  dataMemo.FindStyleResource('background');
  if Obj <> nil then
    begin
      TControl(Obj).Margins := TBounds.Create(TRectF.Create(-1, -1, -1, -1));
      Rectangle1 := TRectangle.Create(Obj);
      Obj.AddObject(Rectangle1);
      Rectangle1.Align := TAlignLayout.Client;
      Rectangle1.Fill.color := dataMemoBackgroundColor;
      // claGhostWhite; // claFloralwhite;
      Rectangle1.Stroke.color := claNull;
      Rectangle1.HitTest := false;
      Rectangle1.SendToBack;
    end;
  dataMemo.TextSettings.FontColor := dataMemoFontColor;
end;


// This gets called when the viewer needs to be updated
procedure TTableFormViewer.ViewerUpdate (Sender : TObject; viewerPackage : TViewerPackage);
var header : TStringList;
begin
  if layout.Visible then
     begin
     simulationData := (sender as TController).simulator.simulationData;
     self.viewerPackage := viewerPackage;
     header := createTableHeader;
     fillDataGrid(header, simulationData);
     header.Free;
     end;
end;


procedure TTableFormViewer.ViewerClear (Sender : TObject);
begin
  dataMemo.Text := '';
end;


function TTableFormViewer.extractColumnIndexes: TIntegerArray;
var
  i, nc, Count: integer;
begin
  nc := getNumberOfSelectedColumns;
  Count := 0;
  setLength(result, nc);
  for i := 0 to length(viewerPackage.YColumnChoice) - 1 do
    if viewerPackage.YColumnChoice[i] then
      begin
        result[Count] := i;
        inc(Count);
      end;
end;


procedure TTableFormViewer.saveAsCSV (fileName : string);
begin
  dataMemo.Lines.SaveToFile(fileName);
end;


function TTableFormViewer.createTableHeader: TStringList;
var
  i, ns: integer;
  YColumnIndices : TIntegerArray;
begin
  ns := getNumberOfSelectedColumns;
  YColumnIndices := extractColumnIndexes;

  result := TStringList.Create;
  result.Add(viewerPackage.XAxisTitle);
  for i := 0 to ns - 1 do
    result.Add(viewerPackage.YColumnNames[YColumnIndices[i]]);
end;


function TTableFormViewer.getNumberOfSelectedColumns: integer;
var
  i: integer;
begin
  result := 0;
  for i := 0 to length(viewerPackage.YColumnChoice) - 1 do
    if viewerPackage.YColumnChoice[i] then
      inc(result);
end;


procedure TTableFormViewer.fillDataGrid(header: TStringList; data: T2DMatrix);
var
  i, j: integer;
  nr, nc, ns: integer;
  str: string;
  YColumnIndices: TIntegerArray;
begin
  nr := data.r;
  nc := data.c;

  ns := getNumberOfSelectedColumns;
  YColumnIndices := extractColumnIndexes;

  dataMemo.GoToTextBegin;
  dataMemo.Lines.Clear;
  dataMemo.BeginUpdate;

  // currentSelectionList is the totality of what can be plotted
  // according to selection made from the select button
  // YColumnIndices are the actual columns the users has picked out to be displayed.

  str := '"' + header[0] + '"';
  for i := 1 to header.Count - 1 do
    str := str + ',"' + header[i] + '"';

  dataMemo.Lines.Add(str);

  for i := 0 to nr - 1 do
    begin
      str := Format('%8.4g', [getElement(data, i, viewerPackage.XColumnIndex)]);
      for j := 0 to ns - 1 do
        str := str + ', ' + Format('%8.4g', [getElement(data, i, YColumnIndices[j])]);
      dataMemo.Lines.Add(str);
    end;
  dataMemo.EndUpdate;
end;




end.
