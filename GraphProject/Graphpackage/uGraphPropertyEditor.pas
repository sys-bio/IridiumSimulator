unit uGraphPropertyEditor;

interface

Uses SysUtils, DesignEditors, DesignIntf;

type
  TGraphPackagePropertyEditor = class(TPropertyEditor)
      procedure showDialog;
    public
      function  GetAttributes: TPropertyAttributes; override;
      procedure Edit; override;
      function  GetName : string; override;
      function  GetValue : string; override;
  end;

procedure Register;

implementation

Uses FMX.Forms, FMX.Dialogs, System.UITypes, ufGraphPackageDialog, uPlottingPanel, uSubGraph;

function TGraphPackagePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

function TGraphPackagePropertyEditor.GetName : string;
begin
   result := 'properties';
end;

function TGraphPackagePropertyEditor.GetValue : string;
begin
   result := '(TSubGraphProperties)';
end;

procedure TGraphPackagePropertyEditor.Edit;
begin
  showDialog;
end;

procedure TGraphPackagePropertyEditor.showDialog;
var DesignerForm : TfrmGraphPackageDlg;
    gp : TSubGraphProperties;
begin
  DesignerForm := TfrmGraphPackageDlg.Create(Application);
  try
    gp := TSubGraphProperties (GetOrdValue);
    DesignerForm.subgraph := (GetComponent(0) as TSubGraph);
    DesignerForm.copyPropertiesToDlg (gp);
    if DesignerForm.ShowModal = mrOK then
       DesignerForm.copyPropertiesFromDlg (gp);

    ((GetComponent(0) as TSubGraph).parentGraph as TRRGraph).redraw;
  finally
    DesignerForm.Free;
  end;
end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TSubGraphProperties), TSubgraph, 'properties', TGraphPackagePropertyEditor);
end;


end.
