unit uGraphPropertyEditor;

interface

uses DesignIntf, DesignEditors;

type
  TMyClassProperty = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

procedure Register;

implementation

//uses MyClassUnit;

procedure TMyClassProperty.Edit;
begin
//  with TMyDialog.Create(nil) do
  //try
 //   ShowModal;
//  finally
//    Free;
//  end;
end;

function TMyClassProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TMyClass), nil, '', TMyClassProperty);
end;
