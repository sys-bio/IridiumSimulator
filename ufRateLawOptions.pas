unit ufRateLawOptions;

{ The rate law checker's settings, and the registry as the user sees it.

  A designed form, so the layout can be adjusted in the IDE. It was built in
  code to begin with -- to avoid a second file that has to stay in step with
  the first -- but that reasoning serves the maintainer at the user's expense
  on a form whose buttons want moving by eye, so it has a .fmx like any other.

  What is NOT in the designer, and cannot be: the contents of the law list.
  They come from the registry, so Populate fills them.

  What it SHOWS matters as much as what it sets.

  A law that failed validation is listed, greyed, with the reason -- not
  quietly absent. An entry that vanishes silently is indistinguishable from
  one that was never added, and the first thing anyone does after editing a
  law is come here to look for it.

  The folders laws are read from are named on the form. Where a program looks
  for user files is otherwise undiscoverable, and it is the one thing somebody
  must know before they can add a law of their own. }

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.IOUtils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ListBox,
{$IF Defined(MSWINDOWS)}
  Winapi.ShellAPI, Winapi.Windows,
{$ELSEIF Defined(POSIX)}
  Posix.Stdlib,
{$ENDIF}
  RateLaw.Registry;

type
  TRateLawOptions = record
    Dynamic:   Boolean;
    ShowNotes: Boolean;
    { Ids the user has switched off. Held as a list rather than as flags on
      the registry, because the registry is rebuilt from disk on every check
      and anything stored on it would not survive that. }
    DisabledIds: TArray<string>;
    class function Default: TRateLawOptions; static;
    function IsDisabled(const AId: string): Boolean;
  end;

  TfrmRateLawOptions = class(TForm)
    chkDynamic: TCheckBox;
    chkNotes: TCheckBox;
    lstLaws: TListBox;
    lblDetail: TLabel;
    lblWhere: TLabel;
    pnlButtons: TLayout;
    btnAll: TButton;
    btnNone: TButton;
    btnOpenFolder: TButton;
    btnSaveCopies: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    procedure lstLawsChange(Sender: TObject);
    procedure btnAllClick(Sender: TObject);
    procedure btnNoneClick(Sender: TObject);
    procedure btnOpenFolderClick(Sender: TObject);
    procedure btnSaveCopiesClick(Sender: TObject);
  private
    FRegistry: TRateLawRegistry;
    FUserDir:  string;
    function EnsureUserDir: Boolean;
  public
    { Fills the parts the designer cannot: the law list, and the line saying
      where laws are read from. }
    procedure Populate(ARegistry: TRateLawRegistry;
                       const AUserDir, AProjectDir: string;
                       const AOptions: TRateLawOptions);
    procedure ReadBack(var AOptions: TRateLawOptions);
  end;

var
  frmRateLawOptions: TfrmRateLawOptions;

{ Shows the dialog. Returns True when the user accepted, with AOptions
  updated. ARegistry is displayed, never modified.

  AUserDir and AProjectDir are shown so the user can find out where laws come
  from. Either may be ''. }
function EditRateLawOptions(AOwner: TComponent; ARegistry: TRateLawRegistry;
                            const AUserDir, AProjectDir: string;
                            var AOptions: TRateLawOptions): Boolean;

implementation

{$R *.fmx}

class function TRateLawOptions.Default: TRateLawOptions;
begin
  Result.Dynamic     := False;   { the expensive half, off until asked for }
  Result.ShowNotes   := True;
  Result.DisabledIds := nil;
end;

function TRateLawOptions.IsDisabled(const AId: string): Boolean;
var
  S: string;
begin
  for S in DisabledIds do
    if SameText(S, AId) then Exit(True);
  Result := False;
end;

procedure TfrmRateLawOptions.lstLawsChange(Sender: TObject);
var
  Law: TRateLawDef;
  Text, P: string;
begin
  lblDetail.Text := '';
  if (FRegistry = nil) or (lstLaws.ItemIndex < 0) then Exit;
  Law := FRegistry.Find(lstLaws.ListItems[lstLaws.ItemIndex].TagString);
  if Law = nil then Exit;

  Text := Law.Expression;
  if Law.Generative then
    Text := Text + '   (a family: instantiated per reaction)';
  if not Law.Valid then
  begin
    Text := Text + sLineBreak + 'REJECTED, and not used:';
    for P in Law.Problems do
      Text := Text + sLineBreak + '   ' + P;
  end
  else if Law.Notes <> '' then
    Text := Text + sLineBreak + Law.Notes;
  lblDetail.Text := Text;
end;

procedure TfrmRateLawOptions.btnAllClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to lstLaws.Count - 1 do
    if lstLaws.ListItems[I].Tag = 1 then     { valid ones only }
      lstLaws.ListItems[I].IsChecked := True;
end;

procedure TfrmRateLawOptions.btnNoneClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to lstLaws.Count - 1 do
    lstLaws.ListItems[I].IsChecked := False;
end;

{ Created on demand, never at start-up: a directory that appears merely
  because the program ran is clutter, but one the user has just asked to look
  at plainly should be there. }
function TfrmRateLawOptions.EnsureUserDir: Boolean;
begin
  Result := False;
  if FUserDir = '' then Exit;
  try
    if not TDirectory.Exists(FUserDir) then
      TDirectory.CreateDirectory(FUserDir);
    Result := True;
  except
    on E: Exception do
      ShowMessage('Could not create' + sLineBreak + FUserDir
                  + sLineBreak + sLineBreak + E.Message);
  end;
end;

procedure TfrmRateLawOptions.btnOpenFolderClick(Sender: TObject);
begin
  if not EnsureUserDir then Exit;
{$IF Defined(MSWINDOWS)}
  ShellExecute(0, 'open', PChar(FUserDir), nil, nil, SW_SHOWNORMAL);
{$ELSEIF Defined(POSIX)}
  _system(PAnsiChar('open "' + AnsiString(FUserDir) + '"'));
{$ENDIF}
end;

{ Writes the loaded laws into the user folder as .json.

  The point is a starting template. Authoring a rate law from the
  specification is far harder than editing a working one, and there was
  otherwise no way to obtain a working one short of reading the source. }
procedure TfrmRateLawOptions.btnSaveCopiesClick(Sender: TObject);
var
  I, N: Integer;
  Target: string;
begin
  if (FRegistry = nil) or not EnsureUserDir then Exit;
  try
    N := 0;
    for I := 0 to FRegistry.Count - 1 do
    begin
      { Qualified: FMX.Objects declares a TPath of its own -- a drawing
        shape -- and it comes later in the uses clause, so the bare name
        resolves to that one. }
      Target := System.IOUtils.TPath.Combine(FUserDir,
                                             FRegistry[I].Id + '.json');
      { Never overwrite. A file already there is the user's own edit of that
        law, and replacing it with the built-in would discard their work in
        the course of an action that reads as harmless. }
      if TFile.Exists(Target) then Continue;
      TFile.WriteAllText(Target, FRegistry[I].ToJsonText, TEncoding.UTF8);
      Inc(N);
    end;

    if N = 0 then
      ShowMessage('Every law is already present in' + sLineBreak + FUserDir)
    else
      ShowMessage(Format('Wrote %d law file(s) to', [N]) + sLineBreak
                  + FUserDir + sLineBreak + sLineBreak
                  + 'Edit one and re-open this dialog to load it. An edited '
                  + 'file replaces the built-in law of the same name; delete '
                  + 'it to go back to the original.');
  except
    on E: Exception do
      ShowMessage('Could not write to' + sLineBreak + FUserDir
                  + sLineBreak + sLineBreak + E.Message);
  end;
end;

procedure TfrmRateLawOptions.Populate(ARegistry: TRateLawRegistry;
  const AUserDir, AProjectDir: string; const AOptions: TRateLawOptions);
var
  Item: TListBoxItem;
  Law: TRateLawDef;
  I: Integer;
  Where: string;
begin
  FRegistry := ARegistry;
  FUserDir  := AUserDir;

  chkDynamic.IsChecked := AOptions.Dynamic;
  chkNotes.IsChecked   := AOptions.ShowNotes;

  Where := 'Read from: the built-in set, then ' + AUserDir;
  if AProjectDir <> '' then
    Where := Where + ', then ' + AProjectDir
  else
    Where := Where + '.  Save the model to use a law set stored beside it.';
  lblWhere.Text := Where;

  lstLaws.BeginUpdate;
  try
    lstLaws.Clear;
    for I := 0 to ARegistry.Count - 1 do
    begin
      Law  := ARegistry[I];
      Item := TListBoxItem.Create(lstLaws);
      Item.Parent    := lstLaws;
      Item.TagString := Law.Id;
      if Law.LawName <> '' then
        Item.Text := Format('%s   -   %s', [Law.Id, Law.LawName])
      else
        Item.Text := Law.Id;

      if Law.Valid then
      begin
        Item.Tag       := 1;
        Item.IsChecked := not AOptions.IsDisabled(Law.Id);
      end
      else
      begin
        Item.Tag       := 0;
        Item.IsChecked := False;
        Item.Enabled   := False;
        Item.Text      := Item.Text + '   [rejected]';
      end;
    end;
  finally
    lstLaws.EndUpdate;
  end;

  if lstLaws.Count > 0 then lstLaws.ItemIndex := 0;
end;

procedure TfrmRateLawOptions.ReadBack(var AOptions: TRateLawOptions);
var
  I: Integer;
begin
  AOptions.Dynamic     := chkDynamic.IsChecked;
  AOptions.ShowNotes   := chkNotes.IsChecked;
  AOptions.DisabledIds := nil;
  for I := 0 to lstLaws.Count - 1 do
    if (lstLaws.ListItems[I].Tag = 1)
       and not lstLaws.ListItems[I].IsChecked then
      AOptions.DisabledIds := AOptions.DisabledIds
        + [lstLaws.ListItems[I].TagString];
end;

function EditRateLawOptions(AOwner: TComponent; ARegistry: TRateLawRegistry;
  const AUserDir, AProjectDir: string;
  var AOptions: TRateLawOptions): Boolean;
var
  F: TfrmRateLawOptions;
begin
  Result := False;
  if ARegistry = nil then Exit;

  { Created and freed per showing rather than kept in the global, so the list
    cannot go stale against a registry that is rebuilt from disk on every
    check. }
  F := TfrmRateLawOptions.Create(AOwner);
  try
    F.Populate(ARegistry, AUserDir, AProjectDir, AOptions);
    if F.ShowModal = mrOk then
    begin
      F.ReadBack(AOptions);
      Result := True;
    end;
  finally
    F.Free;
  end;
end;

end.
