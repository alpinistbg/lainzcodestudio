unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, StdCtrls, Menus, Dialogs, Lua53, SynHighlighterLua, SynEdit,
  LCLIntF, Controls, SynEditMarks,
  Graphics, ActnList, Buttons, StdActns, ExtCtrls, SynEditTypes,
  uwatches, ulocals, luascript;

type
  { TfrmMain }

  TfrmMain = class(TForm)
    actFreeRun: TAction;
    actFind: TAction;
    actFindNext: TAction;
    actFindPrev: TAction;
    actNew: TAction;
    actSave: TAction;
    actRefreshWatches: TAction;
    actShowLocals: TAction;
    actShowWatches: TAction;
    actToggleBkpt: TAction;
    actWatch: TAction;
    actStop: TAction;
    actPause: TAction;
    actStepInto: TAction;
    actStepOver: TAction;
    actRun: TAction;
    ActionList1: TActionList;
    actOpen: TFileOpen;
    actSaveAs: TFileSaveAs;
    cbFindWhat: TComboBox;
    actCut: TEditCut;
    actCopy: TEditCopy;
    actDelete: TEditDelete;
    actPaste: TEditPaste;
    actSelectAll: TEditSelectAll;
    actUndo: TEditUndo;
    ImageList1: TImageList;
    ImageList2: TImageList;
    lblCaretPosition: TLabel;
    lblScriptState: TLabel;
    ListBox1: TListBox;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
    MenuItem43: TMenuItem;
    MenuItem44: TMenuItem;
    MenuItem45: TMenuItem;
    MenuItem46: TMenuItem;
    MenuItem47: TMenuItem;
    MenuItem48: TMenuItem;
    MenuItem49: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem50: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    pnlMessages: TPanel;
    pnlBott: TPanel;
    pnlMenuTools: TPanel;
    pnlSearchTools: TPanel;
    pnlToolbar: TPanel;
    PopupMenu1: TPopupMenu;
    Editor: TSynEdit;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton12: TSpeedButton;
    SpeedButton13: TSpeedButton;
    SpeedButton14: TSpeedButton;
    SpeedButton15: TSpeedButton;
    SpeedButton16: TSpeedButton;
    SpeedButton17: TSpeedButton;
    SpeedButton18: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    splitMain: TSplitter;
    SynLuaSyn1: TSynLuaSyn;
    procedure actFindExecute(Sender: TObject);
    procedure actFindNextExecute(Sender: TObject);
    procedure actFindPrevExecute(Sender: TObject);
    procedure actNewExecute(Sender: TObject);
    procedure actOpenAccept(Sender: TObject);
    procedure actOpenBeforeExecute(Sender: TObject);
    procedure actFreeRunExecute(Sender: TObject);
    procedure actPauseExecute(Sender: TObject);
    procedure actRefreshWatchesExecute(Sender: TObject);
    procedure actRunExecute(Sender: TObject);
    procedure actSaveAsAccept(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSaveUpdate(Sender: TObject);
    procedure actShowLocalsExecute(Sender: TObject);
    procedure actShowWatchesExecute(Sender: TObject);
    procedure actStepIntoExecute(Sender: TObject);
    procedure actStepOverExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure actToggleBkptExecute(Sender: TObject);
    procedure actWatchExecute(Sender: TObject);
    procedure cbFindWhatKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditorSpecialLineColors(Sender: TObject; Line: integer;
      var Special: boolean; var FG, BG: TColor);
    procedure EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem18Click(Sender: TObject);
    procedure MenuItem19Click(Sender: TObject);
    procedure MenuItem20Click(Sender: TObject);
    procedure MenuItem21Click(Sender: TObject);
    procedure MenuItem22Click(Sender: TObject);
    procedure MenuItem23Click(Sender: TObject);
    procedure MenuItem24Click(Sender: TObject);
    procedure MenuItem25Click(Sender: TObject);
    procedure MenuItem26Click(Sender: TObject);
    procedure MenuItem27Click(Sender: TObject);
    procedure EditorGutterClick(Sender: TObject; X, Y, Line: integer;
      mark: TSynEditMark);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
  private
    { private declarations }
    FileName: string;

    procedure ToggleBreakpoint(Line: LongInt);
    procedure ScriptFinalize(AThrStat: Integer);
    procedure CaretPos(ALine, ACol: LongInt; ACenter: Boolean = True);
    procedure ShowScriptState;
    procedure Find(Opt: TSynSearchOptions);

    procedure LuaResume(Sender: TObject);
    procedure LuaPaused(Sender: TObject);
    procedure LuaFinalized(Sender: TObject);
    procedure LuaBeforeHook(Sender: TObject);
    procedure LuaError(Sender: TObject; AtLine: LongInt; AMsg: String);
    procedure LuaHookLine(Sender: TObject; AtLine: LongInt; var HasBrkpt: Boolean);
    procedure LuaPrint(Sender: TObject; AMsg: String);

    function BkptAtLine(ALine: LongInt): TSynEditMark;

    function DoCompile: Boolean;
    procedure DoRun(AStep: TScriptDbgStates);
    procedure DoResume(AStep: TScriptDbgStates);
    procedure DoStop(AReset: Boolean);
    procedure RefreshWatches;
    procedure RefreshLocals;

    function CheckStopped: Boolean;
    function CheckSaved: Boolean;
    procedure UpdateTitle;
    procedure UpdateStatus;
    procedure ClearAllMarks; // incl. breakponts
  public
    { public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  SysUtils, StrUtils;

{$R *.lfm}

const
  // Special line colors
  FG_ACTIVE = clWhite;
  BG_ACTIVE = clBlue;
  FG_BKPT = clWhite;
  BG_BKPT = clRed;
  FG_ACTIVE_ON_BKPT = clWhite;
  BG_ACTIVE_ON_BKPT = clMaroon;

var
  Script: TLuaScript;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Editor.Font.Height := Canvas.GetTextHeight('Fpc');
  ListBox1.Font.Height := Canvas.GetTextHeight('Fpc');
  SynLuaSyn1.ActiveDot := True;
  Caption := Application.Title;

  Script := TLuaScript.Create;
  Script.OnBeforeHook := @LuaBeforeHook;
  Script.OnHookLine := @LuaHookLine;
  Script.OnLuaError := @LuaError;
  Script.OnPaused := @LuaPaused;
  Script.OnPrint := @LuaPrint;
  Script.OnResume := @LuaResume;
  Script.OnFinalized := @LuaFinalized;

  ShowScriptState;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  Script.Free;
end;

procedure TfrmMain.MenuItem18Click(Sender: TObject);
begin
  Editor.InsertTextAtCaret('if () then' + LineEnding + LineEnding +
    'else' + LineEnding + LineEnding + 'end');
end;

procedure TfrmMain.MenuItem19Click(Sender: TObject);
begin
  Editor.InsertTextAtCaret('while () do' + LineEnding + LineEnding + 'end');
end;

procedure TfrmMain.MenuItem20Click(Sender: TObject);
begin
  Editor.InsertTextAtCaret('repeat' + LineEnding + LineEnding + 'until');
end;

procedure TfrmMain.MenuItem21Click(Sender: TObject);
begin
  Editor.InsertTextAtCaret('for i=0,10,1 do' + LineEnding + LineEnding + 'end');
end;

procedure TfrmMain.MenuItem22Click(Sender: TObject);
begin
  Editor.InsertTextAtCaret('for i,v in ipairs() do' + LineEnding + LineEnding + 'end');
end;

procedure TfrmMain.MenuItem23Click(Sender: TObject);
begin
  Editor.InsertTextAtCaret('for k in pairs() do' + LineEnding + LineEnding + 'end');
end;

procedure TfrmMain.MenuItem24Click(Sender: TObject);
begin
  Editor.InsertTextAtCaret('function f()' + LineEnding + '  return' +
    LineEnding + 'end');
end;

procedure TfrmMain.MenuItem25Click(Sender: TObject);
begin
  Editor.InsertTextAtCaret('--');
end;

procedure TfrmMain.MenuItem26Click(Sender: TObject);
begin
  Editor.InsertTextAtCaret('--[[' + LineEnding + ']]');
end;

procedure TfrmMain.MenuItem27Click(Sender: TObject);
begin
  Editor.InsertTextAtCaret('if () then' + LineEnding + LineEnding + 'end');
end;

procedure TfrmMain.EditorGutterClick(Sender: TObject; X, Y, Line: integer;
  mark: TSynEditMark);
begin
  ToggleBreakpoint(Line);
end;


procedure TfrmMain.MenuItem5Click(Sender: TObject);
begin
  Self.Close;
end;

procedure TfrmMain.MenuItem7Click(Sender: TObject);
begin
  OpenURL('https://github.com/lainz/lainzcodestudio/wiki');
end;

procedure TfrmMain.PopupMenu1Popup(Sender: TObject);
begin
  MenuItem8.Enabled := Editor.SelAvail;
  MenuItem10.Enabled := Editor.SelAvail;
end;

procedure TfrmMain.ToggleBreakpoint(Line: LongInt);
var
  Mark: TSynEditMark;
begin
  Mark := BkptAtLine(Line);
  if Mark <> Nil then
    repeat
      Editor.Marks.Remove(Mark);
      Mark.Free;
      Mark := BkptAtLine(Line);
    until Mark = Nil
  else
  begin
    Mark := TSynEditMark.Create(Editor);
    Mark.Line := Line;
    Mark.BookmarkNumber := -99; // brkpt
    Mark.ImageList := ImageList2;
    Mark.ImageIndex := 10;
    Mark.Visible := True;
    Editor.Marks.Add(Mark);
  end;
  Editor.Refresh;
end;

procedure TfrmMain.ScriptFinalize(AThrStat: Integer);
begin
  Script.Finalize(AThrStat);
  ShowScriptState;
  Editor.Refresh;
end;

procedure TfrmMain.CaretPos(ALine, ACol: LongInt; ACenter: Boolean);
begin
  if ACenter then
    if (ALine < Editor.TopLine + 2) or
      (ALine > Editor.TopLine + Editor.LinesInWindow - 2)
    then
      Editor.TopLine := ALine - (Editor.LinesInWindow div 2);
  Editor.CaretY := ALine;
  Editor.CaretX := ACol;
end;

procedure TfrmMain.ShowScriptState;
begin
  actRun.Enabled := not Script.IsRunning or Script.IsPaused;
  actFreeRun.Enabled := not  Script.IsRunning or Script.IsPaused;
  actPause.Enabled :=  Script.IsRunning and not Script.IsPaused;
  actStop.Enabled :=  Script.IsRunning;

  actStepInto.Enabled := not Script.IsFreeRunning;
  actStepOver.Enabled := not Script.IsFreeRunning;

  if Script.IsPaused then
    lblScriptState.Caption := Format('Paused at line %d', [Script.SrcLine])
  else if Script.IsFreeRunning then
    lblScriptState.Caption := 'Running (no brkpt)...'
  else if Script.IsRunning then
    lblScriptState.Caption := 'Running...'
  else
    lblScriptState.Caption := 'Not running';
end;

procedure TfrmMain.Find(Opt: TSynSearchOptions);
begin
  if Editor.SearchReplace(cbFindWhat.Text, '', Opt) = 0 then
    Beep;
end;

procedure TfrmMain.LuaResume(Sender: TObject);
begin
  ShowScriptState
end;

procedure TfrmMain.LuaPaused(Sender: TObject);
begin
  CaretPos(Script.SrcLine, 1);
  ShowScriptState;
end;

procedure TfrmMain.LuaFinalized(Sender: TObject);
begin
  ShowScriptState;
  Editor.Refresh;
end;

procedure TfrmMain.LuaBeforeHook(Sender: TObject);
begin
  Application.ProcessMessages;
end;

procedure TfrmMain.LuaError(Sender: TObject; AtLine: LongInt; AMsg: String);
var
  Pre: String;
begin
  if AtLine < 1 then
    Pre := 'Error: ' else
    Pre := Format('Line %d: ', [Script.SrcLine]);
  ListBox1.Items.Add(Pre + AMsg);
end;

procedure TfrmMain.LuaHookLine(Sender: TObject; AtLine: LongInt;
  var HasBrkpt: Boolean);
begin
  HasBrkpt := BkptAtLine(AtLine) <> Nil;
end;

procedure TfrmMain.LuaPrint(Sender: TObject; AMsg: String);
begin
  ListBox1.Items.AddText(AMsg);
end;

function TfrmMain.BkptAtLine(ALine: LongInt): TSynEditMark;
var
  Marks: TSynEditMarkLine;
  Mark: TSynEditMark;
  I: Integer;
begin
  Result := Nil;
  Marks := Editor.Marks.Line[ALine];
  if Assigned(Marks) then
    for I := 0 to Pred(Marks.Count) do
    begin
      Mark := Marks[I];
      if Mark.BookmarkNumber < 0 then
      begin
        Result := Mark;
        Break;
      end;
    end;
end;


procedure TfrmMain.ClearAllMarks;
begin
  while Editor.Marks.Count > 0 do
    Editor.Marks.Delete(0);
end;

function TfrmMain.DoCompile: Boolean;
begin
  ListBox1.Clear;
  Script.SourceText := Editor.Text;
  Script.SourceName := IfThen(FileName <> '', FileName, 'Noname');
  Result := Script.Compile;
end;

procedure TfrmMain.DoRun(AStep: TScriptDbgStates);
begin
  if Script.IsPaused then
    DoResume(AStep)

  else if not Script.IsRunning then
  begin
    if DoCompile then
      DoResume(AStep);
  end;
end;

procedure TfrmMain.DoResume(AStep: TScriptDbgStates);
begin
  Script.Resume(AStep);
  Editor.Refresh;
  RefreshWatches;
  RefreshLocals;
end;

procedure TfrmMain.DoStop(AReset: Boolean);
begin
  Script.Stop(AReset);
end;

procedure TfrmMain.RefreshWatches;
var
  SID, Cont: String;
  I: Integer;
begin
  with frmWatches do
    for I := 0 to Pred(moWatches.Lines.Count) do
    begin
      SID := Trim(ExtractWord(1, moWatches.Lines[I], ['=']));
      if (SID = '') or not (SID[1] in LUA_ID_FIRST) then
        Continue;
      if Script.IsRunning then
        Cont := Script.GetVarContents(SID) else
        Cont := '(not running)';
      moWatches.Lines[I] := SID + ' = ' + Cont;
    end;
end;

procedure TfrmMain.RefreshLocals;
begin
  with frmLocals do
    moLocals.Text := Script.GetLocalContents(True, False);
end;

function TfrmMain.CheckStopped: Boolean;
begin
  Result := True;
  if  Script.IsRunning or Script.IsPaused then
  case QuestionDlg('', 'Program is running. Do you want to stop it?',
    mtWarning, [mrYes, 'Stop it', 'isdefault', mrCancel, 'Cancel'], 0)
  of
    mrCancel: Result := False;
    mrYes: actStop.Execute;
  end;
end;

function TfrmMain.CheckSaved: Boolean;
begin
  Result := True;
  if Editor.Modified then
    case QuestionDlg('', 'Source modified. Do you want to save changes?', mtWarning, [mrYes, 'Save',
      'isdefault', mrNo, 'Discard changes', mrCancel, 'Cancel'], 0)
    of
      mrNo: ;
      mrCancel: Result := False;
      mrYes:
        begin
          if FileName <> '' then
           actSave.Execute else
           actSaveAs.Execute;
          Result := FileName <> '';
        end;
      end;
end;

procedure TfrmMain.UpdateTitle;
begin
  Caption :=
    Application.Title + ' | ' +
    IfThen(Editor.Modified, '*') +
    IfThen(FileName <> '', FileName, 'Noname');
end;

procedure TfrmMain.UpdateStatus;
begin
  lblCaretPosition.Caption := Format('Line: %d, Col: %d (%s)', [Editor.CaretY,
    Editor.CaretX, IfThen(Editor.InsertMode, 'INS', 'OVR')]);
end;

procedure TfrmMain.actRunExecute(Sender: TObject);
begin
  DoRun([]);
end;

procedure TfrmMain.actSaveAsAccept(Sender: TObject);
begin
  Editor.Lines.SaveToFile(actSaveAs.Dialog.FileName);
  FileName := actSaveAs.Dialog.FileName;
  Editor.Modified := False;
  UpdateTitle; // to put the name on the title
end;

procedure TfrmMain.actSaveExecute(Sender: TObject);
begin
  if FileName = '' then
    actSaveAs.Execute
  else
  begin
    Editor.Lines.SaveToFile(FileName);
    Editor.Modified := False;
  end;
end;

procedure TfrmMain.actSaveUpdate(Sender: TObject);
begin
  actSave.Enabled := Editor.Modified;
end;

procedure TfrmMain.actShowLocalsExecute(Sender: TObject);
begin
  if frmLocals.Visible then
    frmLocals.Hide else frmLocals.Show;
  actShowLocals.Checked := frmLocals.Visible;
end;

procedure TfrmMain.actShowWatchesExecute(Sender: TObject);
begin
  if frmWatches.Visible then
    frmWatches.Hide else frmWatches.Show;
  actShowWatches.Checked := frmWatches.Visible;
end;

procedure TfrmMain.actFreeRunExecute(Sender: TObject);
begin
  DoRun([ssFreeRun]);
end;

procedure TfrmMain.actOpenBeforeExecute(Sender: TObject);
begin
  if not CheckStopped or not CheckSaved then
    Abort;
end;

procedure TfrmMain.actOpenAccept(Sender: TObject);
begin
  Editor.Lines.LoadFromFile(actOpen.Dialog.FileName);
  FileName := actOpen.Dialog.FileName;
  Editor.Modified := False;
  ClearAllMarks;
  UpdateTitle; // to put the name on the title
end;

procedure TfrmMain.actNewExecute(Sender: TObject);
begin
  if CheckStopped and CheckSaved then
  begin
    Editor.ClearAll;
    Editor.Lines.Text := '';
    Editor.Modified := False;
    FileName := '';
    ClearAllMarks;
    Editor.Modified := False;
  end;
end;

procedure TfrmMain.actFindExecute(Sender: TObject);
var
  S: String;
begin
  S := Editor.SelText;
  if S <> '' then
    cbFindWhat.Text := S;
  cbFindWhat.SetFocus;
end;

procedure TfrmMain.actFindNextExecute(Sender: TObject);
begin
  Find([]);
end;

procedure TfrmMain.actFindPrevExecute(Sender: TObject);
begin
  Find([ssoBackwards]);
end;

procedure TfrmMain.actPauseExecute(Sender: TObject);
begin
  Script.Stop(False);
end;

procedure TfrmMain.actRefreshWatchesExecute(Sender: TObject);
begin
  RefreshWatches;
end;

procedure TfrmMain.actStepIntoExecute(Sender: TObject);
begin
  DoRun([ssStepInto]);
end;

procedure TfrmMain.actStepOverExecute(Sender: TObject);
begin
  DoRun([ssStepOver]);
end;

procedure TfrmMain.actStopExecute(Sender: TObject);
begin
  if  Script.IsPaused then
    ScriptFinalize(0) else
    Script.Stop(True);
end;

procedure TfrmMain.actToggleBkptExecute(Sender: TObject);
begin
  ToggleBreakpoint(Editor.CaretY);
end;

procedure TfrmMain.actWatchExecute(Sender: TObject);
var
  S, SID, Cont: String;
  I: Integer = 0;
begin
  S := Trim(Editor.SelText);
  if S = '' then
  begin
    S := Trim(InputBox('Watch', 'Variable', ''));
  end;
  if S = '' then
    Exit;
  for I := 1 to 10 do
  begin
    SID := ExtractWord(I, S, LUA_ID_DELIMITERS);
    if SID = '' then
      Break;
    if not (SID[1] in LUA_ID_FIRST) then
      Continue;
    Cont := Script.GetVarContents(SID);
    if not frmWatches.Visible then
      actShowWatches.Execute;
    frmWatches.moWatches.Lines.Add(SID + ' = ' + Cont);
  end;
end;

procedure TfrmMain.cbFindWhatKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 13 then
    actFindNext.Execute;
end;

procedure TfrmMain.EditorSpecialLineColors(Sender: TObject; Line: integer;
  var Special: boolean; var FG, BG: TColor);
begin
  if BkptAtLine(Line) <> Nil then
  begin
    Special := True;
    if Line = Script.SrcLine then
    begin
      FG := FG_ACTIVE_ON_BKPT;
      BG := BG_ACTIVE_ON_BKPT;
    end else
    begin
      FG := FG_BKPT;
      BG := BG_BKPT;
    end;
  end
  else if Line = Script.SrcLine then
  begin
    Special := True;
    FG := FG_ACTIVE; // clWhite;
    BG := BG_ACTIVE; // clBlue;
  end
  else
    Special := False;
end;

procedure TfrmMain.EditorStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  if [scCaretX, scCaretY, scInsertMode] * Changes <> [] then
    UpdateStatus;
  if scModified in Changes then
  begin
    UpdateTitle;
  end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Script.IsRunning then
    ScriptFinalize(0);
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := CheckStopped and CheckSaved;
end;

end.
