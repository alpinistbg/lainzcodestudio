unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, StdCtrls, Menus, Dialogs, Lua53, SynHighlighterLua, SynEdit,
  LCLIntF, Controls, SynGutterBase, SynEditMarks, SynEditMarkupSpecialLine,
  Graphics, ActnList, Buttons, StdActns, ExtCtrls, uwatches, ulocals, SynEditTypes;

type
  TScriptState = (ssRunning, ssPaused, ssStepInto, ssStepOver, ssFreeRun);
  TScriptStates = set of TScriptState;
  TScriptDbgStates = set of ssStepInto..ssFreeRun;

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

    function BkptAtLine(ALine: LongInt): TSynEditMark;
    function HasBkptAtLine(ALine: LongInt): Boolean;

    function DoCompile: Boolean;
    procedure DoRun(AStep: TScriptDbgStates);
    procedure DoResume(AStep: TScriptDbgStates);
    procedure DoStop(AReset: Boolean);
    procedure ShowError(AErrorMsg: String);
    function GetVarContents(AId: String): String;
    function LuaVarToString(L: Plua_State): String;
    function GetLocalContents(L: Plua_State; AVarArgs, ATemps: Boolean): String;
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
  lcs_registerall, SysUtils, Math, StrUtils;

{$R *.lfm}

const
  // Identifier characters
  ID_FIRST = ['A'..'Z', 'a'..'z', '_'];
  ID_SYMBOL = ID_FIRST + ['0'..'9'];
  ID_DELIMITERS = [#9..#127] - ID_SYMBOL;

  // Special line colors
  FG_ACTIVE = clWhite;
  BG_ACTIVE = clBlue;
  FG_BKPT = clWhite;
  BG_BKPT = clRed;
  FG_ACTIVE_ON_BKPT = clWhite;
  BG_ACTIVE_ON_BKPT = clMaroon;

  PRINT_SEP = ' '; // (or ''?) print() separator
  MAX_TABLE_N = 32; // Max table elements to show
  MAX_TABLE_DEPTH = 5; // Max table depth to show
  MAX_TABLE_STRL = 512; // Max table string length

var
  // Just for the scope
  Script: record
    State: TScriptStates;
    StopRq, ResetRq: Boolean;
    L, Lt: Plua_State;
    LOfs, SrcLine, CallDepth, ReqDepth: LongInt;
    S: TStringList;
  end;

{ TfrmMain }

function Alloc({%H-}ud, ptr: Pointer; {%H-}osize, nsize: size_t): Pointer; cdecl;
begin
  try
    Result := ptr;
    ReallocMem(Result, nSize);
  except
    Result := nil;
  end;
end;

procedure DbgHook(L: Plua_State; ar: Plua_Debug); cdecl;
var
  MustYield, AtBkpt: Boolean;
begin
  Application.ProcessMessages;
  MustYield := False;
  case ar^.event of
    LUA_HOOKCALL:
      Inc(Script.CallDepth);
    LUA_HOOKRET:
      Dec(Script.CallDepth);
    LUA_HOOKLINE:
      begin
        Script.SrcLine := ar^.currentline - Script.LOfs;
        MustYield := Script.StopRq or Script.ResetRq;
        if
          not MustYield and
          not (ssFreeRun in Script.State) and
          (Script.SrcLine > 0)
        then
        begin
          AtBkpt := frmMain.HasBkptAtLine(Script.SrcLine);
          if (ssStepOver in Script.State) then
            MustYield := (Script.CallDepth <= Script.ReqDepth) or AtBkpt
          else
            MustYield := (ssStepInto in Script.State) or AtBkpt;
        end;
      end;
  end;
  if MustYield and lua_isyieldable(L) then
    lua_yield(L, 0);
end;

function StackToStr(L: Plua_State; ASep: String): String;
var
  I, N, T: Integer;
  S, Si: String;
begin
  S := '';
  N := lua_gettop(L);
  for I := 1 to N do
  begin
    T := lua_type(L, I);
    case T of
      LUA_TSTRING, LUA_TNUMBER:
        Si := lua_tostring(L, I);
      LUA_TNIL:
        Si := 'nil';
      LUA_TBOOLEAN:
        if lua_toboolean(L, I) then
          Si := 'true' else
          Si := 'false';
      otherwise
    	Si := '(' + lua_typename(L, T) + ')';
    end;
    if S = '' then
      S := Si else
      S := S + ASep + Si;
  end;
  Result := S;
end;

function print(L: Plua_State): Integer; cdecl;
begin
  frmMain.ListBox1.Items.AddText(StackToStr(L, PRINT_SEP));
  Result := 0;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Editor.Font.Height := Canvas.GetTextHeight('Fpc');
  ListBox1.Font.Height := Canvas.GetTextHeight('Fpc');
  SynLuaSyn1.ActiveDot := True;
  Caption := Application.Title;
  ShowScriptState;
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
  Script.State := [];
  Script.StopRq := False;
  Script.ResetRq := False;
  if LUA_YIELD_ < AThrStat then
    ShowError(lua_tostring(Script.Lt, -1));
  Script.SrcLine := -1;
  lua_close(Script.L);
  Script.S.Free;
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
var
  Running, Paused, FreeRun: Boolean;
begin
  Running := (ssRunning in Script.State);
  Paused := (ssPaused in Script.State);
  FreeRun := (ssFreeRun in Script.State);

  actRun.Enabled := not Running or Paused;
  actFreeRun.Enabled := not Running or Paused;
  actPause.Enabled := Running and not Paused;
  actStop.Enabled := Running;

  actStepInto.Enabled := not FreeRun;
  actStepOver.Enabled := not FreeRun;

  if Paused then
    lblScriptState.Caption := Format('Paused at line %d', [script.SrcLine])
  else if FreeRun then
    lblScriptState.Caption := 'Running (no brkpt)...'
  else if Running then
    lblScriptState.Caption := 'Running...'
  else
    lblScriptState.Caption := 'Not running';
end;

procedure TfrmMain.Find(Opt: TSynSearchOptions);
begin
  if Editor.SearchReplace(cbFindWhat.Text, '', Opt) = 0 then
    Beep;
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

function TfrmMain.HasBkptAtLine(ALine: LongInt): Boolean;
begin
  Result := BkptAtLine(ALine) <> Nil;
end;

procedure TfrmMain.ClearAllMarks;
begin
  while Editor.Marks.Count > 0 do
    Editor.Marks.Delete(0);
end;

function TfrmMain.DoCompile: Boolean;
begin
  ListBox1.Clear;
  Script.L := lua_newstate(@alloc, nil);
  luaL_openlibs(Script.L);
  lua_register(Script.L, 'print', @print);
  Script.S := TStringList.Create;

  RegisterAll(Script.L, Script.S);
  if 0 < luaL_dostring(Script.L, PChar(Script.S.Text)) then
  begin
    ShowError('RegisterAll()');
    ScriptFinalize(0);
    Exit;
  end;

  Script.S.Clear;
  Script.LOfs := 0; // offset of 1-st line
  Script.S.Add(Editor.Text);
  Script.Lt := lua_newthread(Script.L); // for yield/resume
  lua_sethook(Script.Lt, @DbgHook, LUA_MASKLINE + LUA_MASKCALL + LUA_MASKRET, 0);
  Result := (luaL_loadbuffer(Script.Lt, PChar(Script.S.Text),
    Length(Script.S.Text), 'Lainz Code Studio') = 0);
  Script.CallDepth := 0;
  if not Result then
    ShowError(lua_tostring(Script.Lt, -1)); // invalid source line
end;

procedure TfrmMain.DoRun(AStep: TScriptDbgStates);
begin
  if ssPaused in Script.State then
    DoResume(AStep)

  else if not(ssRunning in Script.State) then
  begin
    if DoCompile then
      DoResume(AStep);
  end;
end;

procedure TfrmMain.DoResume(AStep: TScriptDbgStates);
var
  stat: Integer;
begin
  Script.StopRq := False;
  Script.ResetRq := False;
  if ssStepOver in AStep then
    Script.ReqDepth := Max(1, Script.CallDepth);
  Script.State := AStep + [ssRunning];
  ShowScriptState;
  stat := lua_resume(Script.Lt, Script.Lt, 0);
  if stat = LUA_YIELD_ then
  begin
    Exclude(Script.State, ssFreeRun);
    Include(Script.State, ssPaused);
    CaretPos(Script.SrcLine, 1);
    ShowScriptState;
    if Script.StopRq or Script.ResetRq then
    begin
      Script.StopRq := False;
      if Script.ResetRq then
        ScriptFinalize(stat);
    end
  end
  else
    ScriptFinalize(stat);
  Editor.Refresh;
  RefreshWatches;
  RefreshLocals;
end;

procedure TfrmMain.DoStop(AReset: Boolean);
begin
  Script.StopRq := True;
  Script.ResetRq := AReset;
end;

procedure TfrmMain.ShowError(AErrorMsg: String);
var
  Pre: String;
begin
  if Script.SrcLine < 1 then
    Pre := 'Error: ' else
    Pre := Format('Line %d: ', [Script.SrcLine]);
  ListBox1.Items.Add(Pre + AErrorMsg);
end;

function TfrmMain.GetVarContents(AId: String): String;

  function EvaLua(L: Plua_State; AExp: String): Integer;
  begin
    luaL_loadstring(L, PChar('return ' + AExp));
    lua_pcall(L, 0, 1, 0);
    Result := lua_type(L, -1);
  end;

begin
  Result := '';
  if not (ssRunning in Script.State) then
    Exit;
  EvaLua(Script.L, AId);
  try
    Result := LuaVarToString(Script.L);
  finally
    lua_pop(Script.L, 1);
  end;
end;

function TfrmMain.LuaVarToString(L: Plua_State): String;

  function AddQuoted(S: String): String;
  var
    C: Char;
  begin
    Result := '"';
    for C in S do
      if C in [#0, #7, #8, #9, #10, #11, #12, #13, '"', '''', '\'] then
        case C of
           #0: Result := Result + '\0';
           #7: Result := Result + '\a';
           #8: Result := Result + '\b';
           #9: Result := Result + '\t';
          #10: Result := Result + '\n';
          #11: Result := Result + '\v';
          #12: Result := Result + '\f';
          #13: Result := Result + '\r';
        otherwise
          Result := Result + '\' + C;
        end
      else if C < ' ' then
        Result := Result + '\' + RightStr('000' + IntToStr(Ord(C)), 3)
      else
        Result := Result + C;
    Result := Result + '"';
  end;

  function VarToStr(L: Plua_State): String;
  var
    T: Integer;
    S: String;
  begin
    T := lua_type(L, -1);
    case T of
      LUA_TSTRING:
        S := AddQuoted(lua_tostring(L, -1));
      LUA_TNUMBER:
        S := lua_tostring(L, -1);
      LUA_TNIL:
        S := 'nil';
      LUA_TBOOLEAN:
        if lua_toboolean(L, -1) then
          S := 'true' else
          S := 'false';
      otherwise
        S := '(' + lua_typename(L, T) + ')';
    end;
    Result := S;
  end;

  function TblToStr(L: Plua_State; V: Integer): String;
  var
    N, T: Integer;
    S: String;
  begin
    if V > MAX_TABLE_DEPTH then
      Exit('(table)');

    Result := '{';
    N := 0;
    lua_pushnil(L);
    while lua_next(L, -2) <> 0 do
      try
        if N < 1 then
        else if N < MAX_TABLE_N then
          Result := Result + ', '
        else
        begin // do not print after n-th element
          Result := Result + ', ...';
          lua_pop(L, 1);
          Break;
        end;
        Inc(N);

        lua_pushvalue(L, -2);
        try
          S := Trim(ExtractWord(1, lua_tostring(L, -1), ID_DELIMITERS));
          Result := Result + IfThen(S <> '', S, '?')  + ' => ';
        finally
          lua_pop(L, 1);
        end;
        if lua_istable(L, -1) then
          Result := Result + TblToStr(L, V + 1) else
          Result := Result + VarToStr(L);
        if Length(Result) > MAX_TABLE_STRL then
          if V > 1 then
            Break else
            Exit(Result + ' ...');
      finally
        lua_pop(L, 1);
      end;
    Result := Result + '}';
  end;

begin
  Result := '';
  if lua_istable(L, -1) then
    Result := TblToStr(L, 1) else
    Result := VarToStr(L);
end;

function TfrmMain.GetLocalContents(L: Plua_State; AVarArgs, ATemps: Boolean
  ): String;
var
  ar: lua_Debug;
  I: Integer;
  LName: String;
  PLName: PChar;

  procedure L1(AInc: Integer; Temps: Boolean);
  begin
    I := AInc;
    while True do
    begin
      PLName := lua_getlocal(L, @ar, I);
      if PLName = Nil then
        Break
      else
        try
          LName := StrPas(PLName);
          if not Temps and (LName[1] = '(') then
            Continue;
          Result := Result +
            LName + ' = ' + LuaVarToString(L) + LineEnding;
        finally
          I := I + AInc;
          lua_pop(Script.Lt, 1);
        end;
    end;
  end;

begin
  Result := '';
  if not (ssRunning in Script.State) then
    Exit;
  if lua_getstack(L, 0, @ar) <> 1 then
    Exit;
  if AVarArgs then
    L1(-1, True);
  L1(1, ATemps);
end;

procedure TfrmMain.RefreshWatches;
var
  SID, Cont: String;
  I: Integer;
begin
  with frmWatches do
    for I := 0 to Pred(moWatches.Lines.Count) do
    begin
      SID := Trim(ExtractWord(1, moWatches.Lines[I], ['=']{ID_DELIMITERS}));
      if (SID = '') or not (SID[1] in ID_FIRST) then
        Continue;
      if (ssRunning in Script.State) then
        Cont := GetVarContents(SID) else
        Cont := '(not running)';
      moWatches.Lines[I] := SID + ' = ' + Cont;
    end;
end;

procedure TfrmMain.RefreshLocals;
begin
  with frmLocals do
    moLocals.Text := GetLocalContents(Script.Lt, True, False);
end;

function TfrmMain.CheckStopped: Boolean;
begin
  Result := True;
  if Script.State * [ssRunning, ssPaused] <> [] then
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
  Script.StopRq := True;
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
  if ssPaused in Script.State then
    ScriptFinalize(0) else
    Script.ResetRq := True;
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
    SID := ExtractWord(I, S, ID_DELIMITERS);
    if SID = '' then
      Break;
    if not (SID[1] in ID_FIRST) then
      Continue;
    Cont := GetVarContents(SID);
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
  if HasBkptAtLine(Line) then
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
  if ssRunning in Script.State then
    ScriptFinalize(0);
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := CheckStopped and CheckSaved;
end;

end.
