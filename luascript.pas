unit luascript;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  lua53;

const
  // Identifier characters
  LUA_ID_FIRST = ['A'..'Z', 'a'..'z', '_'];
  LUA_ID_SYMBOL = LUA_ID_FIRST + ['0'..'9'];
  LUA_ID_DELIMITERS = [#9..#127] - LUA_ID_SYMBOL;

type
  TScriptState = (ssRunning, ssPaused, ssStepInto, ssStepOver, ssFreeRun);
  TScriptStates = set of TScriptState;
  TScriptDbgStates = set of ssStepInto..ssFreeRun;

  TOnLuaErrorEvent = procedure(Sender: TObject; AtLine: LongInt; AMsg: String) of object;
  TOnHookLineEvent = procedure(Sender: TObject; AtLine: LongInt; var HasBrkpt: Boolean) of object;
  TOnLuaPrint = procedure(Sender: TObject; AMsg: String) of object;

  TLuaException = class(Exception);

  { TLuaScript }

  TLuaScript = class
  private
    FScriptStates: TScriptStates;
    FSourceName: String;
    FStopRq, FResetRq: Boolean;
    FLState, FLtState: Plua_State;
    FLOfs, FSrcLine, FCallDepth, FReqDepth: LongInt;
    FSrcLines: TStringList;

    FOnBeforeHook: TNotifyEvent;
    FOnHookLine: TOnHookLineEvent;
    FOnLuaError: TOnLuaErrorEvent;
    FOnFinalized: TNotifyEvent;
    FOnFinalizing: TNotifyEvent;
    FOnPrint: TOnLuaPrint;
    FOnResume: TNotifyEvent;
    FOnPaused: TNotifyEvent;

    FSourceText: String;

    function GetIsFreeRunning: Boolean;
    function GetIsPaused: Boolean;
    function GetIsRunning: Boolean;
    procedure SetSourceName(AValue: String);
    procedure SetSourceText(AValue: String);
  protected
    procedure DoError(AErrorMsg: String);
    procedure DoBeforeHook;
    procedure DebugHook(L: Plua_State; ar: Plua_Debug); virtual;
    procedure DoPrint(AMsg: String);
  public
    constructor Create;
    function Compile(WithHooks: Boolean = True): Boolean;
    procedure Stop(AReset: Boolean);
    procedure Resume(AStep: TScriptDbgStates);
    procedure Finalize(AThrStat: Integer);

    function GetVarContents(AId: String): String;
    function GetLocalContents(AVarArgs, ATemps: Boolean): String;

    property SourceText: String read FSourceText write SetSourceText;
    property SourceName: String read FSourceName write SetSourceName;

    property IsRunning: Boolean read GetIsRunning;
    property IsPaused: Boolean read GetIsPaused;
    property IsFreeRunning: Boolean read GetIsFreeRunning;
    property SrcLine: LongInt read FSrcLine;

    property OnLuaError: TOnLuaErrorEvent read FOnLuaError write FOnLuaError;
    property OnBeforeHook: TNotifyEvent read FOnBeforeHook write FOnBeforeHook;
    property OnHookLine: TOnHookLineEvent read FOnHookLine write FOnHookLine;
    property OnResume: TNotifyEvent read FOnResume write FOnResume;
    property OnPaused: TNotifyEvent read FOnPaused write FOnPaused;
    property OnFinalizing: TNotifyEvent read FOnFinalizing write FOnFinalizing;
    property OnFinalized: TNotifyEvent read FOnFinalized write FOnFinalized;
    property OnPrint: TOnLuaPrint read FOnPrint write FOnPrint;
  end;


implementation

uses
  lcs_registerall, Math, test_a_json, StrUtils;

const
  PRINT_SEP = ' '; // (or ''?) print() separator
  MAX_TABLE_N = 32; // Max table elements to show
  MAX_TABLE_DEPTH = 5; // Max table depth to show
  MAX_TABLE_STRL = 512; // Max table string length

var
  ThisKey: Integer; // Address used for key to Self

function Alloc({%H-}ud, ptr: Pointer; {%H-}osize, nsize: size_t): Pointer; cdecl;
begin
  try
    Result := ptr;
    ReallocMem(Result, nSize);
  except
    Result := nil;
  end;
end;

function ThisScript(L: Plua_State): TLuaScript; inline;
begin
  lua_pushlightuserdata(L, @ThisKey);
  lua_gettable(L, LUA_REGISTRYINDEX);
  Result := TLuaScript(lua_topointer(L, -1));
  lua_pop(L, 1);
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

function Print(L: Plua_State): Integer; cdecl;
begin
  ThisScript(L).DoPrint(StackToStr(L, PRINT_SEP));
  Result := 0;
end;

procedure DbgHook(L: Plua_State; ar: Plua_Debug); cdecl;
begin
  ThisScript(L).DebugHook(L, ar);
end;

function LuaVarToString(L: Plua_State): String;

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
          S := Trim(ExtractWord(1, lua_tostring(L, -1), LUA_ID_DELIMITERS));
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

{ TLuaScript }

function TLuaScript.Compile(WithHooks: Boolean): Boolean;
begin
  Result := False;

  FLState := lua_newstate(@Alloc, nil);
  luaL_openlibs(FLState);

  luaL_requiref(FLState, 'q64936405', @luaopen_q64936405, True);

  lua_pushlightuserdata(FLState, @ThisKey);
  lua_pushlightuserdata(FLState, Self);
  lua_settable(FLState, LUA_REGISTRYINDEX);

  lua_register(FLState, 'print', @Print);
  lua_register(FLState, 'test_a_js', @test_a_js);
  lua_register(FLState, 'js_to_s', @js_to_s);

  FSrcLines := TStringList.Create;

  RegisterAll(FLState, FSrcLines);
  if 0 < luaL_dostring(FLState, PChar(FSrcLines.Text)) then
  begin
    DoError('RegisterAll()');
    Finalize(0);
    Exit;
  end;

  FSrcLines.Clear;
  FLOfs := 0; // offset of 1-st line
  FCallDepth := 0;
  FSrcLines.Add(FSourceText);
  FLtState := lua_newthread(FLState); // for yield/resume

  if WithHooks then
    lua_sethook(FLtState, @DbgHook, LUA_MASKLINE + LUA_MASKCALL + LUA_MASKRET, 0);
  if 0 < luaL_loadbuffer(FLtState, PChar(FSrcLines.Text), Length(FSrcLines.Text),
      PChar(FSourceName){'Lainz Code Studio'})
  then
  begin
    DoError(lua_tostring(FLtState, -1)); // invalid source line
    Finalize(0);
  end
  else
    Result := True;
end;

procedure TLuaScript.Stop(AReset: Boolean);
begin
  FStopRq := True;
  FResetRq := AReset;
end;

procedure TLuaScript.Resume(AStep: TScriptDbgStates);
var
  stat: Integer;
begin
  FStopRq := False;
  FResetRq := False;
  if ssStepOver in AStep then
    FReqDepth := Max(1, FCallDepth);
  FScriptStates := AStep + [ssRunning];
  if Assigned(FOnResume) then
    FOnResume(Self);
  stat := lua_resume(FLtState, FLtState, 0);
  if stat = LUA_YIELD_ then
  begin
    Exclude(FScriptStates, ssFreeRun);
    Include(FScriptStates, ssPaused);
    if Assigned(FOnPaused) then
      FOnPaused(Self);
    if FStopRq or FResetRq then
    begin
      FStopRq := False;
      if FResetRq then
        Finalize(stat);
    end
  end
  else
    Finalize(stat);
end;

procedure TLuaScript.Finalize(AThrStat: Integer);
begin
  if Assigned(FOnFinalizing) then
    FOnFinalizing(Self);
  FScriptStates := [];
  FStopRq := False;
  FResetRq := False;
  if LUA_YIELD_ < AThrStat then
    DoError(lua_tostring(FLtState, -1));
  FSrcLine := -1;
  lua_close(FLState);
  FSrcLines.Free;
  if Assigned(FOnFinalized) then
    FOnFinalized(Self);
end;

procedure TLuaScript.DoError(AErrorMsg: String);
var
  S: String;
begin
  if Assigned(FOnLuaError) then
    FOnLuaError(Self, FSrcLine, AErrorMsg)
  else
  begin
    if FSrcLine < 1 then
      S := Format('Error: %s', [AErrorMsg]) else
      S := Format('Line %d: %s', [FSrcLine, AErrorMsg]);
    raise TLuaException.Create(S);
  end;
end;

procedure TLuaScript.DoBeforeHook;
begin
  if Assigned(FOnBeforeHook) then
    FOnBeforeHook(Self);
end;

procedure TLuaScript.DebugHook(L: Plua_State; ar: Plua_Debug);
var
  MustYield, AtBkpt: Boolean;
begin
  DoBeforeHook;

  MustYield := False;
  case ar^.event of
    LUA_HOOKCALL:
      Inc(FCallDepth);
    LUA_HOOKRET:
      Dec(FCallDepth);
    LUA_HOOKLINE:
      begin
        FSrcLine := ar^.currentline - FLOfs;
        MustYield := FStopRq or FResetRq;
        if
          not MustYield and
          not (ssFreeRun in FScriptStates) and
          (FSrcLine > 0)
        then
        begin
          AtBkpt := False;
          if Assigned(FOnHookLine) then
            FOnHookLine(Self, FSrcLine, AtBkpt);
          if (ssStepOver in FScriptStates) then
            MustYield := (FCallDepth <= FReqDepth) or AtBkpt
          else
            MustYield := (ssStepInto in FScriptStates) or AtBkpt;
        end;
      end;
  end;
  if MustYield and lua_isyieldable(L) then
    lua_yield(L, 0);
end;

procedure TLuaScript.SetSourceText(AValue: String);
begin
  if FSourceText = AValue then Exit;
  FSourceText := AValue;
end;

function TLuaScript.GetVarContents(AId: String): String;

  function EvaLua(L: Plua_State; AExp: String): Integer;
  begin
    luaL_loadstring(L, PChar('return ' + AExp));
    lua_pcall(L, 0, 1, 0);
    Result := lua_type(L, -1);
  end;

begin
  Result := '';
  if not IsRunning then
    Exit;
  EvaLua(FLState, AId);
  try
    Result := LuaVarToString(FLState);
  finally
    lua_pop(FLState, 1);

  end;
end;

function TLuaScript.GetLocalContents(AVarArgs, ATemps: Boolean
  ): String;
var
  ar: lua_Debug;
  I: Integer;
  LName: String;
  PLName: PChar;
  L: Plua_State;

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
          lua_pop(L, 1);
        end;
    end;
  end;

begin
  Result := '';
  if not IsRunning then
    Exit;
  L := FLtState;
  if lua_getstack(L, 0, @ar) <> 1 then
    Exit;
  if AVarArgs then
    L1(-1, True);
  L1(1, ATemps);
end;

function TLuaScript.GetIsRunning: Boolean;
begin
  Result := (ssRunning in FScriptStates);
end;

procedure TLuaScript.SetSourceName(AValue: String);
begin
  if FSourceName = AValue then Exit;
  FSourceName := AValue;
end;

function TLuaScript.GetIsPaused: Boolean;
begin
  Result := (ssPaused in FScriptStates);
end;

function TLuaScript.GetIsFreeRunning: Boolean;
begin
  Result := (ssFreeRun in FScriptStates);
end;

procedure TLuaScript.DoPrint(AMsg: String);
begin
  if Assigned(FOnPrint) then
    FOnPrint(Self, AMsg);
end;

constructor TLuaScript.Create;
begin
  FSourceName := 'noname';
end;

end.

