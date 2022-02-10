unit test_a_json;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua53, JsonTools;


function luaopen_q64936405(L: Plua_State): Integer; cdecl;
function test_a_js(L: Plua_State): Integer; cdecl;
function js_to_s(L: Plua_State): Integer; cdecl;

implementation

const
  MetaClassName = 'TJsonNode';

type
  TJsonUserData = record
    Node: TJsonNode;
    FreeMe: Boolean;
  end;
  PJsonUserData = ^TJsonUserData;

var
  MyNode: TJsonNode;

{$push}{$implicitexceptions off} // Disable implicit exceptions ----------------

function q64936405_len(L: Plua_State): Integer; cdecl;
begin
  // Note: Lua #table returns count just of non-keyed portion of the table
  with PJsonUserData(luaL_checkudata(L, 1, MetaClassName))^ do
    lua_pushinteger(L, Node.Count);
  Exit(1);
end;

function q64936405_gc(L: Plua_State): Integer; cdecl;
var
  PUD: PJsonUserData;
begin
  PUD := PJsonUserData(luaL_checkudata(L, 1, MetaClassName));
  with PUD^ do
    if FreeMe then
      Node.Free;
  Exit(0);
end;

procedure q64936405_push(L: Plua_State; V: TJsonNode);
var
  D: Double;
  S: String;
begin
  if not Assigned(V) then
  begin
    lua_pushnil(L);
    Exit;
  end;
  case V.Kind of
    nkBool:
      lua_pushboolean(L, V.AsBoolean);
    nkNull:
      lua_pushnil(L); // nil? nonexistent?
    nkNumber:
      begin
        D := V.AsNumber;
        if D <> Trunc(D) then // not quite!
          lua_pushnumber(L, D) else
          lua_pushinteger(L, Trunc(D));
      end;
    nkString:
      begin
        S := V.AsString;
        lua_pushstring(L, S);
        //lua_pushlstring(L, PChar(S), Length(S));
      end;
    nkObject, nkArray:
      begin
        // TODO consider making a weak table somewhere to hold these, so
        // retrieving the same subtable multiple times doesn't make duplicate
        // userdata and also make them compare equal like they would in a real table
        with PJsonUserData(lua_newuserdata(L, SizeOf(TJsonUserData)))^ do
        begin
          Node := V;
          FreeMe := False;
        end;
        lua_getfield(L, LUA_REGISTRYINDEX, MetaClassName);
        lua_setmetatable(L, -2);
      end;
  end;

end;

function q64936405_index(L: Plua_State): Integer; cdecl;
var
  K: lua_Integer;
  RawK: PAnsiChar;
begin
  with PJsonUserData(luaL_checkudata(L, 1, MetaClassName))^ do
    if lua_isnumber(L, 2) then
    begin
      K := lua_tointeger(L, 2);
      if (K >= 1) and (K <= Node.Count) then
        q64936405_push(L, Node.Child(Pred(K)))
      else
        lua_pushnil(L);
    end
    else
    begin
      RawK := luaL_checklstring(L, 2, Nil);
      q64936405_push(L, Node.Child(RawK));
    end;
  Exit(1);
end;

function q64936405_newindex(L: Plua_State): Integer; cdecl;
var
  K: lua_Integer;
  RawK: PAnsiChar;

  function JsonNodeJson(N: TJsonNode): String;
  var
    S: String;
  begin
    S := N.AsJson;
    // Fixup for the name at the front
    if Assigned(N.Parent) and (N.Parent.Kind = nkObject) then
      S := system.Copy(S, Pos('":{"', S) + 2);
    Result := S;
  end;

  function IsArray(vix: Integer): Boolean;
  var
    len: size_t;
  begin
    //function isarray(t)
    //  return #t > 0 and next(t, #t) == nil
    //end
    Result := False;
    len := lua_rawlen(L, vix);
    if len > 0 then
    begin
      lua_pushinteger(L, len);
      Result := lua_next(L, vix) = 0;
      if not Result then
        lua_pop(L, 2); // pop k,v
    end;
  end;

  procedure SetNode(ANode: TJsonNode; vix: Integer); forward;

  // Absolute index expected in vix
  procedure SetNodeTbl(ANode: TJsonNode; vix: Integer);
  var
    A: Boolean;
    S: String;
  begin
    A := IsArray(vix);
    if A then
      ANode.AsArray else
      ANode.AsObject;
    lua_pushnil(L);
    while lua_next(L, vix) <> 0 do
      try
        if A then
          SetNode(ANode.Add, -1)
        else
        begin
          lua_pushvalue(L, -2);
          try
            S := lua_tostring(L, -1);
          finally
            lua_pop(L, 1);
          end;
          SetNode(ANode.Force(S), -1);
        end;
      finally
        lua_pop(L, 1);
      end;
  end;

  // Absolute index expected in vix
  procedure SetNodeVal(ANode: TJsonNode; vix: Integer);
  var
    LB: LongBool;
    lI: lua_Integer;
  begin
    if lua_isuserdata(L, vix) then
    begin
      with PJsonUserData(luaL_checkudata(L, vix, MetaClassName))^ do
        ANode.Value := JsonNodeJson(Node);
    end
    else
    begin
      case lua_type(L, vix) of
        LUA_TSTRING:
          ANode.AsString := lua_tostring(L, vix);
        LUA_TNUMBER:
          begin
            lI := lua_tointegerx(L, vix, @LB);
            if LB then
              ANode.AsNumber := lI else
              ANode.AsNumber := lua_tonumber(L, vix);
          end;
        LUA_TNIL:
          ANode.AsNull;
        LUA_TBOOLEAN:
          ANode.AsBoolean := lua_toboolean(L, vix);
        otherwise
          luaL_argerror(L, vix, 'Incompatible assignment');
      end;
    end;
  end;

  procedure SetNode(ANode: TJsonNode; vix: Integer);
  begin
    // Convert to absolute index as expected by SetNodeTbl/Val
    vix := lua_absindex(L, vix);
    if lua_istable(L, vix) then
      SetNodeTbl(ANode, vix)
    else
      SetNodeVal(ANode, vix);
  end;

begin
  with PJsonUserData(luaL_checkudata(L, 1, MetaClassName))^ do
    if lua_isnumber(L, 2) then
    begin
      K := lua_tointeger(L, 2);
      if Node.Kind <> nkArray then
        Node.AsArray;
      luaL_argcheck(L, (K >= 1) and (K <= Node.Count + 1), 2, 'Only in range 1..Count+1');
      if K = Node.Count + 1 then
        SetNode(Node.Add, 3) else
        SetNode(Node.Child(Pred(K)), 3);
    end
    else
    begin
      RawK := luaL_checklstring(L, 2, Nil);
      if Node.Kind <> nkObject then
        Node.AsObject;
      SetNode(Node.Force(RawK), 3);
    end;
  Exit(0);
end;


// Returns:
//   key, value
function q64936405_next(L: Plua_State): Integer; cdecl;
var
  S: String;
  IX: Integer;
begin
  with PJsonUserData(luaL_checkudata(L, 1, MetaClassName))^ do
  begin
    luaL_argcheck(L, Assigned(Node) and (Node.Kind in [nkObject, nkArray]), 1,
      'Not an object or array');
    if lua_isnil(L, 2) then
      IX := 0
    else
    begin
      S := luaL_checkstring(L, 2);
      for IX := 0 to Pred(Node.Count) do
        if Node.Child(IX).Name = S then
          Break;
      Inc(IX);
    end;
    if IX < Node.Count then
    begin
      lua_pushstring(L, Node.Child(IX).Name);
      q64936405_push(L, Node.Child(IX));
      Exit(2);
    end;
  end;
  lua_pushnil(L);
  Exit(1);
end;

// Returns:
//   next, table, nil
function q64936405_pairs(L: Plua_State): Integer; cdecl;
begin
  with PJsonUserData(luaL_checkudata(L, 1, MetaClassName))^ do
    ;
  lua_pushcfunction(L, @q64936405_next);
  lua_pushvalue(L, -2);
  lua_pushnil(L);
  Exit(3);
end;

function luaopen_q64936405(L: Plua_State): Integer; cdecl;
var
  PUD: PJsonUserData;
begin
  PUD := PJsonUserData(lua_newuserdata(L, SizeOf(TJsonUserData)));
  PUD^.Node := MyNode;
  PUD^.FreeMe := False;

  luaL_newmetatable(L, MetaClassName);
  lua_pushcfunction(L, @q64936405_index);
  lua_setfield(L, -2, '__index');
  lua_pushcfunction(L, @q64936405_newindex);
  lua_setfield(L, -2, '__newindex');
  lua_pushcfunction(L, @q64936405_len);
  lua_setfield(L, -2, '__len');
  lua_pushcfunction(L, @q64936405_pairs);
  lua_setfield(L, -2, '__pairs');
  lua_pushcfunction(L, @q64936405_gc);
  lua_setfield(L, -2, '__gc');
  lua_setmetatable(L, -2);
  // TODO write __pairs and __ipairs
  Exit(1);
end;

function test_a_js(L: Plua_State): Integer; cdecl;
var
  PUD: PJsonUserData;
  S: String;
  N: TJsonNode;
begin
  Result := 0;
  S := luaL_checkstring(L, 1);
  N := TJsonNode.Create;
  try
    N.Parse(S);
    PUD := PJsonUserData(lua_newuserdata(L, SizeOf(TJsonUserData)));
    PUD^.Node := N;
    PUD^.FreeMe := True;

    if luaL_newmetatable(L, MetaClassName) = 1 then
    begin
      lua_pushcfunction(L, @q64936405_index);
      lua_setfield(L, -2, '__index');
      lua_pushcfunction(L, @q64936405_newindex);
      lua_setfield(L, -2, '__newindex');
      lua_pushcfunction(L, @q64936405_len);
      lua_setfield(L, -2, '__len');
      lua_pushcfunction(L, @q64936405_pairs);
      lua_setfield(L, -2, '__pairs');
      lua_pushcfunction(L, @q64936405_gc);
      lua_setfield(L, -2, '__gc');
    end;
    lua_setmetatable(L, -2);

    Result := 1;
  except
    on E: Exception do
    begin
      N.Free;
      luaL_error(L, PChar(E.Message));
    end;
  end;
end;

function js_to_s(L: Plua_State): Integer; cdecl;
begin
  with PJsonUserData(luaL_checkudata(L, 1, MetaClassName))^ do
  begin
    lua_pushstring(L, Node.AsJson);
    Exit(1);
  end;
end;

{$pop}// Restore $IMPLICITEXCEPTIONS value -------------------------------------

initialization
  MyNode := TJsonNode.Create;
  MyNode.Parse('{"query":"calc","id":null,"result":{"ref":3,"gid":1,"card":"111111","state":17,"cardst":"subscription","prest":"unknown","crdprst":"out","vehst":"out","cellst":"vacant","preslist":[],"perlist":null,"translist":null,"sublist":[{"card":"111111","from":20220130000000,"to":20220228000000,"cell":"0000-001","cellcat":"Стандартно","floor":0,"inifee":0,"addfee":0,"depst":0,"regno":"CA6421KM","maker":"Алфа Ромео","model":"145","color":"Бежов","owner":0,"pegn":"111111    ","pname":"<няма>","plast":"<няма>","pcity":"<няма>","pstreet":"<няма>","pphone":"<няма>","pmobile":"","degn":"","dname":"","dlast":"","dcity":"","dstreet":"","dphone":"","dmobile":"","addit":""}],"transt":"","subst":"ok","debt":{"context":"FIX0","price":0,"cells":[["От","До","Дни","Часове","Сума"]],"items":null}}}');

finalization
  MyNode.Free;
end.

