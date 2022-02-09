unit test_a_json;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua53, JsonTools;


function luaopen_q64936405(L: Plua_State): Integer; cdecl;

implementation

type
  PJsonNode = ^TJsonNode;

var
  MyNode: TJsonNode;

function q64936405_len(L: Plua_State): Integer; cdecl;
var
  N: TJsonNode;
begin
  N := PJsonNode(luaL_checkudata(L, 1, 'q64936405'))^;
  lua_pushinteger(L, N.Count);
  Exit(1);
end;

procedure q64936405_push(L: Plua_State; V: TJsonNode);
var
  D: Double;
  S: String;
  N: PJsonNode;
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
        N := PJsonNode(lua_newuserdata(L, SizeOf(TJsonNode)));
        N^ := V;
        lua_getfield(L, LUA_REGISTRYINDEX, 'q64936405');
        lua_setmetatable(L, -2);
      end;
  end;

end;

function q64936405_index(L: Plua_State): Integer; cdecl;
var
  N, N1: TJsonNode;
  K: lua_Integer;
  RawK: PAnsiChar;
begin
  N := PJsonNode(luaL_checkudata(L, 1, 'q64936405'))^;
  if lua_isnumber(L, 2) then
  begin
    K := lua_tointeger(L, 2);
    if (K >= 1) and (K <= N.Count) then
      q64936405_push(L, N.Child(Pred(K)))
    else
      lua_pushnil(L);
  end
  else
  begin
    RawK := luaL_checklstring(L, 2, Nil);
    q64936405_push(L, N.Child(RawK));
  end;
  Exit(1);
end;

function luaopen_q64936405(L: Plua_State): Integer; cdecl;
var
  N: PJsonNode;
begin
  N := PJsonNode(lua_newuserdata(L, SizeOf(TJsonNode)));
  N^ := MyNode;
  luaL_newmetatable(L, 'q64936405');
  lua_pushcfunction(L, @q64936405_index);
  lua_setfield(L, -2, '__index');
  lua_pushcfunction(L, @q64936405_len);
  lua_setfield(L, -2, '__len');
  lua_setmetatable(L, -2);
  // TODO write __pairs and __ipairs
  Exit(1);
end;

initialization
  MyNode := TJsonNode.Create;
  MyNode.Parse('{"query":"calc","id":"","result":{"ref":3,"gid":1,"card":"111111","state":17,"cardst":"subscription","prest":"unknown","crdprst":"out","vehst":"out","cellst":"vacant","preslist":[],"perlist":null,"translist":null,"sublist":[{"card":"111111","from":20220130000000,"to":20220228000000,"cell":"0000-001","cellcat":"Стандартно","floor":0,"inifee":0,"addfee":0,"depst":0,"regno":"CA6421KM","maker":"Алфа Ромео","model":"145","color":"Бежов","owner":0,"pegn":"111111    ","pname":"<няма>","plast":"<няма>","pcity":"<няма>","pstreet":"<няма>","pphone":"<няма>","pmobile":"","degn":"","dname":"","dlast":"","dcity":"","dstreet":"","dphone":"","dmobile":"","addit":""}],"transt":"","subst":"ok","debt":{"context":"FIX0","price":0,"cells":[["От","До","Дни","Часове","Сума"]],"items":null}}}');

finalization
  MyNode.Free;
end.

