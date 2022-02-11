local js = JsonTools
local null = js.null()

a,e=js.New({zk=1,1,2,3,ak=null,bk=3})
b=js.New('{"nullv":null}')
c=js.New({1,2,3,4,5,null})

function f(t, lvl)
  local ind=''
  for i = 1,lvl do ind = ind .. '   ' end
  for k, v in pairs(t) do
    if type(v) == 'userdata' then
      print(ind, k .. ' ( ')
      f(v, lvl+1)
      print(ind, ' ) ')
    else
      print(ind, k .. ' -> ', v)
    end
  end
end

--f(a,0)
print(js.AsJson(a))
print(js.AsJson(b), b.null)
print(js.AsJson(c))

print( a.ak==nil )



