local q64936405 = require('q64936405')
print(q64936405[1])
print(q64936405.query)
--print(q64936405.result)
--print(q64936405.result.ref)
--print(q64936405.result.sublist[1].from)

ttt=test_a_js('{}')

ttt.blah=1

print(ttt.blah)

ttt.blah=3.14

print(ttt.blah)

ttt.blah='blahh'

print(ttt.blah)

ttt.blah = q64936405

function xcyx(t, lvl)
  local ind=''
  for i = 1,lvl do ind = ind .. '   ' end
  for k, v in pairs(t) do
    if type(v) == 'userdata' then
      print(ind, k .. ' ( ')
      xcyx(v, lvl+1)
      print(ind, ' ) ')
    else
      print(ind, k .. ' -> ', v)
    end
  end
end

xcyx(ttt,0)

--ttt=test_a_js('{}')

--print(#q64936405)
--print(#{}, #{1,2,3}, #{one=1,two=2}, #{one=1,two=2, 3})

--q64936405.ooo = 'abc'


