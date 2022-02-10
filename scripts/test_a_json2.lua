ttt=test_a_js('{}')

--ttt.a={{1},{2}}

c99 = {
  {sum = true,

    {daily = true, from = "9h", to = "17h", tail = "30m", --day1
      { -- table for the day stay
        {start = "0h", price = 1.0, desc = '(Д)00:00-01:00'},
        {start = "1h", price = 2.0, desc = '(Д)01:00-02:00'},
        {start = "2h", price = 3.0, each = "1h", add = 1.0, desc = '(Д)>02:00'}
      }
    },

    {daily = true, from = "17h", to = "9h", tail = "30m", --night1
      { -- table for the night stay
        {start = "0h", price = 0.5, desc = '(Н)00:00-01:00'},
        {start = "1h", price = 1.0, each = "1h", add = 0.3, desc = '(Н)>01:00'}
      }
    }
  }
}

ttt[1] = c99

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

--xcyx(ttt,0)
jsjs=     js_to_s(ttt)
print(jsjs)
