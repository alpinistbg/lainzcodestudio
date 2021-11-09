
local function istable(t)
    return type(t) == "table"
end
local function isnumber(t)
    return type(t) == "number"
end
local function isstring(t)
    return type(t) == "string"
end

function str2secs(s)
    local r = 0
    for n in s:gmatch("([1-9]%d-)s") do
        r = r + tonumber(n)
    end
    for n in s:gmatch("([1-9]%d-)m") do
        r = r + tonumber(n) * 60
    end
    for n in s:gmatch("([1-9]%d-)h") do
        r = r + tonumber(n) * 3600
    end
    for n in s:gmatch("([1-9]%d-)d") do
        r = r + tonumber(n) * 3600 * 24
    end
    for n in s:gmatch("([1-9]%d-)w") do
        r = r + tonumber(n) * 3600 * 24 * 7
    end
    return r
end

-- sort table by given key, desc or asc
function sorta(t, key, descending)
    table.sort(
        t,
        function(l, r)
            local ls = str2secs(l[key])
            local rs = str2secs(r[key])
            --return descending and (ls > rs) or (ls < rs)
            return (ls > rs)
        end
    )
    return t
end

-- whether interval [x1, y1] overlaps with [x2, y2]
function overlap(x1, x2, y1, y2)
  return (x1 < y2) and (y1 < x2)
end

-- intersect interval [from, to] with an array of intervals
function intersection(from, to, intervals)
  local list = {}
  for _,v in ipairs(intervals) do
    if overlap(from, to, v[1], v[2]) then
      table.insert(list, {math.max(from, v[1]), math.min(to, v[2])})
    end
  end
  return list
end

function first_instant(intervals)
  local r = 0;
  if #intervals then
    r = intervals[1][1]
    for _,v in ipairs(intervals) do
      r = math.min(r, v[1])
    end
  end
  return r
end

function last_instant(intervals)
  local r = 0;
  if #intervals then
    r = intervals[1][2]
    for _,v in ipairs(intervals) do
      r = math.max(r, v[2])
    end
  end
  return r
end

function start_of_day(tm)
  local t = os.date("*t", tm)
  t.hour, t.min, t.sec = 0, 0, 0
  return os.time(t)
end

function start_of_week(tm)
  local t = os.date("*t", tm)
  --1 2 3 4 5 6 7
  --6 0 1 2 3 4 5
  t.day, t.hour, t.min, t.sec = t.day - (t.wday + 5) % 7, 0, 0, 0
  return os.time(t)
end

-- cyc - 1 for a daily split, !=1 for weekly
function extract_intervals(cyc, from, to, intervals)
  local per, start = 0, 0

  if not intervals or (#intervals < 1) then
    return intervals
  end

  if cyc == 1 then -- daily
    per = 60 * 60 * 24
  else -- weekly
    per = 60 * 60 * 24 * 7
  end

  -- adjust for complementary interval (when from > to)
  if from > to then
    from = from - per
  end

  -- check interval too big
  if (to - from) > per then
    return intervals
  end

  local intfro, intto = first_instant(intervals), last_instant(intervals)
  local t = {}

  if cyc == 1 then -- daily
    start = start_of_day(intfro)
  else -- weekly
    start = start_of_week(intfro)
  end

  while start < intto + per do
    local dlfro = math.max(start + from, intfro)
    local dlto = math.min(start + to, intto)

    if dlfro < dlto then
      local sect = intersection(dlfro, dlto, intervals)
      for _,v in pairs(sect) do table.insert(t, v) end
    end

    start = start + per
  end
  return t
end


-- from
-- to
-- tbl - calculator table (prices)
-- results - collects the results
function calctab(from, to, tbl, results)
    local dur = math.abs(to - from)
    local st = 0
    local p0, p1 = 0.0, 0.0
    local t = {table.unpack(tbl)}
    local desc = ''

    sorta(t, "start", true); -- sort by .start in descending order
    for _, value in ipairs(t) do
        st = value.start and str2secs(value.start) or 0
        if dur >= st then
            p0 = value.price and value.price or 0.0
            if value.each then
                local each = str2secs(value.each)
                local add = value.add and value.add or 0.0
                if (each > 0) then
                    p1 = math.ceil((dur - st) / each) * add
                end
            end
            if value.desc then desc = value.desc
            else desc = "start='" .. value.start .. "'" end
            break --return p
        end
    end

    table.insert(results, {from=from, to=to, price=p0+p1, base=p0, add=p1, desc=desc})
    return p0 + p1
end

-- from
-- to
-- t - calculator table
-- params - parameters for comparing
-- results - collects the results
function calcul(from, to, t, params, results)
    local p = 0.0 --t[1] and t[1] or 0.0 -- first element or 0.0
    local flag = true

    -- if it actually a price table ...
    -- {{start="...", ...
    if t[1] and t[1].start then
      p = calctab(from, to, t, results)
      return p
    end

    -- for all elements without keys
    for _, value in ipairs(t) do
        local intervals = {}
        local splitfrom, splitto = 0, 0
        flag = true

        -- test conditionals
        if flag and t.level then
          flag = (t.level == params.level)
        end

        -- slice periods
        if t.daily then
          intervals = extract_intervals(1, str2secs(t.from), str2secs(t.to), {from, to})
        elseif t.weekly then
          intervals = extract_intervals(2, str2secs(t.from), str2secs(t.to), {from, to})
        end

        -- remove the last period if less than tail
        if t.tail then
          if (intervals[#intervals][2] - intervals[#intervals][1]) < str2secs(t.tail) then
            intervals[#intervals] = nil
          end
        end

        if flag then
          if #intervals < 1 then
            table.insert(intervals, {from, to})
          end
          for _,interval in ipairs(intervals) do
            p = p + calcul(interval[1], interval[2], value, params, results)
          end
        end

        if not t.sum and flag then
          break
        end
    end
    return p
end

day1 = {
    {start = "0h", price = 1.0, desc = '(Д)00:00-01:00'},
    {start = "1h", price = 2.0, desc = '(Д)01:00-02:00'},
    {start = "2h", price = 3.0, each = "1h", add = 1.0, desc = '(Д)>02:00'}
}

night1 = {
    {start = "0h", price = 0.5, desc = '(Н)00:00-01:00'},
    {start = "1h", price = 1.0, each = "1h", add = 0.3, desc = '(Н)>01:00'}
}

d1 = os.date("*t") -- in table
d2 = os.date("!*t") -- in table, coord. universal time
t1 = os.time(d1) -- d1 epoch
t2 = os.time(d2) -- d2 epoch
t3 = os.time() -- now epoch

from, to = t3, t3 + str2secs("1h15m")

params1 = {level = 1}

--d = calcul(from, to, try1, params1)
--print(d)
--print(os.date("%Y-%m-%d %X", (t1)))
--print(os.date("%Y-%m-%d %X", start_of_day(t1)))

entry_exit = {
  {  os.time{year=2021, month=10, day=8, hour=12, min = 43},
     os.time{year=2021, month=10, day=15, hour=17, min = 31}}
  }

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

clock0 = os.clock()

zzz1=extract_intervals(1, str2secs("9h"), str2secs("17h"), entry_exit)
zzz2=extract_intervals(1, str2secs("17h"), str2secs("9h"), entry_exit)

--for _,v in ipairs(zzz1) do
--  print(os.date("%Y-%m-%d %X", v[1]), os.date("%Y-%m-%d %X", v[2]))
--end

for i = 1,1000 do
  results1 = {}
  ppp = calcul(entry_exit[1], entry_exit[2], c99, {}, results1)
end
clock0 = os.clock() - clock0

-- for _,v in ipairs(results1) do
--   print(
--     os.date("%Y-%m-%d %X", v.from),'-',os.date("%Y-%m-%d %X", v.to),
--     '     price=', v.price, '   ', v.base,'+', v.add, '   ', v.desc
--     )
-- end

print('price=', ppp)

print(os.date("%Y-%m-%d %X", start_of_week(entry_exit[1][1])))
print(os.date("%Y-%m-%d %X", start_of_week(entry_exit[1][2])))

dt=os.time{year=2021, month=10, day=8, hour=12, min = 43}

for i = 1,25 do
  print(os.date("%Y-%m-%d %X", dt), os.date("%Y-%m-%d %X", start_of_week(dt)))
  dt = dt + 60*60*24
end

print(clock0)







