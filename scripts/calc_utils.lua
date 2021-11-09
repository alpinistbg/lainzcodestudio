
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

