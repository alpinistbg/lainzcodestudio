fn = {
  first = function(a, b) return a end,
  second = function(a, b) return b end,
  add = function(a, b) return a + b end,
  sub = function(a, b) return a - b end
}

print(fn.first(1, 2))
print(fn.add(fn.first(1, 2), fn.second(1, 2)))
print(fn.sub(fn.first(10, 20), fn.second(20, 11)))
