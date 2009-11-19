OPEN  = "({["
CLOSE = ")}]"
MATCH = {
  ")" => "(",
  "]" => "[",
  "}" => "{"
}

def count(s, pos = 0, b = { "(" => 0, "[" => 0, "{" => 0})
  return 0 if b.values.any? { |v| v < 0}
  return b.values.all? { |v| v == 0} ? 1 end : 0 if pos == s.length

  ch = s[pos]
  if OPEN.index ch
    b[ch] = b[ch] + 1
    return count(s, pos+1, b)
  elif CLOSE.index ch
    b[ch] = b[ch] - 1
    return count(s, pos+1, b)
  else
    dup = b.dup
    total = 0
    OPEN.each_char { |x|
      save = dup[x]
      dup[x] = save + 1
      total = total + count(s, pos + 1, dup)
      dup[x] = save
    }
    CLOSE.each_char { |x|
      y = MATCH[x]
      save = dup[y]
      dup[y] = save - 1
      total = total + count(s, pos + 1, dup)
      dup[y] = save
    }
    return total
  end
end

def main
  n = gets.chomp.to_i
  s = gets.chomp
  puts(count(s))
end

