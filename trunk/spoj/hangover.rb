# https://www.spoj.pl/problems/HANGOVER/

def main
  while true
    r = gets.chomp.to_f
    break if r == 0
    puts("#{search(r)} card(s)")
  end
end

def hang(n)
  n.times.collect{ |x| 1.0/(x+2)}.reduce(:+).to_f
end

def search(target, p=0, q=280)
  return q if p+1 == q
  m = p + (q - p) /2
  v = hang(m)
  if (target < v)
    return search(target, p, m)
  else
    return search(target, m, q)
  end
end

main
