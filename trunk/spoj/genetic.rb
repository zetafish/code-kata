# http://www.spoj.pl/problems/GENETIC/

THUE = {
  0 => [0, 1, 2, 0, 1],
  1 => [0, 2, 0, 1, 2, 1],
  2 => [0, 2, 1, 2, 0, 2, 1]
}

TOCHAR = {
  0 => ?N,
  1 => ?P,
  2 => ?O,
}

GENOME = <<EOF
NPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNONPOPNOPONOPNPONPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPOPNPONPNOPONOPNONPOPNOPONOPNPONPNOPONOPNONPOPNPONPNOPONOPNPONPNONPOPNOPONOPNONPO
EOF

def rewrite(seq, sigma)
  seq.collect{ |x| sigma[x] }.reduce(:+)
end

def genome(n)
  seq = [0]
  n.times { seq = rewrite(seq, THUE) }
  seq.collect { |x| TOCHAR[x]  }.reduce(:+)
end


def main
  g = GENOME
  while (n = gets.chomp.to_i) != 0
    puts(g[0,n])
  end
end

main
