# https://www.spoj.pl/problems/FRACTAN/
#
# Correct but too slow

def sieve(limit)
  bound = Math.sqrt(limit).ceil
  sieve = (0..limit+1).collect{ true }
  p = 2
  while !p.nil? && p <= bound
    q = p*p
    while q <= limit
      sieve[q] = false
      q = q + p
    end
    p =(p+1 .. limit).find{ |n| sieve[n] }
  end
  (2..limit).select{ |n| sieve[n]}
end

Primes = sieve(100)

def factorize(number, primes = Primes)
  f = []
  primes.each_with_index{ |p, i|
    while (number % p).zero?
      number = number / p
      f << i
    end
  }
  f
end

def exponents(number, primes = Primes)
  exp = primes.length.times.collect { 0 }
  factorize(number).each{ |i| exp[i] = exp[i] + 1}
  exp
end


# Parse a line with a problem
def parse(s)
  nums = s.split.collect{ |x| x.to_i}
  m = nums.shift
  #puts ">#{m} #{m.class}"
  return 0 if m.zero?
  n = exponents(nums.shift)
  k = nums.shift
  f = nums.each_slice(2).collect{ |x| x.collect{ |y| factorize(y) } }
  [m, n, f]
end

def pow2?(f)
  f[1..-1].all?{ |x| x.zero? }
end

# multiple a with fraction, return nil if result not integer
def fracmul(a, frac)
  c = a.dup
  frac[1].each { |i|
    return nil if c[i] == 0
    c[i] = c[i] - 1
  }
  frac[0].each { |i| c[i] = c[i] + 1}
  c
end

def solve(m, n, f)
  r = []
  while m.nonzero?

    if pow2?(n)
      #puts "#{n}"
      r << n[0]
      m = m - 1
    end
    if m.nonzero?
      z = nil
      y = f.find { |f| z = fracmul(n, f) }
      n = z
    end
  end
  r
end

def main
  loop do
    m, n ,f = parse(gets)
    break if m.zero?
    r = solve(m, n, f)
    puts "#{r}"
  end
end

main
S0 = "3 4 32 5"
S1 = "1 21 8 170 39 19 13 13 17 69 95 19 23 1 19 13 7 1 3"
S2 = "20 2 14 17 91 78 85 19 51 23 38 29 33 77 29 95 23 77 19 1 17 11 13 13 11 15 2 1 7 55 1"

