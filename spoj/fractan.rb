# https://www.spoj.pl/problems/FRACTAN/
#
#

# Find primes up to limit using the classical sieve algorithm.
def primes_erat1(limit)
  bound = Math.sqrt(limit).ceil
  primes = (2..limit).collect { |n| n }
  good = []
  p = 2
  while !p.nil? && p < bound
    good << p
    primes = primes.select { |n| !(n%p).zero?}
    p = primes.first
  end
  good + primes
end

def primes_erat2(limit, sieve = (0..limit).collect{ true })
  bound = Math.sqrt(limit).ceil
  p = 2
  while !p.nil? && p < bound
    q = p*p
    while q <= limit
      sieve[q] = false
      q = q+p
    end
    p = (p+1..limit).find{ |n| sieve[n] }
  end
  (2..limit).select { |n| sieve[n]}
end

def primes_sundaram(limit)
  sieve = (0..limit+1).collect{ true }
#  sieve[0] = false
  # remove all numbers n with: n = i+j +2ij, (n-i)/(1+2i) > j
  # can be found by n >= i + j + 2ij
  d = 2
  loop do
    b = false
    (1..d-1).each { |i|
      j = d - i
      next if j < i
      n = i + j + 2*i*j
     # puts "check #{i}, #{j} => #{n}"
      if n <= limit
        sieve[n] = false
        b = true
      end
    }
    d = d+1
    break if !b
  end
  primes = (2..limit).select { |n| sieve[n] }
  #puts "#{primes}"
  primes = primes.collect{ |n| 1+2*n }.unshift 2
  primes.select { |p| p <= limit }

end

def wheel(limit, seed)
  n = seed.reduce(:*)
  c = (1..n).collect{ |i| [i] }.unshift []
  x = 1
  while c.last.last < limit
    (1..n).each{ |i| c[i] << x*n+i }
    x = x+1
  end
  c[1].shift
  seed.each { |p|
    c[p] = [p]
    (2..n/p).each{ |i| c[i*p] = [] }
  }
  sieve = (0..limit).collect { false }
  c.flatten.each{ |n| sieve[n] = true }
  sieve
end


def solve_line(s)
  nums = s.split.collect{ |x| x.to_i}
  m = nums.shift
  n = nums.shift
  k = nums.shift
  return null if m == 0
  f = nums.each_slice(2).collect{ |x| Rational(x[0], x[1]) }
  # nums.reduce(:gcd) == 1
  r = solve(m, n, f)

  r
end

#main
S1 = "1 21 8 170 39 19 13 13 17 69 95 19 23 1 19 13 7 1 3"
S2 = "20 2 14 17 91 78 85 19 51 23 38 29 33 77 29 95 23 77 19 1 17 11 13 13 11 15 2 1 7 55 1"

def measure(msg="")
  a = Time.now
  yield
  puts "#{msg}: #{Time.now - a}"
end

def benchmark(n)
  measure("erat1") { primes_erat1(n) }
  measure("erat2") { primes_erat2(n) }
  measure("erat3") { primes_erat2(n, wheel(n, [2,3]))}
  measure("erat4") { primes_erat2(n, wheel(n, [2,3,5]))}
  measure("erat5") { primes_erat2(n, wheel(n, [2,3,5,7]))}
  measure("sundaram") { primes_sundaram(n)}
end
