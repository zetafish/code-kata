require 'thread'
require 'monitor'

# bellard for computing pi
def bellard(n)
  s = Rational(n.odd? ? -1 : 1, 2**(10*n))
  r = 0
  r = r - Rational(1 * 2**5, 4*n+1)
  r = r - Rational(1, 4*n+3)
  r = r + Rational(2**8, 10*n+1)
  r = r - Rational(2**6, 10*n+3)
  r = r - Rational(2**2, 10*n+5)
  r = r - Rational(2**2, 10*n+7)
  r = r + Rational(1, 10*n+9)
  s * r
end

def sum(&fun)
  sum = 0
  n = 0
  loop do
    term = fun.call(n)

  end
end

def bsearch(value, p, q, f)
  # invariant: f(p) <= value < f(q)
  while (p + 1 != q)
    m = p + (q - p)/2
    v = f.call(m)
    if v <= value
      p = m
    else
      q = m
    end
  end
  return p
end

def sqrt(s, digits, iterations)
  x1 = bsearch(s, 0, s, lambda{ |n| n*n}) * 10**digits
  s = s * 10**(2 * digits)
  d = 0
  iterations.times do
    x2 = (x1 + s / x1) / 2
    break if (x2 - x1).abs < 1
    x1 = x2
  end
  #puts d
  x1
end



def arccot(x, unity)
    xpow = unity / x
    n = 1
    sign = 1
    sum = 0
    loop do
        term = xpow / n
        break if term == 0
        sum += sign * (xpow/n)
        xpow /= x*x
        n += 2
        sign = -sign
    end
    sum
end

def spawn_map(actions)
  lock = Monitor.new
  sub = []
  threads = actions.collect { |action|
    Thread.new {
      value = action.call
      lock.synchronize {
        sub.push(value)
      }
    }
  }
  threads.each { |t| t.join }
  sub
end

class MachinLike

  def initialize
    @terms = []
  end

  def term(factor, divisor)
    @terms.push(lambda { |unit| factor * arccot(divisor, unit) })
    self
  end

  def calc_pi_st(digits)
    fudge = 10
    unit = 10 ** (digits + fudge)
    pi = 4 * @terms.collect{ |term| term.call(unit) }.reduce(:+)
    pi / (10 ** fudge)
  end

  def calc_pi_mt(digits)
    fudge = 10
    unit = 10 ** (digits + fudge)
    pi = 4 * spawn_map(@terms.collect { |term| lambda { term.call(unit)} }).reduce(:+)
    pi / (10 ** fudge)
  end
end

# Original formula of John Machin
Machin = MachinLike.new.term(4, 5).term(-1, 239)

# Formula of Kikuo Takano (1982)
Takano = MachinLike.new
  .term(12, 49)
  .term(32, 57)
  .term(-5, 239)
  .term(12, 110443)

# Formula of F.C.W. Stormer (1896)
Stormer = MachinLike.new
  .term(44, 57)
  .term(7, 239)
  .term(-12, 682)
  .term(24, 12943)

# Formula of Hwang Chien-Lih (1997)
ChienLih1 = MachinLike.new
  .term(183, 239)
  .term(32, 1023)
  .term(-68, 5832)
  .term(12, 110443)
  .term(-12, 4841182)
  .term(-100, 6826318)

# Also Hwang Chien-Lih (2003)
ChienLih2 = MachinLike.new
  .term(183, 239)
  .term(32, 1023)
  .term(-68, 5832)
  .term(12, 113021)
  .term(-100, 6826318)
  .term(-12, 33366019650)
  .term(12, 43599522992503626068)


def measure()
  a = Time.now
  yield
  b = Time.now
  b - a
end

def benchmark(digits)
  puts "benchmarking for #{digits} digits"
  values = { }
  machins = [:Machin, :Stormer, :Takano, :ChienLih1, :ChienLih2]
  machins.each { |sym|
    t = measure { values[sym] = Object.const_get(sym).calc_pi(digits) }
    puts "#{sym} => #{t}"
  }

  all_equal = true
  machins.each { |m1|
    machins.each { |m2|
      s1 = values[m1].to_s
      s2 = values[m2].to_s
      eq = s1 == s2
      all_equal = all_equal && eq
      puts "Difference between #{m1} (#{s1}) and #{m2} (#{s2})" if !eq
    }
  }
  puts all_equal ? "All equal" : "Inconsistent"
end

def compare(m1, m2, digits)
  s1 = m1.calc_pi(digits).to_s
  s2 = m2.calc_pi(digits).to_s
  puts s1
  puts s2
  puts "#{s1==s2}"
end

def main
  m = Stormer
  s = m.calc_pi_st(30000).to_s
  puts "#{s[0]}.#{s[1..-1]}"
end

#main
