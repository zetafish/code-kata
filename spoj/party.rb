# http://www.spoj.pl/problems/PARTY
#
# Number : 97
# Code   : Party Schedule
# Limit  : 3 seconds
# Tags   : backtracking

require 'set'

class Party
  attr_reader :fee, :fun
  def initialize(params)
    @fee, @fun = params
  end

  def to_s
    "fun=#{fun}, fee=#{fee}"
  end

end

def read_numbers
  gets.chomp.split.collect{ |x| x.to_i }
end

def main
  loop do
    budget, n = read_numbers
    break if budget.zero? && n.zero?

    p = []
    n.times {
      p << Party.new(read_numbers)
    }
    puts "#{solve(budget, p)}"
    gets
  end
end

def solve(budget, parties, cost = 0, fun = 0, visit = [])
  return [cost, fun, visit] if budget.zero?
  best = [-1, -1]
  parties.each{ |p|
    if p.fee <= budget
      r = solve(budget - p.fee,
                parties.reject{ |x| x == p },
                cost + p.fee,
                fun + p.fun,
                visit + [p])
      if r[1] > best[1]
        best = r
      end
    end
  }
  best
end

def solvex(budget, parties)
  p = parties.sort{ |a,b|
    Rational(a.fun, a.fee) <=> Rational(b.fun, b.fee)}
  puts p
  return [0,0]
end
