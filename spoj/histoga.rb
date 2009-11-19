# https://www.spoj.pl/problems/HISTOGRA/

def read_histo
  gets.chomp.split.collect{ |x| x.to_i }
end

def get_mins(h)
  m = []
  lowest = nil
  h.each { |x|
    lowest = x if lowest.nil? or x < lowest
    m.push(lowest)
  }
  m
end

def solve_histo(h)
  m = get_mins(h)
  puts 0
end

def main
  while true
    h = read_histo
    break if h.shift == 0
    solve_histo(h)
  end
end

main
