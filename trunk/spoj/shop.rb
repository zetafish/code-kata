# http://www.spoj.pl/problems/SHOP/
#
# Number : 96
# Code   : SHOPPING
# Limit  : 3 seconds
# Tags   : graph shortest-path dijkstra

require 'set'

$rows = 0
$cols = 0
$start = nil
$dest = nil
$grid = nil

def main
  loop do
    read_input
    break if $cols.zero?
    puts solve
  end
end

def read_input
  s = gets.chomp.split
  s = gets.chomp.split if s.empty?
  $cols, $rows = s.collect{ |x| x.to_i }
  $grid = Array.new($cols * $rows)
  $rows.times{ |row|
    s = gets.chomp
    s.size.times { |col|
      p = row*$cols + col
      ch = $grid[p] = s[col]
      $start = p if ch == 'S'
      $dest = p if ch == 'D'
    }
  }
end

def around(p)
  c = p % $cols
  r = p / $cols
  yield p-1 if c > 0 && $grid[p-1] != 'X'
  yield p+1 if c+1 < $cols && $grid[p+1] != 'X'
  yield p-$cols if r > 0 && $grid[p-$cols] != 'X'
  yield p+$cols if r+1 < $rows && $grid[p+$cols] != 'X'
end

def solve()
  todo = $grid.size
  visit = Array.new($grid.size, false)
  cost = Array.new($grid.size, $grid.size*10)
  cost[$start] = 0
  while todo.nonzero?
    #puts "#{cost}"
    #puts "#{visit}"

    unv = (0...$grid.size).select{ |i| !visit[i]}
    m = unv.collect{ |i| cost[i]}.min
    a = unv.find{ |i| !visit[i] && cost[i] == m }
    around(a) { |b|
      #puts "a=#{a}, b=#{b}, #{cost[a]}, #{$grid[b].to_i}"
      if !visit[b]
        c = cost[a] + $grid[b].to_i
        cost[b] = c if cost[b] > c
      end
    }
    visit[a] = true
    todo = todo - 1
  end
  cost[$dest]
end

main
