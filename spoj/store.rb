# http://www.spoj.pl/problems/STORE/


require 'set'

Directions = [ :up, :down, :left, :right ]

Stone     = ?s
Keeper    = ?m
Parcel    = ?p
Goal      = ?k
Empty     = ?w

def opposite dir
  { :up => :down,
    :down => :up,
    :left => :right,
    :right => :left }[dir]
end

Point = Struct.new :x, :y do
  def up    ; Point.new x, y-1 ; end
  def down  ; Point.new x, y+1 ; end
  def left  ; Point.new x-1, y ; end
  def right ; Point.new x+1, y ; end

  def dist other
    (x - other.x)**2 + (y - other.y)**2
  end
end

def main
  gets.chomp.to_i.times do |i|
    h = gets.chomp.split.first.to_i
    a = []
    h.times { |j| a << gets.chomp.downcase }
    maze = Maze.new(a)
    puts maze.to_s
  end
end

class Maze
  def initialize(grid)
    @grid = grid
    @height = @grid.length
    @width = @grid.first.length
    @keeper, @parcel, @goal  = [Keeper, Parcel, Goal].collect{ |c| find_point c}
  end

  def find_point c
    each_point { |pt| return pt if self[pt] == c}
  end

  def to_s
    "#{ @keeper }, #{ @parcel }, #{ @goal }, #{ @grid }"
  end

  def [] pt
    @grid[pt.y][pt.x]
  end

  def []= pt, val
    @grid[pt.x][pt.y] = val
  end

  def each_point
    @height.times do |y|
      @width.times do |x|
        yield(Point.new x, y)
      end
    end
  end

  def each_point_around pt
    [:up, :dowm, :left, :right].each do |d|
      q = pt.send(d)
      yield(q) if self===q and self[q]!=Stone and self[q]!=Parcel
    end
  end

  def has_route? keeper, target
    stack = [keeper]
    visit = Set.new << keeper
    while not stack.empty?
      p = stack.pop
      return true if p==target
      each_point_around p do |q|
        stack.push q if visit.add? q
      end
    end
  end

  def === pt
    (0...@width) === pt.x and (0...@height) === pt.y
  end
end

main

# class Maze
#   attr_reader :keeper, :parcel, :goal, :history

#   def initialize maze
#     @history = []
#     @height = maze.length
#     @width = maze[0].length
#     @grid = maze.dup
#     @height.times do |y|
#       @width.times do |x|
#         case @grid[y][x]
#         when :m then @keeper = Point.new x, y
#         when :p then @parcel = Point.new x, y
#         when :k then @goal = Point.new x, y
#         end
#       end
#     end
#   end

#   def [] pt
#     @grid[pt.y][pt.x]
#   end

#   def contains? pt
#     (0...@width) === pt.x and (0...@height) === pt.y
#   end

#   def play dir
#     @history << [keeper, parcel]
#     @keeper = @parcel
#     @parcel = @parcel.send(dir)
#   end

#   def undo
#     @keeper, @parcel = history.pop unless history.empty?
#   end

#   def connected? a, b
#     visit = Set.new << a
#     stack = [a]
#     while not stack.empty?
#       a = stack.pop
#       return true if a.eql? b
#       Directions.each do |dir|
#         c = a.send dir
#         next if not contains? c
#         next if self[c] == :s
#         next if not visit.add? c
#         stack.push c
#       end
#     end
#   end

#   def can_play? dir
#     return false if @parcel == @goal
#     return false if self[ @parcel.send(dir) ] == :s
#     return false if self[ @parcel.send(opposite dir) ] == :s
#     return false if not connected? @keeper, @parcel.send(opposite dir)
#     true
#   end

#   def playable_moves
#     Directions.select{ |dir| can_play? dir }
#   end

#   def snapshot
#     [@keeper.dup, @parcel.dup]
#   end

#   def solve
#     queue = [ playable_moves ]
#     visit = { @parcel => nil }
#     cost = 0
#     best = nil
#     while not queue.empty?

#       p = queue.shift
#       if p == @goal
#         best = cost if not best or cost < best
#         visit[p] = true
#         next
#       end

#       Directions.select{ |dir| can_play? dir}.each do |dir|

#       end
#     end
#   end
# end

# Grid = <<EOF
# SSSSSSSSSSSS
# SwwwwwwwSSSS
# SwSSSSwwSSSS
# SwSSSSwwSKSS
# SwSSSSwwSwSS
# SwwwwwPwwwww
# SSSSSSSwSwSw
# SSSSSSMwSwww
# SSSSSSSSSSSS
# SSSSSSSSSSSS
# EOF

# def parse lines
#   grid = lines.downcase.split.collect do |line|
#     line.split("").collect do |char|
#       char.to_sym
#     end
#   end
#   Maze.new grid
# end
