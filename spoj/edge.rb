class Turtle
  attr_reader :x, :y
  def move_to(x, y)
    puts("#{x} #{y} moveto")
    @x, @y = x, y
  end

  def line_to(x, y)
    puts("#{x} #{y} lineto")
    @x, @y = x, y
  end
end

def simulate(s)
  x, y = 300, 420
  s.each_char { |ch|
    case
    end
  }
end

def main
  begin
    while true
      simulate(gets.chomp)
    end
  rescue
    # nothing
  end
end

main
