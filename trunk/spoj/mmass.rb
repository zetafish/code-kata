def mmass(s)
  stack = [0]
  s = "(#{s})"
  s.each_char { |ch|
    case ch
    when "H" then stack.push(1)
    when "C" then stack.push(12)
    when "O" then stack.push(16)
    when "2".."9" then stack.push(ch.to_i * stack.pop)
    when "(" then stack.push('(')
    when ")"
      begin
        n = 0
        until stack.last == '(' do
          n = n + stack.pop
        end
        stack.pop
        stack.push n
      end
    end
  }
  stack.pop
end


puts(mmass(gets.chomp))

