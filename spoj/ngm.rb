# https://www.spoj.pl/problems/NGM/
n = gets.chomp.to_i
m = n % 10
if (m == 0)
  puts 2
else
  puts 1
  puts m
end
