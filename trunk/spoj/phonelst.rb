# Stripped down version of Trie.
#
# Only insert a string if it data structure remains prefix free
class Trie
  attr_accessor :root, :consistent
  def initialize
    @root = nil
    @consistent = true
  end

  def add(key)
    @root = add_recursive(@root, key, 0) if @consistent
  end

  def consistent?
    @consistent
  end

  class Node
    attr_accessor :left, :mid, :right, :char, :last
    def initialize(char)
      @char = char
      @left = @mid = @right = nil
      @last = false
    end

    def last?
      @last
    end
  end

  def add_recursive(node, string, index)
    char = string[index]
    was_last = !node.nil? && node.last?
    was_nil = node.nil?
    node = Node.new(char) if node.nil?
    if (char < node.char)
      node.left = add_recursive(node.left, string, index)
    elsif (char > node.char)
      node.right = add_recursive(node.right, string, index)
    elsif (index < string.length - 1)
      @consistent = !was_last
      node.mid = add_recursive(node.mid, string, index+1) if @consistent
    else
      @consistent = was_nil
      node.last = true if @consistent
    end
    node
  end

end

def main
  gets.chomp.to_i.times {
    t = Trie.new
    gets.chomp.to_i.times {
      #t.add(gets.chomp)
      s = gets
      t.add(s)
    }
    puts (t.consistent? ? "YES" : "NO")
  }
end

main
