# patches & dice
class Array
  def sum
    sum = self.inject(0) { |x,a| x+a }
    sum
  end
end
