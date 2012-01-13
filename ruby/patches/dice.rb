class Fixnum 
  def d(sides)
    result = Die.new(sides).roll(self)
    { :roll => result.sum, :doubles => result.count == 2 && result.first == result.last } 
  end

end

class Die
  def initialize(sides)
    @sides = (1..sides).to_a
  end

  def roll(number = 1)
    result = []
    number.times { result << @sides.sample }
    result
  end
end


