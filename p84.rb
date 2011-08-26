

$V=nil
def vputs(string)
  puts string if ENV['V'] || $V
end

module P84 
  #decks of cards
  class Deck
    def initialize
      @spent_deck = []
      @deck = []
      initialize_deck
      @deck.shuffle!
    end

    def draw
      @deck = @spent_deck.shuffle if @deck.empty?
      puts "empty" if @deck.empty?
      card = @deck.pop
      @spent_deck << card
      card
    end

    def draw_and_apply_to(player)
      draw.apply(player)
    end

    def inspect
      self.class.name.to_s.split('::').last
    end
    alias_method :name, :inspect


  end

  class CommunityChest < Deck
    def is?(space)
      space == :cc
    end
    protected
    def initialize_deck
      @deck = [Card.inactive] * 14 + 
        [Card.move(:go), Card.move(:jail)]
    end
  end

  class Chance < Deck
    def is?(space)
      space == :ch
    end
    protected
    def initialize_deck
      @deck = [Card.inactive] * 6  + 
        [:go, :jail, :c1, :e3, :h2, :r1, :rr, :rr, :utility].map { |s| Card.move(s) } +
        [Card.back_three]
    end 
  end

  #cards

  #factory
  class Card
    def self.move(loc)
      MoveAhead.new(loc)
    end
    def self.inactive
      Card.new
    end
    def self.back_three
      StaticMoveback.new
    end

    def apply(player) 
      player
    end

    def inspect
      self.class.name.to_s
    end
    def to_s
      inspect
    end
  end

  class MoveAhead < Card
    def initialize(goal)
      @goal = goal
    end

    def apply(player)
      player.move_to(@goal)
      super
    end

  end

  class StaticMoveback < Card
    def apply(player)
      player.move_ahead -3, :mark => false, :interpret => true
      super
    end
  end

  #####
  # player

  class Player



  end

  class GameSet
    def initialize(block)
      @deck = { :cc => CommunityChest.new , :ch => Chance.new } 
      @player_pos = 0   
      @board = build_board
      @total_visits = 0
      @die_roll = block
    end

    def total_visits
      @total_visits
    end

    def position 
      { :position => @player_pos,
        :square => @board[@player_pos] }
    end
    
    def move_ahead(spaces = 1, opts = {})
      mark_space if opts[:mark] 
      move_player(spaces)
      interpret_move if opts[:interpret]
    end

    def move_to(location)
      move_ahead 1, :mark => false #move to the next location, not the current one.
      until self.is_on? location
        move_ahead 1, :mark => false, :interpret => false
      end
      self
    end

    def is_on?(space)
      position[:square].is? space
    end
    
    def make_move
      move_ahead @die_roll.call, :mark => true, :interpret => true
    end

    def each_square
      @board.each do |sq|
        yield sq
      end
    end

    private 

    def mark_space
      @total_visits += 1
      position[:square].mark
    end

    def move_player(spaces)
      @player_pos = (@player_pos + spaces) % @board.size
    end

    def interpret_move
      position[:square].event(self)
    end

    def build_board 
      board = []
      board_spec = [ 
        :go,   nil, :cc,      nil, nil, :r1, nil, :ch, nil,       nil,
        :jail, :c1, :utility, nil, nil, :rr, nil, :cc, nil,       nil,
        :nil,  nil, :ch,      nil, :e3, :rr, nil, nil, :utility,  nil,
        :gtj,  nil, nil,      :cc, nil, :rr, :ch, nil, nil,       :h2
      ]
      board_spec.each do |sq|
        case sq  
        when :go, :jail, :c1, :e3, :h2, :rr, :utility
          board << Square.named([sq]) 
        when :r1 
          board << Square.named([:rr, sq])
        when :cc, :ch
          board << Square.draw_from(@deck[sq])
        when :gtj
          board << Square.go_to_jail
        else
          board << Square.inactive
        end
      end
      board
    end
  end

  class Square

    def self.inactive
      Square.new
    end

    def self.draw_from(deck)
      DrawCard.new(deck)
    end

    def self.go_to_jail
      GoToJail.new
    end

    def self.named(names)
      Named.new(names)
    end

    def event(player)
      player
    end

    def is?(space) 
      false
    end

    def mark
      @times_visited ||= 0
      @times_visited += 1
    end
  
    def visited? 
      !!@times_visited
    end

    def probability_of_visit(gameset)
      if visited?
        (@times_visited * 1.0) / gameset.total_visits
      else
        0
      end
    end

    def inspect
      "#{self.class.name.split('::').last} : #{@times_visited || 0}"
    end
    alias_method :to_s, :inspect

    def name 
      "Inactive Square"
    end
  end

  class GoToJail < Square
    def event(player)
      player.move_to(:jail)
      super
    end
    def name 
      "Go To Jail"
    end
  end

  #this is a bit envious of Deck+Subobjects, but we need it for it's parent's 
  #"mark" functionality, so it provides a bit of a proxy
  class DrawCard < Square
    def initialize(context)
      @context = context
    end

    def event(player)
      @context.draw_and_apply_to(player) 
      super
    end

    def is?(space)
      @context.is?(space) || super
    end

    def to_s
      @context.to_s
    end

    def name
      "Draw from #{@context.name}"
    end
  end

  class Named < Square
    def initialize(names)
      @names = [names].flatten
    end
    def is?(space)
      @names.include?(space) || super
    end
    def to_s
      @names.to_s
    end
    alias_method :name, :to_s
  end



  def self.run
    simulate :name => "2d6", :fn => ->() { 2.d(6) }, :iterations => 10**7
    simulate :name => "2d4", :fn => ->() { 2.d(4) }, :iterations => 10**7
  end

  def self.simulate(opts)
    puts "Running a simulation w/ #{opts[:name]}"
    g = GameSet.new(opts[:fn])
    opts[:iterations].times do |i|
      vputs "steps: #{i}" if i % 100000 == 0
      g.make_move
    end
    i = 0
    g.each_square do |sq|
      i += 1
      puts "Square: #{i}, #{sq.name} has #{sq.probability_of_visit(g) * 100}% chance of visit"
    end
    puts "---------------------------"
  end
end


# patches & dice
class Array
  def sum
    sum = self.inject(0) { |x,a| x+a }
    sum
  end
end

class Fixnum 
  def d(sides)
    Die.new(sides).roll(self).sum
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


