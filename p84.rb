

$V=1
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
      vputs "drew a card #{card}"
      card
    end

    def draw_and_apply_to(player)
      vputs "player current position #{player.position}"
      draw.apply(player)
      vputs "player position after card #{player.position}"
    end

    def inspect
      self.class.name.to_s
    end
    def to_s
      inspect
    end

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
        [:go, :jail, :c1, :e3, :h2, :r1, :railroad, :railroad, :utility].map { |s| Card.move(s) } +
        [Card.back_three]
    end 
  end

  #cards

  #factory
  class Card
    def self.move(loc)
      StaticMovement.new(loc)
    end
    def self.inactive
      Card.new
    end
    def self.back_three
      StaticMoveback.new
    end

    def apply(player) 
      vputs "applying card to player"
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
      vputs "moving player back three spaces"
      player.move_back(3)
      super
    end
  end

  #####
  # player

  class Player



  end

  class GameSet
    def initialize
      @deck = { :cc => CommunityChest.new , :ch => Chance.new } 
      @player_pos = 0   
      @board = build_board
    end

    def position 
      { :position => @player_pos,
        :square => @board[@player_pos] }
    end
    
    def move_ahead(spaces = 1, opts = {})
      move_player(spaces)
      interpret_move if opts[:interpret]
      mark_space if opts[:mark] 
    end

    def move_to(location)
      vputs "moving player to #{location}"
      move_ahead 1, :mark => false #move to the next location, not the current one.
      until self.is_on? location
        move_ahead 1, :mark => false, :interpret => false
      end
      self
    end

    def is_on?(space)
      vputs "comparing #{space} to #{position[:square]}" unless position[:square].to_s == "P84::Square"
      position[:square].is? space
    end
    
    def make_move
      move_ahead 2.d(6), :mark => true, :interpret => true
    end

    private 

    def mark_space

    end

    def move_player(spaces)
      vputs "moving player ahead #{spaces} spaces"
      @player_pos = (@player_pos + spaces) % @board.size
    end

    def interpret_move
      position[:square].event(self)
      vputs position
    end

    def build_board 
      board = []
      board_spec = [ 
        :go,   nil, :cc,      nil, nil, :r1, nil, :ch, nil,       nil,
        :jail, :c1, :utility, nil, nil, :rr, nil, :cc, nil,       nil,
        :nil,  nil, :ch,      nil, :e3, :rr, nil, nil, :utility,  nil,
        :gtj,  nil, nil,      :cc, nil, :rr, :ch, nil, nil,       nil
      ]
      board_spec.each do |sq|
        vputs "building square for #{sq}"
        case sq  
        when :go, :jail, :c1, :e3, :h2, :rr, :utility
          board << Square.named([sq]) 
        when :r1 
          board << Square.named([:rr, sq])
        when :cc, :ch
          board << Square.draw_from(@deck[sq])
        else
          board << Square.inactive
        end
      end
      #vputs board
      board
    end


    #has_a chancedeck
    #has_a communitydeck

    #can deliver a card from a deck
  end

  class Square
    def self.inactive
      Square.new
    end

    def self.draw_from(deck)
      deck
      #DrawCard.new(deck)
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

    def inspect
      self.class.name.to_s
    end
    alias_method :to_s, :inspect
  end

  #envious of Deck object
  #class DrawCard < Square
    #def initialize(context)
      #@context = context
    #end
    #def event(player)
      #@context.draw_and_apply_to(player) 
      #super
    #end
    #def is?(space)
      #@context.is?(space) || super
    #end
    #def to_s
      #@context.to_s
    #end
  #end

  class Named < Square
    def initialize(names)
      vputs "building named square with #{[names].flatten}"
      @names = [names].flatten
    end
    def is?(space)
      @names.include?(space) || super
    end
    def to_s
      @names.to_s
    end
  end
end


# patches & dice
class Array
  def sum
    sum = 0
    self.each do |i|
      sum += i
    end
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



