require './p84/cards'

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
end
