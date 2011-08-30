module P84
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
    alias_method :to_s, :inspect
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
end
