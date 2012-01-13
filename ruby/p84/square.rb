module P84  
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
      (@times_visited * 1.0) / gameset.total_visits 
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
end


