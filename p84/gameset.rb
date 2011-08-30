require './p84/card'
require './p84/deck'
require './p84/square'

module P84 
  class GameSet
    def initialize(block)
      @deck = { :cc => CommunityChest.new , :ch => Chance.new } 
      @player_pos = 0   
      @board = build_board
      @total_visits = 0
      @die_roll = block
      @doubles = 0
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
      move_ahead 1, :mark => false, :interpret => false #move to the next location, not the current one.
      until self.is_on? location
        move_ahead 1, :mark => false, :interpret => false
      end
      self
    end

    def is_on?(space)
      position[:square].is? space
    end

    def each_square
      @board.each do |sq|
        yield sq
      end
    end

    def make_move
      roll = @die_roll.call
      @doubles += 1 if roll[:doubles]
      if @doubles == 3
        move_to(:jail)
        @doubles = 0
      else
        move_ahead roll[:roll], :mark => true, :interpret => true, 
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
end
