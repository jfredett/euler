require './gameset'
require '../patches/dice'
require '../patches/common'

module P84
  def self.run
    simulate :name => "2d6", :fn => ->() { 2.d(6) }, :iterations => 10**7
    simulate :name => "2d4", :fn => ->() { 2.d(4) }, :iterations => 10**7
  end

  def self.simulate(opts)
    require 'ruby-prof'


    RubyProf.start
      puts "Running a simulation w/ #{opts[:name]}"
      g = GameSet.new(opts[:fn])
      opts[:iterations].times do |i|
        g.make_move
      end
    result = RubyProf.stop

    i = 0
    g.each_square do |sq|
      puts "Square: #{i}, #{sq.name} has #{sq.probability_of_visit(g) * 100}% chance of visit"
      i += 1
    end
    puts "---------------------------"
    puts "Profiling results"
    puts
    printer = RubyProf::FlatPrinter.new(result)
    printer.print(STDOUT)
    nil
  end
end

