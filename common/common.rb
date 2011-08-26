
$V=nil
def vputs(string)
  puts string if ENV['V'] || $V
end
