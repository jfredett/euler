THREADS=8
REJECTED=nil


def seive(n)
  $result = []
  ts = []
  ar = (0..n).to_a

  lim = Math.sqrt(n).ceil
  search = (2..lim)

  ar.each_slice(n / THREADS) do |sl|
    ts << Thread.new { seive_section(sl, search) }
  end
  ts.each(&:join)
  $result.flatten.sort
end

def seive_section(ar, sl)
  sl.each do |i|
    ar.reject! { |j| j % i == 0 && j != i }
  end
  $result << ar
end


seive(10**7)
puts "done"
