
require "./hw7.rb"

ZERO = 0.0
ONE = 1.0
TWO = 2.0
THREE = 3.0
FOUR = 4.0
FIVE = 5.0
SIX = 6.0
SEVEN = 7.0
TEN = 10.0

s =  Line.new(5.0,0.0).intersect(LineSegment.new(1.0,5.0,2.0,2.0))
puts s
if not(s.is_a? Point and s.x == ONE and s.y==FIVE)
  puts "failed"
  puts s.is_a? Point
  puts s.is_a? NoPoints
end
