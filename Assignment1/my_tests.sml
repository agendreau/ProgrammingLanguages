(* My test file *)

use "assignment1.sml";

val test1_1 = is_older((1989,10,24),(1991,9,25)) = true
val test1_2 = is_older((1989,10,24),(1989,10,31)) = true
val test1_3 = is_older((1989,10,24),(1989,10,24)) = false
val test1_4 = is_older((1991,9,25),(1989,10,24)) = false
val test1_5 = is_older((1989,10,31),(1989,10,24)) = false

val test2_1 = number_in_month([(2012,2,28),(2013,12,1)],3) = 0
val test2_2 = number_in_month([(2012,2,28),(2012,2,20),(2012,3,4)],2)=2

val test3_1 = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[1,5,6]) = 0
val test3_2 = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,5,6]) = 1
val test3_3 = number_in_months([(2012,2,28),(2013,2,1),(2011,3,31),(2011,3,28)],[2,3,4]) = 4

val test4_1 = dates_in_month([(2012,2,28),(2013,12,1)],3) = []
val test4_2 = dates_in_month([(2012,2,28),(2013,2,1)],2) =
  [(2012,2,28),(2013,2,1)]

val test5_1 = dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(
2011,4,28)],[1,5,6]) = []
val test5_2 = dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(
2011,4,28)],[2,5,6]) = [(2012,2,28)]
val test5_3 = dates_in_months([(2012,2,28),(2013,2,1),(2011,3,31),(2011,3,28)],[2,3,4]) = [(2012,2,28),(2013,2,1),(2011,3,31),(2011,3,28)]

val test6_1 = get_nth(["hi", "there", "how", "are", "you"], 1) = "hi"
val test6_2 = get_nth(["hi", "there", "how", "are", "you"], 5) = "you"
val test7_1 = date_to_string((2013,1,1)) = "January 1, 2013"
val test7_2 = date_to_string((2013,12,31)) = "December 31, 2013"

val test8_1 = number_before_reaching_sum(1,[1,2,3,4,5]) = 0
val test8_2 = number_before_reaching_sum(2,[1,1,23]) = 1

val test9_1 = what_month(1) = 1
val test9_2 = what_month(31) = 1
val test9_3 = what_month(59) = 2
val test9_4 = what_month(365) = 12

val test10_1 = month_range(31,31) = [1]
val test10_2 = month_range(34,31) = []

val test11_1 = oldest([]) = NONE
val test11_2 = oldest([(2012,2,28)]) = SOME (2012,2,28)

val test12_1 = number_in_months_challenge([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28 )],[2,2,3,2,4,3,4,4]) = 3
val test12_2 = number_in_months_challenge([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28 )],[2,3,4]) = 3
val test12_3 =
  dates_in_months_challenge([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,2,3,2,4,3,4,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]
val test12_4 =
  dates_in_months_challenge([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test13_1 = reasonable_date((0,10,24)) = false
val test13_2 = reasonable_date((~1,10,24)) = false
val test13_3 = reasonable_date((1990,13,24)) = false
val test13_4 = reasonable_date((1990,12,32)) = false
val test13_5 = reasonable_date((2000,2,29)) = true
val test13_6 = reasonable_date((1900,2,29)) = false
val test13_7 = reasonable_date((1996,2,29)) = true
val test13_8 = reasonable_date((1989,10,24)) = true



