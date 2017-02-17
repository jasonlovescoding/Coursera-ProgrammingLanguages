(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "hw1.sml";

val test1 = is_older ((1,2,3),(2,3,4)) = true
val test1_1 = is_older ((1,2,2),(1,2,3)) = true
val test1_2 = is_older ((1,3,2),(1,2,3)) = false

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1
val test2_1 = number_in_month ([(2012,2,28),(2013,12,1)],3) = 0
val test2_2 = number_in_month ([(2011,5,28),(2015,5,1)],5) = 2

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val test3_1 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[9,8,12]) = 1
val test3_2 = number_in_months ( [], [1] ) = 0
val test3_3 = number_in_months ( [(1,1,1)], [] ) = 0

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test4_1 = dates_in_month([(2012,2,28),(2013,12,1)],1)
val test4_2 = dates_in_month([(2012,2,28),(2013,12,1),(2011,3,31),(2011,2,28)],2) = [(2012,2,28),(2011,2,28)]

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"
val test6_1 = get_nth(["hi"],1) = "hi"

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"
val test7_1 = date_to_string (4, 2, 29) = "February 29, 4"


val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3
val test8_1 = number_before_reaching_sum (151,  [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]) = 4
val test8_2 = number_before_reaching_sum (9,  [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]) = 8

val test9 = what_month 70 = 3
val test9_1 = what_month 273 = 9
val test9_2 = what_month 334 = 11

val test10 = month_range (31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
val test11_1 = oldest([]) = NONE

val test12_1 =  number_in_months_challenge([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],
		[12,12,3,4,2,2,4,2]) = 4
val test12_2 = dates_in_months_challenge([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[12,12,4,2,3,4]) = [(2013,12,1),(2011,4,28),(2012,2,28),(2011,3,31)]
val test12_2_2 = dates_in_months_challenge([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[4,2,3,4]) <> [(2013,12,1),(2011,4,28),(2012,2,28),(2011,3,31)]

val test13_1 = reasonable_date( (2016,2,29) ) = true
val test13_2 = reasonable_date( (2011,3,31) ) = true
val test13_3 = reasonable_date( (1900,2,29) ) = false




