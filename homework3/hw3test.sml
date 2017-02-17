(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "hw3.sml";

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test1_1 = only_capitals ["Aa", "bB", "cc", "DD"] = ["Aa","DD"]

val test2 = longest_string1 ["A","bc","C"] = "bc"
val test2_2 = longest_string1 ["A", "bc", "de", "F"] = "bc"

val test3 = longest_string2 ["A","bc","C"] = "bc"
val test3_2 = longest_string2 ["A", "bc", "de", "F"] = "de"

val test4a = longest_string3 ["A","bc","C"] = "bc"
val test4a_2 = longest_string3 ["A","bc","cd","C"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"
val test4b_2 = longest_string4 ["A","bc","cd","C"]  = "cd"

val test5 = longest_capitalized ["A","bc","C"] = "A"
val test5_1 = longest_capitalized ["Aa", "bBb", "cc", "DD"]  = "Aa"

val test6 = rev_string "abc" = "cba"
val test6_2 = rev_string "Aabc" = "cbaA"
val test6_3 = rev_string "A" = "A"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
                                                                                    
val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8_2 = all_answers (fn x => SOME[x+1] ) [3,2,1,3] = SOME [4,3,2,4]
val test8_3 = all_answers (fn () => SOME [1] ) [] = SOME []

val test9a = count_wildcards Wildcard = 1
val test9a_2 = count_wildcards (TupleP([Wildcard,Variable("x"),Wildcard])) = 2

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b_2 = count_wild_and_variable_lengths (TupleP([Wildcard,Variable("xs"),Wildcard])) = 4
val test9b_3 = count_wild_and_variable_lengths (TupleP([Wildcard,Variable("xs"),Wildcard,ConstructorP("xs",Variable("ys"))])) = 6

val test9c = count_some_var ("x", Variable("x")) = 1
val test9c_2 = count_some_var ( "xs",TupleP([Wildcard,Variable("xs"),Wildcard,ConstructorP("xs",Variable("xs"))]) ) = 2

val test10 = check_pat (Variable("x")) = true
val test10_2 = check_pat (TupleP([Variable("x"), ConstructorP("x",Variable("t"))])) = true
val test10_3 = check_pat (TupleP([Variable("x"), ConstructorP("t",Variable("x"))])) = false

val test11 = match (Const(1), UnitP) = NONE
val test11_2 = match (Const(1), ConstP(1)) = SOME []

val test12 = first_match Unit [UnitP] = SOME []

val test13 = typecheck_patterns ( [],[Wildcard, ConstP 17,ConstP 4] )
val test13_2 = typecheck_patterns ( [("foo","bar",IntT)], [ConstructorP("foo",ConstP(1)),ConstructorP("foo",ConstP(2)) ] )

