(*My test cases including the provided ones*)

use "hw3provided.sml";

val test1a = only_capitals ["A","B","C"] = ["A","B","C"]
val test1b = only_capitals [] = []
val test1c = only_capitals ["a","b","c"] = []
val test1d = only_capitals ["Alex","mom","Zach"] = ["Alex","Zach"]
val test1e = only_capitals ["a"]= []
val test1f = only_capitals ["Alex"] = ["Alex"]

val test2a = longest_string1 ["A","bc","C"] = "bc"
val test2b = longest_string1 [] = ""
val test2c = longest_string1 ["Alex"] = "Alex"
val test2d = longest_string1 ["Alex","Zach"] = "Alex"

val test3a = longest_string2 ["A","bc","C"] = "bc"
val test3b = longest_string2 [] = ""
val test3c = longest_string2 ["Alex"] = "Alex"
val test3d = longest_string2 ["Alex","Zach"] = "Zach"

val test4a = longest_string3 ["A","bc","C"] = "bc"
val test4b = longest_string3 [] = ""
val test4c = longest_string3 ["Alex"] = "Alex"
val test4d = longest_string3 ["Alex","Zach"] = "Alex"

val test5a = longest_string4 ["A","bc","C"] = "bc"
val test5b = longest_string4 [] = ""
val test5c = longest_string4 ["Alex"] = "Alex"
val test5d = longest_string4 ["Alex","Zach"] = "Zach"

val test6a = longest_capitalized ["A","bc","C"] = "A"
val test6b = longest_capitalized ["alex","zach"] = ""
val test6c = longest_capitalized [] = ""
val test6d = longest_capitalized ["Alex"] = "Alex"
val test6e = longest_capitalized ["Alex","Zach"] = "Alex"

val test7a = rev_string "abc" = "cba"
val test7b = rev_string "a" = "a"
val test7c = rev_string "" = ""

val test8a = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] =
  4;
val test8b = ((first_answer (fn x => if x > 5 then SOME x else NONE)
[1,2,3,4,5];false) handle NoAnswer => true)
val test8c = ((first_answer (fn x => if x > 5 then SOME x else NONE) 
 [];false) handle NoAnswer => true)

val test9a = all_answers (fn x => if x < 4 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test9b = all_answers (fn x => if x > 1 then SOME [x] else NONE)
[2,3,4,5,6,7] = SOME [7,6,5,4,3,2]
val test9c = all_answers (fn x => if x = 1 then SOME [x] else NONE) [] = SOME []

val test10a = count_wildcards Wildcard = 1
val test10b = count_wildcards (ConstP(1)) = 0
val test10c = count_wildcards (TupleP([Wildcard,Variable("x"),ConstP(1)])) = 1
val test10d = count_wildcards (TupleP([Wildcard,Wildcard])) = 2
val test10e = count_wildcards (ConstructorP ("dt", TupleP [Wildcard, Wildcard,
Wildcard])) = 3

 val test11a = count_wild_and_variable_lengths(ConstructorP ("dt", TupleP [Wildcard, Wildcard,Wildcard])) = 3
 val test11b = count_wild_and_variable_lengths (Variable("alex")) = 4
 val test11c = count_wild_and_variable_lengths (ConstP(1)) = 0
 val test11d = count_wild_and_variable_lengths (TupleP
 [Variable("alex"),Wildcard])= 5

val test12a = count_some_var("dt",(ConstructorP ("dt", TupleP
[Variable("dt"),Variable("dt"),Variable("x"),Wildcard]))) = 2
val test12b = count_some_var("x",Variable("y")) = 0
val test12c = count_some_var("x",Wildcard) = 0
val test12d = count_some_var("z", Variable("z")) = 1

val test13a = check_pat (Variable("x")) = true
val test13b = check_pat (ConstructorP ("dt", TupleP [Wildcard,
Wildcard,Wildcard])) = true
val test13c = check_pat (ConstructorP ("dt", TupleP
[Variable("dt"),Variable("dt"),Variable("x"),Wildcard])) = false
val test13d = check_pat (ConstructorP ("dt", TupleP
[Variable("dt"),Variable("x"),Wildcard]))= true

val test14a = match (Unit, UnitP) = SOME []
val test14b = match((Constructor ("dt", Tuple
[Const(1),Unit,Unit])),ConstructorP ("dt", TupleP
[Wildcard,Wildcard,Wildcard])) = SOME []
val test14c =
  match(Tuple[Const(1),Const(2)],TupleP[Variable("x1"),Variable("x2")]) = SOME
  [("x2",Const 2),("x1",Const 1)]
val test14d = match(Tuple[],TupleP[]) = SOME[]

val test15a = first_match Unit [UnitP] = SOME []
val test15b = first_match (Const(1)) [Variable("x"),ConstP(1)] = SOME
[("x",Const 1)]
val test15c = first_match (Const(1)) [] = NONE
val test15d = first_match (Const(1)) [ConstP(1),Variable("x")] = SOME[]
val test15e = first_match (Const(1)) [UnitP] = NONE
