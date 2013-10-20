
use "hw2provided.sml";

val test1_1 = all_except_option("string", ["string"]) = SOME []
val test1_2 = all_except_option("string",["string","1","2","3"]) = SOME ["1","2","3"]
val test1_3 = all_except_option("1",["0","2","1","3","4"]) = SOME ["0","2","3","4"]
val test1_4 = all_except_option("1",["2","3","4","1"]) = SOME ["2","3","4"]
val test1_5 = all_except_option("1",["2","3","4"]) = NONE

val test2_1 = get_substitutions1([["foo"],["there"]], "foo") = []
val test2_2 = get_substitutions1([["foo","f","fo"],["foo","fue"]],"foo") =
  ["f","fo","fue"]
val test2_3 = get_substitutions1([["bar","car"],["lar"]],"foo") = []
val test2_4 = get_substitutions1([[]],"foo") = []

val test3_1 = get_substitutions2([["foo"],["there"]], "foo") = []
val test3_2 = get_substitutions2([["foo","f","fo"],["foo","fue"]],"foo") =
  ["f","fo","fue"]
val test3_3 = get_substitutions2([["bar","car"],["lar"]],"foo") = []
val test3_4 = get_substitutions2([[]],"foo") = []

val test4_1 = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"})
val test4_2 = similar_names([[]],{first="Alex",middle="NA",last = "G"}) =
  [{first="Alex",middle="NA",last = "G"}]

val test5_1 = card_color((Clubs, Num 2)) = Black
val test5_2 = card_color((Diamonds,Ace)) = Red
val test5_3 = card_color((Hearts,Queen)) = Red
val test5_4 = card_color((Spades,King)) = Black

val test6_1 = card_value((Clubs, Num 2)) = 2
val test6_2 = card_value((Diamonds,Queen)) = 10
val test6_3 = card_value((Hearts,Jack)) = 10
val test6_4 = card_value((Spades,King)) = 10
val test6_5 = card_value((Clubs,Ace)) = 11

val test7_1 = remove_card([(Hearts, Ace)], (Hearts, Ace),IllegalMove) = []
val test7_2 = ((remove_card([], (Clubs, Num 2), IllegalMove); false) handle IllegalMove =>
true)
val test7_3 = remove_card([(Hearts,Ace),(Clubs, Num
2),(Diamonds,Queen)],(Clubs,Num 2),IllegalMove) = [(Hearts,Ace),(Diamonds,Queen)]
val test7_4 = ((remove_card([(Hearts,Ace),(Clubs, Num 2)], (Diamonds,
Queen),IllegalMove); false) handle IllegalMove => true)

val test8_1 = all_same_color([(Hearts, Ace), (Diamonds, Ace)]) = true
val test8_2 = all_same_color([]) = true
val test8_3 = all_same_color([(Clubs,Queen),(Diamonds,Ace),(Hearts, Num 5)]) =
  false
val test8_4 = all_same_color([(Clubs,King)]) = true

val test9_1 = sum_cards([(Clubs, Ace),(Clubs, Num 2)]) = 13
val test9_2 = sum_cards([]) = 0
val test9_3 = sum_cards([(Clubs,Num 5)]) = 5
val test9_4 = sum_cards([(Clubs,Queen),(Diamonds, Num 6),(Hearts, Ace),(Spades,
Num 3)])=30

val test10_1 = score([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val test10_2 = score([(Hearts,Num 2),(Diamonds,Queen)],10) = 3
val test10_3 = score([],10) = 5

val test11_1 = officiate([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6
val test11_2 =
  officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],[Draw,Draw,Draw,Draw,Draw],42)= 3
val test11_3 = ((officiate([(Clubs,Jack),(Spades,Num(8))],[Draw,Discard(Hearts,Jack)],42); false) handle IllegalMove => true)
