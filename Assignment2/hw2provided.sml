(*Assignment 2 *)

(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (str : string, sList : string list) = 
  case sList of 
       [] => NONE
     | s::sList' => if same_string(s,str) then SOME(sList')
                    else case all_except_option(str,sList') of
                              NONE => NONE
                            | SOME lst => SOME(s::lst)

fun get_substitutions1(ssList : string list list, str: string) =
  case ssList of
       [] => []
     | ss::ssList' => case all_except_option(str,ss) of
                           NONE => get_substitutions1(ssList',str)
                         |SOME lst => lst @ get_substitutions1(ssList',str)
                         
fun get_substitutions2(ssList : string list list, str : string) = 
  let fun helper_sub(ssList : string list list, str : string, result_list : string
  list) = (* tail recursive helper *)
    case ssList of 
         [] => result_list
       |ss::ssList' => case all_except_option(str,ss) of 
                            NONE => helper_sub(ssList',str,result_list)
                          |SOME lst => helper_sub(ssList',str,result_list@lst)
  in
    helper_sub(ssList,str,[])
  end

fun similar_names(ssList: string list list, name :{first:string,middle:string,last:string}) =
  let val {first=f,middle=m,last=l} = name (* access record in function *)
  in  
    let fun helper(sList : string list, names : {first:string,
    middle:string,last:string} list ) =
      case sList of
           [] => names
         |s::sList' => helper(sList',({first = s, middle = m, last = l}::names))
    in 
      name::helper(get_substitutions2(ssList,f),[]) (*get list of alt names *)
    end
  end
  
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color(c) = 
  case c of (* rank doesn't matter *)
      (Clubs,_) => Black
     |(Diamonds,_) => Red
     |(Hearts,_) => Red
     |(Spades,_) => Black

fun card_value(c) = 
  case c of (* suit doesn't matter *)
       (_,Num i) => i
     |(_,Jack) => 10
     |(_,Queen)=> 10
     |(_,King) => 10
     |(_,Ace) => 11

fun remove_card(cs, c:card, e) = 
  case cs of
       [] => raise e (* card not in card list so raise exception *)
     |cd::cs' => if cd = c then cs' (*assuming only in list once *)
                   else cd::remove_card(cs',c,e)

fun all_same_color(cs) = 
  case cs of 
       [] => true (* no cards vacuously true *)
     |_::[]=>true
     |head::(neck::rest)=>(card_color(head)=card_color(neck) andalso
     all_same_color(neck::rest))

fun sum_cards(cs) =
  let fun sum_helper(cs,acc) = (* tail recursive helper *)
    case cs of 
         [] => acc
       |c::cs'=>sum_helper(cs',card_value(c)+acc)
  in
    sum_helper(cs,0)
  end

fun score(cs: card list, goal: int) = 
  let val sum = sum_cards(cs) 
  in
    let val prelim = (*prelim score*)
      if sum > goal then 3*(sum-goal)
      else goal-sum
    in
      if all_same_color(cs) then prelim div 2
      else prelim
    end
  end

fun officiate(cs: card list, mv: move list, goal: int) = 
 let fun play_game(cs: card list, mv: move list, goal: int, hand: card list)= 
  case mv of 
      [] => score(hand,goal)
    |m::mv'=> case m of 
                   Discard c =>
                   play_game(cs,mv',goal,remove_card(hand,c,IllegalMove))
                 |Draw => case cs of 
                        [] => score(hand,goal)
                      |cd::cs'=> if sum_cards(cd::hand) > goal then score(cd::hand,goal)
                               else play_game(cs',mv',goal,cd::hand)
 in
   play_game(cs,mv,goal,[])
 end

fun numAces(cs: card list) = (* helper function, global since used in both
  score_challenge and officiate_challenge *)  
    case cs of
         [] => 0
       |c::cs' => if card_value(c) = 11 then 1 + numAces(cs')
                      else numAces(cs')

fun possibleSums(cs: card list, aces:int) = (* helper function that determines
  the possible sums using different aces values, global since used in both
  score_challenge and officiate_challenge *)
  let val sum = sum_cards(cs)
  in
    let fun helper(acesLeft, acc) = (* recursive tail helper function *)
      if acesLeft = 0 then sum::acc
      else helper(aces-1,(sum-(aces*11)+aces)::acc)
    in
      helper(aces,[])
    end
  end


fun minList(xs) = (* returns the minimum of a an int list, global since used in
  both score_challenge and officiate_challenge *)
  case xs of
       x::[] =>x
     |x::xs'=>Int.min(x,minList(xs))
                  
fun score_challenge(cs: card list, goal: int) = 
  let fun possiblePrelims(sums: int list, goal: int) = (* possible prelim sums *)
  case sums of
    [] => []
     |sum::sums' => if sum > goal then 3*(sum-goal)::possiblePrelims(sums',goal)
                  else (goal-sum)::possiblePrelims(sums',goal)
  in
    let val min = minList(possiblePrelims(possibleSums(cs,numAces(cs)),goal))
      in
        if all_same_color(cs) then min div 2
        else min
      end
  end

fun officiate_challenge(cs: card list, mv: move list, goal: int) = 
 let fun play_game(cs: card list, mv: move list, goal: int, hand: card list)= 
  case mv of 
      [] => score_challenge(hand,goal)
    |m::mv'=> case m of 
                   Discard c =>
                   play_game(cs,mv',goal,remove_card(hand,c,IllegalMove))
                 |Draw => case cs of 
                        [] => score_challenge(hand,goal)
                      |cd::cs'=> if
                      minList(possibleSums((cd::hand),numAces(cd::hand))) > goal then score(cd::hand,goal)
                               else play_game(cs',mv',goal,cd::hand)
 in
   play_game(cs,mv,goal,[])
 end
