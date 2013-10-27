(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
fun only_capitals(sList : string list) =
  List.filter (fn s => Char.isUpper(String.sub(s,0))) sList

fun longest_string1(sList : string list) = 
  List.foldl(fn (s1,s2) => if String.size(s1)>String.size(s2) then s1 else s2) "" sList


fun longest_string2(sList : string list) = 
  List.foldl(fn (s1,s2) => if String.size(s1)>=String.size(s2) then s1 else s2) "" sList

fun longest_string_helper f (sList: string list)  =
  List.foldl(fn (s1,s2) => if f(String.size(s1),String.size(s2)) then s1 else
    s2)  "" sList

val longest_string3 = longest_string_helper (fn(x,y) => x > y)

val longest_string4 = longest_string_helper (fn(x,y) => x>=y)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = implode o rev o explode

fun first_answer f xs = 
  case xs of
       [] => raise NoAnswer
     |x::xs' => case (f x) of 
                     NONE => first_answer f xs'
                   |SOME v => v

fun all_answers f xs =
  let fun helper(f,xs,acc) = 
    case xs of 
         [] => SOME acc
       |x::xs' => case (f x) of
                     NONE => NONE
                     | SOME v => helper(f,xs',v@acc)
  in
    helper(f,xs,[])
  end

val count_wildcards = g (fn ()=> 1) (fn x => 0)

val count_wild_and_variable_lengths = g (fn () => 1) (fn x => String.size(x))

fun count_some_var (s,p) = 
  g (fn () => 0) (fn x => if s = x then 1 else 0) p

fun check_pat p =
  let fun makeStringList p =
    case p of 
         Wildcard => [] 
       |Variable x => [x]
       |TupleP ps => List.foldl(fn (p,i) => (makeStringList p)@i) [] ps
       |ConstructorP(_,p) => makeStringList p
       |_ => []
  in
    let fun noDuplicates sList =
      case sList of
           [] => true
         |s::sList'=>not (List.exists(fn t => s=t ) sList') andalso (noDuplicates
         sList')
      in
        noDuplicates(makeStringList p)
    end
  end

fun match(v: valu,p:pattern) = 
  case (v,p) of 
       (_,Wildcard) => SOME []
     |(_,Variable x) => SOME [(x,v)]
     |(Unit,UnitP) => SOME []
     |(Const i, ConstP j) => if i=j then SOME [] else NONE
     |(Tuple vs,TupleP ps) => all_answers (fn (x,y) => match(x,y)) (ListPair.zip(vs,ps))
     |(Constructor(s1,v),ConstructorP(s2,p)) => if s1=s2 then match(v,p) else
       NONE 
     |(_,_) => NONE

fun first_match v pList = 
  SOME (first_answer (fn p => match(v,p)) pList) handle NoAnswer => NONE 
