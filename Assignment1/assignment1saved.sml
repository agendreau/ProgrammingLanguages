fun is_older(one:int*int*int, two:int*int*int) = 
  if #1 one < #1 two then true
  else 
    if #1 one = #1 two andalso #2 one < #2 two then true
    else 
      if #1 one = #1 two andalso #2 one = #2 two andalso #3 one < #3 two then true
      else false
  
fun number_in_month (dates:(int*int*int) list, month:int)=
  if null dates then 0
  else 
    if #2 (hd dates) = month then 1+number_in_month(tl dates,month)
    else number_in_month(tl dates,month)

fun number_in_months(dates:(int*int*int) list,months:int list)=
  if null months then 0
  else number_in_month(dates,hd months)+number_in_months(dates,tl months)

fun dates_in_month(dates:(int*int*int) list,month:int)=
  if null dates then []
  else
    let val date = hd dates
    in 
      if #2 date = month then date :: dates_in_month(tl dates,month)
      else dates_in_month(tl dates,month)
    end

fun dates_in_months(dates:(int*int*int) list, months:int list)=
  if null months then []
  else dates_in_month(dates,hd months)@dates_in_months(dates,tl months)

fun get_nth(strings:string list,number:int)=
  if number = 1 then hd strings
  else get_nth(tl strings,number-1)

fun date_to_string(date:int*int*int)=
  let val months =
  ["January","February","March","April","May","June","July","August","September","October","November","December"]
  in
    get_nth(months,#2 date)^" "^Int.toString(#3 date)^", "^Int.toString(#1 date)
  end

fun number_before_reaching_sum(sum:int,numbers:int list)=
  if sum<=0 then ~1
  else 1 + number_before_reaching_sum(sum-(hd numbers), tl numbers)

fun what_month(day:int)=
let val day_list = [31,28,31,30,31,30,31,31,30,31,30,32](*32 since the total sum
needs to be greater than 365 for the previous method to work correctly*)
in
  number_before_reaching_sum(day,day_list) + 1
end

fun month_range(day1:int,day2:int)=
  if day1>day2 then []
  else what_month(day1)::month_range(day1+1,day2)

fun oldest(dates:(int*int*int) list)=
  if null dates then NONE
  else 
    let val tl_ans = oldest(tl dates)
    in if isSome tl_ans andalso is_older(valOf tl_ans,hd dates)
       then tl_ans
       else SOME(hd dates)
    end


fun remove_duplicates(int_list:int list) =
  if null int_list then []
  else 
    let
      fun remove_int(int_list1: int list, remove:int) = 
        if null int_list1 then []
        else
          let val head = hd int_list1
          in
            if head = remove then remove_int(tl int_list1,remove)
            else head::remove_int(tl int_list1,remove)
          end
    in
      hd int_list::remove_duplicates(remove_int(int_list,hd int_list))
    end

fun number_in_months_challenge(dates:(int*int*int) list,months:int list)=
  let val no_duplicates = remove_duplicates(months)
  in
    number_in_months(dates,no_duplicates)
  end

fun dates_in_months_challenge(dates:(int*int*int) list, months: int list)=
  dates_in_months(dates,remove_duplicates(months))


fun reasonable_date(date:int*int*int)=
  let fun get_nth_int(numbers:int list,element:int)=
        if element = 1 then hd numbers
        else get_nth_int(tl numbers,element-1)
  in
    if (#1 date) < 1 orelse (#2 date) < 1 orelse (#2 date) > 12 orelse (#3 date) < 1 then false
    else 
      if #1 date mod 400 = 0 orelse (#1 date mod 4 = 0 andalso #1 date mod 100 <> 0) then let val month_length =[31,29,31,30,31,30,31,31,30,31,30,31]
        in
          if (#3 date) <= get_nth_int(month_length,(#2 date)) then true
          else false
        end
      else let val month_length =[31,28,31,30,31,30,31,31,30,31,30,31]
           in
            if (#3 date) <= get_nth_int(month_length,(#2 date)) then true
            else false
           end
  end
