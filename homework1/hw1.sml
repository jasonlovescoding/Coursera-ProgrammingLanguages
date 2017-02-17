(* Dan Grossman, Coursera PL, HW1 Code *)
(* Jason ZHANG *)

(* 1. Write a function is_older that takes two dates and evaluates to true or false.
      It evaluates to true if the first argument is a date that comes before the second argument.
      If the two dates are the same, the result is false. *)
fun is_older ( x:int*int*int, y:int*int*int ) =
  if (#1 x) <> (#1 y) (* not the same year *)
  then
      if (#1 x) < (#1 y) then true
      else false
  else
      if (#2 x) <> (#2 y) (* same year, different months *)
      then
          if (#2 x) < (#2 y) then true
          else false
      else  (* same year, same month *)
          if (#3 x) < (#3 y) then true
          else false

(* 2. Write a function number_in_month that takes a list of dates and a month (i.e., an int)
      and returns how many dates in the list are in the given month. *)
fun number_in_month ( xs:(int*int*int) list, y:int ) =
  if null xs then 0
  else
      if #2 (hd xs) = y then 1 + number_in_month(tl xs, y)
      else number_in_month(tl xs, y)

(* 3. Write a function number_in_months that takes a list of dates and a list of months (i.e., an int list)
      and returns the number of dates in the list of dates that are in any of the months in the list of months.
      Assume the list of months has no number repeated. Hint: Use your answer to the previous problem. *)
fun number_in_months ( xs:(int*int*int) list, ys:int list ) =
  if null ys then 0
  else number_in_month( xs, hd ys ) + number_in_months( xs, tl ys )


(* 4. Write a function dates_in_month that takes a list of dates and a month (i.e., an int) and returns a
   list holding the dates from the argument list of dates that are in the month. The returned list should
   contain dates in the order they were originally given. *)
fun dates_in_month ( xs:(int*int*int) list, y:int ) =
  if null xs then []
  else
      if #2 (hd xs) = y then (hd xs) :: dates_in_month( tl xs, y )
      else dates_in_month(tl xs, y)

(* 5. Write a function dates_in_months that takes a list of dates and a list of months (i.e., an int list)
   and returns a list holding the dates from the argument list of dates that are in any of the months in
   the list of months. Assume the list of months has no number repeated. Hint: Use your answer to the
   previous problem and SML’s list-append operator (@). *)
fun dates_in_months ( xs:(int*int*int) list, ys:int list ) =
  if null ys then []
  else dates_in_month(xs, hd ys) @ dates_in_months(xs, tl ys)

(* 6. Write a function get_nth that takes a list of strings and an int n and returns the nth element of the
   list where the head of the list is 1st. Do not worry about the case where the list has too few elements:
   your function may apply hd or tl to the empty list in this case, which is okay. *)
fun get_nth ( xs:string list, y:int ) =
  if y = 1 then hd xs
  else get_nth(tl xs, y-1)

(* 7. Write a function date_to_string that takes a date and returns a string of the form January 20, 2013
   (for example). Use the operator ^ for concatenating strings and the library function Int.toString
   for converting an int to a string. For producing the month part, do not use a bunch of conditionals.
   Instead, use a list holding 12 strings and your answer to the previous problem. For consistency, put a
   comma following the day and use capitalized English month names: January, February, March, April,
   May, June, July, August, September, October, November, December. *)
fun date_to_string (x:int*int*int) =
  let val months = ["January", "February", "March", "April", "May", "June", "July",
                    "August", "September", "October", "November", "December"]
      val month = get_nth(months, #2 x)
      val day = Int.toString(#3 x)
      val year = Int.toString(#1 x)
  in
      month ^ " " ^ day ^ ", " ^ year
  end

(* 8. Write a function number_before_reaching_sum that takes an int called sum, which you can assume
   is positive, and an int list, which you can assume contains all positive numbers, and returns an int.
   You should return an int n such that the first n elements of the list add to less than sum, but the first
   n + 1 elements of the list add to sum or more. Assume the entire list sums to more than the passed in
   value; it is okay for an exception to occur if this is not the case. *)
fun number_before_reaching_sum ( sum:int, y:int list ) =
  if sum - ( hd y ) <= 0 then 0 (* the first n+1 elems add to sum OR MORE *)
  else 1 + number_before_reaching_sum( sum - ( hd y ), tl y )

(* 9. Write a function what_month that takes a day of year (i.e., an int between 1 and 365) and returns
   what month that day is in (1 for January, 2 for February, etc.). Use a list holding 12 integers and your
   answer to the previous problem. *)
fun what_month ( day:int ) =
  let val modays = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] (* day cannot exceed 365, meaning a non-leap year *)
  in
      1 + number_before_reaching_sum(day, modays)
  end

(* 10. Write a function month_range that takes two days of the year day1 and day2 and returns an int list
   [m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ..., and mn is the month
   of day day2. Note the result will have length day2 - day1 + 1 or length 0 if day1>day2. *)
fun month_range ( day1:int, day2:int ) =
  if day1 > day2 then []
  else what_month(day1)::month_range( day1+1,day2 )

(* 11. Write a function oldest that takes a list of dates and evaluates to an (int*int*int) option. It
   evaluates to NONE if the list has no dates and SOME d if the date d is the oldest date in the list. *)
fun oldest (xs:(int*int*int) list) =
  if null xs then NONE
  else let fun oldest_nonempty ( xs:(int*int*int) list ) = (* assumes the input xs is non-empty, which is true here *)
             if null (tl xs) then hd xs
             else let val tl_ans = oldest_nonempty(tl xs)
                  in
                      if is_older( hd xs, tl_ans) then hd xs
                      else tl_ans
                  end
       in
           SOME(oldest_nonempty(xs))
       end

(* 12. Challenge Problem: Write functions number_in_months_challenge and dates_in_months_challenge
   that are like your solutions to problems 3 and 5 except having a month in the second argument multiple
   times has no more effect than having it once. (Hint: Remove duplicates, then use previous work.) *)

(* take a list of dates and a list of months (i.e., an int list) and returns the number of dates
   in the list of dates that are in any of the months in the list of months. *)
fun number_in_months_challenge ( xs:(int*int*int) list, ys:int list) =
  let fun remove_dup_month ( ml:int list ) =
        let val ol = [false, false, false, false, false, false,
                      false, false, false, false, false, false] (* indicate the occurence of 12 months *)
            fun remove_dup_month_l ( ml:int list, ol:bool list ) = (* remove dups according to its occurence list *)
              let fun is_inlist ( n:int, ol:bool list ) = (* return whether the nth month occured *)
                    if n = 1 then hd ol
                    else is_inlist( n-1, tl ol )

                  fun add_inlist ( n:int, ol:bool list) = (* validate the occurence of the nth month *)
                    if n = 1 then true::(tl ol)
                    else (hd ol)::add_inlist( n-1, tl ol )
              in
                  if null ml then []
                  else
                      if  is_inlist( hd ml, ol ) then remove_dup_month_l( tl ml, ol )    (* the head occured before, no adding in *)
                      else (hd ml):: remove_dup_month_l( tl ml, add_inlist( hd ml, ol ) ) (* no occurence before, validate and add in *)
              end
        in
            remove_dup_month_l( ml, ol ) (* pass in the original month list and the occurence list *)
        end
  in
      number_in_months( xs, remove_dup_month( ys ) ) (* pass in the original date list and the reduced month list *)
  end


(* take a list of dates and a list of months (i.e., an int list) and returns a list holding the dates
   from the argument list of dates that are in any of the months in the list of months.  *)
fun dates_in_months_challenge ( xs:(int*int*int) list, ys:int list) =
  let fun remove_dup_month ( ml:int list ) =
        let val ol = [false, false, false, false, false, false,
                      false, false, false, false, false, false] (* indicate the occurence of 12 months *)
            fun remove_dup_month_l ( ml:int list, ol:bool list ) = (* remove dups according to its occurence list *)
              let fun is_inlist ( n:int, ol:bool list ) = (* return whether the nth month occured *)
                    if n = 1 then hd ol
                    else is_inlist( n-1, tl ol )

                  fun add_inlist ( n:int, ol:bool list) = (* validate the occurence of the nth month *)
                    if n = 1 then true::(tl ol)
                    else (hd ol)::add_inlist( n-1, tl ol )
              in
                  if null ml then []
                  else
                      if is_inlist( hd ml, ol ) then remove_dup_month_l( tl ml, ol )    (* the head occured before, no adding in *)
                      else(hd ml):: remove_dup_month_l( tl ml, add_inlist( hd ml, ol ) ) (* no occurence before, validate and add in *)
              end
        in
            remove_dup_month_l( ml, ol ) (* pass in the original month list and the occurence list *)
        end
  in
      dates_in_months( xs, remove_dup_month( ys ) ) (* pass in the original date list and the reduced month list *)
  end

(* 13. Challenge Problem: Write a function reasonable_date that takes a date and determines if it
   describes a real date in the common era. A “real date” has a positive year (year 0 did not exist), a
   month between 1 and 12, and a day appropriate for the month. Solutions should properly handle leap
   years. Leap years are years that are either divisible by 400 or divisible by 4 but not divisible by 100.
   (Do not worry about days possibly lost in the conversion to the Gregorian calendar in the Late 1500s.) *)
fun reasonable_date ( x:int*int*int ) =
  let fun is_real_year ( yr:int ) =
        if yr > 0 then true
        else false

      fun is_real_month ( mo:int ) =
        if mo >= 1 andalso mo <= 12 then true
        else false

      fun is_real_date ( date:int ) =
        if date>=1 andalso date <=31 then true
        else false

      fun is_reasonable_date ( yr:int, mo:int, date:int ) = (* assume year, month and date are real *)
        let
            val nleapyr_days = [31, 28, 31, 30, 31, 30,
                                31, 31, 30, 31, 30, 31]
            val leapyr_days =  [31, 29, 31, 30, 31, 30,
                                31, 31, 30, 31, 30, 31]

            fun is_leap_year ( yr:int ) =
              if ( ( yr mod 4 = 0 ) andalso ( yr mod 100 <> 0 ) ) orelse ( yr mod 400 = 0 )
              then true
              else false

            fun get_monthday ( mo:int, dl:int list ) = (* map the month to #days based on the given list *)
              if mo = 1 then hd dl
              else get_monthday( mo-1, tl dl )
        in
            if is_leap_year( yr )
            then (* leap year case *)
                if ( date<=get_monthday( mo, leapyr_days ) ) then true
                else false
            else (* non-leap year case *)
                if ( date<=get_monthday( mo, nleapyr_days ) ) then true
                else false
        end
  in
      if not ( is_real_year( #1 x ) andalso is_real_month( #2 x ) andalso is_real_date( #3 x ) )
      then false
      else
          if is_reasonable_date( #1 x, #2 x, #3 x ) then true
          else false
  end
