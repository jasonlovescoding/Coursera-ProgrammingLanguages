(* Dan Grossman, Coursera PL, HW2 Provided Code *)
(* Jason ZHANG *)

(* Your solutions must use pattern-matching. You may not use the functions null, hd, tl, isSome, or valOf,
   nor may you use anything containing a # character or features not used in class (such as mutation). Note
   that list order does not matter unless specifically stated in the problem *)

(* The provided code defines several types for you. You
   will not need to add any additional datatype bindings or type synonyms. *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* (a) Write a function all_except_option, which takes a string and a string list. Return NONE if the
   string is not in the list, else return SOME lst where lst is identical to the argument list except the string
   is not in it. You may assume the string is in the list at most once. Use same_string, provided to you,
   to compare strings. Sample solution is around 8 lines. *)
fun all_except_option ( str, [] ) = NONE
  | all_except_option ( str, x::xs ) =
    if same_string( str, x ) then SOME xs (* assume str occurs only once *)
    else case all_except_option( str, xs ) of
             NONE => NONE
           | SOME xs' => SOME ( x::xs' )

(* (b) Write a function get_substitutions1, which takes a string list list (a list of list of strings, the substitutions)
   and a string s and returns a string list. The result has all the strings that are in
   some list in substitutions that also has s, but s itself should not be in the result. Example:
   get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred")
   (* answer: ["Fredrick","Freddie","F"] *)
   Assume each list in substitutions has no repeats. The result will have repeats if s and another string are
   both in more than one list in substitutions. Example:
   get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff")
   (* answer: ["Jeffrey","Geoff","Jeffrey"] *)
   Use part (a) and ML’s list-append (@) but no other helper functions. Sample solution is around 6 lines. *)
fun get_substitutions1 ( [], str ) = []
  | get_substitutions1 ( xs::xss, str ) =
    case all_except_option( str, xs ) of
        NONE => get_substitutions1( xss, str )
      | SOME strs' => strs' @ get_substitutions1( xss, str )

(* (c) Write a function get_substitutions2, which is like get_substitutions1 except it uses a tail-recursive
   local helper function. *)
fun get_substitutions2 ( xss, str ) =
  let fun aux ( strs, [] , str ) = strs (* tail end *)
        | aux ( strs, xs::xss, str ) =
          case all_except_option( str, xs ) of
              NONE => aux( strs, xss, str )
            | SOME strs' => aux( strs @ strs', xss, str )
  in aux( [], xss, str )
  end

(* (d) Write a function similar_names, which takes a string list list of substitutions (as in parts (b) and
   (c)) and a full name of type {first:string,middle:string,last:string} and returns a list of full
   names (type {first:string,middle:string,last:string} list). The result is all the full names you
   can produce by substituting for the first name (and only the first name) using substitutions and parts (b)
   or (c). The answer should begin with the original name (then have 0 or more other names). Example:
   similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
   {first="Fred", middle="W", last="Smith"})
   (* answer: [{first="Fred", last="Smith", middle="W"},
   {first="Fredrick", last="Smith", middle="W"},
   {first="Freddie", last="Smith", middle="W"},
   {first="F", last="Smith", middle="W"}] *)
   Do not eliminate duplicates from the answer. Hint: Use a local helper function. Sample solution is
   around 10 lines. *)
fun similar_names ( xss,  { first=f, middle=m, last=l } ) =
  let fun build_fullnames ( [], m, l ) = []
        | build_fullnames ( f::fl, m, l ) = (* take in a list of first names and a middle name and a last name *)
          {first=f, middle=m, last=l}::build_fullnames( fl, m, l ) (* build the corresponding fullname list *)
  in
      { first=f, middle=m, last=l }::build_fullnames( get_substitutions2( xss, f ), m, l ) (* start with the origin *)
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
(* (a) Write a function card_color, which takes a card and returns its color (spades and clubs are black,
   diamonds and hearts are red). Note: One case-expression is enough. *)
fun card_color ( s, _ ) =
  case s of
      Spades => Black
    | Clubs => Black
    | Diamonds => Red
    | Hearts => Red

(* (b) Write a function card_value, which takes a card and returns its value (numbered cards have their
   number as the value, aces are 11, everything else is 10). Note: One case-expression is enough. *)
fun card_value (  _, r  ) =
  case r of
      Num n => n
    | Ace => 11
    | _ => 10

(* (c) Write a function remove_card, which takes a list of cards cs, a card c, and an exception e. It returns a
   list that has all the elements of cs except c. If c is in the list more than once, remove only the first one.
   If c is not in the list, raise the exception e. You can compare cards with =.  *)
fun remove_card ( cs, c, e ) =
    let fun locate_list ( seq, [], _ ) = 0 (* locate the card c in cardlist cs, 0 means not in the list *)
          | locate_list ( seq, c'::cs, c ) =
            if c'=c then seq else locate_list( seq+1, cs, c )

        fun remove_seq ( seq, depth, c::cs ) = (* remove a card according to its sequence location *)
          if seq=depth then cs (* assume the cardlist has at least 1 card *)
          else c::remove_seq( seq, depth+1, cs ) (* assume seq does not overrange cs *)
    in
        case locate_list( 1, cs, c ) of
            0 => raise e
          | seq => remove_seq( seq, 1, cs )
    end
(* no need for that complexity
fun remove_card (cs,c,e) =
    case cs of
	      [] => raise e
      | x::cs' => if x = c then cs' else x :: remove_card(cs',c,e)
*)


(* (d) Write a function all_same_color, which takes a list of cards and returns true if all the cards in the
   list are the same color. Hint: An elegant solution is very similar to one of the functions using nested
   pattern-matching in the lectures. *)
fun all_same_color ( [] ) = true
  | all_same_color ( c::[] ) = true
  | all_same_color ( c1::c2::cs ) =
    if card_color(c1)=card_color(c2)
    then all_same_color( c2::cs )
    else false

(* (e) Write a function sum_cards, which takes a list of cards and returns the sum of their values. Use a locally
   defined helper function that is tail recursive. (Take “calls use a constant amount of stack space” as a
   requirement for this problem.) *)
fun sum_cards ( cs ) =
  let fun aux ( acc, [] ) = acc (* tail end *)
        | aux ( acc, c::cs ) = aux( acc+card_value( c ), cs )
  in aux( 0, cs )
  end

(* (f) Write a function score, which takes a card list (the held-cards) and an int (the goal) and computes
   the score as described above. *)
fun score ( hcs, goal ) =
  let val sum = sum_cards( hcs )
      val p_score = if sum>goal then 3*( sum-goal ) else ( goal-sum )
  in if all_same_color( hcs ) then p_score div 2 else p_score
  end

(* (g) Write a function officiate, which “runs a game.” It takes a card list (the card-list) a move list
   (what the player “does” at each point), and an int (the goal) and returns the score at the end of the
   game after processing (some or all of) the moves in the move list in order. Use a locally defined recursive
   helper function that takes several arguments that together represent the current state of the game. As
   described above:
• The game starts with the held-cards being the empty list.
• The game ends if there are no more moves. (The player chose to stop since the move list is empty.)
• If the player discards some card c, play continues (i.e., make a recursive call) with the held-cards
  not having c and the card-list unchanged. If c is not in the held-cards, raise the IllegalMove
  exception.
• If the player draws and the card-list is (already) empty, the game is over. Else if drawing causes
  the sum of the held-cards to exceed the goal, the game is over (after drawing). Else play continues
  with a larger held-cards and a smaller card-list.
  Sample solution for (g) is under 20 lines. *)
fun officiate ( cs, ms, goal ) =
  let
      fun officiate_by_step ( hcs, _, [], _ ) = score( hcs, goal ) (* empty movelist ends the game *)
        | officiate_by_step ( hcs, cs, (Discard c)::ms, goal ) =  (* Discard changes the held cardlist and then go on *)
          officiate_by_step( remove_card( hcs, c, IllegalMove ), cs, ms, goal )
        | officiate_by_step ( hcs, [], Draw::ms, _ ) = score( hcs, goal ) (* Draw on an empty cardlist ends the game *)
        | officiate_by_step ( hcs, c::cs, Draw::ms, goal  ) = (* Draw changes the held cardlist. Then check the condition *)
          if sum_cards( c::hcs )>goal then score( c::hcs, goal )
          else officiate_by_step( c::hcs, cs, ms, goal )
  in
      officiate_by_step( [], cs, ms, goal )
  end

(* Challenge Problems *)
(* (a) Write score_challenge and officiate_challenge to be like their non-challenge counterparts except
   each ace can have a value of 1 or 11 and score_challenge should always return the least (i.e., best)
   possible score. (Note the game-ends-if-sum-exceeds-goal rule should apply only if there is no sum that
   is less than or equal to the goal.) Hint: This is easier than you might think. *)

fun num_aces ( [] ) = 0
  | num_aces ( ( _, hc_s  )::hcs ) = (* find the number of aces *)
    if hc_s=Ace then 1+num_aces( hcs )
    else num_aces( hcs )
(* Take a card list (the held-cards) and an int (the goal) and computes the score as described above. *)
fun score_challenge ( hcs, goal ) =
  let fun p_score ( hcs, goal, 0 ) = (* if there is no Ace this is the same as before *)
        let val sum=sum_cards(hcs) (* the if-else branches are different if n_ace is not 0 *)
        in if sum>goal then 3*( sum-goal ) else ( goal-sum )
        end
        | p_score ( hcs, goal, n_aces ) = (* n_ace denotes the number of rank-one Aces *)
          let val sum=sum_cards(hcs)-10*n_aces (* Ace ranks one as much as possible to avoid ending *)
          in
              if sum>goal then 3*(sum-goal)
              else Int.min( p_score( hcs, goal, n_aces-1 ), (goal-sum) )
          end

      val p_score_val = p_score( hcs, goal, num_aces(hcs) )
  in
      if all_same_color( hcs ) then p_score_val div 2 else p_score_val
  end

(* Take a card list (the card-list) a move list (what the player “does” at each point),
   and an int (the goal) and returns the score at the end of the game after processing
   (some or all of) the moves in the move list in order. Use a locally defined recursive
   helper function that takes several arguments that together represent the current state of the game.
   As described above:
• The game starts with the held-cards being the empty list.
• The game ends if there are no more moves. (The player chose to stop since the move list is empty.)
• If the player discards some card c, play continues (i.e., make a recursive call) with the held-cards
  not having c and the card-list unchanged. If c is not in the held-cards, raise the IllegalMove
  exception.
• If the player draws and the card-list is (already) empty, the game is over. Else if drawing causes
  the sum of the held-cards to exceed the goal, the game is over (after drawing). Else play continues
  with a larger held-cards and a smaller card-list.
  Sample solution for (g) is under 20 lines. *)
fun officiate_challenge ( cs, ms, goal ) =
  let fun officiate_by_step ( hcs, _, [], _ ) = score_challenge( hcs, goal ) (* empty movelist ends the game *)
        | officiate_by_step ( hcs, cs, (Discard c)::ms, goal ) =  (* Discard changes the held cardlist and then go on *)
          officiate_by_step( remove_card( hcs, c, IllegalMove ), cs, ms, goal )
        | officiate_by_step ( hcs, [], Draw::ms, _ ) = score_challenge( hcs, goal ) (* Draw on an empty cardlist ends the game *)
        | officiate_by_step ( hcs, c::cs, Draw::ms, goal  ) = (* Draw changes the held cardlist. Then check the condition *)
          if sum_cards( c::hcs )-10*num_aces(c::hcs) > goal then score_challenge( c::hcs, goal )
          else officiate_by_step( c::hcs, cs, ms, goal )   (* Ace ranks one as much as possible to avoid ending *)
  in
      officiate_by_step( [], cs, ms, goal )
  end

(* (b) Write careful_player, which takes a card-list and a goal and returns a move-list such that calling
   officiate with the card-list, the goal, and the move-list has this behavior:
• The value of the held cards never exceeds the goal.
• A card is drawn whenever the goal is more than 10 greater than the value of the held cards. As a
  detail, you should (attempt to) draw, even if no cards remain in the card-list.
• If a score of 0 is reached, there must be no more moves.
• If it is possible to discard one card, then draw one card to produce a score of 0, then this must be
  done. Note careful_player will have to look ahead to the next card, which in many card games
  is considered “cheating.” Also note that the previous requirement takes precedence: There must
  be no more moves after a score of 0 is reached even if there is another way to get back to 0.
  Notes:
• There may be more than one result that meets the requirements above. The autograder should
  work for any correct strategy — it checks that the result meets the requirements.
• This problem is not a continuation of problem 3(a). In this problem, all aces have a value of 11. *)
fun careful_player ( cs, goal ) =
    let fun can_reach_goal ( _, [], c, goal ) = NONE (* Discard hc from hcs and draw c. *)
          | can_reach_goal ( hcs1, hc::hcs2, c, goal ) = (* if this gets to the goal return SOME hc. return NONE if not *)
            if sum_cards( hcs1 @ (c::hcs2) )=goal then SOME hc
            else can_reach_goal( hc::hcs1 , hcs2, c, goal )

        fun find_max_card ( c::cs ) = (* find the card with the maximum value. assume the cardlist is non-empty *)
          let fun aux ( [], max_card ) = max_card
          | aux ( c::cs, max_card ) =
            if card_value( c ) > card_value( max_card ) then aux( cs, c )
            else aux( cs, max_card )
          in aux( cs, c )
          end

        fun careful_player_moves ( hcs, [], goal ) = [] (* no more move if the cardlist is empty *)
          | careful_player_moves ( hcs, c::cs, goal ) =
            if score( hcs, goal )=0 then [] (* no more move if the goal is reached *)
            else if sum_cards( hcs )<(goal-10) then Draw::careful_player_moves( c::hcs, cs, goal ) (* this draw is secured *)
            else case can_reach_goal( [], hcs, c, goal ) of
                     SOME hc => [Discard(hc), Draw] (* do one discard and one draw to get to the goal *)
                   | NONE => if sum_cards(c::hcs)<=goal then Draw::careful_player_moves( c::hcs, cs, goal ) (* this draw is cautious *)
                             else let val max_card= find_max_card( hcs )
                                  in (Discard max_card)::careful_player_moves( remove_card( hcs, max_card , IllegalMove ), cs, goal )
                                  end (* else discard the maxcard and go on *)
    in
        careful_player_moves( [], cs, goal )
    end
