(*
                          CS51 Problem Set 6
                       Refs, Streams, and Music
                 Section 1: Mutable Lists and Cycles
                             Spring 2018
 *)

(* The type of mutable lists. *)
type 'a mlist = Nil | Cons of 'a * 'a mlist ref

(*......................................................................
Problem 1: Write a function has_cycle that returns whether a mutable
list has a cycle. You may want a recursive helper function. Don't
worry about space usage.
......................................................................*)
                                      
let has_cycle lst =
	let rec helper (l : 'a mlist ref list) (r : 'a mlist ref) : bool = 
		match List.exists (fun x -> x == r) l with
		| true -> true
		| false -> match !r with
				   | Cons(_, t') -> helper (r :: l) t'
				   | Nil -> false in
	match lst with
	| Nil -> false
	| Cons (_, t) -> helper [] t ;;

(*......................................................................
Problem 2: Write a function flatten that flattens a list (removes its
cycles if it has any) destructively. Again, you may want a recursive
helper function and you shouldn't worry about space.
......................................................................*)
let flatten lst =
	let rec helper (l : 'a mlist ref list) (r : 'a mlist ref) : unit = 
		match List.exists (fun x -> r == x) l with
		| true -> let h = List.hd l in h := Nil; ()
		| false -> match !r with
				   | Cons(_, t') -> helper (r :: l) t'
				   | Nil -> () in
	match lst with
	| Nil -> ()
	| Cons (_, t) -> helper [] t ;;


(*......................................................................
Problem 3: Write mlength, which nondestructively finds the number of
nodes in a mutable list that may have cycles.
......................................................................*)
let mlength lst =
	let rec helper (l : 'a mlist ref list) (r : 'a mlist ref) : int = 
		match List.exists (fun x -> x == r) l with
		| true -> List.length l
		| false -> match !r with
				   | Cons(_, t') -> helper (r :: l) t'
				   | Nil -> (List.length l) + 1 in
	match lst with
	| Nil -> 0
	| Cons (_, t) -> helper [] t ;;
                         
(*======================================================================
Time estimate

Please give us an honest (if approximate) estimate of how long (in
minutes) this part of the problem set took you to complete (per person
on average, not in total).  We care about your responses and will use
them to help guide us in creating future assignments.
......................................................................*)

let minutes_spent_on_part () : int = 200 ;;
