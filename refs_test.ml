(*
                          CS51 Problem Set 6
                       Refs, Streams, and Music
                             Refs Testing
                             Spring 2017
 *)

(* Make your refs solution available for testing *)
open Refs ;;

(* Establish some mutable lists for testing. *)
let list1a = Cons (2, ref Nil) ;;
let list1b = Cons (2, ref list1a) ;;
let list1 = Cons (1, ref list1b) ;;

let reflist = ref (Cons (2, ref Nil)) ;;
let list2 = Cons (1, ref (Cons (2, reflist))) ;;
let _ = reflist := list2 ;;

let rec ones = Cons (1, ref ones) ;;
let list3 = Cons (1, ref (Cons (2, ref ones))) ;;

(* Some example tests. You'll want more. *)
let _ =
  assert(not(has_cycle list1a)) ;
  assert(has_cycle(!reflist)) ;

  assert(mlength list1a = 1) ;
  assert(mlength list1b = 2) ;
  assert(mlength ones = 1) ;
  assert(mlength list3 = 3) ;

  assert(flatten list1a; list1a = Cons (2, ref Nil)) ;
  assert(flatten !reflist; list2 = Cons (1, ref (Cons (2, ref Nil)))) ;
  assert(flatten ones; ones = Cons (1, ref Nil)) ;
  assert(flatten list3; list3 = 
  				Cons (1, ref (Cons (2, ref (Cons (1, ref Nil)))))) ;;
