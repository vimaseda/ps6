(* 
                         CS 51 Problem Set 6
                       Refs, Streams, and Music
                            Part 3: Music
                             Spring 2018
 *) 

module NLS = NativeLazyStreams ;;

exception InvalidHex ;;
exception InvalidPitch ;;

(*----------------------------------------------------------------------
                    Music data types and conversions
 *)

(* Pitches within an octave *)
type p = A | Bb | B | C | Db | D | Eb | E | F | Gb | G | Ab ;;

(* Pitches with octave *)
type pitch = p * int ;;

(* Musical objects *)              
type obj =
  | (* notes with a pitch, duration (float; 1.0 = a measure), and
       volume ([0...128]) *)
    Note of pitch * float * int
  | (* rests with a duration only *)
    Rest of float ;;

(*......................................................................
The functions below may be useful for quickly creating notes to 
test and play with. *)

(* half -- given a pitch, makes a note that is a half of a
   measure long *)
let half (pt : pitch) : obj = Note (pt, 0.5, 60) ;; 

(* quarter -- given a pitch, makes a note that is a quarter of a
   measure long *)
let quarter (pt : pitch) : obj = Note (pt, 0.25, 60) ;; 

(* eighth -- given a pitch, makes a note that is an eighth of a
   measure long *)
let eighth (pt : pitch) : obj = Note (pt, 0.125, 60) ;;

(* quarter_rest -- a rest that is a quarter of a measure *)
let quarter_rest : obj = Rest 0.25 ;;

(* eighth_rest -- a rest that is an eighth of a measure *)
let eighth_rest : obj = Rest 0.125 ;;
  
(*......................................................................
            Event representation of note and rest sequences
 *)
type event =
  | (* start to play a note after the given time (float) and volume
       (int [0..128]) *)
    Tone of float * pitch * int
  | (* stop playing the note with the given pitch after the given time
       (float) *)
    Stop of float * pitch ;;          

(* p_to_int -- Convert pitches to an integer (half-step) representation *)
let p_to_int (p: p) : int =
  match p with
  | C  -> 0 | Db -> 1 | D  -> 2 | Eb -> 3 | E  ->  4 | F ->  5
  | Gb -> 6 | G  -> 7 | Ab -> 8 | A  -> 9 | Bb -> 10 | B -> 11 ;;

(* int_to_p -- Convert integer half-step to pitch *)
let int_to_p (n: int) : p =
  if (n < 0) || (n > 11) then raise InvalidPitch
  else
    let pitches = [C; Db; D; Eb; E; F; Gb; G; Ab; A; Bb; B] in
    List.nth pitches n ;;

(* time_of_event -- Given an event, returns at what time it occurs *)
let time_of_event (e : event) : float =
  match e with
  | Tone (time, _, _) -> time
  | Stop (time, _) -> time ;;

(* shift -- Shift the time of an event so that it occurs later *)
let shift (by : float) (e : event) : event =
  match e with
  | Tone (time, pit, vol) -> Tone (time +. by, pit, vol)
  | Stop (time, pit) -> Stop (time +. by, pit) ;;

(* shift_start -- Shift the start of a stream of events so that it 
   begins later *)
let shift_start (by : float) (str : event NLS.stream)
              : event NLS.stream =
  let NLS.Cons (e, t) = Lazy.force str in
  lazy (NLS.Cons (shift by e, t)) ;;

(*......................................................................
                         Generating MIDI output
 *)

(* hex_to_int -- Converts a hex number in string representation to an
   int *)
let hex_to_int (hex : string) : int = int_of_string ("0x" ^ hex) ;;

(* int_to_hex -- Converts an int to a hex number in string
   representation *)
let int_to_hex (n : int) : string = Printf.sprintf "%02x" n ;;

(* output_hex -- Output a string on the specified output channel *)
let rec output_hex (outchan : out_channel) (hex : string) : unit =
  let len = String.length hex in
  if len = 0 then () else 
    (if len < 2 then raise InvalidHex
     else (output_byte outchan (hex_to_int (String.sub hex 0 2))); 
     (output_hex outchan (String.sub hex 2 (len - 2)))) ;;

(* some MIDI esoterica *)
let ticks_per_q = 32
let header = "4D546864000000060001000100"
             ^ (int_to_hex ticks_per_q)
             ^ "4D54726B" ;;
let footer = "00FF2F00" ;;

(* pitch_to_hex -- Convert a pitch to a string of its hex
   representation *)
let pitch_to_hex (pitch : pitch) : string =
  let (p, oct) = pitch in int_to_hex ((oct + 1) * 12 + (p_to_int p)) ;;

(* time_to_hex -- Convert an amount of time to a string of its hex 
   representation *)
let time_to_hex (time : float) : string =
  let measure = ticks_per_q * 4 in
  let itime = int_of_float (time *. (float measure)) in
  if itime < measure then (int_to_hex itime)
  else "8" ^ (string_of_int (itime / measure))
       ^ (Printf.sprintf "%02x" (itime mod measure)) ;;

let rec insts (playing : (pitch * int) list) (pitch : pitch) 
            : int * ((pitch * int) list) =
  match playing with
  | [] -> (0, [])
  | (pitch2, n) :: t ->
     if pitch2 = pitch then (n, playing)
     else let (n2, p2) = insts t pitch in
          (n2, (pitch2, n) :: p2) ;;

(* stream_to_hex -- Convert a stream of music to a string hex
   representation, but first n events only *)
let rec stream_to_hex (n : int) (str : event NLS.stream) : string =
  if n = 0 then ""
  else match Lazy.force str with
       | NLS.Cons(Tone (t, pitch, vol), tl) -> 
          (time_to_hex t) ^ "90" ^ (pitch_to_hex pitch)
          ^ (int_to_hex vol) ^ (stream_to_hex (n - 1) tl)
       | NLS.Cons(Stop (t, pitch), tl) ->
          (time_to_hex t) ^ (pitch_to_hex pitch) ^ "00"
          ^ (stream_to_hex (n - 1) tl) ;;
              
(* output_midi -- Writes the string representation of music to a midi
   file *)
let output_midi (filename : string) (hex : string) : unit =
  let outchan = open_out_bin filename in
  output_hex outchan header; 
  output_binary_int outchan ((String.length hex) / 2 + 4); 
  output_hex outchan hex; 
  output_hex outchan footer; 
  flush outchan; 
  close_out outchan ;;

(*----------------------------------------------------------------------
             Conversion to and combination of music streams
 *)
  
(*......................................................................
Write a function list_to_stream that builds a music stream from a finite
list of musical objects. The stream should repeat this music forever.
Hint: Use a recursive helper function as defined, which will call itself
recursively on the list allowing you to keep keep the original list
around as well. Both need to be recursive, since you will call both the
inner and outer functions at some point. See below for some examples.
......................................................................*)
let rec list_to_stream (lst : obj list) : event NLS.stream =
  let rec list_to_stream_rec (nlst : obj list) : event NLS.stream =
    match nlst with
    | Note (p, f, i) :: tl -> 
        lazy (Cons (Tone(0., p, i), lazy (Cons (Stop(f, p), 
                                              list_to_stream_rec tl))))
    | Rest (fl) :: Note (p, f, i) :: tl -> 
        lazy (Cons (Tone (fl, p, i), lazy (Cons (Stop (f, p), 
                                                list_to_stream_rec tl))))
    | Rest (f1) :: Rest (f2) :: tl -> 
        list_to_stream_rec (Rest (f1 +. f2) :: tl)
    | Rest (f) :: [] -> list_to_stream (Rest (f) :: lst)
    | [] -> list_to_stream lst
  in list_to_stream_rec lst ;;

(*......................................................................
Write a function pair that merges two event streams. Events that happen
earlier in time should appear earlier in the merged stream. See below
for some examples.
......................................................................*)
let rec pair (a : event NLS.stream) (b : event NLS.stream)
           : event NLS.stream =
  let h1, h2 = NLS.head a, NLS.head b in
  let get_times x =
    match x with
    | Tone (fl, _, _) -> fl
    | Stop (fl, _) -> fl in
  let f, s = get_times h1, get_times h2 in
  match f < s with
  | true -> lazy (Cons (h1, pair (NLS.tail a) (shift_start (-.f) b)))
  | false -> lazy (Cons (h2, pair (shift_start (-.s) a) (NLS.tail b))) ;;



(*......................................................................
Write a function transpose that takes an event stream and moves each pitch
up by half_steps pitches. Note that half_steps can be negative, but
this case is particularly difficult to reason about so we've implemented
it for you. See below for some examples.
......................................................................*)
let transpose_pitch ((p, oct) : pitch) (half_steps : int) : pitch =
  let newp = (p_to_int p) + half_steps in
    if newp < 0 then
      if newp mod 12 = 0 then (C, oct + (newp / 12))
      else (int_to_p (newp mod 12 + 12), oct - 1 + (newp / 12))
    else (int_to_p (newp mod 12), oct + (newp / 12))

let rec transpose (str : event NLS.stream) (half_steps : int)
            : event NLS.stream =
  let NLS.Cons (e, t) = Lazy.force str in
  match e with
  | Tone (fl, p, i) -> 
          lazy (NLS.Cons (Tone (fl, transpose_pitch p half_steps, i), 
                          transpose t half_steps))
  | Stop (fl, p) -> 
          lazy (NLS.Cons (Stop (fl, transpose_pitch p half_steps), 
                          transpose t half_steps)) ;;

(*----------------------------------------------------------------------
                         Testing music streams
 *)

(* ... UNCOMMENT THIS SECTION ONCE YOU'VE IMPLEMENTED 
                 THE FUNCTIONS ABOVE. ... *)

(*......................................................................
For testing purposes, let's start with a trivial example, useful for
checking list_to_stream, transpose, and pair functions. Start with a
simple melody1: *)

let melody1 = list_to_stream [quarter (C,3);
                              quarter_rest;
                              half (E,3)] ;;

(* This melody, when converted to a stream of start and stop events,
should look something like this:

    # NLS.first 5 melody1 ;;
    - : event list =
    [Tone (0., (C, 3), 60); Stop (0.25, (C, 3)); Tone (0.25, (E, 3), 60);
     Stop (0.5, (E, 3)); Tone (0., (C, 3), 60)]

Now, we transpose it and shift the start forward by a quarter note: *)
  
let melody2 = shift_start 0.25
                          (transpose melody1 7) ;;

(* The result is a stream that begins as

s    # NLS.first 5 melody2 ;;
    - : event list =
    [Tone (0.25, (G, 3), 60); Stop (0.25, (G, 3)); Tone (0.25, (B, 3), 60);
     Stop (0.5, (B, 3)); Tone (0., (G, 3), 60)]

Finally, combine the two as a harmony: *)
  
let harmony = pair melody1 melody2 ;;

(* The result begins like this:

    # NLS.first 10 harmony ;;
    - : event list =
    [Tone (0., (C, 3), 60); Tone (0.25, (G, 3), 60); Stop (0., (C, 3));
     Stop (0.25, (G, 3)); Tone (0., (E, 3), 60); Tone (0.25, (B, 3), 60);
     Stop (0.25, (E, 3)); Tone (0., (C, 3), 60); Stop (0.25, (B, 3));
     Tone (0., (G, 3), 60)]

You can write this out as a midi file and listen to it. *)
                              
let _ = output_midi "temp.mid" (stream_to_hex 16 harmony) ;;
   
   (* <----- END OF SECTION TO UNCOMMENT. *)
   
(*......................................................................
The next example combines some scales. Uncomment these lines when you're
done implementing the functions above. You can listen
to it by opening the file "scale.mid". *)


let scale1 = list_to_stream (List.map quarter
                                      [(C,3); (D,3); (E,3); (F,3); 
                                       (G,3); (A,3); (B,3); (C,4)]) ;;

let scale2 = transpose scale1 7 ;; 

let scales = pair scale1 scale2 ;; 

let _ = output_midi "scale.mid" (stream_to_hex 32 scales) ;; 


(*......................................................................
Then with just three lists provided after this comment and and the
functions we defined, produce (a small part of) a great piece of
music. The piece should be four streams merged: one should be the bass
playing continuously from the beginning. The other three should be the
melody, starting 2, 4, and 6 measures from the beginning, respectively.

Define a stream canon for this piece here using the above component
streams bass and melody. Uncomment the definitions above and the lines
below when you're done. Run the program and open "canon.mid" to hear
the beautiful music. *)
   

let bass = list_to_stream
              (List.map quarter [(D, 3); (A, 2); (B, 2); (Gb, 2); 
                                 (G, 2); (D, 2); (G, 2); (A, 2)]) ;; 

let slow = [(Gb, 4); (E, 4); (D, 4); (Db, 4); 
            (B, 3); (A, 3); (B, 3); (Db, 4);
            (D, 4); (Db, 4); (B, 3); (A, 3);
            (G, 3); (Gb, 3); (G, 3); (E, 3)] ;;

let fast = [(D, 3); (Gb, 3); (A, 3); (G, 3);
            (Gb, 3); (D, 3); (Gb, 3); (E, 3); 
            (D, 3); (B, 2); (D, 3); (A, 3);
            (G, 3); (B, 3); (A, 3); (G, 3)] ;; 

let melody = list_to_stream ((List.map quarter slow)
                             @ (List.map eighth fast));;

let canon = pair (pair bass (shift_start 2. melody)) 
                 (pair (shift_start 4. melody) (shift_start 6. melody)) ;;

let _ = output_midi "canon.mid" (stream_to_hex 176 canon);;


(*......................................................................
Four more streams of music for you to play with. Try overlaying them all
and outputting it as a midi file. You can also make your own music here. *)
(*
let part1 = list_to_stream
              [Rest 0.5;  Note((D, 4), 0.75, 60);  
               Note((E, 4), 0.375, 60); Note((D, 4), 0.125, 60);  
               Note((B, 3), 0.25, 60); Note((Gb, 3), 0.1875, 60);  
               Note((G, 3), 0.0625, 60)];; 
  
let part2 = list_to_stream
              [Note((G, 3), 0.1875, 60); Note((A, 3), 0.0625, 60); 
               Note((B, 3), 0.375, 60); Note((A, 3), 0.1875, 60); 
               Note((B, 3), 0.0625, 60); Note((C, 4), 0.5, 60); 
               Note((B, 3), 0.5, 60)];; 

let part3 = list_to_stream
              [Note((G, 3), 1., 60); Note((G, 3), 0.5, 60); 
               Note((E, 3), 0.1875, 60);
               Note((Gb, 3), 0.0625, 60); Note((G, 3), 0.25, 60); 
               Note((E, 3), 0.25, 60)];;

let part4 = list_to_stream
              [Rest(0.25); Note((G, 3), 0.25, 60); 
               Note((Gb, 3), 0.25, 60); Note((E, 3), 0.375, 60);
               Note((D, 3), 0.125, 60); Note((C, 3), 0.125, 60);
               Note((B, 2), 0.125, 60); Note((A, 2), 0.25, 60);
               Note((E, 3), 0.375, 60); Note((D, 3), 0.125, 60)];;
 *)
                         
(*......................................................................
Time estimate

Please give us an honest (if approximate) estimate of how long (in
minutes) this part of the problem set took you to complete (per person
on average, not in total).  We care about your responses and will use
them to help guide us in creating future assignments.
......................................................................*)

let minutes_spent_on_part () : int = 300 ;;
