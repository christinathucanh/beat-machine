;Project: Beat Machine
;Preliminaries: A staple of electronic music is the drum machine. A drum machine allows the user to create patterns of drum beats from a set of sound samples imitating real-world percussive instruments such as a snare drum or a cymbal. They can be found all over modern popular music, e.g., dance pop and electronic music from the 80s and 90s: Phil Collin’s In the Air Tonight uses a Roland CR-78.Michael Jackson’s Man in The Mirror uses a Linn 9000, Aphex Twin’s Heliosphan uses a Roland TR-909.

;In this project, I will use the fundamental programming skills to build a library of functions for constructing and manipulating drum beats. These functions will then explore some classic rhythmic patterns not just from western music, but also other traditions!
; Author: Anh Thuc (Christina) Vu 
; Date: 2022-12-7
; ACKNOWLEDGEMENTS:...

(import music) 
; m: MIDI note? 
; dur: duration? 
; Returns the accent note with louder volume 
(define accent
  (lambda (m dur)
    (mod (dynamics 70) (note m qn ) )
    ))
(accent 61 qn)

; m: MIDI note? 
; dur: duration? 
; Returns the ghost note with lower volume. 

(define ghost
  (lambda (m dur)
    (mod (dynamics 40) (note m qn ) )
    ))
(ghost 59 qn)

; m: MIDI note? 
; dur: duration? 
; Returns the sequence of notes that take an accented note, two ghost notes, and a regular note, in sequence. 
"strokes"
(define strokes
  (seq (accent 38 qn)
    (ghost 38 qn)
    (ghost 38 qn)
    (note 38 qn))) 
strokes 
(mod percussion strokes) 

; m: MIDI note? 
; n: number?
; dur: duration? 
; Returns a sequence of evenly spaced notes of the specified value that fit into dur.
"tremolo"
(define tremolo
  (lambda (n m d)
    (repeat (expt 2 n) (note m (dur (numerator d)(* (denominator d) (expt 2 n)))))
    )) 
(mod percussion (tremolo 2 38 qn)) 
; m: MIDI note? 
; n: number? 
; dur: duration? 
; Returns the collection of notes that evenly spaced notes that fit the duration of the original note.
"roll"
(define roll 
  (lambda (m n d)
    (mod percussion (note m (dur (numerator d)(* (denominator d)(/ 4)))))))
(mod percussion (roll 2 38 qn))
; m: MIDI note? 
; n: number? 
; dur: duration? 
; Returns a grace note of half the duration played before the given note and also played with an accent.
"flam"
(define flam 
  (lambda ( m d)
    (mod percussion (pickup (note m (dur (numerator d)(* (denominator d)(/ 2)))) (accent m d)))))
(flam 64 qn) 
; m: MIDI note? 
; Returns a single drag tap which (in its eighth note version) is a pair of sixteenth note grace notes, followed by a regular eighth note, and then finally an accented eighth note.
(define single-drag-tap
  (lambda (m)
    (mod percussion 
      (seq (repeat 2 (note m sn)) (note m en) (mod (dynamics 70) (note m en))))))
(single-drag-tap 38)
; PART 2 
; play steady eighth notes.
"hi-hat"
(define hi-hat 
  (mod percussion (repeat 4 (note 42 qn))))
hi-hat
; play steady ghost notes, except beats 2 and 4 are accented notes.
"snare"
(define snare 
  (mod percussion (repeat 2 (seq (rest qn) (note 38 qn))))) 
; play two eighth notes, rests for an eighth note, plays an eighth note, and then rests until the last possible eighth note of the measure and plays it. 
snare 
"bass-drum"
(define bass-drum 
  (mod percussion (repeat 2 (seq (note 35 qn) (rest qn)))))
bass-drum
; play hi-hat and bass drum play at the same time 
"pulse-1"
(define pulse-1 
  (mod percussion (par (note 42 qn) (note 35 qn)))) 
pulse-1
; play hi-hat and snare play at the same time 
"pulse-2"
(define pulse-2 
  (mod percussion (par (note 42 qn) (note 38 qn)))) 
pulse-2
; play hi-hat and bass drum play at the same time 
"pulse-3"
(define pulse-3
  (mod percussion (par (note 42 qn) (note 35 qn)))) 
pulse-3
; the hi-hat and snare play at the same time
"pulse-4"
(define pulse-4 
  (mod percussion (par (note 42 qn) (note 38 qn)))) 
pulse-4
; play hi-hat and snare and bass-drum play at the same time 
(define horizontal-simple-rock-beat
  (mod percussion (par hi-hat snare bass-drum))) 

horizontal-simple-rock-beat
; play pulse-1,2,3,4 at the same time
(define vertical-simple-rock-beat
  (mod percussion (seq pulse-1 pulse-2 pulse-1 pulse-2))) 

;beat-machine produces beats consisting of a snare drum, hi-hat, and bass drum.
; Decomposition: I used an anonymous function that takes n as the number of times 
; the composition is repeated, and also have 3 compositions: snare drum, hi-hat, and bass drum
; played at the same time using par.  
(define beat-machine 
  (lambda (n com1 com2 com3)
    (repeat n (par com1 com2 com3))))

; repeat n times composition 1,2,3 
(define simple-rock-beat 
  (repeat 2 (seq hi-hat snare bass-drum))) 

; Hi-hat plays steady eighth notes.
; Snare plays steady ghost notes, except beats 2 and 4 are accented notes.
; Bass drums plays two eighth notes, rests for an eighth note, plays an eighth note, and then rests until the last possible eighth note of the measure and plays it. Note that we typically denote the eighth notes between beats as the “ands” of the preceding beat, so a more concise way to describe the bass drum pattern is: “play one-‘and’, the ‘and’ of two, and the ‘and’ of four.
(define elaborate-rock-beat
  (mod percussion (beat-machine 2
      (repeat 2 hi-hat) 
      (seq (ghost 38 en)(accent 38 en)
        (ghost 38 en)(accent 38 en)
        (ghost 38 en)) (repeat 2 (ghost 38 en))) ))

elaborate-rock-beat 

;;; Part 3 ;;; 

; The bass drum plays on 1, the “and” of 2, 3, and the “and” of 4.
; The snare drum plays on 2 and the “and” of 3. 
; Hi-hat plays eighth notes for the entire bar.
(define latin-beat 
  (mod percussion (beat-machine 2
      (repeat 4 (seq (note 38 qn) (note 42 qn) (note 35 qn)))
      (seq (note 35 qn) (rest qn))
      (seq(note 35 qn) (rest qn) (note 42 qn) (note 42 en)))))
latin-beat 

; The bass drum plays quarter notes for the entire bar.
;The snare drum plays on 2 and 4.
;The hi hat plays on 1 and 3. On beats 2 and 4, we divide up the beat into three equal parts, a triplet, and the hi-hat plays on the first part and the third part of the triplet.

(define swing-beat
  (mod percussion (beat-machine 2
      (repeat 4 (seq (note 38 qn) (note 42 qn )))
      (seq (note 35 qn) (rest qn))
      (seq (note 35 qn) (rest qn) (note 42 qn))))) 

swing-beat 
"swing beat" 
;The bass drum plays on 1 “and”, the “ah” of 3, and the “and” of 4.
;The snare drum plays an accented note on 2 and 4 and plays ghost notes on the “ah” of 2, the “ee” and “and” of 3, and the “ah” of 4.
;The hi-hat plays eighth notes for the entire bar.
(define funk-beat 
  (mod percussion (beat-machine 2 
      (repeat 4 (seq (note 38 qn) (note 42 qn )))
      (seq (note 35 qn) (rest qn))
      (seq (note 38 qn) (rest qn) (note 42 qn)))))

funk-beat 
"funk beat"

;The bass drum plays on 1, 2, and 3 of the measure.
;The snare drum plays on the “and” of 1 and 2. Breaking up the third beat into eight thirty-second notes, the snare is played on the 4th and 7th of those notes. Finally, the snare also plays on the “and” of 4.
;The hi-hat plays only on 4. Breaking up the beat into 2 eighth notes, the hi-hat plays three evenly-spaced notes (eighth note triplets) on 4. 
(define garba-beat
  (mod percussion (beat-machine 2 
      (repeat 4 (seq (note 42 qn) (note 38 qn )))
      (seq (note 42 qn) (rest qn)
        (seq (note 35 qn) (rest qn) (note 42 qn)))
      (seq (note 35 qn) (rest qn) (note 38 qn) (rest qn)
        (seq (note 38 qn) (rest qn) (note 42 qn) (rest qn) (note 38 qn)(rest qn))))))
garba-beat 






