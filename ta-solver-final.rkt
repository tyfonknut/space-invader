;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |ta-solver-starter(1)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; ta-solver-starter.rkt

;  PROBLEM 1:
;
;  Consider a social network similar to Twitter called Chirper. Each user has a name, a note about
;  whether or not they are a verified user, and follows some number of people.
;
;  Design a data definition for Chirper, including a template that is tail recursive and avoids
;  cycles.
;
;  Then design a function called most-followers which determines which user in a Chirper Network is
;  followed by the most people.
;

(define-struct ncount (name count))
;; ncount is (make-ncount String Natural)
;; interp: ncount is counting a number of occurences of a name.

#;
(define (fn-for-ncount nc)
  (... (ncount-name nc)
       (ncount-count nc)))

;; ListOfNcount is one of:
;; - empty
;; - (cons ncount ListOfNcount)

#;
(define (fn-for-lonc lonc)
  (cond [(empty? lonc) (...)]
        [else (... (fn-for-ncount (first lonc))
                   (fn-for-lonc (rest lonc)))]))

(define-struct chirper (name v follows))
;; chirper is (make-chirper String Boolean (listof chirper)
;; interp: A person who is either verified user or not, who follow different people.

;; ListOfChirper is one of:
;; - empty
;; - (cons chirper ListOfChirper)

#;
(define (fn-for-chirper c)
  ;; seen is a (listof String); of already seen chirpers name.
  ;; todo is a (listof chirper); of who to check next.
  (local [(define (fn-for-chirper c todo seen)
            (if (member (chirper-name c) seen)
                (fn-for-lochirp todo seen)
                (fn-for-lochirp (append (chirper-follows c) todo) (cons (chirper-name c) seen)))) ;... (chirper-name c) (chirper-v c)
          (define (fn-for-lochirp todo seen)
            (cond [(empty? todo) (...)]
                  [else (fn-for-chirper
                         (first todo)
                         (rest todo)
                         seen)]))]
    (fn-for-chirper c empty empty)))

(define CA (shared ((-A- (make-chirper "A" true (list -B- -C-)))
                    (-B- (make-chirper "B" true (list -A-)))
                    (-C- (make-chirper "C" true (list -A- -D-)))
                    (-D- (make-chirper "D" true (list -A- -B-)))) -A-))

(check-expect (most-followers CA) "A")

(define (most-followers c)
  ;; seen is a (listof String); of already seen chirpers name.
  ;; todo is a (listof chirper); of who to check next.
  ;; acc is a (listof ncount)
  (local [(define (fn-for-chirper c todo seen acc)
            (if (member (chirper-name c) seen)
                (fn-for-lochirp todo seen acc)
                (fn-for-lochirp (append (chirper-follows c) todo)
                                (cons (chirper-name c) seen) (tally (chirper-follows c) acc)))) ; ... (chirper-name c) (chirper-v c) 
          (define (fn-for-lochirp todo seen acc)
            (cond [(empty? todo) (has-most? acc)] ; <- Return name of the one with the highest follower count.
                  [else (fn-for-chirper
                         (first todo)
                         (rest todo)
                         seen
                         acc)]))]
    (fn-for-chirper c empty empty empty)))

;; ListOfChirper ListOfncount -> ListOfncount
;; interp.

;; If chirper is not in list of ncount, add it and make it count 1
;; 

(check-expect (tally (chirper-follows CA) empty) (list (make-ncount "B" 1) (make-ncount "C" 1)))

(define (tally loch ncount)
  (cond [(empty? loch) ncount]
        [else (tally (rest loch) (increase
                                  (chirper-name (first loch)) ncount))]))

;; String ListOfncount -> Listofncount
;; !!!
(define (increase name ncount)
  (cond [(empty? ncount) (cons (make-ncount name 1) empty)]
        [else 
         (if (string=? name (ncount-name (first ncount)))
             (cons (make-ncount (ncount-name (first ncount)) ; <- If name is same as (first ncount), add1, else skip.
                                (add1 (ncount-count (first ncount)))) (rest ncount))
             (cons (first ncount) (increase name (rest ncount))))]))

#; (define (has-most? lonc))

(check-expect (has-most? (list (make-ncount "A" 2) (make-ncount "B" 4) (make-ncount "C" 3))) "B")

(define (has-most? lonc)
  (local [(define  (work-it nc lonc )
            (cond [(empty? lonc) (ncount-name nc)]
                  [else (if (> (ncount-count nc) (ncount-count (first lonc))) ; <- Compare current largest (nc) with current first element in terms of 
                            (work-it nc (rest lonc))
                            (work-it (first lonc) (rest lonc)))]))]
    (work-it (first lonc) (rest lonc))))

;  PROBLEM 2:
;
;  In UBC's version of How to Code, there are often more than 800 students taking
;  the course in any given semester, meaning there are often over 40 Teaching Assistants.
;
;  Designing a schedule for them by hand is hard work - luckily we've learned enough now to write
;  a program to do it for us!
;
;  Below are some data definitions for a simplified version of a TA schedule. There are some
;  number of slots that must be filled, each represented by a natural number. Each TA is
;  available for some of these slots, and has a maximum number of shifts they can work.
;
;  Design a search program that consumes a list of TAs and a list of Slots, and produces one
;  valid schedule where each Slot is assigned to a TA, and no TA is working more than their
;  maximum shifts. If no such schedules exist, produce false.
;
;  You should supplement the given check-expects and remember to follow the recipe!



;; Slot is Natural
;; interp. each TA slot has a number, is the same length, and none overlap

(define-struct ta (name max avail))
;; TA is (make-ta String Natural (listof Slot))
;; interp. the TA's name, number of slots they can work, and slots they're available for

(define SOBA (make-ta "Soba" 2 (list 1 3)))
(define UDON (make-ta "Udon" 1 (list 3 4)))
(define RAMEN (make-ta "Ramen" 1 (list 2)))

(define NOODLE-TAs (list SOBA UDON RAMEN))



(define-struct assignment (ta slot))
;; Assignment is (make-assignment TA Slot)
;; interp. the TA is assigned to work the slot

;; Schedule is (listof Assignment)


;; ============================= FUNCTIONS


;; (listof TA) (listof Slot) -> Schedule or false
;; produce valid schedule given TAs and Slots; false if impossible

(check-expect (schedule-tas empty empty) empty)
(check-expect (schedule-tas empty (list 1 2)) false)
(check-expect (schedule-tas (list SOBA) empty) empty)

(check-expect (schedule-tas (list SOBA) (list 1)) (list (make-assignment SOBA 1)))
(check-expect (schedule-tas (list SOBA) (list 2)) false)
(check-expect (schedule-tas (list SOBA) (list 1 3)) (list (make-assignment SOBA 3)
                                                          (make-assignment SOBA 1)))

(check-expect (schedule-tas NOODLE-TAs (list 1 2 3 4))
              (list
               (make-assignment UDON 4)
               (make-assignment SOBA 3)
               (make-assignment RAMEN 2)
               (make-assignment SOBA 1)))

(check-expect (schedule-tas NOODLE-TAs (list 1 2 3 4 5)) false)


; (define (schedule-tas tas slots) empty) ;stub

(define (schedule-tas tas0 slots0)
  (local [(define (solve-schedule tas slots schedule)
            (if (taken? slots schedule)
                (sort schedule (lambda (x y) (> (assignment-slot x) (assignment-slot y)))
                      )
                (solve-los tas slots (next-schedule tas schedule))))
          (define (solve-los tas slots los)
            (cond [(empty? los) false]
                  [else (local [(define try (solve-schedule tas slots (first los)))]
                          (if (not (false? try))
                              try
                              (solve-los tas slots (rest los))))]))]
    (solve-schedule tas0 slots0 empty)
    ))

;; slots schedule -> Boolean
;; input. See if all slots are taken.

(define (taken? slots schedule)
  (cond [(empty? slots) true]
        [else
         (andmap (lambda (s) (match-slots? s schedule)) slots)]))

;; Natural schedule -> Boolean
(define (match-slots? n schedule0)
  (local [(define (fn-for-schedule n schedule acc)
            (cond [(empty? schedule) acc]
                  [else
                   (fn-for-schedule n
                                    (rest schedule)
                                    (if (= n (assignment-slot (first schedule)))
                                        (or acc true)
                                        acc))]))]
    (fn-for-schedule n schedule0 false)))

;; (listof ta) schedule -> (listof schedule)
;; make a list of possible schedules.
;; !!!

(define (next-schedule tas schedule)
  (cond [(empty? tas) empty]
        [else (local [(define try (assign-ta? (first tas) schedule))
                      (define remaining (next-schedule (rest tas) schedule))]
                (if try
                    (append
                     (assign-to-schedule (first tas) schedule)
                     remaining)                            
                    remaining))]))

;; ta schedule -> Boolean
;; interp. see if this ta can be added to schedule.
;; !!!
(define (assign-ta? ta schedule)
  (ormap (lambda (slot) (one-slot? slot ta schedule)) (ta-avail ta)))

;; slot ta schedule -> Boolean
;; interp. See if one slot can be added to schedule.
(define (one-slot? slot ta schedule)
  (if (false? (assign-one slot ta schedule))
      false
      true))

;; ta schedule -> schedule
;; interp. add ta to the schedule, that he can be assigned to.

(define (assign-to-schedule ta schedule)
  (local [(define (fn-for-avail ava schedule)
            (cond [(empty? ava) empty]
                  [else (local [(define try (assign-one (first ava) ta schedule))
                                (define remain (fn-for-avail (rest ava) schedule))]
                          (if (not (false? try))
                              (cons try remain)
                              remain
                              ))]))]
    (fn-for-avail (ta-avail ta) schedule)))

;; Natural ta Schedule -> Schedule or false
;; interp. make one assignment and add it to schedule.

(define (assign-one slot ta schedule)
  (if (and (no-conflict (make-assignment ta slot) schedule)
           (not-max ta schedule))
      (append
       schedule
       (list (make-assignment ta slot)))
      false))

;; Natural schedule -> Boolean
;; interp. see is a slot has not been used.
(define (no-conflict a schedule)
  (cond [(empty? schedule) true]
        [else (and (not (= (assignment-slot a) (assignment-slot (first schedule))))
                   (no-conflict a (rest schedule)))]))

;; ta schedule -> Boolean
;; interp. see if a ta has not been added more than they are able.
;; !!!
(define (not-max ta schedule)
  (> (ta-max ta) (count ta schedule)))

;; ta schedule -> Natural
(define (count ta schedule)
  (local [(define (fn-for-s schedule ta count)
            (cond [(empty? schedule) count]
                  [else (fn-for-s (rest schedule) ta
                                  (if (equal? ta (assignment-ta (first schedule)))
                                      (add1 count)
                                      count)
                                  )]))]
    (fn-for-s schedule ta 0)))