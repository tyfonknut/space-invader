;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invader-old) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 0.2)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 0.2)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define TANK-Y-POS (- HEIGHT TANK-HEIGHT/2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(require 2htdp/image)
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; ListOfInvaders is one of:
;; - empty
;; - (cons Invader ListOfInvader)
;; interp. a list of invaders

(define LOI1 empty)
(define LOI2 (cons I1 empty))
(define LOI3 (cons I2 (cons I1 empty)))


#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else (... (fn-for-invader (first loi))
                   (fn-for-loi (rest loi)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons String ListOfInvaders)
;;  - self-reference: (rest loi) is ListOfInvaders

;; ListOfMissiles is one of:
;; - empty
;; - (cons Missiles ListOfMissiles)
;; interp. a list of missiles

(define LOM1 empty)
(define LOM2 (cons M1 empty))
(define LOM3 (cons M2 (cons M1 empty)))


#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else (... (fn-for-missile (first lom))
                   (fn-for-lom (rest lom)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons String ListOfMissiles)
;;  - self-reference: (rest lom) is ListOfMissiles



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))


;; GS -> GS
;; start the world with (main (make-game empty empty T0))
;; 
(define (main GS)
  (big-bang GS                   ; GS
            (on-tick   next-frame)     ; GS -> GS
            (to-draw   render-frame)   ; GS -> Image
            (stop-when game-over)      ; GS -> Boolean
            (on-key    handle-key)))    ; GS KeyEvent -> GS

;; GS -> GS
;; produce the next ...
;; template from game definition.
;;

; (define (next-frame gs) ...) ; stub

(define (next-frame gs)
  (remove-hit (make-game (create-invaders (advance-invaders (game-invaders gs)))
                         (advance-missiles (game-missiles gs))
                         (move-tank (game-tank gs)))))

;; Game -> Game
;; interp. Find missiles and Invaders that collide, and remove both.
(check-expect (remove-hit (make-game empty (list M1) T0)) (make-game empty (list M1) T0))
(check-expect (remove-hit (make-game (list I1) empty T0)) (make-game (list I1) empty T0))
(check-expect (remove-hit (make-game (list I1) (list M2) T0)) (make-game empty empty T0))


; (define (remove-hit game) game) ; stub

(define (remove-hit s)
  (make-game (hit-invader? (game-invaders s) (game-missiles s))
       (hit-missile? (game-missiles s) (game-invaders s))
       (game-tank s)))

;; ListOfInvaders -> ListOfInvaders
;; Create a new invader if there is less than 5 at random.

(define (create-invaders loi) (if (>= (length loi) 5) loi
  (cons (make-invader (random WIDTH) 0 (random 10)) loi)))

;; ListOfInvaders ListOfMissiles -> ListOfInvaders

; (define (hit-invader? loinvader loim) loinvader) ; stub

(define (hit-invader? loi lom)
  (cond [(empty? loi) empty]
        [(empty? lom) loi]
        [else (if (invader-got-hit? (first loi) lom)
                   (hit-invader? (rest loi) lom)
                   (cons (first loi) (hit-invader? (rest loi) lom)))]))

;; Invader ListOfMissiles -> Boolean
;; Interp. find out if a missile is close to an invader.
(check-expect (invader-got-hit? I1 (list M1)) #false)
(check-expect (invader-got-hit? I1 (list M2)) #true)


; (define (invader-got-hit? i lom) #true) ; stub

(define (invader-got-hit? invader lom)
  (cond [(empty? lom) #false]
        [else (if (and
                   (<=(abs (- (invader-x invader) (missile-x (first lom)))) HIT-RANGE)
                   (<=(abs (- (invader-y invader) (missile-y (first lom)))) HIT-RANGE))
                  #true
                   (invader-got-hit? invader (rest lom)))]))

;; ListOfMissiles ListOfInvaders -> ListOfMissiles

(check-expect (hit-missile? (list (make-missile 30 -11)) (list I1)) empty)

; (define (hit-alien? loinvader loim) loinvader) ; stub

(define (hit-missile? lom loi)
  (cond [(empty? loi) lom]
        [(empty? lom) empty]
        [else (if (or (missile-got-hit? (first lom) loi) (<= (missile-y (first lom)) 0))
                   (hit-missile? (rest lom) loi)
                   (cons (first lom) (hit-missile? (rest lom) loi)))]))

;; Missile ListOfInvaders -> Boolean
;; Interp. find out if an invader is close to a missile.

 ; (define (missile-got-hit? missile loi) #false) ;stub

(define (missile-got-hit? missile loi)
  (cond [(empty? loi) #false]
        [else (if (and
                   (<=(abs (- (missile-x missile) (invader-x (first loi)))) HIT-RANGE)
                   (<=(abs (- (missile-y missile) (invader-y (first loi)))) HIT-RANGE))
                  #true
                   (missile-got-hit? missile (rest loi)))]))




;; ListOfInvaders -> ListOfInvaders
;; Advance current invaders.

(check-expect (advance-invaders empty) empty)

; (define (advance-invaders loi) empty) ; stub

; <template from ListOfInvaders>

(define (advance-invaders loi)
  (cond [(empty? loi) empty]
        [else (cons (move-invader (first loi))
                   (advance-invaders (rest loi)))]))

;; Invader -> Invader
;; Move an Invader(main (make-game empty empty T0))
(check-expect (move-invader (make-invader 150 100 12)) (make-invader (+ 150  (* INVADER-X-SPEED (catheter 12))) (+ 100 (* INVADER-Y-SPEED (catheter 12))) 12))
(check-expect (move-invader (make-invader (- WIDTH 1) 100 12)) (make-invader WIDTH (+ 100 (* INVADER-Y-SPEED (catheter 12))) (- 12)))
(check-expect (move-invader (make-invader 1 100 -10)) (make-invader 0 (+ 100 (* INVADER-Y-SPEED (catheter 10))) 10))

(define (move-invader invader)
  (cond [(> (+ (invader-x invader) (* INVADER-X-SPEED (invader-dx invader))) WIDTH)
         (make-invader WIDTH
                (+ (invader-y invader) (* INVADER-Y-SPEED (catheter (abs (invader-dx invader)))))
                ( - (invader-dx invader)))
         ]
        [(< (+ (invader-x invader) (* INVADER-X-SPEED (invader-dx invader))) 0)
         (make-invader 0
                (+ (invader-y invader) (* INVADER-Y-SPEED (catheter (abs (invader-dx invader)))))
                ( - (invader-dx invader)))
         ]
        [else (make-invader (+ (invader-x invader) (* INVADER-X-SPEED (catheter (invader-dx invader))))
                (+ (invader-y invader) (* INVADER-Y-SPEED (catheter (abs (invader-dx invader)))))
                (invader-dx invader))]))

;; Integer Integer -> Integer
;; Calculate Length of side of triangle.
;; Assumes it is 90 degrees.

(define (catheter distance)
  (/ distance (integer-sqrt 2)))

;; ListOfMissiles -> ListOfMissiles
;; Advance current missiles

; (define (advance-missiles lom) lom) ; stub

; <template from ListOfMissiles>
(define (advance-missiles lom)
  (cond [(empty? lom) empty]
        [else (cons (move-missile (first lom))
                   (advance-missiles (rest lom)))]))

;; Missile -> Missile
;; Move a missile.

(check-expect (move-missile (make-missile 150 300)) (make-missile 150 (- 300 MISSILE-SPEED)))

(define (move-missile m) (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

;; tank -> tank
;; Move tank

(check-expect (move-tank (make-tank 100 1)) (make-tank (+ 100 TANK-SPEED) 1))
(check-expect (move-tank (make-tank WIDTH 1)) (make-tank WIDTH 1))
(check-expect (move-tank (make-tank 0 -1)) (make-tank 0 -1))

; (define (move-tank t) t) ; stub

(define (move-tank t)
  (cond [(and (>= (tank-x t) WIDTH) (= (tank-dir t) 1)) t]
        [(and (<= (tank-x t) 0) (= (tank-dir t) -1)) t]
        [else (make-tank (+ (tank-x t) (* TANK-SPEED (tank-dir t))) (tank-dir t))]))


;; GS -> Image
;; render ... 
;;
(check-expect (render-frame (make-game empty empty (make-tank (/ WIDTH 2) 1))) (place-image TANK (/ WIDTH 2) TANK-Y-POS BACKGROUND))
(check-expect (render-frame (make-game empty (list (make-missile 120 300)) (make-tank (/ WIDTH 2) 1))) (place-image MISSILE 120 300 (place-image TANK (/ WIDTH 2) TANK-Y-POS BACKGROUND)))
(check-expect (render-frame (make-game (list I1) empty (make-tank (/ WIDTH 2) -1))) (place-image INVADER (invader-x I1) (invader-y I1) (place-image TANK (/ WIDTH 2) TANK-Y-POS BACKGROUND)))

; (define (render-frame gs) BACKGROUND) ; stub

(define (render-frame s)
  (place-invaders (game-invaders s)
                         (place-missiles (game-missiles s)
                                     (place-tank (game-tank s) BACKGROUND))))

;; tank Image -> Image

(define (place-tank t bgr) (place-image TANK (tank-x t) TANK-Y-POS bgr))

;; ListOfMissiles Image -> Image
; (define (place-missiles lom bgr) BACKGROUND) ; stub

(define (place-missiles lom img)
  (cond [(empty? lom) img]
        [else (missile-img (first lom) (place-missiles (rest lom) img))]))

;; Missile Image -> Image
;; Interp. Place image on a background image.

(check-expect (missile-img (make-missile 120 300) BACKGROUND) (place-image MISSILE 120 300 BACKGROUND))

(define (missile-img missile bgr) (place-image MISSILE (missile-x missile) (missile-y missile) bgr))

;; ListOfInvaders Image -> Image
; (define (place-invaders loi bgr) BACKGROUND) ; stub

(define (place-invaders loi bgr)
  (cond [(empty? loi) bgr]
        [else (invader-img (first loi) (place-invaders (rest loi) bgr))
                   ]))

;; Invader Image -> Image
;; Interp. Place invader on a background image

(define (invader-img invader bgr) (place-image INVADER (invader-x invader) (invader-y invader) bgr))

;; GS -> Boolean
;; Stop the game when an invader has landedTANK-SPEED
;; !!!

(check-expect (game-over (make-game (list I2) empty T0)) #true)
(check-expect (game-over (make-game empty empty T0)) #false)

; (define (game-over gs) #false) ;

(define (game-over s)
  (if (has-landed? (game-invaders s))
                   #true
                   #false))

;; ListOfInvader -> Boolean
(check-expect (has-landed? empty) #false)
(check-expect (has-landed? (list (make-invader 100 0 12) I2)) #true)
(check-expect (has-landed? (list I2)) #true)

; (define (has-landed? loi) #false) ; stub

(define (has-landed? loi)
  (cond [(empty? loi) #false]
        [else (if (invader-landed? (first loi))
                  #true
                   (has-landed? (rest loi)))]))

;; Invader -> Boolean

; (define (invader-landed? invader) #false) ;stub

(define (invader-landed? invader)
  (if (>= (invader-y invader) HEIGHT)
      #true
      #false))

;; GS Key-event -> GS

(define (handle-key gs ke)
  (cond [(key=? ke " ") (make-game (game-invaders gs) (create-missile (game-missiles gs) (tank-x (game-tank gs))) (make-tank (tank-x (game-tank gs)) (tank-dir (game-tank gs))))]
        [(key=? ke "left") (make-game (game-invaders gs) (game-missiles gs) (make-tank (tank-x (game-tank gs)) -1))]
        [(key=? ke "right") (make-game (game-invaders gs) (game-missiles gs) (make-tank (tank-x (game-tank gs)) 1))]
        [else gs]))

;; ListOfMissile Integer -> ListOfMissiles

(define (create-missile lom xpos) (if (>= (length lom) 5)
                                      lom
                                      (cons (make-missile xpos TANK-Y-POS) lom)))