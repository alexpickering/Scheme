(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
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



;; ListOfInvader is one of:
;;  - empty
;;  - (cons Invader ListOfInvader)
;; interp. a list of invaders

(define LOI1 empty)
(define LOI2 (cons (make-invader 150 100 12)
                   (cons (make-invader 100 50 10) empty)))

#;
(define (fn-for-loinvader loinvader)
  (cond [(empty? loinvader) (...)]
        [else
         (... (fn-for-invader (first loinvader))
              (fn-for-loinvader (rest loinvader)))]))

;; Template Rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Invader ListOfInvader)
;;  - reference: (first loinvader) is Invader
;;  - self-reference: (rest loinvader) is ListOfInvader


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))


;; ListOfMissile is one of:
;;  - empty
;;  - (cons Missile ListOfMissile)
;; interp. a list of missiles

(define LOM1 empty)
(define LOM2 (cons (make-missile 150 300)
                   (cons (make-missile 100 50) empty)))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

;; Template Rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Missile ListOfMissile)
;;  - reference: (first lom) is Missile
;;  - self-reference: (rest lom) is ListOfMissile


(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; ==============
;; Functions:


;; Game -> Game
;; start the world with (main 0)

(define (main g)
  (big-bang (make-game empty empty (make-tank (/ WIDTH 2) 0)) ; Game
    (on-tick    move-world)                                   ; Game -> Game
    (to-draw    render-world)                                 ; Game -> Image
    (stop-when  end-game?)                                    ; Game -> Boolean
    (on-key     player-input)))                               ; Game KeyEvent -> Game

;; Game -> Game
;; produce the next state of Game by moving invaders and missiles
(check-expect (move-world (make-game empty
                                     empty
                                     (make-tank (/ WIDTH 2) 0)))
              (make-game empty
                         empty
                         (make-tank (/ WIDTH 2) 0)))
(check-expect (move-world (make-game (cons (make-invader 100 100 5) empty)
                                     (cons (make-missile 200 200) empty)
                                     (make-tank (/ WIDTH 2) 0)))
              (make-game (cons (make-invader (+ (* INVADER-X-SPEED 5) 100)
                                             (+ (* INVADER-Y-SPEED 5) 100)
                                             5) empty)
                         (cons (make-missile 200 (- 200 MISSILE-SPEED)) empty)
                         (make-tank (/ WIDTH 2) 0)))
(check-expect (move-world (make-game (cons (make-invader 50 50 5)
                                           (cons (make-invader 100 100 5) empty))
                                     (cons (make-missile 50 400)
                                           (cons (make-missile 200 200) empty))
                                     (make-tank (/ WIDTH 2) 0)))
              (make-game (cons (make-invader (+ (* INVADER-X-SPEED 5) 50)
                                             (+ (* INVADER-Y-SPEED 5) 50)
                                             5)
                               (cons (make-invader (+ (* INVADER-X-SPEED 5) 100)
                                                   (+ (* INVADER-Y-SPEED 5) 100)
                                                   5) empty))
                         (cons (make-missile 50 (- 400 MISSILE-SPEED))
                               (cons (make-missile 200 (- 200 MISSILE-SPEED)) empty))
                         (make-tank (/ WIDTH 2) 0)))


;(define (move-world game) ...) ;stub

(define (move-world g)
  (make-game (collision-check (create-invaders
                               (move-invaders (game-invaders g)))
                              (game-missiles g))
             (next-missiles (game-missiles g))
             (next-tank (game-tank g))))


;; ListOfInvaders ListOfMissiles -> ListOfInvaders
;; if missile in invader hit-range, invader and missile removed from game
(check-expect (collision-check (make-game empty
                                          empty
                                          (make-tank (/ WIDTH 2) 0)))
              (make-game empty
                         empty
                         (make-tank (/ WIDTH 2) 0)))
(check-expect (collision-check (make-game (cons (make-invader 105 105 5)
                                                (cons (make-invader 250 350 10) empty))
                                          empty
                                          (make-tank (/ WIDTH 2) 0)))
              (make-game (cons (make-invader 105 105 5)
                               (cons (make-invader 250 350 10) empty))
                         empty
                         (make-tank (/ WIDTH 2) 0)))
(check-expect (collision-check (make-game empty
                                          (cons (make-missile 100 100)
                                                (cons (make-missile 25 25) empty))
                                          (make-tank (/ WIDTH 2) 0)))
              (make-game empty
                         (cons (make-missile 100 100)
                               (cons (make-missile 25 25) empty))
                         (make-tank (/ WIDTH 2) 0)))
(check-expect (collision-check (make-game (cons (make-invader 100 100 5)
                                                (cons (make-invader 250 350 10) empty))
                                          (cons (make-missile 100 50)
                                                (cons (make-missile 25 25) empty))
                                          (make-tank (/ WIDTH 2) 0)))
              (make-game (cons (make-invader 100 100 5)
                               (cons (make-invader 250 350 10) empty))
                         (cons (make-missile 100 50)
                               (cons (make-missile 25 25) empty))
                         (make-tank (/ WIDTH 2) 0)))
(check-expect (collision-check (make-game (cons (make-invader 100 100 5)
                                                (cons (make-invader 250 350 10) empty))
                                          (cons (make-missile 100 100)
                                                (cons (make-missile 25 25) empty))
                                          (make-tank (/ WIDTH 2) 0)))
              (make-game (cons (make-invader 250 350 10) empty)
                         (cons (make-missile 25 25) empty)
                         (make-tank (/ WIDTH 2) 0)))
(check-expect (collision-check (make-game (cons (make-invader 105 95 5)
                                                (cons (make-invader 250 350 10) empty))
                                          (cons (make-missile 100 100)
                                                (cons (make-missile 25 25) empty))
                                          (make-tank (/ WIDTH 2) 0)))
              (make-game (cons (make-invader 250 350 10) empty)
                         (cons (make-missile 25 25) empty)
                         (make-tank (/ WIDTH 2) 0)))


(define (collision-check loi lom) loi) ;stub


;; Invader  -> Boolean
;; produces true if missile in invader hit-range

(define (collision? g) ...) ;stub


;; ListOfInvaders -> ListOfInvaders
;; creates new invaders at random interval based on INVADE-RATE
;; !!!

;(define (create-invaders loinvaders) loinvaders) ;stub

(define (create-invaders loinvaders)
  (cond [(< (random 2000) INVADE-RATE)
         (cons (make-invader (random WIDTH)
                             0
                             (- (random 10) (random 10)))
               loinvaders)]
        [else loinvaders]))
         

; ;; ListOfInvaders -> ListOfInvaders
; ;; moves invaders, removes invaders if hit, removes missile that hit them
; (check-expect (next-invaders empty) empty)
; (check-expect (next-invaders (cons (make-invader 100 100 5) empty)
;                              (cons (make-missile 50 400) empty)))
; (cons (make-invader (+ (* INVADER-X-SPEED 5) 100)
;                     (+ (* INVADER-Y-SPEED 5) 100)
;                     5) empty))
; (check-expect (next-invaders (cons (make-invader 100 100 5)
;                                    (cons (make-invader 250 350 10) empty))
;                              (cons (make-missile 100 100)
;                                    (cons (make-missile 25 25) empty)))
;               (cons (make-invader (+ (* INVADER-X-SPEED 10) 250)
;                                   (+ (* INVADER-Y-SPEED 10) 350)
;                                   10) empty))
; (check-expect (next-invaders (cons (make-invader 98 98 5)
;                                    (cons (make-invader 250 350 10) empty))
;                              (cons (make-missile 100 100)
;                                    (cons (make-missile 25 25) empty)))
;               (cons (make-invader (+ (* INVADER-X-SPEED 10) 250)
;                                   (+ (* INVADER-Y-SPEED 10) 350)
;                                   10) empty))
;                              
;               
; 
; (define (next-invaders loinvaders lom) ...) ;stub
; 
; ;(define (next-invaders loinvaders lom)
; ;  (remaining-invaders (move-invaders loinvaders) lom))



;; ListOfInvader -> ListOfInvader
;; moves every invader on-screen by speed amounts
(check-expect (move-invaders empty)
              empty)
(check-expect (move-invaders (cons (make-invader 100 100 5) empty))
              (cons (make-invader (+ (* INVADER-X-SPEED 5) 100)
                                  (+ (* INVADER-Y-SPEED 5) 100)
                                  5) empty))
(check-expect (move-invaders (cons (make-invader 250 350 10)
                                   (cons (make-invader 100 100 5) empty)))
              (cons (make-invader (+ (* INVADER-X-SPEED 10) 250)
                                  (+ (* INVADER-Y-SPEED 10) 350)
                                  10)
                    (cons (make-invader (+ (* INVADER-X-SPEED 5) 100)
                                        (+ (* INVADER-Y-SPEED 5) 100)
                                        5) empty)))

;(define (move-invaders loinvader) ...) ;stub

(define (move-invaders loinvader)
  (cond [(empty? loinvader) empty]
        [else
         (cons (move-invader (first loinvader))
               (move-invaders (rest loinvader)))]))


;; Invader -> Invader
;; moves single invader based on x and y positions and speed
(check-expect (move-invader (make-invader 100 100 5))
              (make-invader (+ (* INVADER-X-SPEED 5) 100)
                            (+ (* INVADER-Y-SPEED 5) 100)
                            5))
(check-expect (move-invader (make-invader 200 300 10))
              (make-invader (+ (* INVADER-X-SPEED 10) 200)
                            (+ (* INVADER-Y-SPEED 10) 300)
                            10))

; (define (move-invader i) ...) ;stub

(define (move-invader invader)
  (cond [(or (> 0 (+ (* INVADER-X-SPEED (invader-dx invader)) (invader-x invader)))
             (< WIDTH (+ (* INVADER-X-SPEED (invader-dx invader)) (invader-x invader))))
         (make-invader (+ (* INVADER-X-SPEED (invader-dx invader)) (invader-x invader))
                       (+ (* INVADER-Y-SPEED (abs (invader-dx invader))) (invader-y invader))
                       (- (invader-dx invader)))]
        [else (make-invader (+ (* INVADER-X-SPEED (invader-dx invader)) (invader-x invader))
                            (+ (* INVADER-Y-SPEED (abs (invader-dx invader))) (invader-y invader))
                            (invader-dx invader))]))


;; ListOfMissiles -> ListOfMissiles
;; checks for offscreen missiles and moves onscreen missiles
(check-expect (next-missiles empty) empty)
(check-expect (next-missiles (cons (make-missile 100 100)
                                   (cons (make-missile 300 400) empty)))
              (cons (make-missile 100 (- 100 MISSILE-SPEED))
                    (cons (make-missile 300 (- 400 MISSILE-SPEED)) empty)))

;(define (next-missiles lom) ...) ;stub

(define (next-missiles lom)
  (onscreen-missiles (move-missiles lom)))


;; ListOfMissiles -> ListOfMissiles
;; removes offscreen missiles, if present in ListOfMissiles
(check-expect (onscreen-missiles empty) empty)
(check-expect (onscreen-missiles (cons (make-missile 100 100)
                                       (cons (make-missile 300 250) empty)))
              (cons (make-missile 100 100)
                    (cons (make-missile 300 250) empty)))
(check-expect (onscreen-missiles (cons (make-missile 100 100)
                                       (cons (make-missile 300 550) empty)))
              (cons (make-missile 100 100)
                    empty))
(check-expect (onscreen-missiles (cons (make-missile 300 550)
                                       (cons (make-missile 100 100) empty)))
              (cons (make-missile 100 100)
                    empty))
                                 
;(define (onscreen-missiles lom) ...) ;stub

(define (onscreen-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (if (onscreen? (first lom))
             (cons (first lom) (onscreen-missiles (rest lom)))
             (onscreen-missiles (rest lom)))]))


;; Missile -> Boolean
;; if part of missile image is within background dimensions, produces true
(check-expect (onscreen? (make-missile 100 100)) true)
(check-expect (onscreen? (make-missile 250 450)) true)
(check-expect (onscreen? (make-missile 100 -100)) false)
(check-expect (onscreen? (make-missile 100
                                       (+ HEIGHT (/ (image-height MISSILE) 2))))
              false)

;(define (onscreen? m) ...) ;stub

(define (onscreen? m)
  (< 0 (missile-y m) (+ HEIGHT (/ (image-height MISSILE) 2))))


;; ListOfMissiles -> ListOfMissiles
;; moves every missile on-screen by speed amounts
(check-expect (move-missiles empty)
              empty)
(check-expect (move-missiles (cons (make-missile 100 100) empty))
              (cons (make-missile 100 (- 100 MISSILE-SPEED)) empty))
(check-expect (move-missiles (cons (make-missile 250 350)
                                   (cons (make-missile 100 100) empty)))
              (cons (make-missile 250 (- 350 MISSILE-SPEED))
                    (cons (make-missile 100 (- 100 MISSILE-SPEED)) empty)))

;(define (move-missiles lom) ...) ;stub

(define (move-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (cons (move-missile (first lom))
               (move-missiles (rest lom)))]))


;; Missile -> Missile
;; moves single missile based on x and y positions and speed
(check-expect (move-missile (make-missile 100 100))
              (make-missile 100 (- 100 MISSILE-SPEED)))
(check-expect (move-missile (make-missile 250 25))
              (make-missile 250 (- 25 MISSILE-SPEED)))

;(define (move-missile m) ...) ;stub

(define (move-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

;; Tank -> Tank
;; moves tank according to tank-dx
(check-expect (next-tank (make-tank 30 0))
              (make-tank 30 0))
(check-expect (next-tank (make-tank 100 5))
              (make-tank 105 5))
(check-expect (next-tank (make-tank 30 -5))
              (make-tank 25 -5))
(check-expect (next-tank (make-tank 0 -5))
              (make-tank 0 -5))
(check-expect (next-tank (make-tank WIDTH 5))
              (make-tank WIDTH 5))

;(define (next-tank t) t) ;stub

(define (next-tank t)
  (if
   (or
    (and (<= (tank-x t) (- WIDTH (/ (image-width TANK) 2)))
         (> (tank-dir t) 0))
    (and (>= (tank-x t) (/ (image-width TANK) 2))
         (< (tank-dir t) 0)))
   (make-tank (+ (tank-x t) (tank-dir t)) (tank-dir t))
   t))

; ;; Notes: DELETE AFTER COMPLETION
; ;;
; ;; when missile hits invader
; ;;  - invader removed from list
; ;;  - missile removed from list
; 
; Make use of the following constants:
; (define HIT-RANGE 10)
; 
; (define INVADE-RATE 100)
; 



;; Game -> Image
;; renders game world
(check-expect (render-world (make-game empty empty (make-tank (/ WIDTH 2) 0)))
              (place-image TANK (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
(check-expect (render-world (make-game (cons (make-invader 100 100 5)
                                             empty)
                                       (cons (make-missile 25 25) empty)
                                       (make-tank (/ WIDTH 2) 0)))
              (place-image INVADER 100 100 (place-image MISSILE 25 25 (place-image TANK
                                                                                   (/ WIDTH 2)
                                                                                   (- HEIGHT TANK-HEIGHT/2)
                                                                                   BACKGROUND))))
(check-expect (render-world (make-game (cons (make-invader 100 100 5)
                                             (cons (make-invader 250 350 10) empty))
                                       (cons (make-missile 100 50)
                                             (cons (make-missile 25 25) empty))
                                       (make-tank (/ WIDTH 2) 0)))
              (place-image INVADER 100 100 
                           (place-image INVADER 250 350
                                        (place-image MISSILE 100 50
                                                     (place-image MISSILE 25 25
                                                                  (place-image TANK
                                                                               (/ WIDTH 2)
                                                                               (- HEIGHT TANK-HEIGHT/2)
                                                                               BACKGROUND))))))

              

;(define (render-world game) ...) ;stub

(define (render-world g)
  (render-invaders (game-invaders g)
                   (render-missiles (game-missiles g)
                                    (place-image TANK
                                                 (tank-x (game-tank g))
                                                 (- HEIGHT TANK-HEIGHT/2)
                                                 BACKGROUND))))
           
           


;; ListOfInvaders Image -> Image
;; renders invaders
(check-expect (render-invaders empty BACKGROUND) BACKGROUND)
(check-expect (render-invaders (cons (make-invader 250 350 10) empty) BACKGROUND)
              (place-image INVADER 250 350 BACKGROUND))
(check-expect (render-invaders (cons (make-invader 100 100 5)
                                     (cons (make-invader 250 350 10) empty))
                               BACKGROUND)
              (place-image INVADER 100 100
                           (place-image INVADER 250 350 BACKGROUND)))

;(define (render-invaders loinvaders) empty-image) ;stub

(define (render-invaders loinvaders img)
  (cond [(empty? loinvaders) img]
        [else
         (place-image INVADER
                      (invader-x (first loinvaders))
                      (invader-y (first loinvaders))
                      (render-invaders (rest loinvaders) img))]))


;; ListOfMissiles Image -> Image
;; renders missiles
(check-expect (render-missiles empty BACKGROUND) BACKGROUND)
(check-expect (render-missiles (cons (make-missile 250 350) empty) BACKGROUND)
              (place-image MISSILE 250 350 BACKGROUND))
(check-expect (render-missiles (cons (make-missile 100 100)
                                     (cons (make-missile 250 350) empty))
                               BACKGROUND)
              (place-image MISSILE 100 100
                           (place-image MISSILE 250 350 BACKGROUND)))

;(define (render-missiles lom) empty-image) ;stub

(define (render-missiles lom img)
  (cond [(empty? lom) img]
        [else
         (place-image MISSILE
                      (missile-x (first lom))
                      (missile-y (first lom))
                      (render-missiles (rest lom) img))]))


;; Game -> Boolean
;; if invader reached ground, produces true
;; !!!
(check-expect (end-game? (make-game (cons (make-invader 100 100 5)
                                          (cons (make-invader 250 350 10) empty))
                                    (cons (make-missile 100 50)
                                          (cons (make-missile 25 25) empty))
                                    (make-tank (/ WIDTH 2) 0)))
              false)
(check-expect (end-game? (make-game (cons (make-invader 100 100 5)
                                          (cons (make-invader 250 525 10) empty))
                                    (cons (make-missile 100 50)
                                          (cons (make-missile 25 25) empty))
                                    (make-tank (/ WIDTH 2) 0)))
              true)
(check-expect (end-game? (make-game (cons (make-invader 100 510 5)
                                          (cons (make-invader 250 350 10) empty))
                                    (cons (make-missile 100 50)
                                          (cons (make-missile 25 25) empty))
                                    (make-tank (/ WIDTH 2) 0)))
              true)
                         
;(define (end-game? game) false) ;stub

(define (end-game? g)
  (cond [(empty? (game-invaders g)) false]
        [else (any-reach-bottom? (game-invaders g))]))


;; ListOfInvaders -> Boolean
;; checks invader list for any reaching screen bottom
(check-expect (any-reach-bottom? empty) false)
(check-expect (any-reach-bottom? (cons (make-invader 100 100 5) empty))
              false)
(check-expect (any-reach-bottom? (cons (make-invader 100 100 5)
                                       (cons (make-invader 250 550 10) empty)))
              true)

;(define (any-reach-bottom? loinvaders) false) ;stub

(define (any-reach-bottom? loinvaders)
  (cond [(empty? loinvaders) false]
        [else (or (reach-bottom? (first loinvaders))
                  (any-reach-bottom? (rest loinvaders)))]))


;; Invader -> Boolean
;; checks single invader for reaching bottom of screen
(check-expect (reach-bottom? (make-invader 100 100 5))
              false)
(check-expect (reach-bottom? (make-invader 100 550 5))
              true)

;(define (reach-bottom? i) false) ;stub

(define (reach-bottom? i)
  (> (invader-y i) (- HEIGHT (/ (image-width INVADER) 2))))


;; Game -> Game
;; takes player input to change game status
(check-expect (player-input G1 "u") G1)
(check-expect (player-input G1 " ")
              (make-game empty
                         (cons (make-missile (tank-x T1) (- HEIGHT TANK-HEIGHT/2)) empty)
                         T1))
(check-expect (player-input G1 "left")
              (make-game empty
                         empty
                         (make-tank (tank-x T1) (- TANK-SPEED))))
(check-expect (player-input G1 "right")
              (make-game empty
                         empty
                         (make-tank (tank-x T1) TANK-SPEED)))


;(define (player-input game ke) ...) ;stub
 
(define (player-input game ke)
  (cond [(key=? ke " ") (fire-missile game ke)]
        [(or (key=? ke "left") (key=? ke "right")) (move-tank game ke)]
        [else game]))


;; Game -> Game
;; if player presses space bar, creates a new missile
(check-expect (fire-missile (make-game empty empty (make-tank (/ WIDTH 2) 0)) "o")
              (make-game empty
                         empty
                         (make-tank (/ WIDTH 2) 0)))
(check-expect (fire-missile (make-game empty empty (make-tank (/ WIDTH 2) 0)) " ")
              (make-game empty
                         (cons (make-missile (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2)) empty)
                         (make-tank (/ WIDTH 2) 0)))
(check-expect (fire-missile (make-game empty
                                       (cons (make-missile 400 250)
                                             (cons (make-missile 100 100) empty))
                                       (make-tank (/ WIDTH 2) 0))
                            " ")
              (make-game empty
                         (cons (make-missile (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2))
                               (cons (make-missile 400 250)
                                     (cons (make-missile 100 100) empty)))
                         (make-tank (/ WIDTH 2) 0)))

;(define (fire-missile game ke) game) ;stub 

(define (fire-missile game ke)
  (cond [(key=? ke " ")
         (make-game (game-invaders game)
                    (cons (make-missile (tank-x (game-tank game))
                                        (- HEIGHT TANK-HEIGHT/2))
                          (game-missiles game))
                    (game-tank game))]
        [else game]))


;; Game -> Game
;; if player presses left or right arrow, moves tank accordingly
(check-expect (move-tank (make-game empty empty (make-tank (/ WIDTH 2) 0)) "i")
              (make-game empty empty (make-tank (/ WIDTH 2) 0)))
(check-expect (move-tank (make-game empty empty (make-tank (/ WIDTH 2) 0)) "right")
              (make-game empty empty (make-tank (/ WIDTH 2) TANK-SPEED)))
(check-expect (move-tank (make-game empty empty (make-tank (/ WIDTH 2) 0)) "left")
              (make-game empty empty (make-tank (/ WIDTH 2) (- TANK-SPEED))))

;(define (move-tank game ke) game) ;stub

(define (move-tank game ke)
  (cond [(key=? ke "left")
         (make-game (game-invaders game)
                    (game-missiles game)
                    (make-tank (tank-x (game-tank game)) (- TANK-SPEED)))]
        [(key=? ke "right")
         (make-game (game-invaders game)
                    (game-missiles game)
                    (make-tank (tank-x (game-tank game))  TANK-SPEED))]
        [else game]))
