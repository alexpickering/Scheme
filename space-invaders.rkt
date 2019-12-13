(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders
;; Run with Intermediate Student Language (ISL)

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


;; ListOfInvadersAndMissiles is one of:
;;  - empty
;;  - (cons ListOfInvaders ListOfMissiles)
;; interp. a list of ListOfInvaders and ListOfMissiles

(define LOIAM empty)
(define LOIAM2 (cons LOI1 LOM1))
(define LOIAM3 (cons LOI2 LOM2))

#;
(define (fn-for-loiam loiam)
  (cond [(empty? loiam) (...)]
        [else
         (fn-for-list-of-invader (first loiam))
         (fn-for-list-of-missile (rest loiam))]))

;; Template Rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons ListOfMissiles empty)
;;  - reference: (first loiam) is ListOfInvaders
;;  - reference: (rest loiam) is ListOfMissiles


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
(check-random (move-world (make-game empty
                                     empty
                                     (make-tank (/ WIDTH 2) 0)))
              (if (< (random 2000) INVADE-RATE)
                  (make-game (cons (make-invader (random WIDTH) 0 (- 2 (* (random 2) 4))) empty)
                             empty
                             (make-tank (/ WIDTH 2) 0))
                  (make-game empty
                             empty
                             (make-tank (/ WIDTH 2) 0))))
(check-random (move-world (make-game (cons (make-invader 100 100 5) empty)
                                     (cons (make-missile 200 200) empty)
                                     (make-tank (/ WIDTH 2) 0)))
              (if (< (random 2000) INVADE-RATE)
                  (make-game (cons (make-invader (random WIDTH) 0 (- 2 (* (random 2) 4)))
                                   (cons (make-invader (+ (* INVADER-X-SPEED 5) 100)
                                                       (+ INVADER-Y-SPEED 100)
                                                       5) empty))
                             (cons (make-missile 200 (- 200 MISSILE-SPEED)) empty)
                             (make-tank (/ WIDTH 2) 0))
                  (make-game (cons (make-invader (+ (* INVADER-X-SPEED 5) 100)
                                                 (+ INVADER-Y-SPEED 100)
                                                 5) empty)
                             (cons (make-missile 200 (- 200 MISSILE-SPEED)) empty)
                             (make-tank (/ WIDTH 2) 0))))
(check-random (move-world (make-game (cons (make-invader 50 50 5)
                                           (cons (make-invader 100 100 5) empty))
                                     (cons (make-missile 50 400)
                                           (cons (make-missile 200 200) empty))
                                     (make-tank (/ WIDTH 2) 0)))
              (if (< (random 2000) INVADE-RATE)
                  (make-game (cons (make-invader (random WIDTH) 0 (- 2 (* (random 2) 4)))
                                   (cons (make-invader (+ (* INVADER-X-SPEED 5) 50)
                                                       (+ INVADER-Y-SPEED 50)
                                                       5)
                                         (cons (make-invader (+ (* INVADER-X-SPEED 5) 100)
                                                             (+ INVADER-Y-SPEED 100)
                                                             5) empty)))
                             (cons (make-missile 50 (- 400 MISSILE-SPEED))
                                   (cons (make-missile 200 (- 200 MISSILE-SPEED)) empty))
                             (make-tank (/ WIDTH 2) 0))
                  (make-game (cons (make-invader (+ (* INVADER-X-SPEED 5) 50)
                                                 (+ INVADER-Y-SPEED 50)
                                                 5)
                                   (cons (make-invader (+ (* INVADER-X-SPEED 5) 100)
                                                       (+ INVADER-Y-SPEED 100)
                                                       5) empty))
                             (cons (make-missile 50 (- 400 MISSILE-SPEED))
                                   (cons (make-missile 200 (- 200 MISSILE-SPEED)) empty))
                             (make-tank (/ WIDTH 2) 0))))

;(define (move-world game) ...) ;stub

(define (move-world g)
  (remove-collision (make-game (create-invaders
                                (move-invaders (game-invaders g)))
                               (next-missiles (game-missiles g))
                               (next-tank (game-tank g)))))


;; Game -> Game
;; removes any invaders and missiles that collide
(check-expect (remove-collision G0) G0)
(check-expect (remove-collision G1) G1)
(check-expect (remove-collision G3)
              (make-game (cons I2 empty) (cons M1 empty) T1))
(check-expect (remove-collision (make-game (list (make-invader 150 100 1)
                                                 (make-invader 250 50 -1)
                                                 (make-invader 200 200 1))
                                           (list (make-missile 140 90)
                                                 (make-missile 260 60)
                                                 (make-missile 200 189))
                                           T1))
              (make-game (list (make-invader 200 200 1))
                         (list (make-missile 200 189))
                         T1))

;(define (remove-collision g) g) ;stub

(define (remove-collision g)
  (let
      ([updated-lists (remove-collision-helper
                       (game-invaders g)
                       (game-missiles g)
                       (collect-colliding-missiles (game-invaders g) (game-missiles g) empty) 0)])
    (make-game (first updated-lists) (rest updated-lists) (game-tank g))))


;; ListOfInvaders ListOfMissiles ListOfMissiles-> ListOfMissiles
;; produces list with all missiles that overlap with invaders
(check-expect (collect-colliding-missiles LOI1 LOM1 empty)
              empty)
(check-expect (collect-colliding-missiles LOI2 LOM2 empty)
              (list (make-missile 100 50)))
(check-expect (collect-colliding-missiles (list I1 I2)
                                          (list M1 M2)
                                          empty)
              (list M2))

;(define (collect-colliding-missiles loi lom locm) locm) ;stub

(define (collect-colliding-missiles loi lom locm)
  (cond[(empty? loi) locm]
       [(empty? (overlap-missiles (first loi) lom))
        (collect-colliding-missiles (rest loi) lom locm)]
       [else
        (collect-colliding-missiles (rest loi) lom
                                    (cons (overlap-missiles (first loi) lom) locm))]))


;; Invader ListOfMissiles -> Missile
;; returns missile if any missiles overlap with invader
(check-expect (overlap-missiles I1 LOM1) empty)
(check-expect (overlap-missiles I1 LOM2) empty)
(check-expect (overlap-missiles (make-invader 150 300 1) LOM2) (make-missile 150 300))
(check-expect (overlap-missiles (make-invader 140 310 1) LOM2) (make-missile 150 300))
(check-expect (overlap-missiles (make-invader 200 200 1) (list (make-missile 200 200)))
              (make-missile 200 200))

;(define (overlap-missiles i lom) empty) ;stub

(define (overlap-missiles i lom)
  (cond [(empty? lom) empty]
        [(overlap? i (first lom)) (first lom)]
        [else (overlap-missiles i (rest lom))]))


;; Invader Missile -> Boolean
;; checks to see if single missle overlaps with invader's HIT-RANGE
(check-expect (overlap? I1 M2) true)
(check-expect (overlap? I1 M1) false)
(check-expect (overlap? (make-invader 100 100 1) (make-missile 90 110)) true)
(check-expect (overlap? (make-invader 100 100 1) (make-missile 89 110)) false)

;(define (overlap? i m) false) ;stub

(define (overlap? i m)
  (and (>= (+ (invader-x i) HIT-RANGE) (missile-x m) (- (invader-x i) HIT-RANGE))
       (>= (+ (invader-y i) HIT-RANGE) (missile-y m) (- (invader-y i) HIT-RANGE))))


;; ListOfInvaders ListOfMissiles ListOfMissiles Natural -> ListOfInvadersAndMissiles
;; removes items in colliding missile list from invader and missile lists
(check-expect (remove-collision-helper LOI1 LOM1 empty 0)
              (cons LOI1 LOM1))
(check-expect (remove-collision-helper LOI2 LOM2 (list (make-missile 100 50)) 0)
              (cons (list (make-invader 150 100 12)) (list (make-missile 150 300))))
(check-expect (remove-collision-helper (list I1 I2) (list M1 M2) (list M2) 0)
              (cons (list I2) (list M1)))
                                       
;(define (remove-collision-helper loi lom locm i) loiam) ;stub

(define (remove-collision-helper loi lom locm i)
  (cond [(empty? locm) (cons loi lom)]
        [(overlap? (list-ref loi i) (first locm))
         (remove-collision-helper
          (remove (list-ref loi i) loi)
          (remove (first locm) lom)
          (rest locm)
          0)]
        [else
         (remove-collision-helper loi lom locm (+ i 1))]))
        

;; ListOfInvaders -> ListOfInvaders
;; creates new invaders at random interval based on INVADE-RATE
(check-random (create-invaders LOI1)
              (if (< (random 2000) INVADE-RATE)
                  (cons (make-invader (random WIDTH) 0 (- 2 (* (random 2) 4))) empty)
                  LOI1))
(check-random (create-invaders LOI2)
              (if (< (random 2000) INVADE-RATE)
                  (cons (make-invader (random WIDTH) 0 (- 2 (* (random 2) 4))) LOI2)
                  LOI2))
(check-random (create-invaders (cons (make-invader 50 50 1)
                                     (cons (make-invader 100 100 -1)
                                           (cons (make-invader 250 250 1) empty))))
              (if (< (random 2000) INVADE-RATE)
                  (cons (make-invader (random WIDTH) 0 (- 2 (* (random 2) 4)))
                        (cons (make-invader 50 50 1)
                              (cons (make-invader 100 100 -1)
                                    (cons (make-invader 250 250 1) empty))))
                  (cons (make-invader 50 50 1)
                        (cons (make-invader 100 100 -1)
                              (cons (make-invader 250 250 1) empty)))))

;(define (create-invaders loinvaders) loinvaders) ;stub

(define (create-invaders loinvaders)
  (cond [(< (random 2000) INVADE-RATE)
         (cons (make-invader (random WIDTH)
                             0
                             (- 2 (* (random 2) 4)))
               loinvaders)]
        [else loinvaders]))


;; ListOfInvader -> ListOfInvader
;; moves every invader on-screen by speed amounts
(check-expect (move-invaders empty)
              empty)
(check-expect (move-invaders (cons (make-invader 100 100 5) empty))
              (cons (make-invader (+ (* INVADER-X-SPEED 5) 100)
                                  (+ INVADER-Y-SPEED 100)
                                  5) empty))
(check-expect (move-invaders (cons (make-invader 250 350 10)
                                   (cons (make-invader 100 100 5) empty)))
              (cons (make-invader (+ (* INVADER-X-SPEED 10) 250)
                                  (+ INVADER-Y-SPEED 350)
                                  10)
                    (cons (make-invader (+ (* INVADER-X-SPEED 5) 100)
                                        (+ INVADER-Y-SPEED 100)
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
                            (+ INVADER-Y-SPEED 100)
                            5))
(check-expect (move-invader (make-invader 200 300 10))
              (make-invader (+ (* INVADER-X-SPEED 10) 200)
                            (+ INVADER-Y-SPEED 300)
                            10))

; (define (move-invader i) ...) ;stub

(define (move-invader invader)
  (cond [(or (> 0 (+ (* INVADER-X-SPEED (invader-dx invader)) (invader-x invader)))
             (< WIDTH (+ (* INVADER-X-SPEED (invader-dx invader)) (invader-x invader))))
         (make-invader (+ (* INVADER-X-SPEED (invader-dx invader)) (invader-x invader))
                       (+ INVADER-Y-SPEED (invader-y invader))
                       (- (invader-dx invader)))]
        [else (make-invader (+ (* INVADER-X-SPEED (invader-dx invader)) (invader-x invader))
                            (+ INVADER-Y-SPEED (invader-y invader))
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
  (< (- 0 (/ (image-height MISSILE) 2)) (missile-y m) (+ HEIGHT (/ (image-height MISSILE) 2))))


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
;; produces true if any invader in ListOfInvaders reaches bottom of screen
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
;; produces true if single invader reaches bottom of screen
(check-expect (reach-bottom? (make-invader 100 100 5))
              false)
(check-expect (reach-bottom? (make-invader 100 550 5))
              true)

;(define (reach-bottom? i) false) ;stub

(define (reach-bottom? i)
  (> (invader-y i) (- HEIGHT (/ (image-height INVADER) 2))))


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
