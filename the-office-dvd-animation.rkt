#reader(lib"read.ss""wxme")WXME0108 ## 
#|
   This file uses the GRacket editor format.
   Open this file in DrRacket version 7.3 or later to read it.

   Most likely, it was created by saving a program in DrRacket,
   and it probably contains a program with non-text elements
   (such as images or comment boxes).

            http://racket-lang.org/
|#

;; the-office-dvd-animation.rkt
;; Author: Alex Pickering
;; Date: 20190930

(require 2htdp/image)
(require 2htdp/universe)

;; ====================
;; Constants:

(define WIDTH 1600)
(define HEIGHT 900)
(define LOGO-WIDTH 300)
(define LOGO-HEIGHT 200)

(define BACKGROUND (rectangle WIDTH HEIGHT "solid" "black"))

(define CTR-X (/ WIDTH 2))
(define CTR-Y (/ HEIGHT 2))


(define B-LOGO .) ;300 width by 200 height

(define R-LOGO .)

(define G-LOGO .)


;; ====================
;; Data Definitions:

(define-struct dvd (x y dx dy cc))
;; dvd is (make-dvd Natural[0, WIDTH],Natural[0, WIDTH], Integer, Integer)
;; interp. (make-dvd x y dx dy) is a dvd-logo with x and y coordinates and velocities and current logo color
;;            the x and y are the placement of the cow
;;            x and y are in screen coordinates (pixels)
;;            dx and dy are pixels per tick
;;            cc is the current color of the logo

(define d1 (make-dvd CTR-X CTR-Y 3 -3 "blue")) ;at center, moving toward top-right
(define d2 (make-dvd 300 400 3 3 "red")); at top-left, moving toward bottom-right
(define d3 (make-dvd 1000 600 -3 -3 "green")); at bottom-right, moving toward top-left
(define d4 (make-dvd 1200 200 -3 3 "blue")); at top-right, moving toward bottom-left
#;
(define (fn-for-dvd d)
  (... (dvd-x d)       ;Natural[0, WIDTH]
       (dvd-y d)       ;Natural[0, HEIGHT]
       (dvd-dx d)      ;Integer
       (dvd-dy d)      ;Integer
       (dvd-cc d)))    ;String, one of "blue", "red", "green"

;; Template rule used:
;; compound: 4 fields


;; ====================
;; Functions:

;; DVD -> DVD
;; start the world with (main (make-dvd CTR-X CTR-Y 3 -3 "blue"))
;; 
(define (main d)
  (big-bang d                 ; DVD
    (on-tick   move-dvd 0.01) ; DVD -> DVD
    (to-draw   render-dvd)    ; DVD -> Image
    (on-mouse  dvd-to-mouse)  ; DVD Integer Integer MouseEvent -> DVD
    (on-key    reset-dvd)))   ; DVD KeyEvent -> DVD

;; DVD -> DVD
;; produce the next set of coordinates for dvd, change direction if image meets edge
(check-expect (move-dvd (make-dvd (/ LOGO-WIDTH 2) (/ LOGO-HEIGHT 2)  3 3 "blue")) (make-dvd (+ (/ LOGO-WIDTH 2) 3) (+ (/ LOGO-HEIGHT 2) 3) 3 3 "blue"))
(check-expect (move-dvd (make-dvd 550 200 -5 -5 "red")) (make-dvd 545 195 -5 -5 "red"))

(check-expect (move-dvd (make-dvd (- WIDTH (/ LOGO-WIDTH 2) 3) 300 3  -3 "green"))
              (make-dvd (- WIDTH (/ LOGO-WIDTH 2)) 297 -3 -3 "blue"))
(check-expect (move-dvd (make-dvd (+ (/ LOGO-WIDTH 2) 3) (+ (/ LOGO-HEIGHT 2) 3) -3 -3 "blue"))
              (make-dvd (/ LOGO-WIDTH 2) (/ LOGO-HEIGHT 2) 3 3 "green"))

 
;(define (move-dvd d) ...) ;stub

; template taken from data definition

(define (move-dvd d)
  (cond [(and ;hits the corner
          (or (<= (+ (dvd-x d) (dvd-dx d) ) (/ LOGO-WIDTH 2) ) ;crosses left or right edge
              (>= (+ (dvd-x d) (dvd-dx d) (/ LOGO-WIDTH 2) ) WIDTH ))
          (or (<= (+ (dvd-y d) (dvd-dy d) ) (/ LOGO-HEIGHT 2)) ;crosses top or bottom edge
              (>= (+ (dvd-y d) (dvd-dy d) (/ LOGO-HEIGHT 2) ) HEIGHT)))
         (make-dvd (+ (dvd-x d) (dvd-dx d)) (+ (dvd-y d) (dvd-dy d))
                   (- (dvd-dx d)) (- (dvd-dy d))
                   (change-color (change-color (dvd-cc d)) ))]
        [(or (<= (+ (dvd-x d) (dvd-dx d) ) (/ LOGO-WIDTH 2) ) ;crosses left or right edge
             (>= (+ (dvd-x d) (dvd-dx d) (/ LOGO-WIDTH 2) ) WIDTH ))
         (make-dvd (+ (dvd-x d) (dvd-dx d)) (+ (dvd-y d) (dvd-dy d))
                   (- (dvd-dx d)) (dvd-dy d)
                   (change-color (dvd-cc d)))]
        [(or (<= (+ (dvd-y d) (dvd-dy d) ) (/ LOGO-HEIGHT 2)) ;crosses top or bottom edge
             (>= (+ (dvd-y d) (dvd-dy d) (/ LOGO-HEIGHT 2) ) HEIGHT))
         (make-dvd (+ (dvd-x d) (dvd-dx d)) (+ (dvd-y d) (dvd-dy d))
                   (dvd-dx d) (- (dvd-dy d))
                   (change-color (dvd-cc d)))]
        [else (make-dvd (+ (dvd-x d) (dvd-dx d)) (+ (dvd-y d) (dvd-dy d))
                        (dvd-dx d) (dvd-dy d) (dvd-cc d) )]))
        

;; DVD -> Image
;; render the dvd image on top of BACKGROUND
(check-expect (render-dvd (make-dvd 500 500 3 -3 "blue")) (place-image B-LOGO 500 500 BACKGROUND))

;(define (render-dvd d) ...) ;stub
;template taken from current-color data definition

(define (render-dvd d)
  (cond [(string=? "blue" (dvd-cc d)) (place-image B-LOGO (dvd-x d) (dvd-y d) BACKGROUND)]
        [(string=? "red" (dvd-cc d)) (place-image R-LOGO (dvd-x d) (dvd-y d) BACKGROUND)]
        [(string=? "green" (dvd-cc d)) (place-image G-LOGO (dvd-x d) (dvd-y d) BACKGROUND)]))


;; current-color -> current-color
;; swaps logo for logo with next color in sequence
(check-expect (change-color "blue") "red")
(check-expect (change-color "red") "green")
(check-expect (change-color "green") "blue")

(define (change-color cc)
  (cond [(string=? "blue" cc) "red"]
        [(string=? "red" cc) "green"]
        [(string=? "green" cc) "blue"]))

;; DVD KeyEvent -> DVD
;; space bar resets dvd logo to center of screen
(check-expect (reset-dvd (make-dvd 500 500 3 -3 "blue") " ")
              (make-dvd CTR-X CTR-Y 3 -3 "blue"))
(check-expect (reset-dvd (make-dvd 500 500 3 -3 "blue") "a")
              (make-dvd 500 500 3 -3 "blue"))
(check-expect (reset-dvd (make-dvd (/ LOGO-WIDTH 2) (/ LOGO-HEIGHT 2)
                                   5 5 "green") " ")
                         (make-dvd CTR-X CTR-Y 5 5 "green"))
(check-expect (reset-dvd (make-dvd (/ LOGO-WIDTH 2) (/ LOGO-HEIGHT 2)
                                   5 5 "green") "a")
                         (make-dvd (/ LOGO-WIDTH 2) (/ LOGO-HEIGHT 2)
                                   5 5 "green"))

;(define (reset-dvd d ke) ... d) ;stub

;(define (reset-dvd d ke) ;template
;  (cond [(key=? ke " ") (... d)]
;        [else 
;         (... d)]))

(define (reset-dvd d ke)
  (cond [(key=? ke " ")
         (make-dvd CTR-X CTR-Y (dvd-dx d) (dvd-dy d) (dvd-cc d) )]
        [else d]))


;; DVD Integer Integer MouseEvent -> DVD
;; sets DVD coordinates to match the mouse, retains dx and dy
(check-expect (dvd-to-mouse (make-dvd CTR-X CTR-Y 3 -3 "blue")
                            500 500 "button-down")
              (make-dvd 500 500 3 -3 "blue") )
(check-expect (dvd-to-mouse (make-dvd CTR-X CTR-Y 3 -3 "blue")
                            850 850 "button-down") 
              (make-dvd CTR-X CTR-Y 3 -3 "blue") )
(check-expect (dvd-to-mouse (make-dvd CTR-X CTR-Y 3 -3 "blue")
                            0 0 "button-up") 
              (make-dvd CTR-X CTR-Y 3 -3 "blue") )
(check-expect (dvd-to-mouse (make-dvd CTR-X CTR-Y 3 -3 "blue")
                            700 500 "enter")
              (make-dvd CTR-X CTR-Y 3 -3 "blue") )

;(define (dvd-to-mouse d x y me) ...) ;stub

;(define (dvd-to-mouse d x y me) ;template
;  (cond [(mouse=? me "button-down") (... d x y)]
;        [else
;         (... d x y)]))

(define (dvd-to-mouse d x y me)
  (cond [(and (mouse=? me "button-down") (or (<= x (/ LOGO-WIDTH 2))
                                             (>= (+ x LOGO-WIDTH) WIDTH)
                                             (<= y (/ LOGO-HEIGHT 2))
                                             (>= (+ y LOGO-HEIGHT) HEIGHT) ) )
        d]
        [(mouse=? me "button-down")
         (make-dvd x y (dvd-dx d) (dvd-dy d) (dvd-cc d) )]
        [else d]))
