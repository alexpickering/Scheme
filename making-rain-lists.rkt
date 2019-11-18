(require 2htdp/image)
(require 2htdp/universe)

;; making-rain-filtered-starter.rkt

;; Create a rain drop wherever there is a mouse click.

;; =================
;; Constants:

(define WIDTH  300)
(define HEIGHT 300)

(define SPEED 1)

(define DROP (ellipse 4 8 "solid" "blue"))

(define MTS (rectangle WIDTH HEIGHT "solid" "light blue"))


;; =================
;; Data definitions:

(define-struct drop (x y))
;; Drop is (make-drop Integer Integer)
;; interp. A raindrop on the screen, with x and y coordinates.

(define D1 (make-drop 10 30))

#;
(define (fn-for-drop d)
  (... (drop-x d) 
       (drop-y d)))

;; Template Rules used:
;; - compound: 2 fields


;; ListOfDrop is one of:
;;  - empty
;;  - (cons Drop ListOfDrop)
;; interp. a list of drops

(define LOD1 empty)
(define LOD2 (cons (make-drop 10 20) (cons (make-drop 3 6) empty)))

#;
(define (fn-for-lod lod)
  (cond [(empty? lod) (...)]
        [else
         (... (fn-for-drop (first lod))
              (fn-for-lod (rest lod)))]))

;; Template Rules used:
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Drop ListOfDrop)
;; - reference: (first lod) is Drop
;; - self reference: (rest lod) is ListOfDrop

;; =================
;; Functions:

;; ListOfDrop -> ListOfDrop
;; start rain program by evaluating (main empty)
(define (main lod)
  (big-bang lod
    (on-mouse handle-mouse)   ; ListOfDrop Integer Integer MouseEvent -> ListOfDrop
    (on-tick  next-drops)     ; ListOfDrop -> ListOfDrop
    (to-draw  render-drops))) ; ListOfDrop -> Image


;; ListOfDrop Integer Integer MouseEvent -> ListOfDrop
;; if mevt is "button-down" add a new drop at that position
(check-expect (handle-mouse empty 220 70 "button-down")
              (cons (make-drop 220 70) empty))
(check-expect (handle-mouse empty 100 100 "button-up")
               empty)


;(define (handle-mouse lod x y mevt) empty) ; stub

;(define (handle-mouse ws x y me)           ; template
;  (cond [(mouse=? me "button-down") (... ws x y)]
;        [else
;         (... ws x y)]))

(define (handle-mouse lod x y me)
  (cond [(mouse=? me "button-down") (cons (make-drop x y) lod)]
        [else lod]))


;; ListOfDrop -> ListOfDrop
;; produce filtered and ticked list of drops
(check-expect (next-drops empty) empty)
(check-expect (next-drops (cons (make-drop 10 20)
                                (cons (make-drop 3 6)
                                      empty)))
              (cons (make-drop 10 (+ 20 SPEED))
                    (cons (make-drop 3 (+ 6 SPEED))
                          empty)))
(check-expect (next-drops (cons (make-drop 50 50)
                                (cons (make-drop 240 90)
                                      (cons (make-drop 14 10)
                                            empty))))
              (cons (make-drop 50 (+ 50 SPEED))
                    (cons (make-drop 240 (+ 90 SPEED))
                          (cons (make-drop 14 (+ 10 SPEED))
                                empty))))

;(define (next-drops lod) empty) ; stub

(define (next-drops lod)
  (cond [(empty? lod) empty]
        [else (if (off-screen? (move-drop (first lod)))
                  (next-drops (rest lod))
                  (cons (move-drop (first lod))
                        (next-drops (rest lod))))]))


;; Drop -> Boolean
;; returns true if drop is off-screen
(check-expect (off-screen? (make-drop 25 50))
              (> (+  50 (/ (image-height DROP) 2)) HEIGHT))
(check-expect (off-screen? (make-drop 125 300))
              (> (- 300 (/ (image-height DROP) 2)) HEIGHT))
(check-expect (off-screen? (make-drop 125 320))
              (> (- 320 (/ (image-height DROP) 2)) HEIGHT))

;(define (off-screen? d) false) ;stub

(define (off-screen? d)
  (> (- (drop-y d)
        (/ (image-height DROP) 2))
     HEIGHT))


;; Drop -> Drop
;; updates drop y-axis by SPEED
(check-expect (move-drop (make-drop 25 50))
              (make-drop 25 (+ 50 SPEED)))
(check-expect (move-drop (make-drop 0 0))
              (make-drop 0 (+ 0 SPEED)))

; (define (move-drop d) d) ;stub

(define (move-drop d)
  (make-drop (drop-x d)
             (+ (drop-y d) SPEED)))


;; ListOfDrop -> Image
;; Render the drops onto MTS
(check-expect (render-drops empty) MTS)
(check-expect (render-drops (cons (make-drop 50 50) empty))
              (place-image DROP 50 50 MTS))
(check-expect (render-drops (cons (make-drop 50 50)
                                  (cons (make-drop 240 90)
                                        (cons (make-drop 14 10)
                                              empty))))
              (place-image DROP 50 50 
                           (place-image DROP 240 90 
                                        (place-image DROP  14 10 MTS))))

;(define (render-drops lod) MTS) ; stub

(define (render-drops lod)
  (cond [(empty? lod) MTS]
        [else
         (place-image DROP (drop-x (first lod)) (drop-y (first lod))
                      (render-drops (rest lod)))]))
