(require 2htdp/image)
;; Problem 2 from How to Design Simple Data course offered here:
;; https://courses.edx.org/courses/course-v1:UBCx+HtC1x+2T2017/course/

; Problem 2:
; Consider a test tube filled with solid blobs and bubbles.  Over time the
; solids sink to the bottom of the test tube, and as a consequence the bubbles
; percolate to the top.  Let's capture this idea in BSL.
; 
; Complete the design of a function that takes a list of blobs and sinks each
; solid blob by one. It's okay to assume that a solid blob sinks past any
; neighbor just below it.
; 
; To assist you, we supply the relevant data definitions.


;; Blob is one of:
;; - "solid"
;; - "bubble"
;; interp.  a gelatinous blob, either a solid or a bubble
;; Examples are redundant for enumerations
#;
(define (fn-for-blob b)
  (cond [(string=? b "solid") (...)]
        [(string=? b "bubble") (...)]))

;; Template rules used:
;; - one-of: 2 cases
;; - atomic distinct: "solid"
;; - atomic distinct: "bubble"


;; ListOfBlob is one of:
;; - empty
;; - (cons Blob ListOfBlob)
;; interp. a sequence of blobs in a test tube, listed from top to bottom.
(define LOB0 empty) ; empty test tube
(define LOB2 (cons "solid" (cons "bubble" empty))) ; solid blob above a bubble

#;
(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else
         (... (fn-for-blob (first lob))
              (fn-for-lob (rest lob)))]))

;; Template rules used
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: 2 fields
;; - reference: (first lob) is Blob
;; - self-reference: (rest lob) is ListOfBlob


;; ListOfBlob -> ListOfBlob
;; produce a list of blobs that sinks the given solid blobs by one
(check-expect (sink empty) empty)
(check-expect (sink (cons "bubble" (cons "solid" (cons "bubble" empty))))
              (cons "bubble" (cons "bubble" (cons "solid" empty))))
(check-expect (sink (cons "solid" (cons "solid" (cons "bubble" empty))))
              (cons "bubble" (cons "solid" (cons "solid" empty))))
(check-expect (sink (cons "solid" (cons "bubble" (cons "bubble" empty))))
              (cons "bubble" (cons "solid" (cons "bubble" empty))))
(check-expect (sink (cons "solid" (cons "bubble" (cons "solid" empty))))
              (cons "bubble" (cons "solid" (cons "solid" empty))))
(check-expect (sink (cons "bubble" (cons "solid" (cons "solid" empty))))
              (cons "bubble" (cons "solid" (cons "solid" empty))))
(check-expect (sink (cons "solid"
                          (cons "solid"
                                (cons "bubble" (cons "bubble" empty)))))
              (cons "bubble" (cons "solid" 
                                   (cons "solid" (cons "bubble" empty)))))

;(define (sink lob) empty) ; stub

(define (sink lob)
  (cond [(empty? lob) empty]
        [(bubbles? (rest lob))
         (if (solid? (first lob))
             (cons "bubble" (replace-next-bubble (rest lob)))
             (cons (first lob) (sink (rest lob))))]
        [else lob]))


;; ListOfBlob -> ListOfBlob
;; swaps last solid in consecutive solid set to "bubble"
(check-expect (replace-next-bubble (cons "bubble" (cons "bubble" empty)))
              (cons "solid" (cons "bubble" empty)))
(check-expect (replace-next-bubble (cons "solid" (cons "bubble" empty)))
              (cons "solid" (cons "solid" empty)))
(check-expect (replace-next-bubble (cons "solid" (cons "bubble" (cons "bubble" empty))))
              (cons "solid" (cons "solid" (cons "bubble" empty))))

;(define (replace-next-bubble lob) ...) ;stub

(define (replace-next-bubble lob)
  (if (bubble? (first lob))
      (cons "solid" (sink (rest lob)))
      (cons (first lob) (replace-next-bubble (rest lob)))))


;; ListOfBlob -> Boolean
;; returns true if list contains one or more bubbles
(check-expect (bubbles? empty) false)
(check-expect (bubbles? (cons "bubble" empty)) true)
(check-expect (bubbles? (cons "solid" (cons "bubble" empty))) true)
(check-expect (bubbles? (cons "solid" (cons "solid" empty))) false)

;(define (bubbles? lob) false) ;stub

(define (bubbles? lob)
  (cond [(empty? lob) false]
        [(bubble? (first lob)) true]
        [else (bubbles? (rest lob))]))


;; Blob -> Boolean
;; if Blob is "bubble", returns true
(check-expect (bubble? "bubble") true)
(check-expect (bubble? "solid") false)

; (define (bubble? blob) false) ;stub

(define (bubble? b)
  (string=? b "bubble"))


;; Blob -> Boolean
;; if Blob is "solid", returns true
(check-expect (solid? "bubble") false)
(check-expect (solid? "solid") true)

; (define (solid? blob) false) ;stub

(define (solid? b)
  (string=? b "solid"))
