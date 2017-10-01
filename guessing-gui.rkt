#lang racket
(require 2htdp/universe 2htdp/image)
(define TEXT-SIZE 20)
(define TEXT-X 30)
(define TEXT-UPPER-Y 0)
(define TEXT-LOWER-Y 250)
(define WIDTH 500)
(define HEIGHT 300)
(define SIZE 100)
(struct interval (small big))
(define HELP-TEXT
  (text "↑ for larger numbers ↓ for smaller ones"
        TEXT-SIZE
        "blue"))
(define HELP-TEXT2
  (text "Press = when your number is guessed; q to quit."
        TEXT-SIZE
        "blue"))
(define COLOR "red")
(define MT-SC
  (place-image/align
   HELP-TEXT TEXT-X TEXT-UPPER-Y "left" "top"
   (place-image/align
    HELP-TEXT2 TEXT-X TEXT-LOWER-Y "left" "bottom"
    (empty-scene WIDTH HEIGHT))))
(define (guess w)
  (quotient (+ (interval-small w) (interval-big w)) 2))
(define (smaller w)
  (interval (interval-small w)
            (max (interval-small w) (sub1 (guess w)))))
(define (bigger w)
  (interval (min (interval-big w) (add1 (guess w)))
            (interval-big w)))
(define (render w)
  (overlay (text (number->string (guess w)) SIZE COLOR) MT-SC))
(define (single? w)
  (= (interval-small w) (interval-big w)))
(define (correct-guess w)
  (interval (guess w) (guess w)))
(define (deal-with-guess w key)
  (cond [(key=? key "up") (bigger w)]
        [(key=? key "down") (smaller w)]
        [(key=? key "q") (stop-with w)]
        [(key=? key "=") (stop-with (correct-guess w))]
        [else w]))
(define (adorn-number n)
  (define as-str (number->string n))
  (string-append (string-append "*" as-str) "*" ))
(define (render-last-scene w)
  (define end-text
    (cond [(single? w) (adorn-number (interval-small w))]
         [else "End"]))
  (overlay (text end-text SIZE COLOR) MT-SC))

(define (start lower upper)
  (big-bang (interval lower upper)
  (on-key deal-with-guess)
  (to-draw render)
  (stop-when single? render-last-scene)))
(start 1 100)