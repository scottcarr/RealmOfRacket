#lang racket
(require 2htdp/image 2htdp/universe)

; graphics
(define SIDE 75)
(define ROTATION 30)
(define DICE-IMAGE (bitmap "graphics/dice1.png"))
(define FOCUS (rotate ROTATION (regular-polygon SIDE 6 "outline" "black")))

; game constants
(define PLAYER# 2)
(define DICE# 3)
(define BOARD 2)
(define GRID (* BOARD BOARD))
(define INIT-PLAYER 0)
(define INIT-SPARE-DICE 10)

(define INSTRUCTIONS (text INSTRUCT TEXT-SIZE TEXT-COLOR))
(define WIDTH (+ (image-width INSTRUCTIONS) 50))
(define HEIGHT 600)
(define TEXT-SIZE 25)
(define TEXT-COLOR "black")
(define INSTRUCT 
  "← and → to move among territories, <enter> to mark, <d> to unmark, and <p> to pass")
(define AI-TURN "It's the Mighty AI's turn")
(define YOUR-TURN "It's your turn")
(define INFO-X-OFFSET 100)
(define INFO-Y-OFFSET 50)

; structs
(struct action (player position))
(struct dice-world (src board gt))
(struct territory (index player dice x y))
(struct game (board player moves))
(struct move (action gt))

(define (draw-focus marked? p-in-focus p t-image)
  (if (or (and (not marked?) (= p-in-focus p))
          (and marked? (not (= p-in-focus p))))
      (overlay FOCUS t-image)
      (t-image)))

(define (add-board-to-scene w s)
  (define board (dice-world-board w))
  (define player (game-player (dice-world-gt w)))
  (define focus? (dice-world-src w))
  (define trtry1 (first board))
  (define p-focus (territory-player trtry1))
  (define t-image (draw-territory trtry1))
  (define image (draw-focus focus? p-focus player t-image))
  (define base-s (add-territory trtry1 image s))
  (for/fold ([s base-s]) ([t (rest board)])
    (add-territory t (draw-territory t) s)))

(define (sum-territory board player)
  (for/fold ([result 0]) ([t board])
    (if (= (territory-player t) player) (+ result 1) result)))
        
(define (winners board)
  (for/fold ([best 0][winners '()]) ([p PLAYER#])
    (define p-score (sum-territory board p))
    (cond [(> p-score best) (values p-score (list p))]
          [(< p-score best) (values best winners)]
          [(= p-score best) (values best (cons p winners))])))

(define (won board)
  (define-values (best-score w) (winners board))
  (if (cons? (rest w)) "It's a tie" "You won."))


(define (draw-end-of-dice-world w)
  (define board (dice-world-board w))
  (define message (text (won board) TEXT-SIZE TEXT-COLOR))
  (define background (add-board-to-scene w (PLAIN)))
  (overlay message background))

(define (PLAIN)
  (define iw (image-width INSTRUCTIONS))
  (define bw (* SIDE 2 BOARD))
  (set! WIDTH  (+ (max iw bw) 50))
  (set! HEIGHT (+ (* SIDE 2 BOARD) 50))
  (empty-scene WIDTH HEIGHT))