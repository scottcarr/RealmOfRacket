#lang racket
(require 2htdp/image 2htdp/universe)

; graphics
(define SIDE 75)
(define OFFSET0 (* 2 SIDE))
(define ROTATION 30)
(define DICE-IMAGE (bitmap "graphics/dice1.png"))
(define FOCUS (rotate ROTATION (regular-polygon SIDE 6 "outline" "black")))
(define COLORS 
  (list (make-color 255 0 0 100) 
        (make-color 0 255 0 100) 
        (make-color 0 0 255 100)))
(define HEX 6)
(define (hexagon color)
  (rotate ROTATION (regular-polygon SIDE HEX "solid" color)))

; game constants
(define PLAYER# 2)
(define DICE# 3)
(define BOARD 2)
(define GRID (* BOARD BOARD))
(define INIT-PLAYER 0)
(define INIT-SPARE-DICE 10)
(define X-OFFSET (image-width (hexagon "black")))
(define Y-OFFSET (* (image-height (hexagon "black")) 3/4))

(define HEIGHT 600)
(define TEXT-SIZE 25)
(define TEXT-COLOR "black")
(define INSTRUCT 
  "← and → to move among territories, <enter> to mark, <d> to unmark, and <p> to pass")
(define INSTRUCTIONS (text INSTRUCT TEXT-SIZE TEXT-COLOR))
(define WIDTH (+ (image-width INSTRUCTIONS) 50))

(define AI-TURN "It's the Mighty AI's turn")
(define YOUR-TURN "It's your turn")
(define INFO-X-OFFSET 100)
(define INFO-Y-OFFSET 50)
(define AI 1)


; structs
(struct action (player position) #:transparent)
(struct dice-world (src board gt) #:transparent)
(struct territory (index player dice x y) #:transparent)
(struct game (board player moves) #:transparent)
(struct move (action gt) #:transparent)
 
(define (territory-set-dice t d)
  (territory (territory-index t) (territory-player t) d (territory-x t) (territory-y t)))

(define (territory-set-player t p)
  (territory (territory-index t) p (territory-dice t) (territory-x t) (territory-y t)))

(define (draw-focus marked? p-in-focus p t-image)
  (if (or (and (not marked?) (= p-in-focus p))
          (and marked? (not (= p-in-focus p))))
      (overlay FOCUS t-image)
      (t-image)))

(define (get-dice-image n)
  DICE-IMAGE)

(define (switch player)
  (modulo (add1 player) PLAYER#))

(define (add-dice-to t)
  (territory-set-dice t (add1 (territory-dice t))))

(define (add b x)
  (if b empty (list x)))

#|
Each hex has six possible neighbors:


      0  / \ 1
    2   |   | 3
     4   \ /  5
Our board looks like:

     0 1
    2 3 
|#
(define (odd-row pos top? bottom? right? left?)
  (append (add top?                (- pos BOARD))        ; upper right
	  (add (or bottom? right?) (add1 (+ pos BOARD))) ; lower right
	  (add (or top? left?)     (sub1 (- pos BOARD))) ; upper left
	  (add bottom?             (+ pos BOARD))        ; lower left
	  (add right?              (add1 pos))           ; right
	  (add left?               (sub1 pos))))         ; left

(define (even-row pos top? bottom? right? left?)
  (append (add (or top? right?)    (- pos BOARD))        ; upper right
	  (add (or bottom? right?) (add1 (+ pos BOARD))) ; lower right
	  (add top?                (sub1 (- pos BOARD))) ; upper left
	  (add bottom?             (+ pos BOARD))        ; lower left 
	  (add right?              (add1 pos))           ; right
	  (add left?               (sub1 pos))))         ; left


(define (neighbors pos)
  (define top? (< pos BOARD))
  (define bottom? (= (get-row pos) (sub1 BOARD)))
  (define even-row? (zero? (modulo (get-row pos) 2)))
  (define right? (zero? (modulo (add1 pos) BOARD)))
  (define left? (zero? (modulo pos BOARD)))
  (if even-row?
      (even-row pos top? bottom? right? left?)
      (odd-row pos top? bottom? right? left?)))
  
(define (distribute board player spare-dice)
  (for/fold ([dice spare-dice] [new-board '()]) ([t board])
    (if (and (= (territory-player t) player)
	     (< (territory-dice t) DICE#)
	     (not (zero? dice)))
	(values (- dice 1) (cons (add-dice-to t) new-board))
	(values dice (cons t new-board)))))

(define (attackable? board player src dst)
  (define dst-t
    (findf (lambda (t) (= (territory-index t) dst)) board))
  (and dst-t
       (= (territory-player src) player)
       (not (= (territory-player dst-t) player))
       (> (territory-dice src) (territory-dice dst-t))))

(define (execute board player src dst dice)
  (for/list ([t board])
    (define idx (territory-index t))
    (cond [(= idx src) (territory-set-dice t 1)]
          [(= idx dst)
           (define s (territory-set-dice t (- dice 1)))
           (territory-set-player s player)]
          [else t])))

(define (game-tree board player dice)
  ;; create tree of attacks from this positiion; add passing move
  (define (attacks board)
    (for*/list ([src board]
		[dst (neighbors (territory-index src))]
                #:when (attackable? board player src dst))
      (define from (territory-index src))
      (define dice (territory-dice src))
      ;(display from)
      ;(display dice)
      (define newb (execute board player from dst dice))
      ;(define more (cons (passes newb) (attacks newb)))
      (define attacks-from-newb 
        (game newb player (cons (passes newb) (attacks newb))))
      (move (list from dst) attacks-from-newb)))
  ;; create a passing move and the rest of the game tree
  (define (passes board)
    (define-values (new-dice newb) (distribute board player dice))
    (move '() (game-tree newb (switch player) new-dice)))
  ;; -- START: --
  (game board player (attacks board)))
 
;; rendering

(define (whose-turn player)
  (if (= player AI) AI-TURN YOUR-TURN))

(define (add-player-info player s)
  (define str (whose-turn player))
  (define txt (text str TEXT-SIZE TEXT-COLOR))
  (place-image txt (- WIDTH INFO-X-OFFSET) INFO-Y-OFFSET s))

(define (draw-dice-world w)
  (add-player-info
   (game-player (dice-world-gt w))
   (add-board-to-scene w (ISCENE))))

(define (draw-dice dice)
  (define first-dice (get-dice-image 0))
  (define height-dice (image-height first-dice))
  (for/fold ([s first-dice]) ([i (- dice 1)])
    (define dice-image (get-dice-image (+ i 1)))
    (define y-offset (* height-dice (+ .5 (* i .25))))
    (overlay/offset s 0 y-offset dice-image)))

(define (color-chooser n)
  (list-ref COLORS n))

(define (add-territory t image scene)
  (place-image image (territory-x t) (territory-y t) scene))

(define (draw-territory t)
  (define color (color-chooser (territory-player t)))
  (overlay (hexagon color) (draw-dice (territory-dice t))))

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

(define (ISCENE)
  (define mt (PLAIN))
  (when (or (> (image-width mt) 1280) (> (image-height mt) 800))
    (error 'scene "it is impossible to draw a ~s x ~s game scene for a 1280 x 800 laptop screen" (image-width mt) (image-height mt)))
  (place-image INSTRUCTIONS (* .5 WIDTH) (* .9 HEIGHT) mt))

(define (dice)
  (add1 (random DICE#)))

(define (get-row pos)
  (quotient pos BOARD))

(define (get-x n)
  (+ OFFSET0
     (if (odd? (get-row n)) 0 (/ X-OFFSET 2))
     (* X-OFFSET (modulo n BOARD))))

(define (get-y n)
  (+ OFFSET0 (* Y-OFFSET (get-row n))))

(define (territory-build)
  (for/list ([n (in-range GRID)])
    (territory n (modulo n PLAYER#) (dice) (get-x n) (get-y n))))

(define (create-world-of-dice-and-doom)
  (define board (territory-build))
  (define gamet (game-tree board INIT-PLAYER INIT-SPARE-DICE))
  (define new-world (dice-world #f board gamet))
  new-world)

(module+ test
  (require rackunit rackunit/text-ui)

  ;; Neighbors
  (check-equal? (neighbors 2) '(0 3))
  (check-equal? (neighbors 0) '(3 2 1))
  (check-equal? (neighbors 1) '(3 0)) 
  (check-equal? (neighbors 3) '(1 0 2))

    ;; legal?
  (check-true 
   (and (attackable? (list (territory 0 0 2 9 0) (territory 3 1 1 9 0)) 0 (territory 0 0 2 9 0) 3) #t))
  (check-false
   (attackable? (list (territory 0 0 2 9 0) (territory 3 1 1 9 0)) 0 (territory 0 0 2 9 0) 0))
  (check-false
   (attackable? (list (territory 0 0 2 9 0) (territory 5 1 1 9 0)) 1 (territory 0 0 2 9 0) 5)))
