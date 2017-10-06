#lang racket
(require 2htdp/image 2htdp/universe)

; constant defs
(define MAX-HEALTH 35)
(define MAX-AGILITY 35)
(define MAX-STRENGTH 35)
(define HEALTH-COLOR "green")
(define AGILITY-COLOR "blue")
(define STRENGTH-COLOR "red")
(define ATTACKS# 3)
(define CLUB-STRENGTH 5)
(define MONSTER# 10)
(define MESSAGE-SIZE 10)
(define MESSAGE-COLOR "green")
(define REMAINING "Turns: ")
(define INSTRUCTION-TEXT-SIZE 10)
(define ATTACK-COLOR "red")
(define INSTRUCTION-TEXT "adfdasfdf")
(define V-SPACER (rectangle 0 10 "solid" "white"))
(define H-SPACER (rectangle 10 0 "solid" "white"))
(define HEALTH-BAR-WIDTH 100)
(define HEALTH-BAR-HEIGHT 10)
(define HEALTH-SIZE 30)
(define STRENGTH "Strength")
(define HEALTH "Health")
(define AGILITY "Agility")
(define PLAYER-IMAGE (bitmap "images/player.bmp"))

; struct defs
(struct orc-world (player lom attack# target instructions) #:mutable)
(struct monster (image [health #:mutable]))
(struct orc monster (club))
(struct hydra monster ())
(struct slime monster (sliminess))
(struct brigand (health))
(struct player (health agility strength) #:mutable)

; misc methods

(define (message str)
  (text str MESSAGE-SIZE MESSAGE-COLOR))

(define (interval+ old delta mx)
  (define new (min (+ old delta) mx))
  (if (> 0 new) 0 new))

; player methods
(define (player-update! setter selector mx)
  (lambda (player delta)
    (setter player (interval+ (selector player) delta mx))))

(define (player-health+ player delta)
  (player-update! set-player-health! player-health MAX-HEALTH))

(define (initialize-player)
  (player MAX-HEALTH MAX-AGILITY MAX-STRENGTH))

(define (random+ n)
  (add1 (random n)))

(define (random-quotient x y)
  (define div (quotient x y))
  (if (> 0 div) 0 (random+ (add1 div))))

(define (random-number-of-attacks p)
  (random-quotient (player-agility p) ATTACKS#))

; render methods

(define (status-bar v-current v-max color label)
  (define w (* (/ v-current v-max) HEALTH-BAR-WIDTH))
  (define f (rectangle w HEALTH-BAR-HEIGHT 'solid color))
  (define b (rectangle HEALTH-BAR-WIDTH HEALTH-BAR-HEIGHT 'outline color))
  (define bar (overlay/align "left" "top" f b))
  (beside bar H-SPACER (text label HEALTH-SIZE color)))

(define (render-player p)
  (define s (player-strength p))
  (define a (player-agility p))
  (define h (player-health p))
  (above/align
   "left"
   (status-bar s MAX-STRENGTH STRENGTH-COLOR STRENGTH)
   V-SPACER
   (status-bar a MAX-AGILITY AGILITY-COLOR AGILITY)
   V-SPACER
   (status-bar h MAX-HEALTH HEALTH-COLOR HEALTH)
   V-SPACER V-SPACER V-SPACER
   PLAYER-IMAGE))

(define (render-orc-world w t additional-text)
  (define i-player (render-player (orc-world-player w)))
  (define i-monster (render-monsters (orc-world-lom w) t))
  (above V-SPACER
         (beside H-SPACER
                 i-player
                 H-SPACER H-SPACER H-SPACER
                 (above i-monster
                        V-SPACER V-SPACER V-SPACER
                        additional-text)
                 H-SPACER)
         V-SPACER))

(define (render-orc-battle w)
  (render-orc-world w (orc-world-target w) (instructions w)))

; world methods

(define (instructions w)
  (define na (number->string (orc-world-attack# w)))
  (define ra (string-append REMAINING na))
  (define txt (text ra INSTRUCTION-TEXT-SIZE ATTACK-COLOR))
  (above txt INSTRUCTION-TEXT))
  
(define (initialize-monsters)
  (build-list
   MONSTER#
   (lambda (_)
     (define health (random+ MONSTER-HEALTH))
     (case (random 4)
       [(0) (orc ORC-IMAGE health (random+ CLUB-STRENGTH))]
       [(1) (hydra HYDRA-IMAGE health)]
       [(2) (slime SLIME-IMAGE (random+ SLIMINESS))]
       [(3) (brigand BRIGAND-IAMGE health)]))))
       
(define (monster-alive? m)
  (= (monster-health m) 0))
  
(define (all-dead? lom)
  (not (ormap monster-alive? lom)))
  
(define (win? w)
  (all-dead? (orc-world-lom w)))

(define (player-dead? p)
  (or (= (player-health 0))
      (= (player-agility 0))
      (= (player-strength 0))))
  
(define (lose? w)
  (define life (player-health (orc-world-player w)))
  (= life 0))


(define (end-of-orc-battle? w)
  (or (win? w) (lose? w)))

(define (initialize-orc-world)
  (define player0 (initialize-player))
  (define lom0 (initialize-monsters))
  (orc-world player0 lom0 (random-number-of-attacks player0) 0))

(define (start)
  (big-bang (initialize-orc-world)
            (on-key player-acts-on-monsters)
            (to-draw render-orc-battle)
            (stop-when end-of-orc-battler? render-the end)))
