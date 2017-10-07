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
(define MONSTER# 12)
(define MESSAGE-SIZE 10)
(define MESSAGE-COLOR "green")
(define REMAINING "Turns: ")
(define INSTRUCTION-TEXT-SIZE 20)
(define ATTACK-COLOR "red")
(define INSTRUCTIONS-1 "Select a monster using the arrow keys")
(define INSTRUCTIONS-2 "S: Stab, F: Fail, H: Heal")
(define INSTRUCTION-TEXT
  (above 
   (text INSTRUCTIONS-2 (- INSTRUCTION-TEXT-SIZE 2) "blue")
   (text INSTRUCTIONS-1 (- INSTRUCTION-TEXT-SIZE 4) "blue")))
(define V-SPACER (rectangle 0 10 "solid" "white"))
(define H-SPACER (rectangle 10 0 "solid" "white"))
(define HEALTH-BAR-WIDTH 100)
(define HEALTH-BAR-HEIGHT 10)
(define HEALTH-SIZE 30)
(define STRENGTH "Strength")
(define HEALTH "Health")
(define AGILITY "Agility")
(define PLAYER-IMAGE (bitmap "images/player.bmp"))
(define ORC-IMAGE (bitmap "images/orc.gif"))
(define HYDRA-IMAGE (bitmap "images/hydra.png"))
(define SLIME-IMAGE (bitmap "images/slime.bmp"))
(define BRIGAND-IMAGE (bitmap "images/brigand.bmp"))
(define SLIMINESS 3)
(define MONSTER-HEALTH0 50)
(define MONSTER-HEALTH 50)
(define PER-ROW 4)
(define HEALING 5)
(define TARGET (rectangle 80 80 'outline "red"))
(define DEAD-TEXT "DEAD") ; overlayed over a monster
(define MONSTER-COLOR "red")
(define END-GAME-TEXT-SIZE 25)


; brigand damage constants
(define HEALTH-DAMAGE 1)
(define AGILITY-DAMAGE 1)
(define STRENGTH-DAMAGE 1)

; player damage contants
(define STAB-DAMAGE 3)
(define FAIL-DAMAGE 1)

; struct defs
(struct orc-world (player lom attack# target instructions) #:mutable #:transparent)
(struct monster (image [health #:mutable]) #:transparent)
(struct orc monster (club) #:transparent)
(struct hydra monster () #:transparent)
(struct slime monster (sliminess) #:transparent)
(struct brigand monster () #:transparent)
(struct player (health agility strength) #:mutable #:transparent)

; misc methods

(define (message str)
  (text str MESSAGE-SIZE MESSAGE-COLOR))

(define (interval+ old delta mx)
  (define new (min (+ old delta) mx))
  (if (> 0 new) 0 new))

(define (interval- old delta)
  (define new (- old delta))
  (if (> 0 new) 0 new))

; player methods
(define (player-update! setter selector mx)
  (lambda (player delta)
    (setter player (interval+ (selector player) delta mx))))

(define (player-health+ player delta)
  (player-update! set-player-health! player-health MAX-HEALTH))

(define (player-agility+ player delta)
  (player-update! set-player-agility! player-agility MAX-AGILITY))

(define (player-strength+ player delta)
  (player-update! set-player-strength! player-strength MAX-STRENGTH))

(define (initialize-player)
  (player MAX-HEALTH MAX-AGILITY MAX-STRENGTH))

(define (random+ n)
  (add1 (random n)))

(define (random-  n)
  (random n))

(define (random-quotient x y)
  (define div (quotient x y))
  (if (> 0 div) 0 (random+ (add1 div))))

(define (random-number-of-attacks p)
  (random-quotient (player-agility p) ATTACKS#))

; render methods

(define (arrange lom)
  (cond
    [(empty? lom) empty-image]
    [else (define r (apply beside (take lom PER-ROW)))
          (above r (arrange (drop lom PER-ROW)))]))

(define (render-monsters lom with-target)
  ;; the currently targeted monster (if needed)
  (define target
    (if (number? with-target)
        (list-ref lom with-target)
        'a-silly-symbol-that-cannot-be-eq-to-an-orc))

  (define (render-one-monster m)
    (define image
      (if (eq? m target)
          (overlay TARGET (monster-image m))
          (monster-image m)))
    (define health (monster-health m))
    (define health-bar
      (if (= health 0)
          (overlay (text DEAD-TEXT END-GAME-TEXT-SIZE "black") (status-bar 0 1 'white ""))
          (status-bar health MONSTER-HEALTH0 MONSTER-COLOR "")))
    (above health-bar image))
  (arrange (map render-one-monster lom)))

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

(define (render-the-end w)
  (overlay (text "The end" END-GAME-TEXT-SIZE "black")
  (render-orc-battle w)))

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
       [(2) (slime SLIME-IMAGE health (random+ SLIMINESS))]
       [(3) (brigand BRIGAND-IMAGE health)]))))

(define (decrease-attack# w)
  (set-orc-world-attack#! w (sub1 (orc-world-attack# w))))

(define (damage-monster m delta)
  (set-monster-health! m (interval- (monster-health m) delta)))

(define (current-target w)
  (list-ref (orc-world-lom w) (orc-world-target w)))

(define (move-target w delta)
  (define new (+ (orc-world-target w) delta))
  (set-orc-world-target! w (modulo new MONSTER#)))

(define (give-monster-turn-if-attack#=0 w)
  (when (zero? (orc-world-attack# w))
    (define player (orc-world-player w))
    (all-monsters-attack-player player (orc-world-lom w))
    (set-orc-world-attack#! w (random-number-of-attacks player))))

(define (all-monsters-attack-player player lom)
  (define (one-monster-attacks-player m)
    (cond
      [(orc? m)
       (player-health+ player (random- (orc-club m)))]
      [(hydra? m)
       (player-health+ player (random- (monster-health m)))]
      [(slime? m)
       (player-health+ player -1)
       (player-agility+ player
                        (random- (slime-sliminess m)))]
      [(brigand? m)
       (case (random 3)
         [(0) (player-health+ player HEALTH-DAMAGE)]
         [(1) (player-agility+ player AGILITY-DAMAGE)]
         [(2) (player-strength+ player STRENGTH-DAMAGE)])]))
  (define live-monster (filter monster-alive? lom))
  (for-each one-monster-attacks-player live-monster))

(define (monster-alive? m)
  (not (= (monster-health m) 0)))
  
(define (all-dead? lom)
  (not (ormap monster-alive? lom)))
  
(define (win? w)
  ;(display "win\n")
  (all-dead? (orc-world-lom w)))

#|
(define (player-dead? p)
  (or (= (player-health 0))
      (= (player-agility 0))
      (= (player-strength 0))))
|#
  
(define (lose? w)
  ;(display "lose\n")
  (define life (player-health (orc-world-player w)))
  (= life 0))

(define (end-turn w)
  (set-orc-world-attack#! w 0))

(define (heal w)
       (decrease-attack# w)
       (player-health+ (orc-world-player w) HEALING))

(define (stab w)
  (decrease-attack# w)
  (define target
    (list-ref (orc-world-lom w) (orc-world-target w)))
  (define damage
    (random-quotient (player-strength (orc-world-player w))
                     STAB-DAMAGE))
  (damage-monster target damage))

(define (flail w)
  (decrease-attack# w)
  (define target (current-target w))
  (define alive (filter monster-alive? (orc-world-lom w)))
  (define pick#
    (min
     (random-quotient (player-strength (orc-world-player w))
                      FAIL-DAMAGE)
     (length alive)))
  (define getem (const target (take alive pick#)))
  (for-each (lambda (m) (damage-monster m 1)) getem))

(define (end-of-orc-battle? w)
  (or (win? w) (lose? w)))

(define (initialize-orc-world)
  (define player0 (initialize-player))
  (define lom0 (initialize-monsters))
  (orc-world player0 lom0 (random-number-of-attacks player0) 0 ""))

(define (player-acts-on-monsters w k)
  (cond
    [(zero? (orc-world-attack# w)) (void)]
    [(key=? "s" k) (stab w)]
    [(key=? "h" k) (heal w)]
    [(key=? "f" k) (flail w)]
    [(key=? "e" k) (end-turn w)]
    [(key=? "n" k) (initialize-orc-world)]
    [(key=? "right" k) (move-target w +1)]
    [(key=? "left" k) (move-target w -1)]
    [(key=? "down" k) (move-target w (+ PER-ROW))]
    [(key=? "up" k) (move-target w (- PER-ROW))])
  (give-monster-turn-if-attack#=0 w)
  w)

(define (start)
  (big-bang (initialize-orc-world)
            (on-key player-acts-on-monsters)
            (to-draw render-orc-battle)
            (stop-when end-of-orc-battle? render-the-end)))
