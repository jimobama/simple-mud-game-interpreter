#lang racket
(require "game-window.rkt")

;; the function start the game
(define (start-game)
(reset)
(define game (new  game-window% 
                          [label "MONSTER HUNTER 0.1v"]                      
                          [width WIDTH]
                          [height (- HEIGHT 40)]))
  ;;the game window to be visible to users
   (send game show #t)
  )
 
;; start the game 
(start-game)
