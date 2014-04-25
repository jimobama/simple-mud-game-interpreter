#lang racket
(require "game-timer.rkt")
(require racket/gui);; for the sound api
(provide game-timer)
#|
 AUTHOR : Obaro I Johnson
 PURPOSE:  School Assesment


This file contains the code that will operated on the user enemies and point calculations

|#
;;draw the game header here 
[define game-timer (new game-timer%)]
(require "conversation.rkt")
(require "room.rkt")

;;provide interface mutating functions
;;make the function global for any file that implement it
(provide move reset buy-bullet shot)
;;; the code section makes the functions global
(provide enemy enemy? enemy-name enemy-position)
(provide auto-creation-enemies kill)
(provide  get-enemies get-point is-direction-found?)
;make the constants visible
(provide MY-BULLETS MAXIMUM-POINT MINIMUM-POINT MAXIMUM-ENEMIES-PER-ROOM)
(provide TOTAL-KILLED-ENEMIES ENEMIES DEFAULT-BULLET SHUTTING-DIRECTIONS MY-POINTS CURRENT-USER-ROOM)

;;DECLARATION OF THE CONSTANTS 
(define DEFAULT-BULLET 10)
(define SHUTTING-DIRECTIONS '(left right back front))
(define MY-POINTS 0)
(define MY-BULLETS 5)
(define MAXIMUM-POINT 10)
(define MINIMUM-POINT 1)
(define MAXIMUM-ENEMIES-PER-ROOM 5)
(define TOTAL-KILLED-ENEMIES 0)
(define ENEMIES '())
(define BULLET-POINTS 3)
  


;;random price of bullet base of the number of enemies

(define (get-bullet-price)
  (cond
    [(empty? CURRENT-USER-ROOM)   (set! BULLET-POINTS 0)]
    [else
  (set! BULLET-POINTS
        (+ 3
           (random 
            (let [(to-random (length (room-enemies CURRENT-USER-ROOM)))]
              (cond 
                [(<= to-random 0) (set! to-random 1)]
                )to-random)))        
        )]))

;; Structure definations and functions start here


;create the enemy structure
(struct enemy((name #:mutable) (position #:mutable))#:transparent)

;;return the user point
(define (get-point)
  (+ MINIMUM-POINT (random MAXIMUM-POINT))
  );end the function

;;get a random position for enemies
(define (get-random-position)
  (define index (random (length SHUTTING-DIRECTIONS)))
  (list-ref SHUTTING-DIRECTIONS index)
  );;end 

;;assigned the enenmies to the given room
(define (get-enemies)
  (cond
    ((not (room? CURRENT-USER-ROOM)) #f)
    (else
     (set-room-enemies! CURRENT-USER-ROOM ENEMIES)
  (set! ENEMIES '())))
  );;end function

;;ad the enemies to the enemies buffer
(define (add-random-enemy p)
  (cond
    [(not (enemy? p)) (set-conversation-color "red")
                      (add-conversation "please type -help to see command")
                      (set-conversation-color "white")]
    [else
     (set! ENEMIES
           (let loop [(lstenemy ENEMIES)]
             (cond
               [(empty? lstenemy) (cons p '())]
               [else
                (cons (car lstenemy)(loop (cdr lstenemy)))])))]))
;;end function



;;create random number of enemies in a room
(define (auto-creation-enemies)
  ;;get the random number of enemies to create
  (define n (random MAXIMUM-ENEMIES-PER-ROOM))
  ;;create the enemies by the number of n
  (let loop [(counter n)]
    ;;condition to test if n is valid number
    (cond
      ((<= counter 0)
       (set-conversation-color "yellow ")
       (add-conversation (format "type -help to view help list"))
       (set-conversation-color "white"))
      [else 
       (define p (enemy counter (get-random-position)))
       (add-random-enemy p)
       (loop (- counter 1))])))
;;end function


;Check if the dir give exist
(define (is-direction-found? dir)
  (let loop ((lst SHUTTING-DIRECTIONS))
    (cond
      ((empty? lst) #f)
      ( (eq? dir (car lst)) #t)
      (else (loop (cdr lst))))))



;; kill and removed enemies from the enemy buffer
(define (kill dir)
  (define is-shot #f)
  (cond
    [(not (is-direction-found? dir)) (add-conversation (format "invalid command , type -help for command details~a\n" dir))]
    [else
     (cond
       [(empty? (room-enemies CURRENT-USER-ROOM)) (add-conversation (format "No monster in room , zero target killed \n"))]
       [else
         (let loop [(lst (room-enemies CURRENT-USER-ROOM))]
                             (cond                               
                               ((or (empty? lst) (not (list? lst)))
                                (set-conversation-color "blue")
                                (add-conversation (format "No target killed, no monster found "))                                            
                                             (set-conversation-color "white"))
                               [else
                                (define p (car lst))
                                (cond
                                  [(eq? (enemy-position p) dir) (set! is-shot #t) (set-room-enemies! CURRENT-USER-ROOM (cdr lst))]
                                  [else
                                   (cons (car lst) (loop (cdr lst)))])]))])])

  is-shot;return the status of the shuting
  )
;;end 



;;this function  will move the user to the next room dir given
(define (move dir) 
  
  (define exit-dir (get-door dir))
          (cond
            ((is-door-exit? dir) (change-room-to exit-dir )
                                ;; place a consition here please!
                                 (cond
                                   ((not (zero? exit-dir ))
                                 (auto-creation-enemies)
                                 (get-enemies)
                                  (get-bullet-price)))
                                 
                                 ) 
             (set-conversation-color "blue")
              (add-conversation (format " Okay I am here in ~a " dir))
                (set-conversation-color "white")
            [else
               (set-conversation-color "red")
               (add-conversation (format "invalid command enter? type help  \n"))
               (set-conversation-color "white")]))
;;End the function to move the user from one room to another

;;the dying sound function of a mustard
(define (mustard-dying-sound)
  (define filename "dying.wav")
  (play-sound filename #t)
  )

#| The  Method reset the game 


|#

(define (reset)
   (room-init);;initialised the rooms again
   (auto-creation-enemies);; auto create room enemies
   (get-enemies); put the enemies into the room
   (get-bullet-price)
   (set! MY-POINTS 0); reset the points to zero
   (send game-timer reset)
  (set! TOTAL-KILLED-ENEMIES  0)
   (set! MY-BULLETS DEFAULT-BULLET); reset the bullet to default
  )


;;The function shot a bullect to the  given direction dir given
(define (shot dir)

  (cond
    [(zero? MY-BULLETS) 
     (set-conversation-color "blue")
     (add-conversation (format "Hey! you have no bullet to fire\n"))   
     (set-conversation-color "white")]
    (else
     (set! MY-BULLETS (- MY-BULLETS 1))
     
     (cond
       [(kill dir) 
        (define points (get-point))
        
         (set-conversation-color "blue")
         (add-conversation (format "1 target killed at ~a " dir))
         (add-conversation (format "you have earn ~a points  \n" points))
         (set-conversation-color "white")
        (set! TOTAL-KILLED-ENEMIES (+ TOTAL-KILLED-ENEMIES 1)) 
        (set! MY-POINTS (+ MY-POINTS points))]
       ))));;end the function

;;The function buy a bullect 
(define (buy-bullet)
  
(cond
[(>= MY-POINTS BULLET-POINTS) (set! MY-BULLETS (+ MY-BULLETS 1)) (set! MY-POINTS (- MY-POINTS BULLET-POINTS)) #t]
[else
 (set-conversation-color "blue")
  (add-conversation (format "You dont have bullet in your gum"))
  (add-conversation (format ", have to purchase new bullet"))
  (set-conversation-color "blue")
#f])
)



