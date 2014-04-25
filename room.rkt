#lang racket
(require "conversation.rkt") 

;;Provide the fnction to be global

(provide CURRENT-USER-ROOM)
(provide  create-random-rooms
          is-door-exit?
          position-user
          get-door
          change-room-to
          room-init
          room?
          room 
          set-room-enemies!
          room-id
          room-enemies
          room-way-out
           door
           door-north 
           door-south
           door-west 
           door-east
           door? 
          )




;;creating the room and door structures

(struct door ( (north #:mutable) (south #:mutable) (west #:mutable)  (east #:mutable) )#:transparent)
(struct room (id (enemies #:mutable ) (way-out #:mutable ))#:transparent)

;;CONSTANTS
(define MAX-ROOM 5)
(define MIN-ROOM 2)
(define ROOM-DIRECTORY '()) 
(define CURRENT-USER-ROOM (room 0 '() (door 0 0 0 0)))




;;FUNCTION DEFINATIONS TO MANIPULATED ROOMS


;;This method add room to the room directory
(define (add-room->dir new-room)
  (set! ROOM-DIRECTORY
   (let loop [(r-dir ROOM-DIRECTORY)]
     (cond 
       [(empty? r-dir) (cons new-room '())]
       [else
        (cons (car r-dir) (loop (cdr r-dir)))]))))
 ;;end method


;;This function will close the room, that is self-link to its self when its randomly created
(define  (close-room-self-linked aroom)
   (cond
      [(room? aroom) 
       ;;check if the room id is same with the link door
        (cond
           [ (eq? (room-id aroom) (door-north (room-way-out aroom )) ) (set-door-north! (room-way-out aroom ) 0)]
           [ (eq? (room-id aroom) (door-south (room-way-out aroom ))) (set-door-south! (room-way-out aroom ) 0)]
           [ (eq? (room-id aroom) (door-west (room-way-out aroom ))) (set-door-west! (room-way-out aroom ) 0)]
           [ (eq? (room-id aroom) (door-east (room-way-out aroom ))) (set-door-east! (room-way-out aroom ) 0)]
           )]))


;;This function will random create door links base of the number of rooms
;;and return the door
(define (create-random-door)
  (define n (+ 1 (length ROOM-DIRECTORY)))
   (define north (random n))
   (define south (random n))
   (define west  (random n))
   (define east (random n))
   (define v-door (door north south west east))
    ;;return the door 
   v-door
 );end method


;;This function will create n number of rooms with default door closed
;;randomly
(define (auto-room-creation n)
   (let loop ((counter n))
    (cond
      [(<= counter 0) #t]
      [else
       ;;create the ramdon rooms
       (define new-room (room counter  '() (create-random-door)))
       (add-room->dir new-room)
       (loop (- counter 1))]))
     
  );end method



;;This is the main function that will create and auto link the rooms

(define (create-random-rooms)
   (define ncounter (+ MIN-ROOM (random MAX-ROOM)))
    ;;auto create the n of rooms  and then put them into the room list  
    (auto-room-creation  ncounter)
    ;;allocated room exit link
    (auto-asigned-room-link)
)
;;this function will inter-link the rooms together
(define (auto-asigned-room-link)
  (define size (length ROOM-DIRECTORY))
;;loop and access the rooms and link then together
   (let loop [(rm-list ROOM-DIRECTORY)]
      (cond
        [(empty? ROOM-DIRECTORY) #f]
         [(empty? rm-list ) #t]
         [else
           (set-room-way-out! (car rm-list) (create-random-door))
           (close-room-self-linked (car rm-list))
           (loop (cdr rm-list))])))

;;this function will get the room 
 (define (get-room id)   
       ;;then continue with the getting the room
       (let loop ((lst ROOM-DIRECTORY))
         (cond
           ((empty? lst) null)
           ((eq? id (room-id (car lst))) (car lst))
           [else
            (loop (cdr lst))]))
         
  )

;;check if the value passed is a valid position for a door
(define (is-door-exit? dir)
  (cond 
     ((eq? dir 'north) #t)
     ((eq? dir 'south) #t)
     ((eq? dir 'east) #t)
     ((eq? dir 'west) #t)
     (else #f)
  ))

;;this function will auto position user 
(define (position-user)
  (define  n (+ 1 (length ROOM-DIRECTORY)))
    ;;if the random number is 0 add 1 to it
  (cond
    [(zero? n) (set! n (+ 1 n))])
  ;;set the current user room
  (set! CURRENT-USER-ROOM (get-room (random n)))

  )
;;This function will change the current room to another room id given
(define (change-room-to index)
    (cond
      [(<= index 0) 
       (set-conversation-color "blue")
       (add-conversation (format "You have to break the wall then, \n"))
       (add-conversation (format "else find another way out\n"))
       (set-conversation-color "white")]
      [else 
        (set! CURRENT-USER-ROOM (get-room index)) #t]
     ))

;;get the door exit id, the function will return the exit number of the next room

(define (get-door dir)
   (cond
     [(not (is-door-exit? dir) ) 0]
     [else
       (cond
         ( (eq? dir 'north)  (door-north (room-way-out CURRENT-USER-ROOM)))
         ( (eq? dir 'south)   (door-south (room-way-out CURRENT-USER-ROOM)))
         ( (eq? dir 'west)    (door-west (room-way-out CURRENT-USER-ROOM)))
         ( (eq? dir 'east)   (door-east (room-way-out CURRENT-USER-ROOM))))]))
      

;create a function will initialised the first room to start with

(define (room-init)
  (set! ROOM-DIRECTORY '())
  (create-random-rooms)
  (position-user)
  );;end the functions



