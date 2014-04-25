#lang racket
(require "room.rkt")
(require "user.rkt")
(require racket/gui)
(provide  
 render
 draw-header-details
 right-panel
 )



;;CONSTANTS
(define WIDTH-BOX 150)
(define HEIGHT-BOX 40)

#|THE SECTION THAT RENDER A ROOM INFORMATION TO THE SCREEN|#

;;tHE FUNCTION WILL DRAW LINES OF DEMACATIONS IN THE GIVEN X AND Y POSITIONS TO 300
(define (draw-demacation-lines dc x y)
  (let loop [(lines 4) (ax  x) (ay y)]
    (cond
       [(<= lines 0) 'created-lines]
       [else
            (set! ay (+ ay  50))
            
           (send dc draw-line x ay (+ ax 312) ay)  
   (loop (- lines 1)  ax ay)]))
  );;END METHOD


;;this function will draw the test  on the box 
(define (draw-box dc x y w h str)
  (cond
     [(>= (string-length str) w) (set! w (+ (string-length str)  w ))]
     )
  (send dc draw-rectangle x y w h)
  (send dc draw-text str (+ x 15) (+ y 10) )
  )
;;this function will draw the enemy and its position in the house
(define (draw-enemy dc id position x y)
   
   (draw-box dc (+ x 350) (+ y 50)  WIDTH-BOX HEIGHT-BOX (format "~a" id))
   (send dc set-brush "white" 'solid)
  (send dc set-text-foreground  "blue")
   (draw-box dc (+ x 350 150) (+ y 50)  WIDTH-BOX HEIGHT-BOX position)
   (send dc set-brush "black" 'transparent)
  );;end function

;;this function will render enemies list to the screen
(define (render-game-enemies dc x y aroom-enemies)   
   ;;define a variable to hold the start of the x and y 
  (define a-start-x x)
  (define a-start-y y) 
  (define old-font (send dc get-font))
  (send dc set-font  (make-object font% 12 'modern 'normal 'bold)) 
  (send dc set-text-foreground  "black")
  (send dc set-brush "green" 'solid)
  (draw-enemy dc "MONSTER " "LOCATION" x y)
  (send dc set-font   old-font) 
 ;;check if the aroom-enemies is a list of enemies
  (set! y (+ y 45))
 
  ;;draw the enemies  and positions
   (cond
     [(not (list? aroom-enemies)) #f]
     [(null? aroom-enemies) #f]    
     [else
      (let loop( [lst-enmies aroom-enemies] (ay y))
        (cond
           [(empty? lst-enmies) '()]
           [(enemy? (car lst-enmies))
             (define e (car lst-enmies))
             (draw-enemy dc (enemy-name e ) (symbol->string (enemy-position e )) x ay)     
             (set! y  (+ ay 45))
             (loop (cdr lst-enmies) (+ ay 45))]))])
             ;;get the first item on the list
              
  

 
  )
 
;The function will render exit location to the screen in the current room
(define (render-doors dc x y  adoor)
  (define NORTH 0)
  (define SOUTH 0)
  (define WEST 0)
  (define EAST 0)

  ;;check if the door is a door object pass
  (cond
     ((door? adoor)
       (set! NORTH (door-north adoor))
       (set! SOUTH (door-south adoor))
       (set! WEST (door-west adoor))
       (set! EAST (door-east adoor)))
     );;end condition if
      
  (set! y (+ y 50))
  (define start-position-y y)
  (send dc set-pen "white" 3 'solid)
  (send dc draw-rectangle x y  315 (+ 200 50))  
   (draw-demacation-lines dc x y)
  
  (set! y (+ y 10))
   (send dc set-text-foreground  "green")
  (send dc draw-text "EXIT(s)" (+ 20 x) y)
  (send dc draw-line (+ 140 x) (- y 10) (+ 140 x) (+ y 190 50))
  (send dc draw-text "EXIT TO " (+ x 150) y)

  ;;draw the north exit
  (send dc set-text-foreground  "green")
   (send dc draw-text "NORTH" (+ 20 x) (+ y 50))
  (send dc set-text-foreground  "white")
  ;;check if north door is wall
  (cond 
    [(not (zero? NORTH))      
     (send dc draw-text  (format "Room ~a" NORTH) (+ 140 x 20) (+ y 50))]
    [else
     (send dc draw-text  (format "Wall ") (+ 140 x 20) (+ y 50))])
  
  (send dc set-text-foreground  "green")
  (send dc draw-text "SOUTH" (+ 20 x) (+ y 50 50))
  (send dc set-text-foreground  "white")
  ;;check if there is no door at south
   (cond 
    [(not (zero? SOUTH))      
      (send dc draw-text  (format "Room ~a" SOUTH) (+ 140 x 20) (+ y 50 50))] 
    [else
     (send dc draw-text  (format "Wall ") (+ 140 x 20) (+ y 50 50))])
  
  
  
  (send dc set-text-foreground  "green")
  (send dc draw-text "WEST" (+ 20 x) (+ y 50 50 50))
  (send dc set-text-foreground  "white")
  ;;checking if the WEST is zero
  (cond 
    [(not (zero? WEST))      
      (send dc draw-text  (format "Room ~a" WEST) (+ 140 x 20) (+ y 50 50 50))]  
    [else
     (send dc draw-text  (format "Wall ") (+ 140 x 20) (+ y 50 50 50))])

  
  
   (send dc set-text-foreground  "green")
  (send dc draw-text "EAST" (+ 20 x) (+ y 50 50 50 50))
  (send dc set-text-foreground  "white")
  
    ;;checking if the EAST is zero
  (cond 
    [(not (zero? EAST))      
      (send dc draw-text  (format "Room ~a" EAST) (+ 140 x 20) (+ y 50 50 50 50))  ]  
    [else
     (send dc draw-text  (format "Wall ") (+ 140 x 20) (+ y 50 50 50 50))])
  
  
   (send dc set-pen "black" 1 'transparent)
  
 );;end method ro render doors exits


;;this function draw the room details to the screen
(define (render-room-info  dc room x y w)
  
  (cond
    ((room? room)
    (send dc set-text-foreground  "white")
   (send dc set-pen "yellow" 2 'solid)
   (set! y (+ 30 y))
    (send dc set-font  (make-object font% 22 'modern 'normal 'bold)) 
   (send dc draw-text " YOU ARE CURRENT IN ROOM " (+ x 5) y)
    (send dc draw-text  (number->string (room-id room)) (+ (+ x 370) 60) y)
    (send dc set-font  (make-object font% 12 'modern )) 
  (set! y (+ 70 y))
  (send dc draw-rectangle x (- y 10) (* w 0.48) 40 )
  (send dc set-pen "black" 1 'transparent)
  ;;draw the game room current details
   (send dc set-text-foreground  "green")
   
   (send dc draw-text  "Below's the room exits and monster location if any " (+ x 10)  y)
   (send dc set-text-foreground  "white")
  
   ;;total enemies
 ; (send dc set-text-foreground  "green")
;  (send dc draw-text  "TOTAL MONSTERS IN ROOM " (+ (+ x 110) 60)  y)
  ;;(send dc set-text-foreground  "white")
  ;(send dc draw-text (number->string (length (room-enemies room))) (+ (+ (+ x 120) 80) 220) y)
  )))


;;The function draw the header and the right panel on the window


(define (draw-header-details  dc x y w h)
  
  (send dc set-font  (make-object font% 22 'modern 'normal 'bold)) 
  (send dc set-text-foreground  "yellow")
  (send dc draw-text "Elapsed Time " (+ x 10) (+ y 10))
  (send dc set-text-foreground  "white")
  (send dc set-font  (make-object font% 32 'modern)) 
 
  (send dc draw-text (send game-timer to-string) (+ x 30) (+ y 40))
  (send dc set-font  (make-object font% 22 'modern 'normal 'bold)) 
  
  (send dc set-text-foreground  "yellow")
  (send dc draw-text "Bullets" (+ x 10 400) (+ y 10))
  (send dc set-text-foreground  "white")
  (send dc set-font  (make-object font% 32 'modern)) 
  (send dc draw-text (number->string MY-BULLETS) (+ x 30 450) (+ y 40))
  (send dc set-font  (make-object font% 22 'modern 'normal 'bold)) 
  
  (send dc set-text-foreground  "yellow")
  (send dc draw-text "Earn Points " (+ x 10 280 400) (+ y 10))
  (send dc set-text-foreground  "white")
  (send dc set-font  (make-object font% 32 'modern)) 
  (send dc draw-text (number->string  MY-POINTS) (+ x 60 210 450) (+ y 40))
  (send dc set-font  (make-object font% 22 'modern 'normal 'bold)) 
    (send dc set-text-foreground  "yellow")
  (send dc draw-text "Monster(s) Killed" (+ x 10 280 400 300) (+ y 10))
  (send dc set-text-foreground  "white")
  (send dc set-font  (make-object font% 32 'modern)) 
  (send dc draw-text (number->string  TOTAL-KILLED-ENEMIES) (+ x 60 210 450 360) (+ y 40))
  
  (send dc set-font  (make-object font% 12 'modern))
  
  )
(define (right-panel dc x y w h)
  (send dc set-brush "black" 'solid)
  (send dc draw-rectangle (+ x 5) (+  y 15) (- w 15) (- h 35))
  ;;draw room 
  (render dc CURRENT-USER-ROOM x y w)
  )

;;this function will draw the room information to the screen
 (define (render dc room x y w)
    (cond
      ((not (room? room)) (create-random-rooms) (position-user))
      (else
       (render-room-info dc room x y w)
       (set! y (+ y 100))
        (render-doors dc x y  (room-way-out room))  
        (render-game-enemies dc x y (room-enemies room))))
 
   );;end function render room
       
  

