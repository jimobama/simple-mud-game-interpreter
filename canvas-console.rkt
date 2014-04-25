#lang racket
(require racket/gui)
(require srfi/14)
(require "stack.rkt")
(require "render-game-data.rkt")
(require "interpreter.rkt" "user.rkt")
(require "conversation.rkt" )
;;reset
(reset)



(provide canvas-console%)



#|
The class is call the canvas-console% that handle the graphical text interaction between the user and the intepreter
It provides interface for the user to draw text to the screen and in any color specified and will
and images lines and other shaped and it will, but the user must manager positions by he or her
self if it calls the dc method directly
Ammunition


|#



;;this is the game console window where every graphic world is draw and it update its world every 100 mini-seconds
(define canvas-console% (class canvas%
                          [super-new]
                          [define character-stack (new stack%)]                   
                          [define screen-color "blue"]
                          [define prompt-string "> "]
                          [define margin-left 10]
                       
                          [define region (new region% 
                                              [dc                                            
                                               (send this get-dc)])]
                          (define dc (send region get-dc))
                          [define interpreter (new interpreter% 
                                                   [console this]
                                                   )]
                          (define timer (new timer%
                                             [interval 500]
                                             [notify-callback 
                                              (lambda ()
                                                (send game-timer counter)
                                                (send this refresh-now)
                                                (send this flush)
                                               
                                                
                                                )
                                              
                                              ]))
                          
                          ;;initialised methods
                          (send interpreter run "story")
                          
                          (define/public (set-prompt-string str)
                            (set! prompt-string str))
                          (define/public (get-string-buffer)
                             (get-conversations)
                            )
                          ;;The method handle the key event and display it on screen
                          (define/override (on-char k) 
                            
                            ;;handle the key events for printer key 
                            (cond 
                              ;;check if the event is release or enter key is pressed
                              [(eq? (send k get-key-code) 'release)  (key-sound)]
                              ;handle enter key event
                              [(eq? (send k get-key-code)'up) (send this move-text-down  12)]
                              [(eq? (send k get-key-code)'down) (send this move-text-up 12)]
                              [(or (eq? (send k get-key-code) #\return ) (eq? (send k get-key-code) 'numpad-enter))
                             
                               ;;ile interpreter process the command        
                               (add-conversation (string-append "command > " (send character-stack to-string)))
                               (send interpreter run (send character-stack to-string))        
                               (send character-stack empty)
                               ;check height before draw
                               (cond
                                 [(>= (get-conversation-y) (- (send this get-height) 60)) (send this move-text-up (- (get-conversation-y) (- (send this get-height) 100)))])
                               ]
                              ;;check if back space is pressed then removed the key from the stack 
                              [(eq?  (send k get-key-code) #\backspace) (send character-stack n-pop)]
                              [else
                               (cond
                                 [(char? (send k get-key-code))
                                  (send character-stack  push (send k get-key-code))]            
                                 )]);;end else statement and cond method  
                            
                            ;(send this refresh-now)
                            
                            
                            );;end the on-char method that handle key press
                          
                        ;;key sound
                          (define/private (key-sound)
                            (define filename "beep.wav")
                            (play-sound filename #t)
                            )
                          ;;all gl drawing here
                          
                          (define (render-room-information)
                            
                            (define start-x (* (send this get-width) 0.50))
                            (define start-y 100)
                            (define end-x (send this get-width))
                            (define end-y (- (send this get-height) 50))
                            
                            (send  dc  set-clipping-rect  start-x start-y end-x end-y)
                                    
                            
                            ;;start drawing here
                            (send this set-background "black")                            
                            (right-panel dc start-x  start-y  end-x end-y)
                            
                            )
                          
                       
                          
                          ;;will clear the streen to the color specified
                          (define/public (set-background color)
                            ;(define dc  (send this get-dc)) 
                            ;;check if the color is a string value else convert to string
                            (cond
                              [(symbol? color) (set! color (symbol->string color))]
                              );;end cond
                            ;;clear the screen
                            (send dc set-brush color 'solid)  
                            ;;cover the screen with a rectangle   
                            (send dc clear);;clear the screen
                            (send dc draw-rectangle(send this get-x) (send this get-y) (send this get-width)  (send this get-width));       
                            )
                          
                          ;;this method draw the header of the game title
                          (define/private (game-header)
                            (send dc  set-clipping-rect (send this get-x) (send this get-y) (send this get-width) 100)
                            (set-background "blue")
                            (send dc set-pen "yellow" 2 'solid)
                            (send dc set-brush "black" 'solid)
                            (send dc draw-line (send this get-x) 100 (send this get-width) 100) 
                            ;;this function draw the game headers details 
                            (draw-header-details dc (send this get-x)  (send this get-y) (send this get-width) 100);
                            
                            (send dc set-pen "black" 2 'transparent)
                            )
                          
                          
                          
                          
                          (define/public (draw-conversation) 
                            ; (send this set-background "black") 
                            ;;set the view port
                            (send dc  set-clipping-rect 0 100 (* (send this get-width) 0.50) (- (send this get-height) 80))
                            ;;set the buffer and set the background
                            (send this set-background "black")
                            
                            
                            ;;loop the buffer      
                            (let loop [(buffer  (get-conversations))]
                              ;;check if the list is empty or not
                              (cond 
                                [(empty? buffer) (set-last-string->position)]
                                
                                [else
                                 (let [(string-text (second (car buffer)))
                                       (textcolor (first (car buffer)))
                                       (x (first (cdr (cdr (car buffer)))))
                                       (y (second (cdr (cdr (car buffer)))))]
                                   ;;get the canvas draw content
                                   ;(define dc dc)
                                   (send dc set-text-foreground textcolor)
                                   (send dc draw-text string-text x y)
                                   (set-conversation-y y)) 
                                 
                                 
                                 (loop (cdr buffer))]))        
                            ;;set the y position for the next text
                            (set-conversation-y (+ Y-POSITION (send (send dc get-font ) get-point-size)))
                            
                            );;end loop
                          
                          
                          ;;the method will position the last position ,
                          (define/private (set-last-string->position)
                            (cond
                              [(empty?  (get-conversations)) (set-conversation-y DEFAULT-Y) (set-conversation-x 0)]
                              [else                 
                               (let[(last-string-draw-desc (list-ref  (get-conversations) (- (length  (get-conversations)) 1)))]                                
                                 (cond
                                   [(empty? last-string-draw-desc) (set-conversation-y 10)(set-conversation-x 10)]
                                   [else 
                                    (let [ (textcolor (car last-string-draw-desc))
                                           (string-text (second last-string-draw-desc))
                                           (x (first (cdr (cdr last-string-draw-desc))))
                                           (y (second (cdr (cdr last-string-draw-desc))))   ]            
                                      
                                      ;(define dc dc)     
                                      (set-conversation-y (+ (get-conversation-y) (send (send dc get-font ) get-point-size)))                     
                                      (set-conversation-x  0))]))]);//for the next input
                            
                            
                            
                            ;;draw the input box
                            )
                          
                                                 
                          
                    ;;end the method to return the list object and how to draw the string
                          (define/public (move-text-up line)
                            ;create a tem memory to hold buffer
                            (define temp-buffer  (get-conversations))
                            ;;clear the old buffer
                            (clear->string-buffer)        
                            (let loop [(buffer temp-buffer) (reduce line)]
                              (cond
                                [(empty? buffer)   #f]
                                [else                   
                                 ;; get the value of old buffer
                                 (let [(string-text (second (car buffer)))
                                       (textcolor (first (car buffer)))
                                       (x (first (cdr (cdr (car buffer)))))
                                       (y (second (cdr (cdr (car buffer)))))]
                                   ;;fill in temp buffer
                                   ;Descrease y the number of line 3 times 
                                   (define new-y (- y reduce))
                                   (insert-conversation (to->list-buffer textcolor string-text x new-y ))
                                   
                                   )                 
                                 (loop (cdr buffer) reduce)]))
                            )
                          
                          
                          ;;the method add string to buffer 
                          ;;the method return the temp buffer                    
                          ;the method clear the temp buffer 
                          
                          (define/public (clear->string-buffer)  
                            ;;call the global variable to clear the converstaion
                             (empty-conversations)
                            )
                       
                          
                          
                          (define/public (get-line-height)
                            (send (send dc get-font ) get-point-size)
                            )
                          
                         
                          
                          (define/private (send-key->screen)     
                            ;;now draw the stack characters 
                            ;(define dc dc );//get the dc content    
                            (send dc set-text-foreground "white")
                            (define box-y (- (send this get-height) 50))
                            (send dc draw-text (send character-stack to-string)(get-conversation-x) box-y)
                            );;end
                          
                          (define/private (draw-input-box)
                            ;set the view port to draw on
                            
                            (send dc  set-clipping-rect 0 (- (send this get-height) 100)  (send this get-width) (send this get-height))
                            ; (define dc dc)
                            (send dc set-text-foreground "blue")
                            (send dc set-brush "black" 'solid)  
                            (send dc set-pen "white" 3 'solid)
                            ;;draw box will start   
                            
                            (define box-y (- (send this get-height) 60))
                            (send dc draw-rounded-rectangle 0 box-y   (send this get-width) 60 5)            
                            (set! box-y (+ box-y 10))
                            (set-conversation-x (+ (get-conversation-x) 10))
                            (send dc draw-text prompt-string (get-conversation-x) box-y)
                            ;;set the x-position to the size of the font times the number of character to draw and decrease by 0.7
                            (set-conversation-x (+ (* (string-length prompt-string) (send dc get-char-width ) ) 40))
                            (send dc set-pen "white" 1 'transparent)
                            
                            )
                          
                          ;;the method will move the text up on the screen
                          (define/public (move-text-down line)
                            ;create a tem memory to hold buffer
                            (define temp-buffer  (get-conversations))
                            ;;clear the old buffer
                            (clear->string-buffer)        
                            (let loop [(buffer temp-buffer) (reduce line)]
                              (cond
                                [(empty? buffer)   #f]
                                [else                   
                                 ;; get the value of old buffer
                                 (let [(string-text (second (car buffer)))
                                       (textcolor (first (car buffer)))
                                       (x (first (cdr (cdr (car buffer)))))
                                       (y (second (cdr (cdr (car buffer)))))]
                                   ;;fill in temp buffer
                                   ;;descrease y the number of line 3times 
                                   (define new-y (+ y reduce))                                
                                   (insert-conversation (to->list-buffer textcolor string-text x new-y ))                                   
                                   )                 
                                 (loop (cdr buffer) reduce)]))
                            )
                          ;;overide the paint method and the 
                          (define/override (on-paint)
                            (send this draw-conversation)
                            (render-room-information)
                            (game-header)
                            (draw-input-box) 
                            (send-key->screen)
                            )
                          
                          
                          ));;end class canvas-console

