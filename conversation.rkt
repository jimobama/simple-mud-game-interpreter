#lang racket

;;make the functions and the constant visible for other users
(provide  DEFAULT-Y  X-POSITION  Y-POSITION)

;;making the function visible to other module

(provide add-conversation
         insert-conversation
         set-conversation-color
         to->list-buffer
         get-conversation-x 
         get-conversation-y
         set-conversation-x
         set-conversation-y
         empty-conversations 
         get-conversations)


;Global fields declarations 

(define string-buffer '())
(define conversation-color "white")
(define margin-left 0)


;Constant declarations for conversations
(define DEFAULT-COVERSATION-X 10)
(define DEFAULT-Y 100)
(define X-POSITION 0)
(define Y-POSITION DEFAULT-Y)


;;mutator and getter

(define (set-conversation-x ax)
  (set! X-POSITION ax))

(define (get-conversation-x)
  X-POSITION )
;;mutator to access the y position
(define (set-conversation-y ay)
  (set! Y-POSITION ay))

(define (get-conversation-y)
  Y-POSITION )


;;the mutators (getter and setter(transformer))    

(define  (get-conversations)
  string-buffer
  )

(define (empty-conversations)
  (set! string-buffer '()))





;a function to insert a conversation str, list object 
(define (insert-conversation lst-str)
  (cond
    [(and (list? lst-str) (not (null? lst-str)))
     
     ;;add how the string should be draw it takes the correct default values which must be set              
     (set! string-buffer (let loop [(lst string-buffer)]
                           (cond
                             ([empty? lst] (cons lst-str lst))
                             (else
                              (cons (car lst) (loop (cdr lst)))))))                           
     ;;this function will redraw the string-buffer to the screen and clear the previous ones
     ]) 
  );;end the function here





;;The method set the text color ,on how the canvas will draw it
 (define  (set-conversation-color strcolor)
    (set! conversation-color strcolor))

 ;;This method will make a list of the parameter and return it                                      
(define (to->list-buffer color str x y) 
      (define lst (cons color (cons str (cons x (cons y '())))))
   ;;increase the next string position to draw    
   (set! Y-POSITION  (+ Y-POSITION  17))
   lst
  )



  ;;this method will add the default text color to the text
                          (define (add-conversation str )
                            ;;send the string to the interpreter 
                             (cond 
                              [(eq? str "") 'nothing]
                              [else                                   
                               ;(display y-pos)
                               (set! X-POSITION DEFAULT-COVERSATION-X)
                               (define str-desc (to->list-buffer conversation-color str X-POSITION  Y-POSITION ))        
                              
                               ; (send interpreter  run str)
                               (insert-conversation str-desc)
                               ])
                            
                            );end the function here
