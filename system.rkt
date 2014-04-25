#lang racket
(require srfi/13)
(require "actions.rkt")
(require "conversation.rkt")
(provide symbol->number)
(provide sys-help
         sys-exit
         sys-clear
         sys-story
         )


; The function will display the game sorry and the hep
(define (sys-story)
   (set-conversation-color "green")
   (add-conversation "Welcome to Monster Hunter 0.1v")
   (set-conversation-color "white")
   (add-conversation "Obaro is a monster hunter who go from room to room to ")
   (add-conversation " kill monster for fun to earn points to buy more bullet.")
   (add-conversation "The game is a command line game , which required the ")
   (add-conversation "user to type string of text to communicate with obaro ")
   (add-conversation "using the commands shown below To communicate with ")
   (add-conversation "obaro , you have to type any of the commands as in")
  (add-conversation "normal command-line processing interface with the commmands provided")
  (set-conversation-color "yellow")
  (add-conversation "Game Tour")
  (set-conversation-color "white")
  (add-conversation "To be able to scroll the text up and down ")
  (add-conversation " use the arrow keys on the keyboard.")
  
  ;;Call the help command rto display the command
   (sys-help)
  
  (set-conversation-color "white")
  (add-conversation "-----------------------------------")

  

  
  (set-conversation-color "green")
  (add-conversation  "to start the  game  type [continue] command  or exit to close")
  )
;;the system command to print user helps
 (define  (sys-help)
     (set-conversation-color "yellow")
      (add-conversation "------------------------------------")
     (set-conversation-color "white")
   
     (let loop ((action-lst ACTIONS-TABLE))
       ;;check if the lst is empty
        (cond
          [(empty? action-lst)
           (set-conversation-color "white")
           (add-conversation "------------------------------------")
          ]
          [else
           ;;print the command to string functions
            (define cmd ( car action-lst))
              
           (set-conversation-color "red")
           (add-conversation (string-append (symbol->string (send cmd get-name)) 
                                            (format " ~a"  (get-parameter-type-format (send cmd get-parameter)) )))
            (set-conversation-color "white")
            (add-conversation  (format " -> ~a" (send cmd get-desc)))
           (set-conversation-color "white")
           (loop (cdr action-lst))])
         
        );;end loop
   )


;;return the parameter in human or type format
(define (get-parameter-type-format par)
  (cond
    [(empty? par)  ""]
    [else
      (format "<~a>" (car par))])) 

;;function to exit the window
(define (sys-exit)
  (exit)
 )

;;the function clear the conversation list

(define (sys-clear)
   (empty-conversations)
  )
;; convert the symbol to number
(define (symbol->number symbol)
   (let* [ (str-val (symbol->string  symbol)) (n (string->number str-val))]
     n));;edn method
