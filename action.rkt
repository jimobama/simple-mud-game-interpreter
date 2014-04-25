#lang racket
(provide action%)
(provide command-object?)


#|The class module , enable us to add keywords, or actions and their parameter to the game,
This part of the game , assumed the user is only going to communicate with its player, so this action are mostly directly
apply the player.
move 10 , means move the player 10position from its origin position move -10 is telling the user to move 10position backword

THE ACTION PROCEDURE MUST HAVE JUST , ONE LIST PARAMETER , EVEN IF ITS MULTIPLE PARAMETER
|#

;the structure of the command object
(struct object ((name #:mutable))#:transparent);
;The command structure are all objects
(struct  command-object(                        
                        (parameter #:mutable)
                        (on #:mutable)
                        (procedure #:mutable)
                        (desc #:mutable)
                        )#:transparent
                         #:super struct:object )


;THE CLASS DEFINATION OF THE COMMAND 
;;Command object declaration starts here
(define action% (class object%
                   [super-new]
   (field (cmd (command-object "" "" "" "" "" )))
   ;; this method set the command list
    (define/public (set-parameter lst)
        (cond
           ((not (list? lst)) (error (format "~a is not a paramter ")))
           [else
            (set-command-object-parameter! cmd lst)]));end method
  ;method set the command parameter number  
  (define/public (get-parameter)
     (command-object-parameter cmd))
                   
                   
;; function set command name
(define/public (set-name a-name)
  (set-object-name! cmd a-name))
;;function get the command name
(define/public (get-name)
  (object-name cmd))
                  

;;the function get and set the priority of the method
                   
   (define/public (set-on n)
      (cond
         ((not (symbol? n) ) (error (format "This action is on what object ~a" n)))
         (else
          (set-command-object-on! cmd n);//set the proity
          )))
      ;;get the proity
    (define/public (get-on)
      (command-object-on cmd))
                   
    (define/public (to-string)
       (define str   (format "<~a  ~a>\t : ~a " (symbol->string (get-name)) (get-parameter)(get-desc)))
       str      
      )
                   
      ;set the fingure-of-speech
     (define/public (set-procedure sn)
       (cond
         ((not (procedure? sn)) (error "paramter must be a procedure please! with the same parameter as the parameter format set"))
         [(not (eq? (send this get-parameter-count) (procedure-arity  sn))) 
          (error (format "fatal error: @[set-procedure] the parameter format  ~a did not matched with the procedure passed" (send this get-parameter)))] 
         (else
            (set-command-object-procedure! cmd sn))))
                   
                   ;;get the fingure-of-speech
    (define/public (get-procedure)
      (command-object-procedure cmd));
                   
;set the command descriptions
                   (define/public (set-desc des)
                     (cond
                        ( (not (string? des)) (error "command desciption must be a string value"))
                        (else
                         (set-command-object-desc! cmd des))))
                   ;;get the command description
                   
                   (define/public (get-desc)
                     (command-object-desc cmd))
                    
                  ;;get the paramter count
                  (define/public (get-parameter-count)                    
                    (length (send this get-parameter)))
                  
                   
#| The method that will return the main command object|#
                   (define/public (get)
                     cmd)
                   (define/public (set command)
                     (cond 
                       ((not (command-object? command)) (error "contract violented , the parameter must be a command object"))
                       (else
                        (set! cmd command))))))





