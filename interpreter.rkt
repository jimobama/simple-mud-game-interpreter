#lang racket
(require "parser.rkt");; the module for the parser
(require "actions.rkt");;The module actions commands /procedure manager
(provide interpreter%);;make it visible  to other modules
(require "procedures.rkt" )
(require "conversation.rkt")


#|This class map the text action to the procedures and report ,keep error logs |#
(define interpreter%(class object%
                      [init-field console]
                      [super-new]
                      [field (parser (new parser%))]
                      [field (actions (new actions%))]
            
                                                              
                      (define (exec fmethod)
                         ; this method will return each procedure in the action table
                           (cond
                              ;;check if the function exist anyway
                              [(empty? fmethod) #f]
                              ;;check if the parameter passed are same
                              [(not (eq? (send (send actions get-action (car fmethod))
                                               get-parameter-count) (length (second  fmethod))))
                               (push-error (format "I dont understand want you mean by ~a ~a\n please re-type want you mean!\n" (car fmethod) (first (cdr fmethod))))]
                              [(zero? (send (send actions get-action (car fmethod)) get-parameter-count))
                               ((send (send actions get-action (car fmethod)) get-procedure))]
                              [else
                               ;;call the procedure for the action and execute it
                               ((send (send actions get-action (car fmethod)) get-procedure) (cdr fmethod))
                               
                               
                               ]))                             
                   ;;function exec ends here
                      
                      (define/public (push-error str)
                         (set-conversation-color "red")                        
                         (add-conversation str)
                         (set-conversation-color "white")
                      
                        )
                      ;;end push error to list                        
                        
          (define/public (run script)
              (cond
                [(eq? script "")  (push-error "") ]
                [else
                 (send parser set-input script)
                 (send parser  process)
                 (cond 
                   [(send parser is-there-commands?)                   
                   (send parser lmap exec)]
                   [else
                    (send this push-error (format "invalid command typed "))
                   ]
                   )]))
                         
            
                       
                           
        (define/public (set-console aconsole)
               (set! console aconsole)
              )
          
                      
      ));;end of the class
  
  

