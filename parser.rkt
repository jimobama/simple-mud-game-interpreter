#lang racket
(require "actions.rkt")
(provide parser%)


#|
The parse class object get the user inputs convert them to tokens and then
save the in the parser table(association list) and can be access with the lmap function which takes ,
a function with one parable as the command format

|#

(define PARSER-TABLE '())
(define parser% (class object%
                       [super-new]
                       [field (actions (new actions%))]
                       [field  (str-commands "")]
                       
                       (define/public (get-input)
                         str-commands;
                         );;end function
                       ;; The function set the string commands passed
                       (define/public (set-input strcommands)
                         (cond
                            ((or (eq? strcommands "") (not (string? strcommands)))
                             (error "throw exception: require string parameter"))
                            (else (set! str-commands strcommands))));;end function    
                            
                     ;; The function allow the user to access the commands filter from the user inputs                            
                            (define/public (lmap func)
                              (let loop((cmds PARSER-TABLE))
                                (cond
                                   ((not (list? cmds)) (set! PARSER-TABLE '()))
                                   ((null? cmds) (set! PARSER-TABLE '()))
                                   (else
                                     (func (car cmds))
                                     (loop (cdr cmds))))))
                       
                       ;This parse method will read the command as a string and then execute each
                        (define/public (process)                                             
                          (let loop[(commands (send actions filter (send actions parser (send this get-input))))
                                    (tokens (send actions parser (send this get-input)))]
                           ;check and make such there is a command to put on the queue
                             (cond
                              [(empty? commands) #t]
                              [(empty? tokens) '()]
                              [else                               
                                (queue (car commands) tokens ) 
                                (loop (cdr commands) (take-out-func (car commands) tokens 
                                                                    (send (send actions get-action (car commands)) get-parameter-count)))])));end the method
                  ;;this function will check if the command list is empty 
                  (define/public (is-there-commands?)
                     (cond
                       [(empty? PARSER-TABLE) #f]
                       [else #t]))
                       
                     ;;The method remove the function and return the rest tokens
                           (define(n-remove tokens n)
                             (cond
                               [(<= n -1) tokens]
                               [(empty? tokens) tokens]
                               [else
                                (n-remove (cdr tokens) (- n 1))]))
                            
                            ;;The method take out the first occur function and its parameter from the token
                            (define (take-out-func command tokens n)
                              
                              (cond
                                [(empty? tokens) tokens]
                                [ (eq? (car tokens) command)
                                  ;;now remove the 
                                  (n-remove tokens n)]
                                [else 
                                 (take-out-func command (cdr tokens) n)])) ;;end
                            
                         
                       ;;This method will queue the command 
                       (define (queue command tokens)                             
                           (cond
                             ;;get the command and get the parameter count
                             ((zero? (send (send actions get-action command) get-parameter-count )) (add-to-queue  (cons command '(()))))
                             [else
                             (let* [(parameter (get-command-parameters command tokens))
                                    (func-format (parameterised-command  command parameter))]
                              
                               ( add-to-queue  func-format))]));;end exec method         
                          
                       ;;The interpreter send the command to the queue to table in a format that is easy to execute
                       
                       (define (add-to-queue command)
                         
                              ;; the sub codes update the queue table  
                            (set! PARSER-TABLE 
                                  (let loop [(queue-table PARSER-TABLE)]
                                  (cond
                                   [(empty? queue-table) (cons command queue-table)]
                                   [else
                                    (cons (car queue-table) (loop (cdr queue-table)))]))))
                            
                            ;;private method that will get the 1 to n term token
                  (define (get-parameter tokens n)
                    ;(display (format "~a = ~a \n\n" tokens n))
                    (cond
                      [(zero? n) '()]
                      [(null?  tokens) '()]
                      [else
                       (cons (car tokens) 
                             (get-parameter (cdr tokens) (- n 1)))]))                                   
                            
     ;;it is a private/local method
    ;; The function will get the user command parameter  in a form of a list object                                  
    (define (get-command-parameters command  tokens)       
  ;; let find the command 
   (let loop ([list-tokens tokens])                    
     (cond
         [(null? list-tokens) '()]
         [(and (eq? command (car list-tokens)) (not (null? (cdr list-tokens)))) 
          (get-parameter (cdr list-tokens) (send (send actions get-action command) get-parameter-count)) ]
          ;get paramter      
          [else
          (loop (cdr list-tokens))])))
                            ;; The function will then parameterised the command ,
                       ;; which will be later added to the queue table for execution
                       
                       (define (parameterised-command command param)                         
                               (list command param))));;end method



