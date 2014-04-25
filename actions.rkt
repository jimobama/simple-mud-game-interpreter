#lang racket
(require srfi/13);;The class is include because of string->tokenize 
(require "action.rkt")


#|This the available interface when this file is required|#
(provide action%)
(provide actions% ACTIONS-TABLE)

;;the actions list
(define ACTIONS-TABLE '())

(define actions% (class object%
                     ;;call the default constructor of the object
                      [super-new]
                      [field (error-msg "")]
                                      
                   
                      ;;define public methods
                  ;;This method accept only one paramter , of type action% class
                  ;; and the action name,paramter and procedure must be set else throw an exception error
                   (define/public (get-last-error)
                      error-msg
                     )
                   (define (set-last-error err)
                     (set! error-msg err)
                     )
                   
                   (define/public (get)
                     ACTIONS-TABLE;return the action list
                     )
                     (define(set actions)
                     (set! ACTIONS-TABLE actions);return the action list
                     )
                  
                      (define/public (add action)
                          (cond
                            [(not (command-object? (send action get))) (error "@add: contract violent parameter 1 must be a action object ")]
                            [else
                             ;;check if the action is already added to the command list
                             (cond
                               ;;check if the action field is validated and the action command is added already
                               ((not (is-validated? action)) (error (send this get-last-error)))
                               ((is-defined? action) (error (format "@add: (~a action) has be already added to the command list" (send action get-name)) ) )
                               (else
                                (set
                                      (let loop [(lst-command ACTIONS-TABLE)]
                                        ;make the command to be list
                                        (cond 
                                          [(null? lst-command) (cons action '())]
                                          [else 
                                           (cons (car lst-command) (loop (cdr lst-command)))])))))])
                        );;end add method here                  
                  
                      (define/public (remove action)
                          (cond
                            [(not (command-object? (send action get))) (error "parameter mismatched @ remove method")]
                            [else (set
                             (let loop [(actions (send this get))]
                                (cond
                                  [(empty? actions ) actions]
                                  [(eq? (car actions) action) (cdr actions)]
                                  [else
                                    (cons (car actions) (loop (cdr actions)))])))]))
                                    
                                    
                               
                        
                   ;;this method will return the action if found else returns null
                      (define/public (get-action action-name)
                          ;; change the passed parameter to lower
                        (set! action-name (string->symbol(string-downcase  (symbol->string action-name))))
                        ;;start the loop and find the action with the name action-name
                         (let loop [(actions (send this get))]
                          (cond
                              [(empty? actions) null]
                              ;;check if the action-name is equal if its then return  the action
                              [(eq? (send (car actions) get-name) action-name) (car actions)]
                              [else
                                (loop (cdr actions))])))
                           
                     
                  ;;This function convert the action object to list
                      (define (to->list action)
                        (cond
                          [(not (command-object? (send action get))) (error (format "fatal error: paramter must be a command object "))]
                          [else
                           (cons (send action get-name) 
                                 (list   
                                  (cons (send action get-parameter)
                                        (cons (send action get-on)  
                                              (cons (send action get-procedure) (cons (send action get-desc)'()))))))])
                       )
                      
                      (define/public (filter tokens)
                        (cond
                          [(null? tokens ) '()]
                          [else
                           (let loop ((lst-commands (map to->list (send this get))) (a-tokens tokens))
                             (cond 
                               ((null? a-tokens ) '()) 
                               (else
                                (cond 
                                  [(is-found? (car a-tokens)) (cons (car a-tokens)   (loop lst-commands (cdr a-tokens)))]
                                  [else
                                   (loop lst-commands (cdr a-tokens))]))))])
                        );;the method filter ends here
                      
                      (define/public (parser str-input)
                           ;analysis the string stream into tokens
                        (let* ( [str-tokens (string-tokenize (string-downcase str-input))]
                                [tokens (map string->symbol str-tokens)]) 
                          tokens)
                        )
                      
                   ;This method will check if the command action name already exists
                   (define (is-found?  name-action)
                     (let loop ((lst-commands (map to->list (send this get))))
                       (cond
                         ((null? lst-commands ) #f)
                         ((empty? lst-commands ) '())
                         ((equal? (caar lst-commands) name-action) #t)
                         (else (loop (cdr lst-commands)))))
                     );;end the method is found here
                   
                  ;This method check if the command action as already be added to the list
                      (define (is-defined? action)
                        (cond
                          [(not (command-object? (send action get))) #f]
                          [else     
                           (let loop [(lst-command (to->list action)) (command-lists (map to->list (send this get)))]
                             (cond 
                               [(null? command-lists) #f]
                               [(equal? (caar command-lists) (car lst-command)) #t]
                               [else (loop lst-command (cdr command-lists))]))])
                        );;end is-defined method here
                   
                   ;;check if the action command is set
                   (define (is-validated? action)
                     
                     (cond
                       [(not (command-object? (send action get))) (error "fatal error:@[is-valdated?] parameter mismatched")]
                       [else
                         (cond
                           [(eq? (send action get-name) "") (set-last-error "Enter command action name please!") #f]
                           [(eq? (send action get-desc) "") (set-last-error "Enter command action descriptions please!")  #f]
                           [(not (procedure? (send action get-procedure))) (set-last-error "add callback procedure for the command please")  #f]
                           [(eq? (send action get-on) "") (set-last-error "specified the on the action will act on please!")  #f]
                           [else #t])]))                                   
                   
                   ));;end of the class object




