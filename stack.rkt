#lang racket
(provide stack%)

;;the sack module class  start here
(define stack% (class object%
     [super-new]
     [field (stack-lst '())]
  ;;the method to pop out value from the bottom of the stack
     (define/public (pop)
       (define v #f)
        (cond
           ((not (send this is-empty?))
            (set! v (car stack-lst)) (set! stack-lst (cdr stack-lst)  ))
           [else
            #f])
       v
       )
        ;;the method to pop out value from the top of the stack         
       (define/public (n-pop)
           (define v #f)
          (cond
           ((not (send this is-empty?))
            (set! v (car (reverse stack-lst))) (set! stack-lst (reverse (cdr (reverse stack-lst)  ))))
           [else
            #f])
       v
       )
  ;;the method that will push c into the stack
     (define/public (push c)
       (set! stack-lst 
        (let loop [(lst stack-lst)]
           (cond
              [(empty? lst) (cons c lst)]
              [else
                (cons (car lst) (loop (cdr lst)))])))
          )
      ;; the method that check if the stack is empty
    (define/public (is-empty?)
        (cond
           ((empty? stack-lst) #t)
           [else #f])
      )
  ;;the method that will return the size of the stack
  (define/public (get-size)
     (length stack-lst)
    )
   ;; the method that will empty the stack              
  (define/public (empty)
    (set! stack-lst '())
    )
                 
 ;;The method that will return the character in the stack as a string
(define/public (to-string)
  (define str "")
  (let loop( (chars stack-lst))
     (cond 
        ((empty? chars) "")
        (else
          (cond
            ((char? (car chars)) (set! str (string-append str (string (car chars)))))
            ((not (char? (car chars))) (set! str (string-append str (car chars))))
            )
          (loop (cdr chars))))) str)
  
  ));;end class stack








