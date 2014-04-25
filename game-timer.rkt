#lang racket
(provide game-timer%)
(define  game-timer% (class object%
                       [super-new]                      
                       [field (secs 0)]
                       [field  (mins 0)]  
                       [field (hrs 0)]   
                       ;;get the mini
                       (define/public (get-minutes)
                         mins)
                       (define/public (get-seconds)
                         secs)
                       (define/public (to-string)
                          (format "~a:~a:~a "hrs mins secs)
                         )
                       
                       ;;increase mini
                        (define/public (counter)
                          
                            (cond
                               [(not (number? mins)) (set! mins 0)]
                               [(= secs 59) (set! mins (+ mins 1)) (set! secs 1)]
                               [(= mins 59) (set! hrs (+ hrs  1)) (set! mins 0)]
                               [else
                                 (set! secs [+ secs 1])])                            
                          )
                       (define/public (reset)
                         
                         (set! secs 1)
                         (set! mins 0)
                         (set! hrs 0)
                         
                         )
                       
                       ))
