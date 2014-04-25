#lang racket
(require racket/gui)
#|
The Module contains the required 

|#
(require "user.rkt")
(require "actions.rkt")
(require "system.rkt")
(provide symbol->number)
(provide pmove)


;;create functions
(define (pmove dir)
      (move (caar dir))
     )
;;create the pshut function to handle 
 (define (pshut dir)
   (shot (caar dir)))


  (define (wall-break)
     (define  help-window (new frame%
                               [label "Help"]
                               [width 300]
                               [height 300]))
    (send help-window  show #t)
                               
    )




;create the action lis
(define actions (new actions%))

;;create the user procedure to handle move function
(define user-shot (new action%))
(send  user-shot set-name 'shoot)
(send  user-shot set-desc "it fires shot to location of the monster ")
(send  user-shot set-parameter '(location))
(send  user-shot set-procedure pshut)
(send  user-shot set-on 'users)
(send actions add  user-shot)

(define user-move (new action%))
(send  user-move set-name 'move)
(send  user-move set-desc "allow the user to move to the exit[north,east,west,south]")
(send  user-move set-parameter '(exit))
(send  user-move set-procedure  pmove)
(send  user-move set-on 'users)
(send actions add  user-move)


;;create the user procedure to handle user to buy a bullet
(define purchase-bullet (new action%))
(send  purchase-bullet set-name 'purchase)
(send  purchase-bullet set-desc "purchase gun bullet armon")
(send  purchase-bullet set-parameter '())
(send  purchase-bullet set-procedure buy-bullet)
(send  purchase-bullet set-on 'users)
(send actions add   purchase-bullet)




;;create the user procedure to handle user to buy a bullet
(define p-reset (new action%))
(send  p-reset set-name 'reset)
(send  p-reset set-desc "restart the game")
(send  p-reset set-parameter '())
(send  p-reset set-procedure reset)
(send  p-reset set-on 'users)
(send actions add  p-reset)


;(sys-story) for the game story
(define p-story (new action%))
(send  p-story  set-name 'story)
(send  p-story  set-desc " The command display the story of the game and tutorial on how to play it ")
(send  p-story set-parameter '())
(send  p-story  set-procedure sys-story)
(send  p-story  set-on 'system)
(send actions add  p-story)


;;create the user procedure to handle user to buy a bullet
(define p-help (new action%))
(send  p-help  set-name 'help)
(send  p-help  set-desc " show the command list ")
(send  p-help  set-parameter '())
(send  p-help  set-procedure sys-help)
(send  p-help  set-on 'system)
(send actions add  p-help)

;;the procedure command clear the window 
(define p-sys-clear-conversation (new action%))
(send  p-sys-clear-conversation  set-name 'clear)
(send  p-sys-clear-conversation  set-desc " clear the conversation board at the left")
(send  p-sys-clear-conversation  set-parameter '())
(send  p-sys-clear-conversation  set-procedure sys-clear)
(send  p-sys-clear-conversation  set-on 'system)
(send actions add  p-sys-clear-conversation )

;;exit the system window
(define p-sys-exit (new action%))
(send  p-sys-exit  set-name 'close)
(send  p-sys-exit  set-desc " close the system window")
(send  p-sys-exit set-parameter '())
(send  p-sys-exit  set-procedure sys-exit)
(send  p-sys-exit  set-on 'system)
(send actions add  p-sys-exit )

;;break wall
(define p-wall-break (new action%))
(send  p-wall-break  set-name 'break)
(send  p-wall-break  set-desc "This command will break the wall then, it cost 2 bullet")
(send  p-wall-break  set-parameter '())
(send  p-wall-break  set-procedure wall-break)
(send  p-wall-break  set-on 'system)
(send actions add  p-wall-break )
