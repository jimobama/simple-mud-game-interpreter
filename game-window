#lang racket

;;The file contains the racket game window object which inherit from the in-build racket gui frame% object
(require racket/gui)
(require "constant.rkt")
(require "canvas-console.rkt" )
(require "user.rkt")
;;The file create the game window and the canvas
;console inserted on it to display the game view

(define game-window% (class frame%
                          [super-new]                         
                          [field (dialog-console (new canvas-console%
                                                       [parent this]
                                                       ))]                           
                          ))

                   
  
