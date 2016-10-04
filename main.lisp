(in-package #:2048-mcclim)

;;; colors

(defun hex->color (r g b)
  (apply #'clim:make-rgb-color (mapcar (lambda (num)
                                         (float (/ num 255)))
                                       (list r g b))))

(defparameter +color-app-background+ (hex->color #xFA #xF8 #xEF))   ;#faf8ef
(defparameter +color-board-background+ (hex->color #xBB #xAD #xA0)) ;#bbada0

(clim:define-application-frame 2048-game
    () ()
    (:menu-bar nil)
    (:panes
     (game-main :application
                :width 800
                :heigh 600
                :background +color-app-background+))
    (:layouts
     (default game-main)))

(defun run ()
  (clim:run-frame-top-level (clim:make-application-frame '2048-game)))
