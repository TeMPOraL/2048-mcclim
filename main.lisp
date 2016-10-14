(in-package #:2048-mcclim)

;;; colors

(defun hex->color (r g b)
  (apply #'clim:make-rgb-color (mapcar (lambda (num)
                                         (float (/ num 255)))
                                       (list r g b))))

(defparameter +color-app-background+ (hex->color #xFA #xF8 #xEF))   ;#faf8ef
(defparameter +color-board-background+ (hex->color #xBB #xAD #xA0)) ;#bbada0

(defvar *title-text-style* (clim:make-text-style :fix :bold 24))

(defun make-game-cell (x y)
  (clim:make-pane 'clim:push-button
                  :label (format nil "(~A ~A)" x y)
                  :width 64
                  :height 64))

(clim:define-application-frame 2048-game
    () ()
    (:menu-bar nil)
    (:panes
     ;; (window-title :title
     ;;               :display-string "2048-McCLIM")
     (game-main :application
                :width 800
                :height 800
                :scroll-bars nil
                :background +color-app-background+)
     (title-label :label
                  :label "2048-McCLIM"
                  :align-x :center
                  :text-style *title-text-style*)
     (cell-0 (make-game-cell 0 0))
     (cell-1 (make-game-cell 0 1))
     (cell-2 (make-game-cell 0 2))
     (cell-3 (make-game-cell 0 3))
     (cell-4 (make-game-cell 1 0))
     (cell-5 (make-game-cell 1 1))
     (cell-6 (make-game-cell 1 2))
     (cell-7 (make-game-cell 1 3))
     (cell-8 (make-game-cell 2 0))
     (cell-9 (make-game-cell 2 1))
     (cell-A (make-game-cell 2 2))
     (cell-B (make-game-cell 2 3))
     (cell-C (make-game-cell 3 0))
     (cell-D (make-game-cell 3 1))
     (cell-E (make-game-cell 3 2))
     (cell-F (make-game-cell 3 3)))
    (:layouts
     (default
         (clim:vertically ()
           title-label
           game-main
           (clim:tabling (:grid nil)
             (list cell-0 cell-1 cell-2 cell-3)
             (list cell-4 cell-5 cell-6 cell-7)
             (list cell-8 cell-9 cell-A cell-B)
             (list cell-C cell-D cell-E cell-F))))))

(defun run ()
  (clim:run-frame-top-level (clim:make-application-frame '2048-game)))
