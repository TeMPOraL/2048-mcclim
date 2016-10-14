(in-package #:2048-mcclim)

;;; colors

(defun hex->color (r g b)
  (apply #'clim:make-rgb-color (mapcar (lambda (num)
                                         (float (/ num 255)))
                                       (list r g b))))

(defparameter +color-app-background+ (hex->color #xFA #xF8 #xEF))   ;#faf8ef
(defparameter +color-board-background+ (hex->color #xBB #xAD #xA0)) ;#bbada0

(defvar *title-text-style* (clim:make-text-style :fix :bold 24))

(defvar *game-highscore* 0)


;;; game UI elements and layout
(clim:define-application-frame 2048-game
    ()
  ((game-board :initform (make-array '(4 4))
               :accessor game-board)
   (game-score :initform 0
               :accessor game-score)
   (game-highscore :initform *game-highscore*
                   :accessor game-highscore)
   (game-debug-output :initform nil
                      :accessor game-debug-output))
    (:menu-bar nil)
    (:panes
     ;; (window-title :title
     ;;               :display-string "2048-McCLIM")
     (title-label :label
                  :label "2048-McCLIM"
                  :align-x :center
                  :text-style *title-text-style*)
     (score-box :application
                :align-x :right
                :scroll-bars nil
                :display-function 'draw-score-box)
     (tag-line :label
               :label "Join the numbers and get to the 2048 tile!")
     (new-game-button :push-button
                      :label "New game")
     (timer-box :text-field
                :editable-p nil
                :value "00:00:00")
     (game-main :application
                :width 800
                :height 800
                :scroll-bars nil
                :background +color-app-background+
                :display-function 'draw-game-board)
     (instructions-box :label
                       :label "Use WASD keys to play.")
     (debug-output :text-field          ;would prefer it to be a label, but whatevs.
                   :editable-p nil
                   :value "<DEBUG OUTPUT>"))
    (:layouts
     (default
         (clim:vertically ()
           title-label
           score-box
           (clim:horizontally ()
             tag-line new-game-button)
           timer-box
           game-main
           instructions-box
           (setf (game-debug-output clim:*application-frame*) debug-output)))))


;;; game logic
;;; TODO


;;; input
;;; TODO Figure out one day how to use arrow keys instead.
(define-2048-game-command (move-left :keystroke #\a)
    ()
  (let ((label (game-debug-output clim:*application-frame*)))
    (setf (clim:gadget-value label) "Move left invoked!")))

(define-2048-game-command (move-right :keystroke #\d)
    ()
  (let ((label (game-debug-output clim:*application-frame*)))
    (setf (clim:gadget-value label) "Move right invoked!")))

(define-2048-game-command (move-up :keystroke #\w)
    ()
  (let ((label (game-debug-output clim:*application-frame*)))
    (setf (clim:gadget-value label) "Move up invoked!")))

(define-2048-game-command (move-down :keystroke #\s)
    ()
  (let ((label (game-debug-output clim:*application-frame*)))
    (setf (clim:gadget-value label) "Move down invoked!")))


;;; drawing

(defun draw-board-cell (stream board row column)
  ;; TODO prettier game board
  (format stream "|(~A ~A) - ~A|" row column (aref board row column)))

(defmethod draw-game-board ((2048-game 2048-game) stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (clim:formatting-table (stream)
    (dotimes (row 4)
      (clim:formatting-row (stream)
        (dotimes (column 4)
          (clim:formatting-cell (stream)
            (draw-board-cell stream (game-board 2048-game) row column)))))))

(defmethod draw-score-box ((2048-game 2048-game) stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (clim:formatting-table (stream)
    (clim:formatting-column (stream)
      (clim:formatting-cell (stream :align-x :center)
        (format stream "SCORE"))
      (clim:formatting-cell (stream :align-x :center)
        (format stream "~A" (game-score 2048-game))))
    (clim:formatting-column (stream)
      (clim:formatting-cell (stream :align-x :center)
        (format stream "BEST"))
      (clim:formatting-cell (stream :align-x :center)
        (format stream "~A" (game-highscore 2048-game))))))

;;; entry point
(defun run ()
  (clim:run-frame-top-level (clim:make-application-frame '2048-game)))
