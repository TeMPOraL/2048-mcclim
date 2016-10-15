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
(defvar *game-timer* nil)


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
                :scroll-bars nil
                :display-function 'draw-score-box)
     (tag-line :label
               :label "Join the numbers and get to the 2048 tile!")
     (new-game-button :push-button
                      :label "New game")
     (timer-box :application
                :scroll-bars nil
                :display-function 'draw-timer-box)
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
           (2/32 title-label)
           (1/32 score-box)
           (1/32 (clim:horizontally ()
                  tag-line new-game-button))
           (1/32 timer-box)
           (25/32 game-main)
           (1/32 instructions-box)
           (1/32 (setf (game-debug-output clim:*application-frame*) debug-output))))))


;;; utils
(defun debug-format (format-string &rest format-args)
  (when clim:*application-frame*
    (setf (clim:gadget-value (game-debug-output clim:*application-frame*))
          (apply #'format nil format-string format-args))))


;;; game logic
;;; TODO

(defun game-timer-started ()
  *game-timer*)

(defun start-game-timer ()
  (setf *game-timer* (local-time:now)))

(defun stop-game-timer ()
  (setf *game-timer* nil))

(defun get-game-time-as-string ()
  (if *game-timer*
      (progn
        (let* ((diff (local-time:timestamp-difference (local-time:now) *game-timer*))
               (seconds (floor (mod diff 60)))
               (minutes (floor (mod (/ diff 60) 60)))
               (hours (floor (/ diff 3600))))
          (format nil "~2,'0d:~2,'0d:~2,'0d" hours minutes seconds)))
      (progn
        (debug-format "Game timer not started!")
        "00:00:00")))


;;; input
;;; TODO Figure out one day how to use arrow keys instead.
(define-2048-game-command (move-left :keystroke #\a)
    ()
  (debug-format "CMD: Move left invoked!"))

(define-2048-game-command (move-right :keystroke #\d)
    ()
  (debug-format "CMD: Move right invoked!"))

(define-2048-game-command (move-up :keystroke #\w)
    ()
  (debug-format "CMD: Move up invoked!"))

(define-2048-game-command (move-down :keystroke #\s)
    ()
  (debug-format "CMD: Move down invoked!"))


;;; drawing

(defun board-cell-background-ink (value)
  (declare (ignore value))
  ;; TODO
  +color-app-background+)

(defun board-cell-text-ink (value)
  (declare (ignore value))
  ;; TODO
  +color-board-background+)

(defun board-cell-text-size (value)
  (declare (ignore value))
  ;; TODO
  24)

(defun draw-board-cell (stream board row column)
  ;; TODO prettier game board
  (let ((value (aref board row column)))
    (clim:draw-rectangle* stream
                          0 0
                          100 100
                          :ink (board-cell-background-ink value))
    (clim:draw-text* stream
                     (format nil "~D" value)
                     50 50
                     :align-x :center
                     :align-y :center
                     :ink (board-cell-text-ink value)
                     :text-size (board-cell-text-size value))))

(defmethod draw-game-board ((2048-game 2048-game) stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (clim:draw-rectangle* stream 0 0 450 450  :ink +color-board-background+)
  (clim:formatting-table (stream :x-spacing 10 :y-spacing 10)
    (dotimes (row 4)
      (clim:formatting-row (stream)
        (dotimes (column 4)
          (clim:formatting-cell (stream :align-x :center :align-y :center)
            (draw-board-cell stream (game-board 2048-game) row column))))))
  #+nil(clim:with-room-for-graphics (stream)                              ;FIXME do I need it?
                                    (clim:with-translation (stream 10 10) ;FIXME why this doesn't work?
                                                           )))

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

(defmethod draw-timer-box ((2048-game 2048-game) stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (format stream (get-game-time-as-string)))


;;; entry point
(defun run ()
  (clim:run-frame-top-level (clim:make-application-frame '2048-game)))
