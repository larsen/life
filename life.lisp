;; Conway's Life

(ql:quickload "lispbuilder-sdl")

(defpackage :life
  (:use :cl :sdl)
  (:export main))

(in-package :life)

(defparameter *grid-width* 50)
(defparameter *grid-height* 50)
(defparameter *grid* nil)

(defparameter *window-width* 600)
(defparameter *window-height* 600)
(defparameter *window* nil)

(defun over-grid-do (grid f)
  (loop
     for i from 0 to (- *grid-width* 1)
     do (loop
           for j from 0 to (- *grid-height* 1)
           do (funcall f grid i j))))

(defun init-grid ()
  (let ((grid (make-array (list *grid-width* *grid-height*))))
    (over-grid-do grid
                  (lambda (g x y) (setf (aref g x y) (random 2))))
    grid))

(defun cell-width () (/ *window-width* *grid-width*))
(defun cell-height () (/ *window-height* *grid-height*))

;; Rules
;; 1. Any live cell with fewer than two live neighbours dies, as if caused by under-population.
;; 2. Any live cell with two or three live neighbours lives on to the next generation.
;; 3. Any live cell with more than three live neighbours dies, as if by over-population.
;; 4. Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction

(defun count-neighbours (grid i j)
  (flet ((get-status (x y)
           (aref grid x y))
         (up (idx)
           (if (= 0 idx) (- *grid-height* 1) (- idx 1)))
         (down (idx)
           (if (= (- *grid-height* 1) idx) 0 (+ idx 1)))
         (left (idx)
           (if (= 0 idx) (- *grid-width* 1) (- idx 1)))
         (right (idx)
           (if (= (- *grid-width* 1) idx) 0 (+ idx 1))))
    (+ (get-status (up i)   (left j))
       (get-status (up i)   j)
       (get-status (up i)   (right j))
       (get-status i        (left j))
       (get-status i        (right j))
       (get-status (down i) (left j))
       (get-status (down i) j)
       (get-status (down i) (right j)))))
                           
(defun new-cell-status (grid i j)
  (let ((current-status (aref grid i j))
        (neighbours (count-neighbours grid i j)))
    (cond ((and (= current-status 1) (< neighbours 2)) 0) ;; 1
          ((and (= current-status 1) (or (= neighbours 2)
                                         (= neighbours 3))) 1) ;; 2
          ((and (= current-status 1) (> neighbours 3)) 0) ;; 3
          ((and (= current-status 0) (= neighbours 3)) 1) ;; 4
          (t 0))))

(defun update-grid (grid)
  (let ((tmp-grid (make-array (list *grid-width* *grid-height*))))
    (over-grid-do tmp-grid
                  (lambda (g x y) (setf (aref g x y) (new-cell-status grid x y))))
    (setf *grid* tmp-grid)))

(defun render-grid (grid)
  (clear-display *black*)
  (let ((cw (cell-width))
        (ch (cell-height)))
    (over-grid-do grid
                  (lambda (g x y) (when (eq (aref g x y) 1)
                                    (draw-rectangle (rectangle :x (+ 1 (* x ch))
                                                               :y (+ 1 (* y cw))
                                                               :h (- ch 1)
                                                               :w (- cw 1))
                                                    :color *green*))))))

(defun main ()
  (setf *grid* (init-grid))
  (with-init ()
    (setf *window*
          (window *window-width* *window-height*))
    (setf (frame-rate) 24)
    (clear-display *black*)
    (with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
                       (case key
                         (:sdl-key-escape (push-quit-event))))
      (:idle ()
             (update-grid *grid*)
             (render-grid *grid*)
             (update-display)))))
