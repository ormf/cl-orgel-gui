;;; 
;;; widget-defs.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2023 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;;
;;; Revision history: See git repository.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Gnu Public License, version 2 or
;;; later. See https://www.gnu.org/licenses/gpl-2.0.html for the text
;;; of this agreement.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; **********************************************************************

(in-package :cl-orgel-gui)

#|
non-selectable
user-select (none, bzw. auto)

style: user-select:

UserSelect 
|#

(defun multi-vslider (container &key (num 8) (width 80) (height 100) (background-color "white") (colors #("lightblue")) (thumbcolor "transparent")
                                  receiver-fn)
  (let* ((msl-container (create-div container
                                    :style (format nil "color: transparent; background-color: transparent;border: none;width: ~Apx;height: ~Apx;display: flex;" width height)))
         (vsliders (v-collect (n num) (vslider
                                       msl-container
                                       :border-right-width (if (< n (1- num)) 0 1)
                                       :background-color background-color
                                       :color (aref colors (mod n num))
                                       :thumbcolor thumbcolor))))
    (loop for vsl in vsliders
          for idx from 0
          do (let ((vsl vsl) (idx idx))
               (set-on-input
                vsl
                (lambda (obj)
                  (declare (ignore obj))
                  (let ((val (value vsl)))
                    (if receiver-fn (funcall receiver-fn idx val vsl)))))
               (set-on-mouse-move
                vsl
                (lambda (obj event-data)
                  (declare (ignore obj))
                  (when (or (getf event-data :shift-key))
                    (let ((val (- 100 (getf event-data :y))))
                      (when val
                        (setf (value vsl) val)
                        (if receiver-fn (funcall receiver-fn idx val vsl)))))))))
    (values vsliders msl-container)))

(defun vslider
    (container &key (value 0.0) (min 0.0) (max 100.0) (thumbcolor "black") (color "#3071A9")
                 (border-right-width 1) (background-color "#fff")
                 receiver-fn)
    "vertical slider including behaviour."
  (let ((vsl
          (create-form-element
           container :range
           :class "vslider"
           :style (format nil "--border-right-width: ~Apx;--slider-thumb: ~A;--slider-color: ~A;--slider-background: ~A;min-width: 0;flex: 1 1 0;height: 100%;"
                          border-right-width thumbcolor color background-color)
           :value (format nil "~a" value)
           :min (format nil "~a" min)
           :max (format nil "~a" max)
           :orient "vertical")))
    (if receiver-fn
        (set-on-input
         vsl
         (lambda (obj)
           (declare (ignore obj))
           (let ((val (value vsl)))
;;;                                       (format t "vsl~a: ~a~%" (1+ idx) val)
             (funcall receiver-fn val vsl)))))
    vsl))

(defun numbox (container &key (color "#3071A9")
                           (background-color "#fff")
                           (selected-foreground "black")
                           (selected-background "lightblue")
                           (value 0)
                           label
                           label-style
                           slot
                           receiver-fn)
  (let ((elem
          (create-form-element
           container :text
           :class "numbox"
           :value (format nil "~,1f" value)
           :style (format nil ";--text-color: ~A;background-color: ~A:--numbox-selected-foreground: ~A;--numbox-selected-background: ~A;"
                          color background-color selected-foreground selected-background)
           :label (if label (create-label container :content (ensure-string label) :style (or label-style "margin-right: 5px;")))))
        mouse-dragged
        startvalue)
;;;    (clog::unbind-event-script elem "onmousedown")
    (set-on-mouse-down
     elem
     (lambda (obj event-data)
       (declare (ignore obj))
       (setf startvalue (read-from-string (or (value elem) "0")))
       (let ((startpos (getf event-data :y)))
         (set-on-mouse-move
          elem
          (let ((last-y startpos) (last-val startvalue))
            (lambda (obj event-data)
              (declare (ignore obj))
              (let* ((y (getf event-data :y))
                     (scale (if (getf event-data :shift-key) 0.1 1))
                     (val (+ last-val (* scale (- last-y y)))))
                (when (/= y last-y)
                  (unless mouse-dragged
                    (setf (style elem "--numbox-selected-foreground") color)
                    (setf (style elem "--numbox-selected-background") background-color))
                  (setf mouse-dragged t)
                  (setf (value elem) (format nil "~,1f" val))
                  (synchronize-numbox(format nil "~,1f" val) elem)
                  (setf last-y y last-val val)))))))))
    (set-on-key-up
     elem
     (lambda (obj event)
       (declare (ignore obj))
       (when (equal (getf event :key) "Enter")
         (let ((val (value elem)))
           (unless (numberp (read-from-string val))
             (setf val (format nil "~,1f" startvalue)))
           (setf (value elem) val)
           (funcall receiver-fn slot val elem))
         (blur elem))))
    (set-on-mouse-up
     elem
     (lambda (obj event-data)
       (declare (ignore event-data))
       (set-on-mouse-move
        obj
        (lambda (obj event-data)
          (declare (ignore obj event-data))))
       (if mouse-dragged (progn
                           (blur elem)
                           (setf (style obj "--numbox-selected-foreground") selected-foreground))
                           (setf (style obj "--numbox-selected-background") selected-background))
                           (setf mouse-dragged nil)))
    elem))
