;;; 
;;; cl-orgel-gui.lisp
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

(defstruct orgel
  (numbox 0.0)
  (level-sliders (make-array 16)))

(defparameter *background-color* "#607d8b")

(defparameter *colors* #("#3071A9" "#00ff00" "#ffff00" "#ff00ff" "#00ffff" "#ff0380" "#33d5a4" "#1040d8"))

(defparameter *colors* #("#ffa0ff" "#ffffa0" "#a0ffff" "#ffe0e0" "#a0ffa0" "#e0e0ff" "#efd7e7" "#cccccc"))

(defparameter *vsl-colors*
  (map 'vector (lambda (idx)  (aref *colors* idx)) '(0 0 1 0 2 1 3 0 4 2 5 1 6 3 7 0)))

(defparameter *curr-orgel-state*
  (make-orgel :level-sliders (make-array 16 :initial-element 0.0)))

(defvar *global-connection-hash*
  (make-hash-table* :test 'equalp)
  "map connection to orgel form elems.")

(defun new-connection-id ()
  (format nil "~a" (uuid:make-v1-uuid)))

(defun synchronize-vsl (idx val self)
  (setf (aref (orgel-level-sliders *curr-orgel-state*) idx) val)
  (maphash (lambda (connection-id connection-hash)
             (declare (ignore connection-id))
             (let ((elem (aref (orgel-level-sliders (gethash "orgel" connection-hash))
                               idx)))
               (unless (equal self elem) (setf (value elem) val))))
           clog-connection::*connection-data*))

(defun synchronize-numbox (val self)
  (setf (orgel-numbox *curr-orgel-state*) val)
  (maphash (lambda (connection-id connection-hash)
             (declare (ignore connection-id))
             (let ((elem (orgel-numbox (gethash "orgel" connection-hash))))
               (unless (equal self elem) (setf (value elem) val))))
           clog-connection::*connection-data*))

(defun vslider (container &key (value 0.0) (min 0.0) (max 100.0) (color "#3071A9")
                            (background-color "#fff"))
  (create-form-element
   container :range
   :class "vslider"
   :style (format nil "--slider-color: ~A;background-color: ~A" color background-color)
   :value (format nil "~a" value)
   :min (format nil "~a" min)
   :max (format nil "~a" max)))

(defun numbox (container &key (color "#3071A9")
                           (background-color "#fff")
                           (value 0))
  (let ((elem
          (create-form-element
           container :text
           :class "numbox"
           :value (format nil "~,1f" value)
           :style (format nil "--numbox-selected-foreground: ~A;--text-color: ~A;background-color: ~A" "green" color background-color))))
;;;    (clog::unbind-event-script elem "onmousedown")
    (set-on-mouse-down
     elem
     (lambda (obj event-data)
       (declare (ignore obj))
       (let ((startpos (getf event-data :y))
             (startvalue (read-from-string (or (value elem) "0"))))
         (set-on-mouse-move
          elem
          (let ((last-y startpos) (last-val startvalue))
            (lambda (obj event-data)
              (declare (ignore obj))
              (let* ((y (getf event-data :y))
                     (scale (if (getf event-data :shift-key) 0.1 1))
                     (val (+ last-val (* scale (- last-y y)))))
                (setf (value elem) (format nil "~,1f" val))
                (synchronize-numbox(format nil "~,1f" val) elem)
                (setf last-y y last-val val))))))))
    (set-on-mouse-up
     elem
     (lambda (obj event-data)
       (declare (ignore event-data))
       (set-on-mouse-move
        obj
        (lambda (obj event-data)
          (declare (ignore obj event-data))))))
    elem))

(defun on-help-about (obj)
  (let* ((about (create-gui-window obj
                                   :title   "About"
                                   :content "<div class='w3-black'>
                                         <center><img src='/img/clogwicon.png'></center>
                                         <center>clog-plunger</center></div>
                                         <div><p><center>A New App</center>
                                         <center>(c) 2022 - Ungefähr</center></p></div>"
                                   :hidden  t
                                   :width   200
                                   :height  200)))
    (window-center about)
    (setf (visiblep about) t)
    (set-on-window-can-size about (lambda (obj)
                                    (declare (ignore obj))()))))


(defun on-new-window (body)
  (let ((orgel (make-orgel))
        connection-id)
    (clog-gui-initialize body)
    (setf connection-id (clog::connection-id body))
;;;    (load-script (html-document body) "js/numberbox.js")
    (setf (title (html-document body)) "Orgel Sliders")
    ;; When doing extensive setup of a page using connection cache
    ;; reduces rountrip traffic and speeds setup.
    (load-css (html-document body) "/css/w3.css")
    (load-css (html-document body) "./css/custom-gui-elems.css")
    (setf (gethash "orgel" (gethash connection-id clog-connection::*connection-data*))
          orgel)
    (with-connection-cache (body)
      (let* (p1 nbs1 nb1 ms1 vsliders)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Panel 1 contents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; (make-data-list vsl1 '("#ffffff"
        ;;                       "#ff0000"
        ;;                       "#00ff00"
        ;;                       "#0000ff"
        ;;                       "#ff00ff"))
        (create-br body)
        (setf p1  (create-div body :style "margin-left: 20px;"))
        ;; Create form for panel 1
;;;        (create-br p1)
        (setf nbs1 (create-div p1)) ;;; container for numberbox(es)
        (setf nb1 (numbox nbs1))
        (setf ms1  (create-div p1 :style "border: none;")) ;;; container vor multisliders
        (setf vsliders (v-collect (n 16) (vslider
                                          ms1
                                          :background-color *background-color*
                                          :color (aref *vsl-colors* n))))
        (setf (value nb1) (orgel-numbox *curr-orgel-state*))
        (setf (orgel-numbox orgel) nb1)
        (loop for vsl in vsliders
              for idx from 0
              do (progn
                   (setf (width vsl) 100)
                   (setf (height vsl) 10)
                   (setf (value vsl) (aref (orgel-level-sliders *curr-orgel-state*) idx))
                   (let ((idx idx) (vsl vsl)) ;;; close around function to catch the current value for idx and vsl
                     (setf (aref (orgel-level-sliders orgel) idx) vsl)
                     (set-on-input vsl
                                   (lambda (obj)
                                     (declare (ignore obj))
                                     (let ((val (value vsl)))
;;;                                       (format t "vsl~a: ~a~%" (1+ idx) val)
                                       (synchronize-vsl idx val vsl))))
                     ;; (set-on-mouse-down vsl
                     ;;                    (lambda (obj event-data)
                     ;;                      (declare (ignore obj event-data))
                     ;;                      (setf *mouse-down* t)))
                     ;; (set-on-mouse-up vsl
                     ;;                    (lambda (obj event-data)
                     ;;                      (declare (ignore obj event-data))x
                     ;;                      (setf *mouse-down* nil)))
                     (set-on-mouse-move
                      vsl
                      (lambda (obj event-data)
                        (declare (ignore obj))
                        (when (getf event-data :shift-key)
                          (let ((value (- 100 (getf event-data :y))))
;;;                                              (format t "vsl~a: ~a~%" (1+ idx) event-data)
                            (when value
                              (setf (value vsl) value)
                              (synchronize-vsl idx value vsl)
                              ))))))))
;;;        (setf (width p1) 162)
;;;        (setf (height p1) 100)
;;;        (setf (background-color p1) "w3-black")
;;;        (set-border p1 :thin :solid :black)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Panel 2 contents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Panel 3 contents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Tab functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (add-class body "w3-blue-grey") ;;; background color
        (add-class nb1 "w3-text-black") ;;; text style
        ;; (let* ((menu-bar    (create-gui-menu-bar body))
        ;;        (icon-item   (create-gui-menu-icon menu-bar :on-click 'on-help-about))
        ;;        (full-screen (create-gui-menu-full-screen menu-bar)))
        ;;   (declare (ignore icon-item full-screen)))
        ))
    (run body)
;;;    (remhash connection-id *global-connection-hash*)
    ))

(defun start-orgel-gui ()
  "Start Orgel Gui."
  (setf *global-connection-hash* (make-hash-table* :test 'equalp))
  (initialize 'on-new-window
              :static-root (merge-pathnames "./www/"
                                            (asdf:system-source-directory :cl-orgel-gui)))
  (open-browser))

;;; (start-orgel-gui)

;;; (create-form-element)
