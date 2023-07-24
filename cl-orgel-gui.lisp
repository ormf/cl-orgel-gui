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
  (numbox "0.0")
  (level-sliders (make-array 16)))

(defparameter *curr-orgel-state*
  (make-orgel :level-sliders (make-array 16 :initial-element "0.0")
              :numbox "0"))

(defparameter *background-color* "#607d8b")

(defparameter *colors* #("#3071A9" "#00ff00" "#ffff00" "#ff00ff" "#00ffff" "#ff0380" "#33d5a4" "#1040d8"))

(defparameter *colors* #("#ffa0ff" "#ffffa0" "#a0ffff" "#ffe0e0" "#a0ffa0" "#e0e0ff" "#efd7e7" "#cccccc"))

(defparameter *vsl-colors*
  (map 'vector (lambda (idx)  (aref *colors* idx)) '(0 0 1 0 2 1 3 0 4 2 5 1 6 3 7 0)))

(defvar *global-connection-hash*
  (make-hash-table* :test 'equalp)
  "map connection to orgel form elems.")

(defun new-connection-id ()
  (format nil "~a" (uuid:make-v1-uuid)))

(defun synchronize-vsl (idx val self)
  (let ((val-string (ensure-string val)))
    (setf (aref (orgel-level-sliders *curr-orgel-state*) idx) val-string)
    (maphash (lambda (connection-id connection-hash)
               (declare (ignore connection-id))
               (let ((orgel (gethash "orgel" connection-hash)))
                 (when orgel (let ((elem (aref (orgel-level-sliders orgel) idx)))
                               (unless (equal self elem) (setf (value elem) val-string))))))
             clog-connection::*connection-data*)))

(defun synchronize-numbox (val self)
  (let ((val-string (ensure-string val)))
    (setf (orgel-numbox *curr-orgel-state*) val-string)
    (maphash (lambda (connection-id connection-hash)
               (declare (ignore connection-id))
               (let ((orgel (gethash "orgel" connection-hash)))
                 (when orgel
                   (let ((elem (orgel-numbox (gethash "orgel" connection-hash))))
                     (unless (equal self elem) (setf (value elem) val-string))))))
             clog-connection::*connection-data*)))



#|
(defun vslider (container &key (value 0.0) (min 0.0) (max 100.0) (color "#3071A9")
                            (background-color "#fff"))
  (create-form-element
   container :range
   :class "vslider"
   :style (format nil "--slider-color: ~A;background-color: ~A" color background-color)
   :value (format nil "~a" value)
   :min (format nil "~a" min)
   :max (format nil "~a" max)))
|#

(defun ensure-string (token)
  (if (stringp token) token (format nil "~S" token)))

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


(defparameter *vsliders* nil)

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
      (let* (p1 nbs1 nb1 vsliders)
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
        (setf nb1 (numbox nbs1 :label "freq" :color "black" :background-color "#fff"))
;;;        (setf ms1  (create-div p1 :style "color: transparent; border: none;width: 100px;height: 100px;display: flex;")) ;;; container der multisliders
        ;; (setf vsliders (v-collect (n 16) (vslider
        ;;                                   ms1
        ;;                                   :background-color *background-color*
        ;;                                   :color (aref *vsl-colors* n)
        ;;                                   :thumbcolor "transparent")))
        (setf vsliders
              (multi-vslider p1 :num 16 :width 160 :colors *vsl-colors* :background-color "transparent"
                                         :receiver-fn #'synchronize-vsl))

        (create-br p1)
        (multi-vslider p1 :colors *vsl-colors* :background-color "transparent")
        (loop for vsl in vsliders
              for idx from 0
              do (progn
                   (setf (value vsl)
                         (aref (orgel-level-sliders *curr-orgel-state*) idx))
                   (setf (aref (orgel-level-sliders orgel) idx) vsl)))
        (setf (value nb1) (orgel-numbox *curr-orgel-state*))
        (setf (orgel-numbox orgel) nb1)

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
