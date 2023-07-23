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

(defun synchronize-vsl (idx val)
  (setf (aref (orgel-level-sliders *curr-orgel-state*) idx) val)
  (maphash (lambda (connection-id orgel)
             (declare (ignore connection-id))
             (setf (value (aref (orgel-level-sliders orgel) idx)) val))
           *global-connection-hash*))
(defstruct orgel
  (level-sliders (make-array 16)))

(defparameter *background-color* "#607d8b")

(defparameter *colors* #("#3071A9" "#00ff00" "#ffff00" "#ff00ff" "#00ffff" "#ff0380" "#33d5a4" "#1040d8"))

(defparameter *colors* #("#ffa0ff" "#ffffa0" "#a0ffff" "#ffe0e0" "#a0ffa0" "#e0e0ff" "#efd7e7" "#cccccc"))

(defparameter *vsl-colors*
  (map 'vector (lambda (idx)  (aref *colors* idx)) '(0 0 1 0 2 1 3 0 4 2 5 1 6 3 7 0)))

#|
(defun create-form-string (element-type &rest args
                                &key (name nil)
                                  (class nil)
                                  (style nil)
                                  (hidden nil)
                                  &allow-other-keys)
  (declare (ignorable name))
  (dolist (key '(name class style hidden html-id auto-place))
    (remf args key))
  (format nil "<input type='~A'~@[~A~]~@[~A~]~{~(A~)= '~A'~^ ~}/>"
                               (escape-string element-type :html t)
                               (when class
                                 (format nil " class='~A'"
                                         (escape-string class :html t)))
                               (when (or hidden style)
                                 (format nil " style='~@[~a~]~@[~a~]'"
                                         (when hidden "visibility:hidden;")
                                         style))
                               args))

(format nil "~{~(~A~)= '~A'~^ ~}" '(:name "Georg" :blah 0.0))

|#

(defvar *curr-orgel-state*
  (make-orgel :level-sliders (make-array 16 :initial-element 0.0)))

(defvar *global-connection-hash*
  (make-hash-table* :test 'equalp)
  "map connection to orgel form elems.")


#|
(defun send-message (user msg)
  (maphash (lambda (key value)
             (declare (ignore key))
             (create-span value :content (format nil "~A : ~A<br>" user msg))
             (setf (scroll-top value) (scroll-height value)))
*global-list-box-hash*))



(defun vslider (container &key (value 0.0) (min 0.0) (max 1.0))
  (create-form-element container :range :value (format nil "~a" value) :min (format nil "~a" min)
                                        :max (format nil "~a" max)
                                        :style "position: absolute;top: 40%; transform: rotate(270deg);"))
|#

(defun vslider (container &key (value 0.0) (min 0.0) (max 100.0) (color "#3071A9")
                            (background-color "#fff"))
  (create-form-element
   container :range
   :class "vslider"
   :style (format nil "--slider-color: ~A;background-color: ~A" color background-color)
   :value (format nil "~a" value)
   :min (format nil "~a" min)
   :max (format nil "~a" max)))

(defun numbox (container &key (value 0.0) (min 0.0) (max 100.0) (color "#3071A9")
                            (background-color "#fff"))
  (create-form-element
   container :text
   :class "numberbox"
   :style (format nil "--text-color: ~A;background-color: ~A" color background-color)))
   
(defparameter *sliders* nil)

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

(defun new-connection-id ()
  (uuid:make-v1-uuid))

(defparameter *mouse-down* nil)

(defun on-new-window (body)
  (let ((connection-id (new-connection-id))
        (orgel (make-orgel)))
    (clog-gui-initialize body)
    (setf (title (html-document body)) "Orgel Sliders")
    ;; When doing extensive setup of a page using connection cache
    ;; reduces rountrip traffic and speeds setup.
    (load-css (html-document body) "/css/w3.css")
    (load-css (html-document body) "./css/custom-gui-elems.css")
    (setf (gethash connection-id *global-connection-hash*) orgel)
    (with-connection-cache (body)
      (let* (;;last-tab
             ;; Note: Since the there is no need to use the tmp objects
             ;;       we reuse the same symbol name (tmp) even though
             ;;       the compiler can mark those for garbage collection
             ;;       early this is not an issue as the element is
             ;;       created already in the browser window.
             ;;
             ;;       See tutorial 33 for a far more elegant approach
             ;;       that uses with-clog-create for this type of code
             ;;       based layout.
             ;;
             ;; Create tabs and panels
             (tmp (create-br body))
             (p1  (create-div body))
             ;; Create form for panel 1
             (tmp (create-br p1))
             (f1 (create-form p1))
             (ms1  (create-div p1 :style "border: none;"))
             (vsliders (v-collect (n 16) (vslider ms1 :background-color *background-color* :color (aref *vsl-colors* n)))))
        (declare (ignore tmp))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Panel 1 contents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; (make-data-list vsl1 '("#ffffff"
        ;;                       "#ff0000"
        ;;                       "#00ff00"
        ;;                       "#0000ff"
        ;;                       "#ff00ff"))
        (loop for vsl in vsliders
              for idx from 0
              do (progn
                   (setf (width vsl) 100)
                   (setf (height vsl) 10)
                   (setf (value vsl) (aref (orgel-level-sliders *curr-orgel-state*) idx))
                   (let ((idx idx) (vsl vsl) mouse-down) ;;; close around function to catch the current value for idx and vsl
                     (setf (aref (orgel-level-sliders orgel) idx) vsl)
                     (set-on-input vsl
                                   (lambda (obj)
                                     (declare (ignore obj))
                                     (let ((val (value vsl)))
;;;                                       (format t "vsl~a: ~a~%" (1+ idx) val)
                                       (synchronize-vsl idx val))))
                     ;; (set-on-mouse-down vsl
                     ;;                    (lambda (obj event-data)
                     ;;                      (declare (ignore obj event-data))
                     ;;                      (setf *mouse-down* t)))
                     ;; (set-on-mouse-up vsl
                     ;;                    (lambda (obj event-data)
                     ;;                      (declare (ignore obj event-data))x
                     ;;                      (setf *mouse-down* nil)))
                     (set-on-mouse-move vsl
                                        (lambda (obj event-data)
                                          (declare (ignore obj))
                                          (when (getf event-data :shift-key)
                                            (let ((value (- 100 (getf event-data :y))))
;;;                                              (format t "vsl~a: ~a~%" (1+ idx) event-data)
                                              (when value
                                                (setf (value vsl) value)
                                                (synchronize-vsl idx value)
                                              ))))))))
        (setf (width p1) 162)
        (setf (height p1) 100)
        (setf (background-color p1) "w3-black")
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
        (add-class body "w3-blue-gray") ;;; background color
        ;; (let* ((menu-bar    (create-gui-menu-bar body))
        ;;        (icon-item   (create-gui-menu-icon menu-bar :on-click 'on-help-about))
        ;;        (full-screen (create-gui-menu-full-screen menu-bar)))
        ;;   (declare (ignore icon-item full-screen)))
        ))
    (run body)
    (remhash connection-id *global-connection-hash*)))

(defun start-orgel-gui ()
  "Start Orgel Gui."
  (setf *global-connection-hash* (make-hash-table* :test 'equalp))
  (initialize 'on-new-window
              :static-root (merge-pathnames "./www/"
                                            (asdf:system-source-directory :cl-orgel-gui)))
  (open-browser))

;;; (start-orgel-gui)

;;; (create-form-element)
