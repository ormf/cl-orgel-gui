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

(defmethod create-form-element ((obj clog-obj) element-type
                                &key (name nil)
                                  (value nil)
                                  (min nil)
                                  (max nil)
                                  (label nil)
                                  (class nil)
                                  (style nil)
                                  (hidden nil)
                                  (html-id nil)
                                  (auto-place t))
  (let ((element (create-child
                  obj (format nil "<input type='~A'~@[~A~]~@[~A~]~@[~A~]~@[~A~]~@[~A~]~@[~A~]/>"
                              (escape-string element-type :html t)
                              (when class
                                (format nil " class='~A'"
                                        (escape-string class :html t)))
                              (when (or hidden style)
                                (format nil " style='~@[~a~]~@[~a~]'"
                                        (when hidden "visibility:hidden;")
                                        style))
                              (when value (format nil " value='~A'" (escape-string value :html t)))
                              (when min (format nil " min='~A'" (escape-string min :html t)))
                              (when max (format nil " max='~A'" (escape-string max :html t)))
                              (when name  (format nil " name='~A'" (escape-string name :html t))))
                  :clog-type  'clog-form-element
                  :html-id    html-id
                  :auto-place auto-place)))
    (when label
      (label-for label element))
    element))

(defun vslider (container &key (value 0.0) (min 0.0) (max 100.0))
  (create-form-element
   container :range
   :value (format nil "~a" value)
   :min (format nil "~a" min)
   :max (format nil "~a" max)
   :style "position: relative; margin-top: 45px; margin-bottom: 45px;margin-left: -45px;margin-right: -45px; transform: rotate(270deg);"))
   
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

(defun on-new-window (body)
  (let ((connection-id (new-connection-id))
        (orgel (make-orgel)))
    (clog-gui-initialize body)
    (setf (title (html-document body)) "Orgel Sliders")
    ;; When doing extensive setup of a page using connection cache
    ;; reduces rountrip traffic and speeds setup.
    (load-css (html-document body) "/css/w3.css")
    (load-css (html-document body) "/css/custom-gui-elems.css")
    (setf (gethash connection-id *global-connection-hash*) orgel)
    (with-connection-cache (body)
      (create-style-block body :content "input[type=range] {height: 18px;-webkit-appearance: none; */margin: 0px 0;width: 100px;height: 10px;-moz-range-track: {width: 100%;height: 10px;cursor: pointer;animate: 0.2s;background: #ffffff;border-radius: 0px;border: 1px solid #000000;}}input[type=range]:focus {outline: none;}input[type=range]::-webkit-slider-runnable-track {width: 100%;height: 10px;cursor: pointer;animate: 0.2s;background: #3071A9;border-radius: 5px;border: 1px solid #000000;}input[type=range]::-webkit-slider-thumb {box-shadow: 1px 1px 1px #000000;border: 1px solid #000000;height: 10px;width: 1px;border-radius: 0px;background: #FFFFFF;cursor: pointer;-webkit-appearance: none;margin-top: -1px;}input[type=range]:focus::-webkit-slider-runnable-track {background: #3071A9;}input[type=range]::-moz-range-track {width: 100%;height: 10px;cursor: pointer;animate: 0.2s;background: #ffffff;border-radius: 0px;border: 1px solid #000000;}input[type=range]::-moz-range-progress {background: #6666cc;height: 10px;}input[type=range]::-moz-range-thumb {display: none;border: none;height: 10px;width: 0px;border-radius: 0px;background: #000;cursor: pointer;}input[type=range]::-ms-track {width: 100%;height: 10px;cursor: pointer;animate: 0.2s;background: transparent;border-color: transparent;color: transparent;}input[type=range]::-ms-fill-lower {background: #3071A9;border: 1px solid #000000;border-radius: 0px;box-shadow: 1px 1px 1px #000000;}input[type=range]::-ms-fill-upper {background: #ffffff;border: 1px solid #000000;border-radius: 0px;box-shadow: 1px 1px 1px #000000;}input[type=range]::-ms-thumb {margin-top: 1px;border: none;height: 10px;width: 1px;border-radius: 0px;background: #FFFFFF;cursor: pointer;}input[type=range]:focus::-ms-fill-lower {background: #3071A9;}input[type=range]:focus::-ms-fill-upper {background: #ffffff}")

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
             (f1  (create-form p1))
             (vsliders (v-collect (n 16) (vslider f1))))
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
                   (let ((idx idx) (vsl vsl)) ;;; close around function to catch the current value for idx and vsl
                     (setf (aref (orgel-level-sliders orgel) idx) vsl)
                     (set-on-input vsl
                                   (lambda (obj)
                                     (declare (ignore obj))
                                     (let ((val (value vsl)))
;;;                                       (format t "vsl~a: ~a~%" (1+ idx) val)
                                       (synchronize-vsl idx val)))))))
        (setf (width p1) 162)
        (setf (height p1) 100)
        (setf (background-color p1) "w3-black")
        (set-border p1 :thin :solid :black)
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
  (initialize 'on-new-window
              :static-root (merge-pathnames "www/"
                                            (asdf:system-source-directory :cl-orgel-gui)))
  (open-browser))
