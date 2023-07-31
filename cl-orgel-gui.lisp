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

(defmethod clog:create-div ((obj clog-obj) &rest args
                            &key (content "")
                              (style nil)
                              (hidden nil)
                            height width
                              (class nil)
                              (html-id nil)
                              (auto-place t)
                            &allow-other-keys)
  (when (or hidden style width height)
    (setf (getf args :style) (format nil "~@[~a~]~@[~a~]~@[~a~]~@[~a~]"
                                     (when hidden "visibility:hidden;") style
                                     (when width (format nil "width: ~a;" (addpx width)))
                                     (when height (format nil "height: ~a;" (addpx height))))))
  (dolist (key '(:hidden :html-id :auto-place :width :height)) (remf args key))
  (when class (setf (getf args :class) (format nil "~A" (escape-string class :html t))))
  (create-child obj (format nil "<div ~{~(~A~)= \"~(~a~)\"~^ ~}>~A</div>"
                            args
                            content)
                :clog-type  'clog-div
                :html-id    html-id
                :auto-place auto-place))

(defun on-new-window (body)
  (let ((orgel-gui (make-orgel-gui))
        connection-id)
    (clog-gui-initialize body)
    (setf connection-id (clog::connection-id body))
    (load-script (html-document body) "js/vumeter.js")
    (setf (title (html-document body)) "Orgel Sliders")
    (add-class body "w3-blue-grey") ;;; background color
    (load-css (html-document body) "/css/w3.css")
    (load-css (html-document body) "./css/custom-gui-elems.css")
    (setf (gethash "orgel-gui" (gethash connection-id clog-connection::*connection-data*))
          orgel-gui)
    ;; When doing extensive setup of a page using connection cache
    ;; reduces rountrip traffic and speeds setup.
    (let ((orgel (aref (orgel-gui-orgeln orgel-gui) 0))
          (global-orgel-ref (aref (orgel-gui-orgeln *curr-orgel-state*) 0)))
      (with-connection-cache (body)
        (let* (p1 p2 p3 p4 nbs1 nbs2 tg1-container tg2-container vsliders vu1 vu2)
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
          (create-div p1 :content "Orgel01" :style "align: bottom; padding-bottom: 10px;")
          (setf p4  (create-div p1 :width 190 :height 150 :style "display: flex;justify-content: space-between;flex: 0 0 auto;"))
          (setf p3  (create-div p4))
          (setf p2 (create-div p3 :style "width: 160px;height: 60px;display: flex;justify-content: space-between;margin-bottom: 10px"))
          (setf nbs1 (create-div p2 :style "width: 75px;font-size: 6pt;display: flex;flex-direction: column;justify-content: space-between;")) ;;; container for numberbox(es)
          (setf nbs2 (create-div p2 :style "width: 73px; font-size: 6pt;display: flex;flex-direction: column;justify-content: space-between;")) ;;; container for numberbox(es)
          (init-numboxes
           (:ramp-up :ramp-down :exp-base :base-freq :max-amp :min-amp)
           (nbs1 nbs1 nbs1 nbs2 nbs2 nbs2)
           orgel global-orgel-ref
           :size 6)
          (setf tg1-container (create-div nbs1 :style "display: flex;justify-content: right;")) ;;; container for right alignment of toggle
          (toggle tg1-container :content "phase" :toggle-content "inv" :size 6 :background "lightgreen" :selected-background "red"
                                :selected-foreground "white" :slot :phase
                                :receiver-fn (lambda (slot state obj) (declare (ignore obj))
                                               (format t "~S clicked, state: ~a!~%" slot state)))
          (setf tg2-container (create-div nbs2 :style "display: flex;justify-content: right;")) ;;; container for right alignment of toggle
          (toggle tg2-container :content "bandp" :toggle-content "notch" :size 6 :background "lightgreen" :selected-background "orange"
                                :style "align-content: right;" :slot :bandp
                                :receiver-fn (lambda (slot state obj) (declare (ignore obj))
                                               (format t "~S clicked, state: ~a!~%" slot state)))
;;;        (create-br p3)
          (setf *my-vu* (setf vu1 (multi-vu p3 :num 16 :width 160 :height 80 :led-colors :blue :direction :up :background "#444"
                                               :inner-background "#444"
                                               :border "none" :inner-border "thin solid black" :inner-padding-bottom "0px"
                                               :inner-padding "0"
                                               :style "margin-bottom: 10px;")))
          (create-div p1 :height 10)
          (setf vsliders (create-slider-panel p1 :label "Level"))
          (hslider p1 :background-color "#444" :color "#444" :thumbcolor "orange" :height "8px" :width "160px")
          (dolist (label '("Delay" "Bp" "Gain" "Osc-Level"))
            (setf vsliders (create-slider-panel p1 :label label)))
          (vslider p4 :style "width: 10px;height: 100% ;--slider-thumb-height: 2px;--slider-thumb-width: 100%;flex: 0 0 auto;"
                      :thumbcolor "orange" :color "#444" :background "#444")
;;;        (create-br p1)
          (loop for vsl in vsliders
                for idx from 0
                do (progn
                     (setf (value vsl) (aref (orgel-level-sliders global-orgel-ref) idx))
                     (setf (aref (orgel-level-sliders orgel) idx) vsl)))
          (setf vu2 (vumeter p1 :db-val -30 :led-colors :blue :direction :up))
          ;;        (setf (attribute vu1 "db-val") -100)
          ;;        (setf (attribute vu2 "db-val") 12)
          )))))

(defparameter *my-vu* nil)

;;; (setf (attribute *my-vu* "db-val") 4)

;;; (setf (width vu1))
    (defun start-orgel-gui ()
  "Start Orgel Gui."
  (initialize 'on-new-window
              :static-root (merge-pathnames "./www/"
                                            (asdf:system-source-directory :cl-orgel-gui)))
  (open-browser))

;;; (start-orgel-gui)

;;; (create-form-element)


;;; (create-context2d disp)

