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

(defparameter *my-vus* nil)
(defparameter *preset-panel* nil)


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
                                     (when hidden "visibility:hidden;")
                                     style
                                     (when width (format nil "width: ~a;" (addpx width)))
                                     (when height (format nil "height: ~a;" (addpx height))))))
  (dolist (key '(:hidden :html-id :auto-place :width :height)) (remf args key))
;;;  (break "args: ~S" args)
  (when class (setf (getf args :class) (format nil "~A" (escape-string class :html t))))
  (create-child obj (format nil "<div ~{~(~A~)= \"~(~a~)\"~^ ~}>~A</div>"
                            args
                            content)
                :clog-type  'clog-div
                :html-id    html-id
                :auto-place auto-place))

(defun create-orgel-gui (orgelidx container orgel global-orgel-ref)
      (let* (p1 p2 p3 p4 p5 p6 p7 nbs1 nbs2 tg1-container tg2-container vsliders)
        (create-br container)
        (setf p1  (create-div container :style "margin-left: 10px;"))
        (create-div p1 :content (format nil "Orgel~2,'0d" (1+ orgelidx)) :style "align: bottom; padding-bottom: 10px;")
        (setf p4  (create-div p1 :width 180 :height 150 :style "display: flex;justify-content: space-between;flex: 0 0 auto;"))
        (setf p3  (create-div p4))
        (setf p2 (create-div p3 :style "width: 160px;height: 60px;display: flex;justify-content: space-between;margin-bottom: 10px"))
        (setf nbs1 (create-div p2 :style "width: 75px;font-size: 6pt;display: flex;flex-direction: column;justify-content: space-between;")) ;;; container for numberbox(es)
        (setf nbs2 (create-div p2 :style "width: 73px; font-size: 6pt;display: flex;flex-direction: column;justify-content: space-between;")) ;;; container for numberbox(es)
        (init-numboxes
         (:ramp-up :ramp-down :exp-base :base-freq :max-amp :min-amp)
         (nbs1 nbs1 nbs1 nbs2 nbs2 nbs2)
         orgelidx orgel global-orgel-ref
         :size 6)
        (setf tg1-container (create-div nbs1 :style "display: flex;justify-content: right;")) ;;; container for right alignment of toggle
        (init-toggle :phase tg1-container orgelidx orgel global-orgel-ref :content "phase" :toggle-content "inv" :size 6 :background "lightgreen" :selected-background "red" :selected-foreground "white")
        (setf tg2-container (create-div nbs2 :style "display: flex;justify-content: right;")) ;;; container for right alignment of toggle
        (init-toggle :bandp tg2-container orgelidx orgel global-orgel-ref :content "bandp" :toggle-content "notch" :size 6 :background "lightgreen" :selected-background "orange" :selected-foreground "black")
        (setf p7 (create-div p3 :style "position: relative;" :height 80 :width 160))
        (multiple-value-bind (vus vu-container) (multi-vu p7 :num 16 :width 160 :height 80 :led-colors :blue :direction :up :background "#444"
                                                          :inner-background "#444"
                                                          :border "none" :inner-border "thin solid black" :inner-padding-bottom "0px"
                                                          :inner-padding "0"
                                                          :style "margin-bottom: 10px;position: absolute;top: 0;left: 0;")
          (declare (ignore vus))
          (when (zerop orgelidx)
            (let ((preset-panel
                    (create-div p7 :height 80
                                   :style "border: thin solid black;position: absolute;top: 0;left: 0;display: none;justify-content: space-between;width: 100%;")))

              (create-div preset-panel :content "Presets" :style "margin: 2px;")
              (let* ((prv (create-button preset-panel :class "btn" :content "prev" :style "font-size: 8px;background: #bbb;"))
                     (nb (numbox preset-panel :size 6 :min 0 :max 127))
                     (nxt (create-button preset-panel :class "btn" :content "next" :style "font-size: 8px;background: #bbb;")))
                (set-on-click
                 prv
                 (lambda (obj)
                   (declare (ignore obj))
;;;                   (format t "prv clicked!~%")
                   (let ((curr (read-from-string (value nb))))
                     (when (> curr (read-from-string (attribute nb "min")))
                       (setf (value nb) (1- curr))))))
                (set-on-click
                 nxt
                 (lambda (obj)
                   (declare (ignore obj))
                   (let ((curr (read-from-string (value nb))))
                     (when (< curr (read-from-string (attribute nb "max")))
                       (setf (value nb) (1+ curr))))
;;;                   (format t "next clicked!~%")
                   )))
              (create-br preset-panel)
              (create-button preset-panel :class "btn" :content "recall" :style "font-size: 8px;background: #d5ffd5;")
              (create-button preset-panel :class "btn" :content "store" :style "font-size: 8px;background: #ffd5d5;")
              (create-br preset-panel)
              (create-button preset-panel :class "btn" :content "load"  :style "font-size: 8px;background: #d5ffd5;")
              (create-button preset-panel :class "btn" :content "save"  :style "font-size: 8px;background: #ffd5d5;")
              (setf *my-vus* vu-container)
              (setf *preset-panel* preset-panel)
              (install-preset-key-switch container (html-id vu-container) (html-id preset-panel))
              (set-on-key-up container
                             (lambda (obj event) (declare (ignore obj))
                               (format t "keyup!~%"))))))
        ;;; main volume slider
        (init-vslider :main-volume p4 orgelidx orgel global-orgel-ref)
        (create-div p1 :height 10) ;;; distance
        (setf p5 (create-div p1 :width 180 :height 100 :style "padding-bottom: 5px;display: flex;justify-content: space-between;flex: 0 0 auto;"))
        (setf p6 (create-div p5 :style "display: block;"))
        (setf vsliders (create-slider-panel p6 :label "Level" :receiver-fn (make-orgel-array-receiver :level-sliders orgelidx global-orgel-ref)))
        (init-vslider :bw p5 orgelidx orgel global-orgel-ref)
        (loop for vsl in vsliders
              for idx from 0
              do (progn
                   (setf (value vsl) (aref (orgel-level-sliders global-orgel-ref) idx))
                   (setf (aref (orgel-level-sliders orgel) idx) vsl)))
;;;        (hslider p1 :background "#444" :color "#444" :thumbcolor "orange" :height "8px" :width "160px")
        (init-hslider :bias p1 orgelidx orgel global-orgel-ref :height "8px" :width "160px")

        (dolist (label '("Delay" "Bp" "Gain" "Osc-Level"))
          (let ((slot-name (make-symbol (format nil "~:@(~a-sliders~)" label))))
            (setf vsliders (create-slider-panel p1
                                                :label label
                                                :receiver-fn (make-orgel-array-receiver
                                                              slot-name
                                                              orgelidx global-orgel-ref)))

;;;        (create-br p1)
            (let ((accessor-fn (slot->function "orgel" slot-name)))
              (loop for vsl in vsliders
                    for idx from 0
                    do (progn
                         (setf (value vsl) (aref (funcall accessor-fn global-orgel-ref) idx))
                         (setf (aref (funcall accessor-fn orgel) idx) vsl))))))
;;        (setf vu2 (vumeter p1 :data-db -30 :led-colors :blue :direction :up))
        ;;        (setf (attribute vu1 "data-db") -100)
        ;;        (setf (attribute vu2 "data-db") 12)
        ))

(defun on-new-window (body)
  (let ((orgel-gui (make-orgel-gui))
        connection-id)
    (clog-gui-initialize body)
    (setf connection-id (clog::connection-id body))
    (load-script (html-document body) "js/vumeter.js")
    (load-script (html-document body) "js/toggle.js")
    (setf (title (html-document body)) "Orgel Sliders")
    (add-class body "w3-blue-grey") ;;; background color
    (load-css (html-document body) "/css/w3.css")
    (load-css (html-document body) "./css/custom-gui-elems.css")
    (setf (gethash "orgel-gui" (gethash connection-id clog-connection::*connection-data*))
          orgel-gui)
    ;; When doing extensive setup of a page using connection cache
    ;; reduces rountrip traffic and speeds setup.
    (with-connection-cache (body)
      (let ((gui-container (create-div body :style "display: flex;")))
        (dotimes (i 10)
          (let ((orgel (aref (orgel-gui-orgeln orgel-gui) i))
                (global-orgel-ref (aref (orgel-gui-orgeln *curr-orgel-state*) i)))
            (create-orgel-gui i gui-container orgel global-orgel-ref)
            
;;;            (setf *tg1* (init-toggle :phase gui-container 0 orgel global-orgel-ref :content "phase" :toggle-content "inv" :size 6 :background "lightgreen" :selected-background "red" :selected-foreground "white"))
            
            ))))))



;;; (setf (attribute (elt (aref *my-vus* 0) 2) "data-db") 10)
;;; (setf (attribute (aref *tg1* 0) "data-val") 0.0)
;;; (setf (attribute (aref *tg1* 0) "data-val") 1.0)
;;; (setf (attribute (aref *tg1* 1) "data-val") 0.0)
;;; (setf (attribute (aref *tg1* 1) "data-val") 1.0)

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

;;; *curr-orgel-state*
(setf (attribute *my-vus* "hidden") t)

(setf (style *my-vus* :display) "flex")

(progn
  (setf (style *my-vus* :display) "none")
  (setf (style *preset-panel* :display) "block"))



*preset-panel*

(progn
  (setf (style *my-vus* :display) "flex")
  (setf (style *preset-panel* :display) "none"))


(remove-attribute *my-vus* "hidden")
112 und 113

(js-execute container
                          "document.onkeyup = function (event) {
  if (event.which == 112 || event.keyCode == 112) {
  }
  if (event.which == 113 || event.keyCode == 113) {
  }
};
")
