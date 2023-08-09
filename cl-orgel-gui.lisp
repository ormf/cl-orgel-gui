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

(defun create-preset-panel (container vu-container)
  (let ((preset-panel
          (create-div container :height 80
                         :style "border: thin solid black;position: absolute;top: 0;left: 0;display: none;justify-content: space-between;width: 100%;")))
    (create-div preset-panel :content "Presets" :style "margin: 2px;")
    (let* ((prv (init-button preset-panel :content "prev" :active-bg "orange"
                                          :background "#bbb" :style "font-size: 8px;"))
           (nb (numbox preset-panel :size 6 :min 0 :max 127))
           (nxt (init-button preset-panel :content "next" :active-bg "orange"
                                          :background "#bbb" :style "font-size: 8px;")))
      (set-on-click
       prv
       (lambda (obj)
         (declare (ignore obj))
         (let ((curr (read-from-string (value nb))))
           (when (> curr (read-from-string (attribute nb "min")))
             (setf (value nb) (1- curr))))))
      (set-on-click
       nxt
       (lambda (obj)
         (declare (ignore obj))
         (let ((curr (read-from-string (value nb))))
           (when (< curr (read-from-string (attribute nb "max")))
             (setf (value nb) (1+ curr))))))
      (create-br preset-panel)
      (let ((recall-btn
              (init-button preset-panel :content "recall" :active-bg "orange"
                                        :background "#d5ffd5" :style "font-size: 8px;"))
            (store-btn
              (init-button preset-panel :content "store" :active-bg "orange"
                                        :background "#ffd5d5" :style "font-size: 8px;"))
            load-btn
            save-btn)
        (create-br preset-panel)
        (setf load-btn (init-button preset-panel :content "load" :active-bg "orange"
                                                 :background "#d5ffd5" :style "font-size: 8px;"))
        (setf save-btn (init-button preset-panel :content "save" :active-bg "orange"
                                                 :background "#ffd5d5" :style "font-size: 8px;"))
        (set-on-click
         recall-btn
         (lambda (obj)
           (declare (ignore obj))
           (format t "recall preset ~d!~%" (read-from-string (value nb)))))
        (set-on-click
         store-btn
         (lambda (obj)
           (declare (ignore obj))
           (format t "store preset ~d!~%" (read-from-string (value nb)))))
        (set-on-click
         load-btn
         (lambda (obj)
           (declare (ignore obj))
           (format t "load presets!~%")))
        (set-on-click
         save-btn
         (lambda (obj)
           (declare (ignore obj))
           (format t "save presets!~%")))
        (install-preset-key-switch container (html-id vu-container) (html-id preset-panel))))))

(defun create-orgel-gui (orgelidx container orgel global-orgel-ref)
      (let* (p1 p2 p3 p4 p5 p7 nbs1 nbs2 tg1-container tg2-container vsliders)
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
        (init-toggle :phase tg1-container orgelidx orgel global-orgel-ref
                     :content "phase" :toggle-content "inv" :size 6 :background "lightgreen"
                     :value-on "-1.0" :value-off "1.0" :selected-background "red" :selected-foreground "white")
        (setf tg2-container (create-div nbs2 :style "display: flex;justify-content: right;")) ;;; container for right alignment of toggle
        (init-toggle :bias-type tg2-container orgelidx orgel global-orgel-ref
                     :content "bandp" :toggle-content "notch" :size 6 :background "lightgreen"
                     :selected-background "orange" :selected-foreground "black")
        (setf p7 (create-div p3 :style "position: relative;" :height 80 :width 160))
        (multiple-value-bind (vus vu-container)
            (init-multi-vu :meters p7 orgelidx orgel global-orgel-ref
                           :num 16 :width 160 :height 80
                           :led-colors :blue
                           :direction :up :background "#444"
                           :inner-background "#444"
                           :border "none" :inner-border "thin solid black"
                           :inner-padding-bottom "0px"
                           :inner-padding "0"
                           :style "margin-bottom: 10px;position: absolute;top: 0;left: 0;"
                           :receiver-fn nil)
          (declare (ignore vus))
          (when (zerop orgelidx)
            (create-preset-panel p7 vu-container)))
        ;;; main volume slider
        (init-vslider :main p4 orgelidx orgel global-orgel-ref)
        (create-div p1 :height 10) ;;; distance
        (create-div p1 :content "Level" :style *msl-title-style*)
        (setf p5 (create-div p1 :width 180 :height 100 :style "padding-bottom: 5px;display: flex;justify-content: space-between;flex: 0 0 auto;"))
;;;        (setf p6 (create-div p5 :style "display: block;"))

        (setf vsliders
              (apply #'multi-vslider p5 :receiver-fn (make-orgel-array-receiver :level orgelidx global-orgel-ref) *msl-style*))
        (init-vslider :bias-bw p5 orgelidx orgel global-orgel-ref)
        (loop for vsl in vsliders
              for idx from 0
              do (progn
                   (setf (value vsl) (* 100.0 (aref (orgel-level global-orgel-ref) idx)))
                   (setf (aref (g-orgel-level orgel) idx) vsl)))
;;;        (hslider p1 :background "#444" :color "#444" :thumbcolor "orange" :height "8px" :width "160px")
        (init-hslider :bias-pos p1 orgelidx orgel global-orgel-ref :height "8px" :width "160px")

        (dolist (label '("Delay" "Q" "Gain" "Osc-Level"))
          (let ((slot-name (make-symbol (format nil "~:@(~a~)" label))))
            (setf vsliders (create-slider-panel
                            p1
                            :label label
                            :receiver-fn (make-orgel-array-receiver slot-name orgelidx global-orgel-ref)))

;;;        (create-br p1)
            (let ((g-accessor-fn (slot->function "g-orgel" slot-name))
                  (accessor-fn (slot->function "orgel" slot-name)))
              (loop for vsl in vsliders
                    for idx from 0
                    do (progn
                         (setf (value vsl) (* 100.0 (aref (funcall accessor-fn global-orgel-ref) idx)))
                         (setf (aref (funcall g-accessor-fn orgel) idx) vsl))))))
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
      (let ((gui-container (create-div body :style "display: flex;overflow: auto;margin-right: 15px;padding-bottom:30px;")))
        (dotimes (i 10)
          (let ((orgel (aref (orgel-gui-orgeln orgel-gui) i))
                (global-orgel-ref (aref *curr-state* i)))
            (create-orgel-gui i gui-container orgel global-orgel-ref)))))))

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

