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
  (ramp-up "29.0")
  (ramp-down "29.0")
  (exp-base "0.0")
  (base-freq "0.0")
  (min-amp "0.0")
  (max-amp "1.0")
  (level-sliders (make-array 16)))

(defparameter *curr-orgel-state*
  (make-orgel :level-sliders (make-array 16 :initial-element "0.0")))

(defparameter *background-color* "#607d8b")
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

(defun synchronize-numbox (slot val self)
  (let ((val-string (ensure-string val)))
    (setf (slot-value *curr-orgel-state* slot) val-string)
    (maphash (lambda (connection-id connection-hash)
               (declare (ignore connection-id))
               (let ((orgel (gethash "orgel" connection-hash)))
                 (when orgel
                   (let ((elem (slot-value (gethash "orgel" connection-hash) slot)))
                     (unless (equal self elem) (setf (value elem) val-string))))))
             clog-connection::*connection-data*)))

(defmethod clog:create-div ((obj clog-obj) &key (content "")
                                        (style nil)
                                        (db-val 100)
                                        (hidden nil)
                                        (class nil)
                                        (html-id nil)
                                        (auto-place t))
  (create-child obj (format nil "<div~@[~A~]~@[~A~]~@[~A~]>~A</div>"
                            (when class
                              (format nil " class='~A'"
                                      (escape-string class :html t)))
                            (when (or hidden style)
                              (format nil " style='~@[~a~]~@[~a~]'"
                                      (when hidden "visibility:hidden;")
                                      style))
                            (if db-val (format nil " db-val = ~a" db-val))
                            content)
                :clog-type  'clog-div
                :html-id    html-id
                :auto-place auto-place))


(defun on-new-window (body)
  (let ((orgel (make-orgel))
        connection-id)
    (clog-gui-initialize body)
    (setf connection-id (clog::connection-id body))
    (load-script (html-document body) "js/vumeter.js")
    (setf (title (html-document body)) "Orgel Sliders")
    (add-class body "w3-blue-grey") ;;; background color
    (load-css (html-document body) "/css/w3.css")
    (load-css (html-document body) "./css/custom-gui-elems.css")
    (setf (gethash "orgel" (gethash connection-id clog-connection::*connection-data*))
          orgel)
    ;; When doing extensive setup of a page using connection cache
    ;; reduces rountrip traffic and speeds setup.
    (with-connection-cache (body)
      (let* (p1 p2 nbs1 nbs2 tg1 tg2 vsliders vu1 vu2 vutest vuleds)
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
;;;        (setf ms1  (create-div p1 :style "color: transparent; border: none;width: 100px;height: 100px;display: flex;")) ;;; container als Platzhalter
        (setf p2 (create-div p1 :style "width: 160px;height: 60px;display: flex;justify-content: space-between;"))
        (setf nbs1 (create-div p2 :style "width: 75px;font-size: 6pt;display: flex;flex-direction: column;justify-content: space-between;")) ;;; container for numberbox(es)
        (setf nbs2 (create-div p2 :style "width: 73px; font-size: 6pt;display: flex;flex-direction: column;justify-content: space-between;")) ;;; container for numberbox(es)
        ;; (setf nb-freq (numbox nbs1 :label "freq" :color "black" :background-color "#fff" :receiver-fn #'synchronize-numbox :slot 'orgel-freq))
        ;; (setf (orgel-freq orgel) nb-freq)
        ;; (setf (value nb-freq) (orgel-freq *curr-orgel-state*))
;;        (setf d1 (create-div nbs1 :style "display: flex;align-items: baseline;justify-content: space-between;"))
        (init-numboxes
         (:ramp-up :ramp-down :exp-base :base-freq :max-amp :min-amp)
         (nbs1 nbs1 nbs1 nbs2 nbs2 nbs2)
         :size 6)
        (setf tg1 (create-div nbs1 :style "display: flex;justify-content: right;"))

        (toggle tg1 :content "phase" :toggle-content "inv" :size 6 :background "lightgreen" :selected-background "red"
                   :selected-foreground "white" :slot :phase
                   :receiver-fn (lambda (slot state obj) (declare (ignore obj))
                                  (format t "~S clicked, state: ~a!~%" slot state)))
        (setf tg2 (create-div nbs2 :style "display: flex;justify-content: right;"))
        (toggle tg2 :content "bandp" :toggle-content "notch" :size 6 :background "lightgreen" :selected-background "orange"
                        :style "align-content: right;" :slot :bandp
                        :receiver-fn (lambda (slot state obj) (declare (ignore obj))
                                       (format t "~S clicked, state: ~a!~%" slot state)))        (setf vsliders
              (multi-vslider p1 :num 16 :width 160 :colors *vsl-colors* :background-color "transparent"
                                         :receiver-fn #'synchronize-vsl))
        (hslider p1 :background-color "#444" :color "#444" :thumbcolor "orange" :height "8px" :width "160px")
        (create-br p1)
;;;        (multi-vslider p1 :colors *vsl-colors* :background-color "transparent")
        (loop for vsl in vsliders
              for idx from 0
              do (progn
                   (setf (value vsl)
                         (aref (orgel-level-sliders *curr-orgel-state*) idx))
                   (setf (aref (orgel-level-sliders orgel) idx) vsl)))
;;        (setf vu1 (create-canvas p1 :class "vumeter" :width "10px" :height "126px" :data-val 30))
        (setf *my-vu*
              (setf vu2 (create-div p1 :class "vumeter" :style "width: 10px; height: 126px;background-color: #222;justify-content: center;" :db-val -100)))
        
        ;; (js-execute body (format nil "vumeter(~A, {\"boxCount\": 40, \"boxGapFraction\": 0.01, \"max\":100, })"
        ;;                          (jquery vu1)))
        (js-execute body (format nil "vumeter(~A, {\"boxCount\": 40, \"boxGapFraction\": 0.01, \"max\":100, \"db-val\":0})"
                                 (jquery vu2)))


;;;        (jquery-execute vu1 (format nil "setAttribute('data-val', '~A')" (escape-string 45)))

;;;        (js-execute (jquery vu1) (format nil "setAttribute('data-val', '~A')" (escape-string 45)))

        (setf (attribute vu2 "db-val") 12)
;;;        (setf (property vu1 :width) 150)

        ;; (setf vutest (create-div p1 :class "vumeter" :style "width: 10px;height: 85px; background-color: #222;justify-content: center;"))
        ;; (setf vuleds (create-div vutest :style "width: 100%;padding: 2px;height: 100%;display: flex;flex-direction: column; justify-content: space-between;"))
        ;; 
        ;; (create-span vuleds :style "background-color: #0f0; border: thin solid black;height: 100%;width: 100%;")
        ;; (loop repeat 39
        ;;       do (create-span vuleds :style "background-color: #0f0; border: thin solid black;border-top-style: none; margin:0;height: 100%;width: 100%;"))

        
;;;        (set-border p1 :thin :solid :black)
        )) ;;; text style))
))

(defparameter *my-vu* nil)

;;; (setf (attribute *my-vu* "db-val") -60)
;;; (setf (attribute *my-vu* "db-val")4)

;;; (setf (width vu1))
    (defun start-orgel-gui ()
  "Start Orgel Gui."
  (setf *global-connection-hash* (make-hash-table* :test 'equalp))
  (initialize 'on-new-window
              :static-root (merge-pathnames "./www/"
                                            (asdf:system-source-directory :cl-orgel-gui)))
  (open-browser))

;;; (start-orgel-gui)

;;; (create-form-element)


;;; (create-context2d disp)

