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
                   (let ((elem (funcall (symbol-function slot) (gethash "orgel" connection-hash))))
                     (unless (equal self elem) (setf (value elem) val-string))))))
             clog-connection::*connection-data*)))

(defun on-new-window (body)
  (let ((orgel (make-orgel))
        connection-id)
    (clog-gui-initialize body)
    (setf connection-id (clog::connection-id body))
;;;    (load-script (html-document body) "js/numberbox.js")
    (setf (title (html-document body)) "Orgel Sliders")
    (load-css (html-document body) "/css/w3.css")
    (load-css (html-document body) "./css/custom-gui-elems.css")
    (setf (gethash "orgel" (gethash connection-id clog-connection::*connection-data*))
          orgel)
    ;; When doing extensive setup of a page using connection cache
    ;; reduces rountrip traffic and speeds setup.
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
;;;        (setf ms1  (create-div p1 :style "color: transparent; border: none;width: 100px;height: 100px;display: flex;")) ;;; container als Platzhalter
        (setf nbs1 (create-div p1)) ;;; container for numberbox(es)
        ;; (setf nb-freq (numbox nbs1 :label "freq" :color "black" :background-color "#fff" :receiver-fn #'synchronize-numbox :slot 'orgel-freq))
        ;; (setf (orgel-freq orgel) nb-freq)
        ;; (setf (value nb-freq) (orgel-freq *curr-orgel-state*))
        (init-numbox :ramp-up nbs1)
        (init-numbox :ramp-down nbs1)
        (init-numbox :exp-base nbs1)
        (init-numbox :base-freq nbs1)
        (init-numbox :max-amp nbs1)
        (init-numbox :min-amp nbs1)
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

;;;        (set-border p1 :thin :solid :black)
        (add-class body "w3-blue-grey") ;;; background color
        )) ;;; text style))

        (run body)))

    (defun start-orgel-gui ()
  "Start Orgel Gui."
  (setf *global-connection-hash* (make-hash-table* :test 'equalp))
  (initialize 'on-new-window
              :static-root (merge-pathnames "./www/"
                                            (asdf:system-source-directory :cl-orgel-gui)))
  (open-browser))

;;; (start-orgel-gui)q

;;; (create-form-element)
