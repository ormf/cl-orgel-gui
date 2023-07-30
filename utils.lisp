;;; 
;;; utils.lisp
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

(defun ensure-string (token)
  (if (stringp token) token (format nil "~S" token)))

(defmacro init-numbox (slot parent &key (size 10))
  (let ((name (intern (format nil "~:@(nb-~a~)" slot)))
        (accessor (intern (format nil "~:@(orgel-~a~)" slot)))
        (slot-label (intern (format nil "~:@(~a~)" slot))))
    `(let* (
            (container (create-div ,parent :style "display: flex;align-items: baseline;justify-content: space-between;"))            
            (,name (numbox container
                           :label ,(format nil "~(~A~)" (slot-label slot))
                           :color "black" :background-color "#fff"
                           :receiver-fn #'synchronize-numbox
                           :slot ',slot-label
                           :size ,size)))
       (setf (,accessor orgel) ,name)
       (setf (value ,name) (,accessor *curr-orgel-state*)))))


;;; (init-numbox :base-freq nbs1)

(defun collect-terms (slots containers size)
  (loop
    for slot in slots
    for container in containers
    collect `(init-numbox ,slot ,container :size ,size)))

#|
(collect-terms '(:ramp-up :ramp-down :exp-base :base-freq :max-amp :min-amp)
               '(nbs1 nbs1 nbs1 nbs2 nbs2 nbs2))
|#

(defmacro init-numboxes (slots containers &key (size 10))
  `(progn
     ,@(collect-terms slots containers size)))

(defparameter *slot-labels* '((:ramp-down . :ramp-dwn)))

(defun slot-label (slot)
  (or (cdr (assoc slot *slot-labels*)) slot))

;;; (slot-label :ramp-up)

