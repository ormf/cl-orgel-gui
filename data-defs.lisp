;;; 
;;; data-defs.lisp
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

(defvar *orgelcount* 10)

(defstruct orgel
  (base-freq 0.0 :type float)
  (phase 0.0 :type float)
  (bias-pos 0.0 :type float)
  (bias-bw 0.0 :type float)
  (bias-type 0.0 :type float)
  (main 0.0 :type float)
  (min-amp 0.0 :type float)
  (max-amp 0.0 :type float)
  (ramp-up 0.0 :type float)
  (ramp-down 0.0 :type float)
  (exp-base 0.0 :type float)
  (level (make-array 16 :initial-element 0.0) :type simple-array)
;;;  (bias-level (make-array 16 :initial-element 0.0) :type simple-array)
  (delay (make-array 16 :initial-element 0.0) :type simple-array)
  (q (make-array 16 :initial-element 0.0) :type simple-array)
  (gain (make-array 16 :initial-element 0.0) :type simple-array)
  (osc-level (make-array 16 :initial-element 0.0) :type simple-array))

(defstruct g-orgel
  (ramp-up "29.0")
  (ramp-down "29.0")
  (exp-base "0.0")
  (base-freq "0.0")
  (min-amp "0.0")
  (max-amp "1.0")
  (phase "1.0")
  (bias-type "0.0")
  (level (make-array 16))
  (delay (make-array 16))
  (q (make-array 16))
  (gain (make-array 16))
  (osc-level (make-array 16))
  (meters (make-array 10))
  (main "0.0")
  (bias-bw "0.0")
  (bias-pos "0.0"))

(defstruct gui-presets
  (num "0.0"))

(defstruct orgel-gui
  (orgeln (make-array *orgelcount*
                     :initial-contents (v-collect (n *orgelcount*) (make-g-orgel))))
  (presets (make-gui-presets)))

(defparameter *curr-state*
  (make-array *orgelcount*
              :initial-contents (v-collect (n *orgelcount*) (make-orgel))))

(defparameter *orgel-mlevel*
  (make-array *orgelcount*
              :element-type 'simple-array
              :initial-contents
              (loop
                for i below *orgelcount*
                collect (make-array 16 :element-type 'float
                                       :initial-contents (loop for x below 16 collect 0.0))))
  "all volume levels currently measured in pd (permanently updated).")
