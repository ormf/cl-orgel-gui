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
                     :initial-contents (v-collect (n *orgelcount*) (make-orgel))))
  (presets (make-gui-presets)))

(defparameter *curr-orgel-state* (make-orgel-gui))
