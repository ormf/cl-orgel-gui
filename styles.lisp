;;; 
;;; styles.lisp
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

(defparameter *background-color* "#607d8b")

(defparameter *colors* #("#ffa0ff" "#ffffa0" "#a0ffff" "#ffe0e0" "#a0ffa0" "#e0e0ff" "#efd7e7" "#cccccc"))

(defparameter *vsl-colors*
  (map 'vector (lambda (idx)  (aref *colors* idx)) '(0 0 1 0 2 1 3 0 4 2 5 1 6 3 7 0)))

(defparameter *msl-title-style* "font-size: 8px;align: bottom; padding-top: 0px; padding-bottom: 0px;")

(defparameter *msl-style* `(:num 16 :width 160 :colors ,*vsl-colors* :background "transparent"))

