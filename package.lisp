;;;; package.lisp
;;
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

(defpackage #:cl-orgel-gui
  (:use #:cl #:orm-utils #:cellctl #:clog #:clog-gui)
  (:shadowing-import-from #:clog #:rotate)
  (:export #:orgel #:make-orgel #:orgel-ramp-up #:orgel-ramp-down #:orgel-exp-base #:orgel-base-freq #:orgel-min-amp #:orgel-max-amp #:orgel-phase #:orgel-bias-type #:orgel-level #:orgel-delay #:orgel-q #:orgel-gain #:orgel-osc-level
;; ;;; #:orgel-meters
           #:orgel-main #:orgel-bias-bw #:orgel-bias-pos #:start-orgel-gui
           #:start-orgel-gui

           #:ramp-up #:ramp-down #:exp-base #:base-freq #:min-amp #:max-amp #:phase #:bias-type #:level #:delay #:q #:gain #:osc-level
;; ;;; #:meters
           #:main #:bias-bw #:bias-pos #:*curr-state* #:*orgel-mlevel*
           #:orgel-gui-orgeln
           #:start-orgel-gui


           ))
