;;; 
;;; scratch.lisp
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

(defun start-orgel-gui2 ()
  "Start Orgel Gui."
  (initialize 'on-new-window
              :port 8081
              :static-root
              (merge-pathnames "./www/" (asdf:system-source-directory :clog-orgel-widgets)))
  (open-browser))

(setf (val (aref (aref *orgel-mlevel* 2) 5)) 100)

*curr-state*

(set-on-new-window 'page2 :path "/page2")

cl-orgelctl::*orgel-freqs*

(make-orgel-gui)

(start-orgel-gui2)
(quote )

(dolist (slot '(:ramp-up :ramp-down :exp-base :base-freq :min-amp :max-amp))
          (macroexpand-1 (init-numbox slot 'nbs1)))

(dolist (slot '(:ramp-up :ramp-down :exp-base :base-freq :min-amp :max-amp))
  (init-numbox :freq nbs1))

(let ((slot :freq)
      (parent 'nbs1)
      (name (intern (format nil "~:@(nb-~a~)" :freq)))
      (accessor (intern (format nil "~:@(orgel-~a~)" :freq))))

  )



(defun get-inner-form (name accessor parent)
  `(progn
     (setf ,name
           (numbox ,parent :label (format nil "~A" slot) :color "black" :background-color "#fff" :receiver-fn #'synchronize-numbox :slot ',accessor))
     (setf (,accessor orgel) nb-freq)
     (setf (value ,name)
           (,accessor *curr-orgel-state*))))

(defmacro init-numbox (slot parent)
  `(let ((name ,(intern (format nil "~:@(nb-~a~)" slot)))
         (accessor ,(intern (format nil "~:@(orgel-~a~)" slot))))
     ,(get-inner-form name accessor parent)))

(init-numbox :ramp-up nbs1)

(let ((orgel (make-orgel)))

  (setf (slot-value *curr-orgel-state* ':ramp-up) "1"))

(symbol-function 'orgel-ramp-up)

(setf (slot-value *curr-orgel-state* slot) val-string)


;;; (defparameter *colors* #("#3071A9" "#00ff00" "#ffff00" "#ff00ff" "#00ffff" "#ff0380" "#33d5a4" "#1040d8"))

(let ((nb1 (orgel-numbox (gethash "5AAECF80-29FB-11EE-9B10-5405DBF54355" *global-connection-hash*))))
  (setf (value nb1) "0")
    (set-on-key-up nb1
                 (lambda (obj event)
                   (declare (ignore obj))
                   (when (equal (getf event :key) "Enter")
                     (format t "enter!~%"))))
  )

(let ((nb1 (orgel-numbox (gethash "orgel"
                                  (gethash "b89f5ea4af5ea717ae6d9e64b1d26ac7"
                                           clog-connection::*connection-data*)))))
  (setf (value nb1) "0")
  (clog::unbind-event-script nb1 :keyup)
  (set-on-key-up nb1
                 (lambda (obj event)
                   (declare (ignore obj))
                   (when (equal (getf event :key) "Enter")
                     (let ((val (value nb1)))
                       (setf (value nb1) val)
                       (synchronize-numbox val nb1))
                     (format t "enter!~%")))))

(blur)
clog-connection::*connections*


100 99-81 80-61 60-56 55-51 50-46 45-41 40-36 35-31 30-28 27-26 25-23 22-21 20-19 18-17 16-15 14-13 12-11 10 9-8  7  6  5  4  3   2    1  0  1  2   3  4   5  6-7 8  9-11 12
0   1     2     3     4     5     6     7     8     9     10    11    12    13    14    15    16    17    18  19  20 21 22 23 24  25  27  29 31 33 34 35 36 37    38 39   40
1   19    20    5     5     5     5     5     5     3     2     3     2     2     2     2     2     2     1   2   1  1  1  1  1   1   1  1   1   1  1  1  1  2  1  3 1

(ql:quickload "orm-utils")


(apply #'+ '(1   19    20    5     5     5     5     5     5     3     3     3     2     2     2     2     2     2     1   2   1  1  1  1  1   1   1  1   1   1  1  1  1  2  1  3 1))

(length
         (apply #'append
               (mapcar (lambda (x y) (repeat x y))
                       '(1   19    20    5     5     5     5     5     5     3     2    3     2     2     2     2     2     2     1   2   1  1  1  1  1   1   1  1   1   1  1  1  1  2  1  3 1)
                                          
                       '(0   1     2     3     4     5     6     7     8     9     10    11    12    13    14    15    16    17    18  19  20 21 22 23 24  25  27  29 31 33 34 35 36 37    38 39   40)
                       )))

(format t "[~{~A~^, ~}]"
        (apply #'append
               (mapcar (lambda (x y) (repeat x y))
                       '(1   19    20    5     5     5     5     5     5     3     2     3     2     2     2     2     2     2     1   2   1  1  1  1  1   1   1  1   1   1  1  1  1  2  1  3 1)
                                          
                       '(0   1     2     3     4     5     6     7     8     9     10    11    12    13    14    15    16    17    18  19  20 21 22 23 24  25  27  29 31 33 34 35 36 37    38 39   40)
                       )))


(progn (dotimes (i 16)
         (setf (attribute (elt *my-vu* i) "db-val") -12))
       (loop repeat 5000
             with last = (repeat 16 0)
             do
                (let ((idx (random 16)))
                  (setf (attribute (elt *my-vu* idx) "db-val") (incf (elt last idx) (- (random 4.0) 2)))
                  (sleep 0.01))))

(/ 110 13)



(cols->jsarray '(#x445500 #x006680 #x0088aa #x00aad4 #x00ccff #x2ad4ff #x55ddff #x80e5ff #xaaeeff #xd5f6ff))

(cols->js
 '(#x445500 #x006680 #x0088aa #x00aad4 #x00ccff #x2ad4ff #x55ddff #x80e5ff #xaaeeff #xd5f6ff)
 :name "Blue")

(cols->js
 '(#x005500 #x008000 #x00aa00 #x00d400 #x00ff00 #x2aff2a #x55ff55 #x80ff80 #xaaffaa #xd5ffd5)
 :name "Green")

(list #x2a #x55 #x80 #xaa #xd5)
42


(let ((name "Red"))
  (format t "~{var ~{col~a~d = 'rgba(~{~a~^, ~}, 1.0)';~%~}~}"
          (loop for main in '(85 128 170 213 255 255 255 255 255 255)
                for other in '(0 0 0 0 0 42 85 128 170 213)
                for num from 1
                collect (list name num (list main other other)))))


(defparameter *elems*
  (loop for val being the hash-values of
        clog-connection::*connection-data*
        collect (aref (orgel-gui-orgeln (gethash "orgel-gui" val)) 0)))
;;; (#<clog-form-element {1001CA1483}> #<clog-form-element {1001663AF3}>)

(setf (attribute (orgel-phase (first *elems*)) "data-val") 0.0)
(setf (attribute (orgel-phase (first *elems*)) "val") 0.0)

(setf (attribute (orgel-phase (first *elems*)) "data-val") 1.0)

(setf (attribute (first *my-vus*) "db-val") -40)


(apply #'equal
)

(defun count1 (n)
  (loop
    :for x :below n
    collect (* x x)))

(destructuring-bind (x (y z) &rest rest) '(4 (5 6) hallo test)
  (list* x y z rest))

(count1 20)

(let ((seq (split-sequence:split-sequence #\SPACE "1 2 3 4 5")))
  (cond
    ((= seq ....) )
    (:else)))


(case (cm:status->opcode 144)
  (:note-on ...)
  ((:note-on :note-off) )

  )

(js-query)

clog::*connection-cache*

(setf (val (aref (aref *orgel-mlevel* 0) 0)) 1.0)


div.parent{
 position: relative;
 width: 10px;
height: 100px;
background: #666;
}
div.child{
  position: absolute;
  left: 0;
  bottom: 0;
  width: 100%;
  height: 80%;
background-image: repeating-linear-gradient(to top, #000 0%, rgba(0,0,0,0) 1px, rgba(0,0,0,0) 2.5px),linear-gradient(to top, rgba(0, 85, 100, 1.0) 0%, rgba(0, 102, 128, 1.0) 2%, rgba(0, 136, 170, 1.0) 10%, rgba(0, 170, 212, 1.0) 17.5%, rgba(0, 190, 245, 1.0) 25.0%, rgba(50, 202, 255, 1.0) 32.5%, rgba(85, 211, 255, 1.0) 40.0%, rgba(128, 222, 255, 1.0) 47.5%, rgba(170, 235, 255, 1.0) 55.0%, rgba(213, 246, 255, 1.0) 62.5%, rgba(255, 170, 170, 1.0) 70.0%, rgba(255, 128, 128, 1.0) 77.5%, rgba(255, 85, 85, 1.0) 85.0%, rgba(255, 42, 42, 1.0) 92.5%, rgba(255, 0, 42, 1.0) 100%);
  background-position: bottom;
}

  background-image: repeating-linear-gradient(to top, #333 0%, #333 1px, rgba(0,0,0,0) 1px, rgba(0,0,0,0) 2px),linear-gradient(to top, blue 0%, red 100%);
  background-size: 10px 100px;

background-image: repeating-linear-gradient(to top, #000 0%, rgba(0,0,0,0) 1px, rgba(0,0,0,0) 2.5px),linear-gradient(to top, rgba(0, 85, 100, 1.0) 0%, rgba(0, 102, 128, 1.0) 2%, rgba(0, 136, 170, 1.0) 10%, rgba(0, 170, 212, 1.0) 17.5%, rgba(0, 190, 245, 1.0) 25.0%, rgba(50, 202, 255, 1.0) 32.5%, rgba(85, 211, 255, 1.0) 40.0%, rgba(128, 222, 255, 1.0) 47.5%, rgba(170, 235, 255, 1.0) 55.0%, rgba(213, 246, 255, 1.0) 62.5%, rgba(255, 170, 170, 1.0) 70.0%, rgba(255, 128, 128, 1.0) 77.5%, rgba(255, 85, 85, 1.0) 85.0%, rgba(255, 42, 42, 1.0) 92.5%, rgba(255, 0, 42, 1.0) 100%);
  background-size: 10px 100px;

div.child:nth-child(2){
 left: 11px;
 height: 60%;
}


Lidia Kreutzinger

lkr@steinway-frankfurt.de

21.12. 14:00-15:00


(map '()
 (lambda (x y) (format t "~a ~a%, " y (/ x 0.40)))
 
 '(0 1 4 7 10 13 16 19 22 25 28 31 34 37 40)
 '("rgba(0, 85, 100, 1.0)"
   "rgba(0, 102, 128, 1.0)"
   "rgba(0, 136, 170, 1.0)"
   "rgba(0, 170, 212, 1.0)"
   "rgba(0, 190, 245, 1.0)"
   "rgba(50, 202, 255, 1.0)"
   "rgba(85, 211, 255, 1.0)"
   "rgba(128, 222, 255, 1.0)"
   "rgba(170, 235, 255, 1.0)"
   "rgba(213, 246, 255, 1.0)"
   "rgba(255, 170, 170, 1.0)"
   "rgba(255, 128, 128, 1.0)"
   "rgba(255, 85, 85, 1.0)"
   "rgba(255, 42, 42, 1.0)"))

<div class="parent">
<div class="child">
</div>
<div class="child">
</div>
</div>
