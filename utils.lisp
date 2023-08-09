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

(defmacro init-toggle (slot parent orgelidx local-orgel global-orgel
                       &key (size 10)
                         (content "")
                         (toggle-content "")
                         (color "black")
                         (background "white")
                         (selected-background "gray")
                         (selected-foreground "black")
                         (value-off "0.0")
                         (value-on "1.0")
)
  (let ((name (intern (format nil "~:@(tg-~a~)" slot)))
        (g-accessor (intern (format nil "~:@(g-orgel-~a~)" slot)))
        (accessor (intern (format nil "~:@(orgel-~a~)" slot))))
    `(let* ((,name (toggle ,parent
                           :size ,size
                           :content ,content
                           :toggle-content ,toggle-content
                           :color ,color
                           :background ,background
                           :selected-background ,selected-background
                           :selected-foreground ,selected-foreground
                           :value-off ,value-off
                           :value-on ,value-on
                           :slot ,slot
                           :receiver-fn (make-orgel-attr-val-receiver ,slot ,orgelidx ,global-orgel))))
       (setf (,g-accessor ,local-orgel) ,name)
       (setf (attribute ,name "data-val") (val (,accessor ,global-orgel)))
       ,name)))

(defmacro init-vslider (slot parent orgelidx local-orgel global-orgel
                        &key (size 10)
                          (color "#444")
                          (background "#444")
                          (thumbcolor "orange"))
  (let ((name (intern (format nil "~:@(tg-~a~)" slot)))
        (g-accessor (intern (format nil "~:@(g-orgel-~a~)" slot)))
        (accessor (intern (format nil "~:@(orgel-~a~)" slot))))
    `(let* ((,name (vslider ,parent
                            :style (format nil "box-sizing: border-box;width: ~apx;height: 100%;--slider-thumb-height: 2px;--slider-thumb-width: 100%;flex: 0 0 auto;" ,size)
                            :thumbcolor ,thumbcolor
                            :color ,color
                            :background ,background
                            :receiver-fn (make-orgel-attr-val-receiver ,slot ,orgelidx ,global-orgel))))
       (setf (,g-accessor ,local-orgel) ,name)
       (setf (value ,name) (* 100 (val (,accessor ,global-orgel))))
       ,name)))



(defmacro init-hslider (slot parent orgelidx local-orgel global-orgel
                        &key ;;;(size 10)
                          (width 100)
                          (height 10)
                          (color "#444")
                          (background "#444")
                          (thumbcolor "orange"))
  (let ((name (intern (format nil "~:@(tg-~a~)" slot)))
        (g-accessor (intern (format nil "~:@(g-orgel-~a~)" slot)))
        (accessor (intern (format nil "~:@(orgel-~a~)" slot))))
    `(let* ((,name (hslider ,parent
                            :thumbcolor ,thumbcolor
                            :color ,color
                            :background ,background
                            :width ,width
                            :height ,height
                            :receiver-fn (make-orgel-attr-val-receiver ,slot ,orgelidx ,global-orgel))))
       (setf (,g-accessor ,local-orgel) ,name)
       (setf (value ,name) (* 100 (val (,accessor ,global-orgel))))
       ,name)))

(defmacro init-button (container &key (content "") style (background "#fff") (active-bg "#444"))
  (let ((btn (gensym "btn")))
    `(let ((,btn
             (create-button ,container :content ,content :style ,(format nil "style = ~@[~A;~]background: ~A;" style background))))
       (set-on-mouse-down
        ,btn
        (lambda (obj evt)
          (declare (ignore obj evt))
          (setf (style ,btn "background") ,active-bg)))
       (set-on-mouse-up
        ,btn
        (lambda (obj evt)
          (declare (ignore obj evt))
;;;                   (format t "prv clicked!~%")
          (setf (style ,btn "background") ,background)))
       ,btn)))

(defmacro init-numbox (slot parent orgelidx local-orgel global-orgel &key (size 10))
  (let ((name (intern (format nil "~:@(nb-~a~)" slot)))
        (g-accessor (intern (format nil "~:@(g-orgel-~a~)" slot)))
        (accessor (intern (format nil "~:@(orgel-~a~)" slot)))
        (slot-label (intern (format nil "~:@(~a~)" slot))))
    `(let* (
            (container (create-div ,parent :style "display: flex;align-items: baseline;justify-content: space-between;"))            
            (,name (numbox container
                           :label ,(format nil "~(~A~)" (slot-label slot))
                           :color "black" :background-color "#fff"
                           :receiver-fn (make-orgel-val-receiver ,slot ,orgelidx ,global-orgel)
                           :slot ',slot-label
                           :size ,size)))
       (setf (,g-accessor ,local-orgel) ,name)
       (setf (value ,name) (val (,accessor ,global-orgel))))))


;;; (init-numbox :base-freq nbs1 (aref (orgel-gui-orgeln *papierrohrorgeln*) 0))

(defun collect-terms (slots containers orgelidx local-orgel global-orgel size)
  (loop
    for slot in slots
    for container in containers
    collect `(init-numbox ,slot ,container ,orgelidx ,local-orgel ,global-orgel :size ,size)))

#|
(collect-terms '(:ramp-up :ramp-down :exp-base :base-freq :max-amp :min-amp)
               '(nbs1 nbs1 nbs1 nbs2 nbs2 nbs2))

(init-numboxes (:ramp-up :ramp-down :exp-base :base-freq :max-amp :min-amp)
(nbs1 nbs1 nbs1 nbs2 nbs2 nbs2)
orgel1 orgel2
:size 6)

|#

(defmacro init-numboxes (slots containers orgelidx local-orgel global-orgel &key (size 10))
  `(progn
     ,@(collect-terms slots containers orgelidx local-orgel global-orgel size)))

(defmacro init-multi-vu (slot parent orgelidx local-orgel global-orgel
                         &key (num 8) (width 80) (height 100) (background "#444") (direction :up) (border "none")
                           (inner-background "var(--vu-background)") (inner-border "thin solid black") (inner-padding "0")
                           (inner-padding-bottom "0px")
                           (led-colors :blue) (style "margin-bottom: 10px;position: absolute;top: 0;left: 0;")
                           receiver-fn)
  (declare (ignore orgelidx global-orgel))
  (let ((vus (gensym "vus"))
        (container (gensym "vu-container"))
        (g-accessor (intern (format nil "~:@(g-orgel-~a~)" slot)))
;;;        (accessor (intern (format nil "~:@(orgel-~a~)" slot)))
        )
    `(multiple-value-bind (,vus ,container)
         (multi-vu ,parent :num ,num :width ,width :height ,height
                           :led-colors ,led-colors
                           :direction ,direction :background ,background
                           :inner-background ,inner-background
                           :border ,border :inner-border ,inner-border
                           :inner-padding-bottom ,inner-padding-bottom
                           :inner-padding ,inner-padding
                           :style ,style
                           :receiver-fn ,receiver-fn)
       (setf (,g-accessor ,local-orgel) ,vus)
       (loop for vu in ,vus
             for idx from 0
             do (setf (attribute vu "data-db") (- (val (aref (aref *orgel-mlevel* orgelidx) idx)) 100)))
       (values ,vus ,container))))


(defparameter *slot-labels* '((:ramp-down . :ramp-dwn)))

(defun slot-label (slot)
  (or (cdr (assoc slot *slot-labels*)) slot))

;;; (slot-label :ramp-up)

(defun hex->rgb (num)
  (list
   (ash (logand num #xff0000) -16)
   (ash (logand num #xff00) -8)
   (logand num #xff)))

(defun cols->js (cols &key (name ""))
  (format t "~{var ~{col~a~d = 'rgba(~{~a~^, ~}, 1.0)';~%~}~}"
          (loop
            for num from 1
            for col in cols
            collect (list name num (hex->rgb col)))))

(defun cols->jsarray (cols)
  (format nil "[~{'rgba(~{~a~^, ~}, 1.0)'~^, ~}]" (mapcar #'hex->rgb cols)))

(defun create-slider-panel (container &key label receiver-fn)
  (create-div container :content label :style *msl-title-style*)
  (apply #'multi-vslider container :receiver-fn receiver-fn *msl-style*))

(defun make-orgel-attr-val-receiver (slot orgelidx global-orgel-ref &key (attribute "data-val"))
  (let ((slot-symbol (intern (format nil "~:@(~a~)" slot) 'cl-orgel-gui)))
    (lambda (val self)
      (let* ((val-string (ensure-string val))
             (orgel-val (/ (read-from-string val-string)
                           (if (member slot '(:main :bias-bw)) 100.0 1.0)))
;;;             (num-val (read-from-string val-string))
             )
        (setf (val (slot-value global-orgel-ref slot-symbol)) orgel-val)
;;        (cl-orgelctl::orgel-ctl (cl-orgelctl::orgel-name (1+ orgelidx)) slot orgel-val)
        (maphash (lambda (connection-id connection-hash)
                   (declare (ignore connection-id))
;;;                   (break "~a" (gethash "orgel-gui" connection-hash))
                   (let* ((orgel-gui (gethash "orgel-gui" connection-hash)))
                     (when orgel-gui (let ((elem (slot-value (aref (orgel-gui-orgeln orgel-gui) orgelidx) slot-symbol)))
;;;                                       (break "self: ~a~% elem: ~a" self elem)
                                       (unless (equal self elem) (setf (attribute elem attribute) val-string))))))
                 clog-connection::*connection-data*)))))

(defun make-orgel-val-receiver (slot orgelidx global-orgel-ref)
  (let ((slot-symbol (intern (format nil "~:@(~a~)" slot) 'cl-orgel-gui)))
    (lambda (val self)
      (let* ((val-string (ensure-string val))
;;;             (num-val (read-from-string val-string))
             (orgel-val (/ (read-from-string val-string)
                           (if (member slot '(:main :bias-pos :bias-bw)) 100.0 1.0))))
;;;        (break "val-receiver: ~S" slot)
        (setf (val (slot-value global-orgel-ref slot-symbol)) orgel-val)
;;        (cl-orgelctl::orgel-ctl (cl-orgelctl::orgel-name (1+ orgelidx)) slot orgel-val)
        (maphash (lambda (connection-id connection-hash)
                   (declare (ignore connection-id))
;;;                   (break "~a" (gethash "orgel-gui" connection-hash))
                   (let* ((orgel-gui (gethash "orgel-gui" connection-hash)))
                     (when orgel-gui (let ((elem (slot-value (aref (orgel-gui-orgeln orgel-gui) orgelidx) slot-symbol)))
;;;                                       (break "self: ~a~% elem: ~a" self elem)
                                       (unless (equal self elem) (setf (value elem) val-string))))))
                 clog-connection::*connection-data*)))))

(defun make-orgel-array-receiver (slot orgelidx global-orgel-ref)
  (let ((g-accessor (slot->function "g-orgel" slot))
        (accessor (slot->function "orgel" slot)))
    (lambda (idx val self)
      (let* ((val-string (ensure-string val))
;;;             (num-val (read-from-string val-string))
             (orgel-val (/ (read-from-string val-string) 100.0))
             )
        (setf (val (aref (funcall accessor global-orgel-ref) idx)) orgel-val)
;;        (cl-orgelctl::orgel-ctl-fader (cl-orgelctl::orgel-name (1+ orgelidx)) slot (1+ idx) orgel-val)
        (maphash (lambda (connection-id connection-hash)
                   (declare (ignore connection-id))
;;;                   (break "~a" (gethash "orgel-gui" connection-hash))
                   (let* ((orgel-gui (gethash "orgel-gui" connection-hash))
                          (orgel (aref (orgel-gui-orgeln orgel-gui) orgelidx)))
                     (when orgel-gui (let ((elem (aref (funcall g-accessor orgel) idx)))
;;;                                       (break "~a" orgel)
                                       (unless (equal self elem) (setf (value elem) val-string))))))
                 clog-connection::*connection-data*)))))

#|

(defun synchronize-vsl (idx val self)
  (let ((val-string (ensure-string val)))
    (setf (aref (orgel-level *curr-orgel-state*) idx) val-string)
    (maphash (lambda (connection-id connection-hash)
               (declare (ignore connection-id))
               (let ((orgel-gui (gethash "orgel-gui" connection-hash)))
                 (when orgel-gui (let ((elem (aref (orgel-level orgel) idx)))
                                   (unless (equal self elem) (setf (value elem) val-string))))))
             clog-connection::*connection-data*)))

(defun synchronize-numbox (slot val self)
  (let ((val-string (ensure-string val)))
    (setf (slot-value *curr-orgel-state* slot) val-string)
    (maphash (lambda (connection-id connection-hash)
               (declare (ignore connection-id))
               (let ((orgel-gui (gethash "orgel-gui" connection-hash)))
                 (when orgel-gui
                   (let ((elem (slot-value (gethash "orgel-gui" connection-hash) slot)))
                     (unless (equal self elem) (setf (value elem) val-string))))))
             clog-connection::*connection-data*)))

(defun make-vsl-synchronizer (slot orgelidx)
  (labels ((accessor)))

  )
|#

(defun slot->function (struct-name slot &optional (package 'cl-orgel-gui))
  "get the function object for a slot of a struct with prefix"
  (symbol-function (intern (string-upcase (format nil "~a-~a" struct-name slot)) package)))

#|
(defun get-vsl-accessor (orgelidx slot faderidx)
  (lambda (orgelgui)
    (aref (funcall (slot->function "gui-orgel" slot)
                   (gui-orgel-data-params
                    (aref (orgel-gui-orgeln orgelgui) orgelidx)))
          faderidx)))
|#

(defun install-preset-key-switch (container vu-id preset-panel-id)
  (js-execute
   container
   (format nil "document.onkeydown = function (event) {
  if (event.which == 112 || event.keyCode == 112) {
   document.getElementById('~a').style.display = \"flex\";
   document.getElementById('~a').style.display = \"none\";
  }
  if (event.which == 113 || event.keyCode == 113) {
   document.getElementById('~a').style.display = \"none\";
   document.getElementById('~a').style.display = \"block\";
  }
};
" vu-id preset-panel-id vu-id preset-panel-id)))
