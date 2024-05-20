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

(defmacro rebind-as-list (tokens)
  (mapcar (lambda (tk) (list tk `(uiop:ensure-list ,tk))) tokens))

(defmacro init-toggle (slot parent orgelidx local-orgel global-orgel
                       &key (size 10)
                       (value 0)
                         (values ''("0" "1"))
                         (label ''("" ""))
                         (text-color ''("black"))
                         (background ''("white" "gray")))
  (let ((name (intern (format nil "~:@(tg-~a~)" slot)))
        (g-accessor (intern (format nil "~:@(g-orgel-~a~)" slot)))
        (accessor (intern (format nil "~:@(orgel-~a~)" slot))))
    `(let* ((,name (toggle ,parent
                           :size ,size
                           :label ,label
                           :text-color ,text-color
                           :background ,background
                           :values ,values
                           :value ,value
                           :val-change-cb (make-orgel-val-receiver ,slot ,orgelidx ,global-orgel))))
       (setf (,g-accessor ,local-orgel) ,name)
       (setf (attribute ,name "data-val") (val (,accessor ,global-orgel)))
       ,name)))

(defmacro init-vslider (slot parent orgelidx local-orgel global-orgel
                        &key
                          css
                          (color "#444")
                          (background "#444")
                          (thumbcolor "orange")
                          (height "100px")
                          (width "8px")
                          db)
  (let ((name (intern (format nil "~:@(tg-~a~)" slot)))
        (g-accessor (intern (format nil "~:@(g-orgel-~a~)" slot)))
        (accessor (intern (format nil "~:@(orgel-~a~)" slot))))
    `(let* ((,name (vslider ,parent
;;                            :style (format nil "box-sizing: border-box;width: ~apx;height: 100%;--slider-thumb-height: 2px;--slider-thumb-width: 100%;flex: 0 0 auto;" ,size)
                            :css (append
                                  `(:--thumb-color ,,thumbcolor
                                    :--bar-color ,,color
                                    :background ,,background
                                    :width ,,width
                                    :height ,,height
                                    :flex "0 0 auto")
                                  ,css)
                            :val-change-cb (make-orgel-val-receiver ,slot ,orgelidx ,global-orgel :db ,db))))
       (setf (,g-accessor ,local-orgel) ,name)
       (setf (value ,name) (val (,accessor ,global-orgel)))
       ,name)))



(defmacro init-hslider (slot parent orgelidx local-orgel global-orgel
                        &key ;;;(size 10)
                          (width "100 px")
                          (height "8px")
                          (color "#444")
                          (background "#444")
                          (thumbcolor "orange"))
  (let ((name (intern (format nil "~:@(tg-~a~)" slot)))
        (g-accessor (intern (format nil "~:@(g-orgel-~a~)" slot)))
        (accessor (intern (format nil "~:@(orgel-~a~)" slot))))
    `(let* ((,name (hslider ,parent
                            :css `(:--thumb-color ,,thumbcolor
                                   :--bar-color ,,color
                                   :background ,,background
                                   :width ,,width
                                   :height ,,height
                                   :flex "0 0 auto")
                            :val-change-cb (make-orgel-val-receiver ,slot ,orgelidx ,global-orgel))))
       (setf (,g-accessor ,local-orgel) ,name)
       (setf (value ,name) (val (,accessor ,global-orgel))))))

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

(defmacro init-numbox (slot parent orgelidx local-orgel global-orgel &key (size 10) (min nil) (max nil))
  (let ((name (intern (format nil "~:@(nb-~a~)" slot)))
        (g-accessor (intern (format nil "~:@(g-orgel-~a~)" slot)))
        (accessor (intern (format nil "~:@(orgel-~a~)" slot)))
        (slot-label (intern (format nil "~:@(~a~)" slot))))
    `(let* (
            (container (create-div ,parent :css '(:display "flex" :align-items "baseline" :justify-content "space-between")))            
            (,name (numbox container
                           :label ,(format nil "~(~A~)" (slot-label slot))
                           :min ,min
                           :max ,max
                           :color "black" :background-color "#fff"
                           :val-change-cb (make-orgel-val-receiver ,slot ,orgelidx ,global-orgel)
                           :slot ',slot-label
                           :size ,size)))
       (setf (,g-accessor ,local-orgel) ,name)
       (setf (value ,name) (val (,accessor ,global-orgel))))))


;;; (init-numbox :base-freq nbs1 (aref (orgel-gui-orgeln *papierrohrorgeln*) 0))

(defun collect-terms (slots containers ranges orgelidx local-orgel global-orgel size)
  (loop
    for slot in slots
    for container in containers
    for range in ranges
    collect `(init-numbox ,slot ,container ,orgelidx ,local-orgel ,global-orgel :min ,(first range) :max ,(second range) :size ,size)))

(defmacro init-numboxes (slots containers ranges orgelidx local-orgel global-orgel &key (size 10))
  `(progn
     ,@(collect-terms slots containers ranges orgelidx local-orgel global-orgel size)))

(defmacro init-multi-vu (slot parent orgelidx local-orgel global-orgel
                         &key (num 8) (width "80px") (height "100px") (background "#444") (direction :up) (border "none")
                           (inner-background "var(--vu-background)") (inner-border "thin solid black") (inner-padding "0") (display-map :pd)
                           (inner-padding-bottom "0px")
                           (led-colors :blue) (css '(:margin-bottom 10px :position absolute :top 0 :left 0))
                           val-change-cb)
  (declare (ignore orgelidx global-orgel))
  (let ((vus (gensym "vus"))
        (mvu (gensym "mvu"))
        (g-accessor (intern (format nil "~:@(g-orgel-~a~)" slot)))
;;;        (accessor (intern (format nil "~:@(orgel-~a~)" slot)))
        )
    `(let* ((,mvu (multi-vu ,parent :num ,num :width ,width :height ,height
                                    :led-colors ,led-colors
                                    :direction ,direction :background ,background
                                    :inner-background ,inner-background
                                    :border ,border :inner-border ,inner-border
                                    :inner-padding-bottom ,inner-padding-bottom
                                    :inner-padding ,inner-padding
                                    :display-map ,display-map
                                    :css ,css
                                    :val-change-cb ,val-change-cb))
            (,vus (meters ,mvu)))
       
       (setf (,g-accessor ,local-orgel) (meters ,mvu))
       (loop for vu across ,vus
             for idx from 0
             do (setf (attribute vu "data-db") (- (val (aref (aref *orgel-mlevel* orgelidx) idx)) 100)))
       ,mvu)))

(defmacro init-kbd-multi-vu (slot parent gui-orgeln global-orgel
                         &key (num 8) (width "80px") (height "100px") (background "#444") (direction :up) (border "none")
                           (inner-background "var(--vu-background)") (inner-border "thin solid black") (inner-padding "0")
                           (inner-padding-bottom "0px")
                           (led-colors :blue) (css '(:margin-bottom 10px :position absolute :top 0 :left 0))
                           val-change-cb)
  (declare (ignore orgelidx global-orgel))
  (let ((vus (gensym "vus"))
        (mvu (gensym "mvu"))
        (g-meter-array (gensym "g-meter-array"))
        (g-accessor (intern (format nil "~:@(g-orgel-~a~)" slot)))
;;;        (accessor (intern (format nil "~:@(orgel-~a~)" slot)))
        )
    `(let* ((,mvu (multi-vu ,parent :num ,num :width ,width :height ,height
                                    :led-colors ,led-colors
                                    :direction ,direction :background ,background
                                    :inner-background ,inner-background
                                    :border ,border :inner-border ,inner-border
                                    :inner-padding-bottom ,inner-padding-bottom
                                    :inner-padding ,inner-padding
                                    :css ,css
                                    :val-change-cb ,val-change-cb))
            (orgel-freq-vector (make-array 160))
            (,vus (meters ,mvu))
            (,g-meter-array (coerce
                             (loop for i below *orgelcount*
                                   collect (make-array 16))
                             'vector)))
       
       (loop for idx below *orgelcount* do (setf (,g-accessor (aref gui-orgeln idx)) (aref ,g-meter-array idx)))
       (loop for vu across ,vus
             for idx from 0
             do (let* ((orgel-ref (aref orgel-freq-vector idx))
                          (orgel-idx (1- (third orgel-ref)))
                          (array-idx (1- (fourth orgel-ref))))
                  (setf (attribute vu "data-db") (- (val (aref (aref *orgel-mlevel* orgel-idx) array-idx)) 100))
                  (setf (aref (aref ,g-meter-array orgel-idx) array-idx) vu)))
       ,mvu)))


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

(defun create-slider-panel (container &key label val-change-cb)
  (create-div container :content label :css *msl-title-css*)
  (apply #'multi-slider container :val-change-cb val-change-cb *msl-style*))

#|
(defun make-orgel-attr-val-receiver (slot orgelidx global-orgel-ref &key (attribute "data-val"))
  (let ((slot-symbol (intern (format nil "~:@(~a~)" slot) 'cl-orgel-gui)))
    (lambda (val self)
;;      (break "attr-val-receiver: ~a" val self)
      (let* ((val-string (ensure-string val))
             (orgel-val (/ (read-from-string val-string)
                           (if (member slot '(:main :bias-bw)) 1.0 1.0)))
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
;;      (break "val-receiver: ~a" val self)
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
;;      (break "array-receiver: ~a ~a ~a" idx val self)
      (let* ((val-string (ensure-string val))
;;;             (num-val (read-from-string val-string))
             (orgel-val (read-from-string val-string)))
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
|#

(defun slot->function (struct-name slot &optional (package 'cl-orgel-gui))
  "get the function object for a slot of a struct with prefix"
  (symbol-function (intern (string-upcase (format nil "~a-~a" struct-name slot)) package)))

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
