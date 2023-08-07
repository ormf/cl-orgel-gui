;;; 
;;; widget-defs.lisp
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

#|
non-selectable
user-select (none, bzw. auto)

style: user-select:

UserSelect 
|#

(defun multi-vslider (container &key (num 8) (width 80) (height 100) (background "white") (colors #("lightblue")) (thumbcolor "transparent")
                                  receiver-fn)
  (let* ((msl-container (create-div container
                                    :style (format nil "color: transparent; background-color: transparent;border: none;width: ~Apx;height: ~Apx;display: flex;padding: 0.5pt" width height)))
         (vsliders (v-collect (n num) (vslider
                                       msl-container
                                       :border-right-width (if (< n (1- num)) 0 1)
                                       :background background
                                       :color (aref colors (mod n num))
                                       :thumbcolor thumbcolor))))
    (loop for vsl in vsliders
          for idx from 0
          do (let ((vsl vsl) (idx idx))
               (set-on-input
                vsl
                (lambda (obj)
                  (declare (ignore obj))
                  (let ((val (value vsl)))
                    (if receiver-fn (funcall receiver-fn idx val vsl)))))
               (set-on-mouse-move
                vsl
                (lambda (obj event-data)
                  (declare (ignore obj))
                  (when (or (getf event-data :shift-key))
                    (let ((val (- 100 (getf event-data :y))))
                      (when val
                        (setf (value vsl) val)
                        (if receiver-fn (funcall receiver-fn idx val vsl)))))))))
    (values vsliders msl-container)))

#|
(defun vslider
    (container &key (value 0.0) (min 0.0) (max 100.0) (thumbcolor "black") (color "#3071A9")
                 (border-right-width 1) (background-color "#fff")
                 receiver-fn)
    "vertical slider including behaviour."
  (let ((vsl
          (create-form-element
           container :range
           :class "vslider"
           :style (format nil "--border-right-width: ~Apx;--slider-thumb: ~A;--slider-color: ~A;--slider-background: ~A;min-width: 0;flex: 1 1 0;height: 100%;--slider-thumb-height: thin;slider-thumb-width: 100%;"
                          border-right-width thumbcolor color background-color)
           :value (format nil "~a" value)
           :min (format nil "~a" min)
           :max (format nil "~a" max)
           :orient "vertical")))
    (if receiver-fn
        (set-on-input
         vsl
         (lambda (obj)
           (declare (ignore obj))
           (let ((val (value vsl)))
;;;                                       (format t "vsl~a: ~a~%" (1+ idx) val)
             (funcall receiver-fn val vsl)))))
    vsl))
|#


(defun hslider
    (container &key (value 0.0) (min 0.0) (max 100.0) (thumbcolor "black") (color "#3071A9")
                 (border-right-width 1) (background "#fff") width height
                 receiver-fn)
    "horizontal slider including behaviour."
  (let ((hsl
          (create-form-element
           container :range
           :class "hslider"
           :style (format nil "--border-right-width: ~Apx;
--slider-thumb: ~A;--slider-color: ~A;
--slider-background: ~A;min-width: 0;flex: 1 1 0;--slider-thumb-height: 100%;
--slider-thumb-width: 2px;height: ~A;~@[width: ~A;~]"
                          border-right-width thumbcolor color background
                          (or height "10px") width)
           :value (format nil "~a" value)
           :min (format nil "~a" min)
           :max (format nil "~a" max))))
    (if receiver-fn
        (set-on-input
         hsl
         (lambda (obj)
           (declare (ignore obj))
           (let ((val (value hsl)))
;;;                                       (format t "vsl~a: ~a~%" (1+ idx) val)
             (funcall receiver-fn val hsl)))))
    hsl))

(defun vslider
    (container &key (value 0.0) (min 0.0) (max 100.0) (thumbcolor "black") (color "#3071A9")
                 (border-right-width 1) (background "#fff") style
                 receiver-fn)
    "vertical slider including behaviour."
  (let ((vsl
          (create-form-element
           container :range
           :class "vslider"
           :style (format nil "--border-right-width: ~Apx;--slider-thumb: ~A;--slider-color: ~A;--slider-background: ~A;min-width: 0;flex: 1 1 0;height: 100%;--slider-thumb-height: thin;slider-thumb-width: 100%;~@[~A~]"
                          border-right-width thumbcolor color background style)
           :value (format nil "~a" value)
           :min (format nil "~a" min)
           :max (format nil "~a" max)
           :orient "vertical")))
    (if receiver-fn
        (set-on-input
         vsl
         (lambda (obj)
           (declare (ignore obj))
           (let ((val (value vsl)))
;;;                                       (format t "vsl~a: ~a~%" (1+ idx) val)
             (funcall receiver-fn val vsl)))))
    vsl))

(defun numbox (container &key (color "#3071A9")
                           (background-color "#fff")
                           (selected-foreground "black")
                           (selected-background "lightblue")
                           (min 0)
                           (max 127)
                           (value 0)
                           (size 10)
                           label
                           label-style
                           slot
                           receiver-fn)
  (let ((elem
          (create-form-element
           container :text
           :class "numbox"
           :value (format nil "~,1f" value)
           :style (format nil ";--text-color: ~A;align: center;background-color: ~A:--textbox-selected-foreground: ~A;--textbox-selected-background: ~A;font-size: ~Apt;width: ~Apx;height: ~Apx;"
                          color background-color selected-foreground selected-background size
                          (* size 5) (* size 2))
           :min min
           :max max
           :label (if label (create-label container :content (ensure-string label) :style (or label-style "margin-right: 0px;")))))
        mouse-dragged
        startvalue)
;;;    (clog::unbind-event-script elem "onmousedown")
    (set-on-mouse-down
     elem
     (lambda (obj event-data)
       (declare (ignore obj))
       (setf startvalue (read-from-string (or (value elem) "0")))
       (let ((startpos (getf event-data :y)))
         (set-on-mouse-move
          elem
          (let ((last-y startpos) (last-val startvalue))
            (lambda (obj event-data)
              (declare (ignore obj))
              (let* ((y (getf event-data :y))
                     (scale (if (getf event-data :shift-key) 0.1 1))
                     (val (+ last-val (* scale (- last-y y)))))
                (when (/= y last-y)
                  (unless mouse-dragged
                    (setf (style elem "--textbox-selected-foreground") color)
                    (setf (style elem "--textbox-selected-background") background-color))
                  (setf mouse-dragged t)
                  (let ((val-string (format nil "~,1f" val)))
                    (setf (value elem) val-string)
                    (if receiver-fn (funcall receiver-fn val-string elem)))
                  (setf last-y y last-val val)))))))))
    (set-on-key-up
     elem
     (lambda (obj event)
       (declare (ignore obj))
       (when (equal (getf event :key) "Enter")
         (let ((val (value elem)))
           (unless (numberp (read-from-string val))
             (setf val (format nil "~,1f" startvalue)))
           (setf (value elem) val)
           (if receiver-fn (funcall receiver-fn val elem)))
         (blur elem))))
    (set-on-mouse-up
     elem
     (lambda (obj event-data)
       (declare (ignore event-data))
       (set-on-mouse-move
        obj
        (lambda (obj event-data)
          (declare (ignore obj event-data))))
       (if mouse-dragged (progn
                           (blur elem)
                           (setf (style obj "--textbox-selected-foreground") selected-foreground))
                           (setf (style obj "--textbox-selected-background") selected-background))
                           (setf mouse-dragged nil)))
    elem))

(defmethod clog::create-button ((obj clog-obj)
                                &rest args
                                &key (content "")
                                  (style nil)
                                  (hidden nil)
                                  (class nil)
                                  (html-id nil)
                                  (auto-place t)
                                &allow-other-keys)
  (create-child obj (format nil "<button~@[~A~]~@[~A~]~@[~A~]>~A</button>"
                            (when class
                              (format nil " class='~A'"
                                      (escape-string class :html t)))
                            (when (getf args :data-val) (format nil "data-val = ~a" (ensure-string (getf args :data-val))))
                            (when (or hidden style)
                              (format nil " style='~@[~a~]~@[~a~]'"
                                      (when hidden "visibility:hidden;")
                                      style))
                            content)
                :clog-type  'clog-button
                :html-id    html-id
                :auto-place auto-place))



(defun toggle (container &key (style "") (content "") (size 6)
                           (color "black")
                           (background "white")
                           (selected-foreground "black")
                           (selected-background "orange")
                           (value-off "0.0")
                           (value-on "1.0")
                           receiver-fn (toggle-content "")
                           slot)
  (declare (ignore slot))
  (let ((btn (create-button container :class "toggle"
                                   :style (format nil "~A width: ~A;height: ~Apx;font-size: ~Apt;color: ~A;background-color: ~A;"
                                                     style (* 5 size) (* 2 size) size color background)
                                   :content content
                                   :data-val "0.0")))
    (let ((str (format nil "toggle(~A, { \"colorOff\": '~(~a~)', \"backgroundOff\": '~(~a~)', \"labelOff\": '~(~a~)', \"valueOff\": '~(~a~)', \"colorOn\": '~(~a~)', \"backgroundOn\": '~(~a~)', \"labelOn\": '~(~a~)', \"valueOn\": '~(~a~)'})"
                            (jquery btn) color background content value-off selected-foreground selected-background toggle-content value-on)))
      (js-execute btn str))
    (set-on-click
     btn
     (lambda (obj)
       (declare (ignore obj))
       (let ((new-val (if (equal (attribute btn "data-val") value-on)
                           value-off value-on)))
         (setf (attribute btn "data-val") new-val)         
         (if receiver-fn (funcall receiver-fn new-val btn)))))
;;     (set-on-mouse-down
;;      btn
;;      (lambda (obj event)
;;        (declare (ignore obj event))
;; ;;;       (setf mouse-down t)
;;        (if button-state
;;            (progn
;;              ;; (setf (style obj "color") color)
;;              ;; (setf (style obj "background-color") background)
;;              ;; (setf (text obj) content)
;;              )
;;            (progn
;;              ;; (setf (style obj "color") selected-foreground)
;;              ;; (setf (style obj "background-color") selected-background)
;;              ;; (if toggle-content (setf (text obj) toggle-content))
;;              ))))
    btn))

(deftype vu-type () '(member :led :bar))
(deftype vu-input-mode () '(member :db :lin))
(deftype vu-display-map () '(member :pd :lin :dblin))
(deftype vu-direction () '(member :up :down :left :right))

(defun addpx (token)
  (if (numberp token) (format nil "~apx" token) token))

;;; (format nil "~(~a~)" :--vu-background)
(defun vumeter (container &rest args
                &key style (width 10) (height 126) background
                  (direction :up) (border "thin solid black")
                  (vu-type :led) (input-mode :db) (display-map :pd) led-colors (bar-color "'rgba(60,60,255,1.0)'")
                  (data-db -100) inner-padding inner-padding-bottom
                &allow-other-keys)
  (declare (ignorable style border)
           (type vu-direction direction)
           (type vu-type vu-type)
           (type vu-input-mode input-mode)
           (type vu-display-map display-map))
  (dolist (key '(:type :input-mode :display-map :led-colors :data-db :style :inner-padding :inner-padding-bottom)) (remf args key))
  (setf (getf args :direction) direction)
  (setf (getf args :border) border)
  (setf (getf args :width) (format nil "~a" (addpx (if (member direction '(:left :right)) height width))))
  (setf (getf args :height) (format nil "~a" (addpx (if (member direction '(:left :right)) width height))))
  (when background (setf (getf args :--vu-background) background))
;;;  (break (format nil "justify-content: center;~{~(~A~): \"~a\"~^; ~}" args))
  (let ((vu-container (create-div container :class "vumeter" :style (format nil "justify-content: center;background-color: var(--vu-background);~{~(~A~): ~a; ~}~@[~a~]" args style) :data-db data-db)))
    (js-execute vu-container (format nil "vumeter(~A, {\"boxCount\": 40, \"ledColors\": ~a, \"barColor\": ~a, \"vuType\": '~(~a~)', \"inputMode\": '~(~a~)', \"displayMap\": '~(~a~)', \"direction\": '~(~a~)', \"innerPadding\": '~(~a~)', \"innerPaddingBottom\": '~(~a~)'})"
                                  (jquery vu-container)
                                  (if led-colors (if (symbolp led-colors) (format nil "\"~(~a~)\"" led-colors) (cols->jsarray led-colors)) "false")
                                  bar-color
                                  vu-type input-mode display-map direction
                                  (or inner-padding "false")
                                  (or inner-padding-bottom "false")))))


#|
(break (format nil "vumeter(~A, {\"boxCount\": 40, \"ledColors\": ~a, \"barColor\": ~a, \"vuType\": '~a', \"inputMode\": '~a', \"displayMap\": '~a' })"
                   "CLOG"
                   (if led-colors (cols->jsarray led-colors) "false")
                                  bar-color
                                  (jquery vu-container)
                                  vu-type input-mode display-map))
|#

(defun multi-vu (container &key (num 8) (width 80) (height 100) background (direction :up) (border "")
                           (inner-background "var(--vu-background)") (inner-border "") inner-padding inner-padding-bottom
                           led-colors style
                             receiver-fn)
  (declare (ignorable receiver-fn))
  (let* ((mvu-container (create-div container
                                    :style (format nil "color: transparent; background-color: ~a;border: ~a;width: ~Apx;height: ~Apx;display: flex;padding: 0pt;~@[~A~]" (or background "transparent") border width height style)))
         (vus (v-collect (n num) (vumeter
                                  mvu-container
                                  :led-colors led-colors
                                  :direction direction
                                  :border inner-border
                                  :data-db 0
                                  :width "100%"
                                  :height "100%"
                                  :border-right-width (if (< n (1- num)) 0 1)
                                  :background (or inner-background background "var(--vu-background)")
                                  :inner-padding inner-padding
                                  :inner-padding-bottom inner-padding-bottom))))
    ;; (loop for vu in vus
    ;;       for idx from 0
    ;;       do (let ((vu vu) (idx idx))
    ;;            (set-on-input
    ;;             vsl
    ;;             (lambda (obj)
    ;;               (declare (ignore obj))
    ;;               (let ((val (value vsl)))
    ;;                 (if receiver-fn (funcall receiver-fn idx val vsl)))))
    ;;            (set-on-mouse-move
    ;;             vsl
    ;;             (lambda (obj event-data)
    ;;               (declare (ignore obj))
    ;;               (when (or (getf event-data :shift-key))
    ;;                 (let ((val (- 100 (getf event-data :y))))
    ;;                   (when val
    ;;                     (setf (value vsl) val)
    ;;                     (if receiver-fn (funcall receiver-fn idx val vsl)))))))))
    (values vus mvu-container)))
