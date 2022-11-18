;; cl-facts
;; Copyright 2011-2022 Thomas de Grivel <thodg@kmx.io>
;;
;; Permission is hereby granted to use this software granted
;; the above copyright notice and this permission paragraph
;; are included in all copies and substantial portions of this
;; software.
;;
;; THIS SOFTWARE IS PROVIDED "AS-IS" WITHOUT ANY GUARANTEE OF
;; PURPOSE AND PERFORMANCE. IN NO EVENT WHATSOEVER SHALL THE
;; AUTHOR BE CONSIDERED LIABLE FOR THE USE AND PERFORMANCE OF
;; THIS SOFTWARE.

(in-package :facts)

;;  View

(defclass view ()
  ((refresh :initarg refresh
            :reader view-refresh
            :type function)
   (index :initform (make-usl :lessp lessp)
          :reader view-index
          :type usl)))

(defmacro view-refresher ((view) &body body)
  `(lambda (,view)
     ,@body))

(defun refresh-view (view)
  (funcall (view-refresh view) view))

(defmacro defview (name &body body)
  `(progn
     (defvar ,name)
     (setq ,name (make-instance 'view
                                :refresh (view-refresher
                                          (,name) ,@body)))
     (refresh-view ,name)
     ',name))
