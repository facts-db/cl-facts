;; cl-facts
;; Copyright 2011, 2023 Thomas de Grivel <thodg@kmx.io>
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

;;  Transactions

(defvar *transaction* nil)
(defvar *db-path* nil)
(defvar *db-path-defaults* (make-pathname :type "facts"))
(defvar *db-log-path-defaults* (make-pathname :type "facts-log"))
(defvar *transaction-vars* nil)
(defvar *transaction-lock* (bordeaux-threads:make-lock))

(defun transaction-var (value name)
  (let ((cell (rassoc name *transaction-vars* :test #'eq)))
    (if cell
        (setf (car cell) value)
        (push (cons value name) *transaction-vars*))))

(defun transaction-vars ()
  *transaction-vars*)

(defstruct transaction
  (completed nil :type (member nil t))
  (log () :type list))

(defmacro log-transaction-operation (op &rest args)
  (unless (rollback-function op)
    (warn "Undefined rollback function for ~S" op))
  `(when *transaction*
     (push (list ',op ,@args)
           (transaction-log *transaction*))))

(defun db-path ()
  (and *db-path*
       (merge-pathnames *db-path* *db-path-defaults*)))

(defun db-log-path ()
  (and *db-path*
       (merge-pathnames *db-path* *db-log-path-defaults*)))

(defun commit-transaction (tx)
  (let ((path (db-log-path)))
    (when path
      (ensure-directories-exist path)
      (with-open-file (out path
                           :direction :output
                           :if-exists :append
                           :if-does-not-exist :create)
        (dolist (operation (reverse (transaction-log tx)))
          (write (sublis (transaction-vars) operation)
                 :stream out
                 :readably t
                 :pretty nil)
          (fresh-line out)))))
  (setf (transaction-completed tx) t))

(defun rollback-transaction (tx)
  (dolist (operation (transaction-log tx))
    (apply #'rollback operation)))

(defmacro with-mutex ((mutex) &body body)
  (declare (type symbol mutex))
  (let ((g!result (gensym "RESULT-")))
    `(let ((,mutex (bordeaux-threads:make-lock
                    (symbol-name ',mutex)))
           ,g!result)
       (if (bordeaux-threads:with-lock-held (,mutex)
             (setf ,g!result (progn ,@body))
             t)
           ,g!result
           (error "Could not acquire ~S." ',mutex)))))

(defmacro with-transaction (&body body)
  `(if *transaction*
       (progn ,@body)
       (with-mutex (transaction-mutex)
         (let ((*transaction* (make-transaction)))
           (unwind-protect (prog1 (progn ,@body)
                             (commit-transaction *transaction*))
             (unless (transaction-completed *transaction*)
               (rollback-transaction *transaction*)))))))
