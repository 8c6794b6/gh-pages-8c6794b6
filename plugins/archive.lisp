;;;; Plugin code to render archive page.

(defpackage :coleslaw-archive
  (:use :cl :coleslaw)
  (:export :enable))

(in-package :coleslaw-archive)

(defclass archive (content)
  ((title :initarg :title :reader coleslaw::title-of)
   (format :initarg :format :reader coleslaw::archive-format))
  (:default-initargs :format :md))

(defmethod initialize-instance :after ((object archive) &key)
  (with-slots (coleslaw::url coleslaw::text format) object
    (setf coleslaw::url (make-pathname :defaults coleslaw::url)
          format (alexandria:make-keyword (string-upcase format))
          coleslaw::text (render-text coleslaw::text format))))

(defmethod render ((object archive) &key next prev)
  (declare (ignore next prev))
  (funcall (theme-fn 'archive)
           (list :config *config*
                 :archive object
                 :posts (coleslaw::by-date (find-all 'post)))))

(defmethod publish ((doc-type (eql (find-class 'archive))))
  (dolist (archive (find-all 'archive))
    (write-document archive)))

(defun enable ())
