;;;; tags.lisp -- Inject tag index in HTML.

(defpackage coleslaw-tags
  (:use :cl :coleslaw)
  (:export :enable))

(in-package :coleslaw-tags)

(defmethod render :around ((object coleslaw::tag-index) &key prev next)
  (declare (ignore prev next))
  (funcall (theme-fn 'tag)
           (list :tag object
                 :config *config*
                 :contents (coleslaw::index-content object))))

(defmethod publish :after ((doc-type
                            (eql (find-class 'coleslaw::tag-index))))
  (dolist (tag (find-all (class-name doc-type)))
    (write-document tag (theme-fn 'base))))

(defparameter *site-tags* nil
  "Variable to hold site tags.")

(defun enable ()
  (labels ((write-tag (s tag)
             (format s "<a href=\"~a/tag/~a.html\">~a</a>"
                     (domain *config*)
                     (coleslaw::tag-slug tag)
                     (coleslaw::tag-name tag)))
           (write-tags (s tags)
             (if (null (cdr tags))
                 (write-tag s (car tags))
                 (progn
                   (write-tag s (car tags))
                   (format s " • ")
                   (write-tags s (cdr tags)))))
           (inject-p (x)
             (declare (ignore x))
             (or *site-tags*
                 (let ((result (with-output-to-string (s)
                                 (write-tags s coleslaw::*all-tags*))))
                   (setf *site-tags* result)))))
    (add-injection #'inject-p :body)))
