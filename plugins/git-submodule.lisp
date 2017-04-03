(defpackage :coleslaw-git-submodule
  (:use #:cl)
  (:import-from #:coleslaw
                #:*config*
                #:deploy
                #:deploy-dir
                #:rel-path)
  (:export #:enable))

(in-package #:coleslaw-git-submodule)

(defmethod deploy :after (staging)
  (let ((gen-dir (truename (deploy-dir *config*))))
    (delete-file (rel-path gen-dir "index.html"))
    (uiop:copy-file (rel-path gen-dir "1.html")
                    (rel-path gen-dir "index.html"))))

(defun enable ())
