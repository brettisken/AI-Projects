(in-package :cl-user)
(defpackage queenapp.web
  (:use :cl
        :caveman2
        :queenapp.config
        :queenapp.view
        :queenapp.db
        :datafly
        :sxql)
  (:export :*web*))
(in-package :queenapp.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Routing rules

(defroute "/" (&key (|size| "8") _parsed)
  (let (domain)
    (cond ((second _parsed) ;;if domain restricted parse the input from html
	   (dolist (x (cdr (assoc "queens" _parsed :test #'string=)))
	     (if (<= (parse-integer (car x)) (parse-integer |size|))
	     (push (list (intern (format nil "Q~A" (car x)) :keyword)
			 (parse-integer (cdr x)))
		   domain)))))
    (render #P"index.html"
	    (n-queens:generate-solution (parse-integer |size|) domain))))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
