;;; cat Package definition

(defpackage #:cats
  (:use :cl)
  (:export #:generate-main-readme
           #:generate-cats-readmes
           #:get-cats-names))
