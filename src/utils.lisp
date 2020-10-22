;;; General utilities

(in-package #:cats)

(defvar image-extensions
  '("jpg" "jpeg" "bmp" "png" "gif")
  "The image extensions that are accepted by is-image as valid image files.")

(defun is-extension (pathname extension)
  "Returns a bool indicating if pathname has the given extension."
  (string= extension (pathname-type pathname)))

(defun is-image (pathname)
  "Returns a bool indicating if the given pathname represents an image."
  (let* ((comparator (lambda (extension) (is-extension pathname extension)))
         (compared-list (map 'list comparator  image-extensions)))
    (reduce (lambda (a b) (or a b)) compared-list)))

(defun is-not-image (pathname)
  "Returns a bool indicating if the given pathname does not represents an image."
  (not (is-image pathname)))

(defun str-replace (string substring replacement)
  "Returns a new string with substring replaced with replacement."
  (let ((needle-position (search substring string)))
    (if (null needle-position)
        string
        (let* ((needle-length (length substring))
               (before (subseq string 0 needle-position))
               (after (subseq string (+ needle-position needle-length))))
          (concatenate 'string before replacement after)))))

(defun portable-pathname (pathname &optional (system 'cats))
  "PORTABLE-PATHNAME consider two possible dirnames: local and system-wide.

Borrowed from: https://github.com/commonlispbr/starwar/blob/master/src/path.lisp
"
  (if (probe-file pathname)
      pathname
      (asdf:system-relative-pathname system pathname)))

(defun pathjoin (&rest paths)
  "PATHJOIN joins individual paths with / separator"
  (portable-pathname (format nil "~{~a~^/~}" paths)))
