;;; General utilities

(defconstant image-extensions
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
