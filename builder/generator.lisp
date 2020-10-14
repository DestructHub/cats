;;; Functions related to the README.md(s) generation

(load "utils.lisp")

(defun get-folder-name (pathname)
  "Return only the pathname's folder as string instead of the entire path."
  (first (last (pathname-directory pathname))))

(defun get-cats-names ()
  "Return a list of the cats names based on the cats directory."
  (let* ((directory (uiop:subdirectories "../cats"))
         (folders (map 'list #'get-folder-name directory)))
    folders))

(defun get-cat-pictures (cat-name)
  "Return a list of pathnames of all images of a given cat name string."
  (remove-if #'is-not-image (uiop:directory-files (format nil "../cats/~a/" cat-name))))

(defun get-cat-profile-picture (cat-name)
  "Return the picture relative path (from root) that will be used on the main README.md for the given cat name string."
  (let* ((files (get-cat-pictures cat-name))
         (first-file (file-namestring (first files))))
    (format nil "./cats/~a/~a" cat-name first-file)))

(defun generate-img (url &key (width 250))
  "Generate a image tag that shows the image in the given url."
  (format nil "<img src=\"~a\" width=\"~a\">" url width))

(defun generate-table-cell (cat-name)
  "Return a string of a table cell for the given cat name string."
  (format nil "<td align=\"center\"><a href=\"./cats/~a\">~a<strong>~a</strong></a></td>"
          cat-name
          (generate-img (get-cat-profile-picture cat-name))
          cat-name))

(defun generate-table-rows (cats-names &key (chunk-size 4))
  "Returns a string with one row for each cat name in the given list.\
Each row will have a number of elements corresponding to the given chunk size."
  (let* ((current-names (subseq cats-names 0 (min (length cats-names) chunk-size)))
         (cells (map 'list #'generate-table-cell current-names))
         (cells-string (apply #'concatenate 'string cells))
         (row-string (format nil "<tr>~a</tr>" cells-string)))
    (if (null (nth chunk-size cats-names))
        row-string
        (concatenate 'string row-string (generate-table-rows (subseq cats-names chunk-size))))))

(defun generate-table (cats-names)
  "Returns a string of the entire table for a given list of names."
  (format nil "<table>~a</table>" (generate-table-rows cats-names)))

(defun generate-single-cat-readme (cat-name)
  "Generate a README.md for the given cat name string."
  (let* ((files (get-cat-pictures cat-name) )
         (files-names (map 'list #'file-namestring files)))
    (with-open-file (out-stream (concatenate 'string "../cats/" cat-name "/README.md") :direction :output :if-does-not-exist :create :if-exists :overwrite)
      (write-line (concatenate 'string "# Meet " cat-name) out-stream)
      (loop for file-name in files-names
            do (write-line (generate-img file-name) out-stream)))))

(defun generate-cats-readmes (cats-names)
  "Generate a README.md for each cat name in the given list."
  (loop for cat-name in cats-names
        do (generate-single-cat-readme cat-name)))

(defun generate-main-readme ()
  "Generate the main (root) README.md based on the template file."
  (with-open-file (in-stream "template.md" :if-does-not-exist :error)
    (let ((template-string (make-string (file-length in-stream))))
      (read-sequence template-string in-stream)
      (with-open-file (out-stream "../README.md" :direction :output :if-does-not-exist :create :if-exists :overwrite)
        (write-string (str-replace template-string "{{table}}" (generate-table (get-cats-names))) out-stream)))))

