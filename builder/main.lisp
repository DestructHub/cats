(load "generator.lisp")

(defun main ()
  "Generates both the main README.md and the individual README.md of each cat."
  (generate-main-readme)
  (generate-cats-readmes (get-cats-names)))
