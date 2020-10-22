(asdf:defsystem :cats
  :description "Generators for DestructHub/cats"
  :version "0.0.1"
  :author "Mateus Felipe C C Pinto <mateusfccp@gmail.com>"
  :licence "MIT"
  :pathname "src"
  :components ((:file "package")
               (:file "utils" :depends-on ("package"))
               (:file "generator" :depends-on ("package" "utils"))))
