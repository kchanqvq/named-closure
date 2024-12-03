(asdf:defsystem named-closure
  :version "0.0.1"
  :author "Qiantan Hong <qhong@alum.mit.edu>"
  :maintainer "Qiantan Hong <qhong@alum.mit.edu>"
  :license "MIT"
  :description "Named closures"
  :components ((:file "named-closure"))
  :depends-on (:closer-mop
               :alexandria :serapeum
               :iterate :trivial-cltl2))
