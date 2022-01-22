(asdf:defsystem named-closure
  :version "0.0.1"
  :author "Qiantan Hong <qhong@alum.mit.edu>"
  :maintainer "Qiantan Hong <qhong@alum.mit.edu>"
  :license "GPLv3+"
  :description "Named closures"
  :components ((:file "named-closure"))
  :depends-on (:closer-mop :alexandria :hu.dwim.walker :hu.dwim.util))
