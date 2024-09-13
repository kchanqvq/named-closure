(uiop:define-package named-closure
  (:use #:closer-common-lisp)
  (:export #:defnclo #:nclo))
(in-package :named-closure)

(hu.dwim.util:eval-always
  (defun lambda-list-fvs (lambda-list)
    (multiple-value-bind (requires optionals rest keywords)
        (alexandria:parse-ordinary-lambda-list lambda-list)
      (nconc requires (mapcar #'car optionals)
             (and rest (list rest))
             (mapcar #'cadar keywords))))

  (defun lambda-list-serialize-form (lambda-list)
    (multiple-value-bind (requires optionals rest keywords)
        (alexandria:parse-ordinary-lambda-list lambda-list)
      (when (and rest keywords)
        (warn "Both &rest and &key arguments provided.
NAMED-CLOSURE might not function properly."))
      `(list* ,@requires
              ,@(mapcar #'car optionals)
              ,@(mapcan #'car keywords)
              ,rest)))
  (defun make-function-name (symbol)
    (intern (concatenate 'string "MAKE-" (symbol-name symbol))
            (symbol-package symbol)))
  (defvar *inhibit-walker-eval-load-time-value* nil "Mega Haxx!")
  (defmethod initialize-instance :after ((self hu.dwim.walker:load-time-value-form) &key &allow-other-keys)
    (unless *inhibit-walker-eval-load-time-value*
      (setf (hu.dwim.walker:value-of self) (eval (hu.dwim.walker:body-of self)))))
  (defun walk-fvs (form env)
    (handler-bind
        ((hu.dwim.walker:simple-walker-style-warning
           (lambda (c) (declare (ignore c)) (invoke-restart 'muffle-warning))))
      (let* ((*inhibit-walker-eval-load-time-value* t)
             (walked (hu.dwim.walker:walk-form form :environment (hu.dwim.walker:make-walk-environment env))))
        (values
         (delete-duplicates
          (mapcar #'hu.dwim.walker:name-of
                  (remove-if-not
                   (lambda (elt) (typep elt 'hu.dwim.walker:unwalked-lexical-variable-reference-form))
                   (hu.dwim.walker:collect-variable-references walked))))
         (hu.dwim.walker:unwalk-form walked))))))

(defun prevent-eval (form)
  (if (constantp form) form `',form))

(defclass nclo () ((code :allocation :class))
  (:metaclass funcallable-standard-class))

(defmacro defnclo (name lambda-list-1 lambda-list-2 &body body)
  "Defines a named closure type.

Similar to
 (defun make-NAME LAMBDA-LIST-1
    (lambda LAMBDA-LIST-2 . BODY))

except that `make-NAME' now returns a funcallable object with slots
corresponding to variables declared in LAMBDA-LIST-1, has readable
print syntax, and re-evaluating the DEFNCLO updates the function
definition of all such funcallable objects. Closed variables with the
same names are carried over across update."
  (let ((fvs (lambda-list-fvs lambda-list-1))
        (make-function-name (make-function-name name)))
    (multiple-value-bind (forms decls doc)
        (alexandria:parse-body body :documentation t)
      `(symbol-macrolet ((the-lambda
                           (lambda (object ,@lambda-list-2)
                             ,@decls
                             (with-slots ,fvs object
                               ,@forms))))
         (defclass ,name (nclo)
           ((code :initform the-lambda :allocation :class) ,@(mapcar #'list fvs))
           (:metaclass funcallable-standard-class)
           ,@(when doc `((:documentation ,doc))))
         (defmethod initialize-instance ((object ,name) &key)
           (set-funcallable-instance-function
            object (lambda (&rest args) (apply (slot-value object 'code) object args))))
         (defun ,make-function-name ,lambda-list-1
           (let ((object (make-instance ',name)))
             ,@(mapcar (lambda (fv) `(setf (slot-value object ',fv) ,fv)) fvs)
             object))
         (defmethod print-object ((object ,name) stream)
           (with-slots ,fvs object
             (format stream "#.~s"
                     (cons ',make-function-name
                           (mapcar #'prevent-eval ,(lambda-list-serialize-form lambda-list-1))))))
         (let ((class (find-class ',name)))
           (when (class-finalized-p class)
             (setf (slot-value (class-prototype class) 'code)
                   the-lambda)))
         ',name))))

(defmacro nclo (name lambda-list &body body &environment env)
  "Similar to (lambda LAMBDA-LIST . BODY).

Returns a funcallable object with slots corresponding to free variable
in BODY, has readable print syntax, and if `nclo' with the same NAME
is encountered (for example, if re-evaluated from REPL), the function
definition of all such funcallable objects is updated. Closed
variables with the same names are carried over across update."
  (multiple-value-bind (fvs expanded)
      (walk-fvs `(lambda ,lambda-list ,@body) env)
    (assert (eq (car expanded) 'function))
    (assert (eq (caadr expanded) 'lambda))
    #+sbcl
    (sb-kernel::%compiler-defclass name nil nil fvs)
    `(funcall (make-function-name
               (load-time-value (defnclo ,name (&key ,@fvs) ,@ (cdadr expanded))))
              ,@(mapcan (lambda (fv) (list (alexandria:make-keyword fv) fv)) fvs))))
