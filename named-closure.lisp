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
    (intern (concatenate 'string "MAKE-" (symbol-name symbol))))
  (defun funcallable-class-name (symbol)
    (intern (concatenate 'string (symbol-name symbol) "-CLASS")))
  (defun walk-fvs (form env)
    (delete-duplicates
     (mapcar #'hu.dwim.walker:name-of
             (remove-if-not
              (lambda (elt) (typep elt 'hu.dwim.walker:unwalked-lexical-variable-reference-form))
              (hu.dwim.walker:collect-variable-references
               (hu.dwim.walker:walk-form form :environment (hu.dwim.walker:make-walk-environment env))))))))

(defclass nclo () () (:metaclass funcallable-standard-class))

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
        (make-function-name (make-function-name name))
        (funcallable-class-name (funcallable-class-name name)))
    (multiple-value-bind (forms decls doc)
        (alexandria:parse-body body :documentation t)
      `(progn
         (defclass ,funcallable-class-name (nclo)
           ,(mapcar #'list fvs)
           (:metaclass funcallable-standard-class))
         (defmethod initialize-instance ((object ,funcallable-class-name) &key)
           (set-funcallable-instance-function
            object (lambda (&rest args) (apply ',name object args))))
         (defun ,make-function-name ,lambda-list-1
           (let ((object (make-instance ',funcallable-class-name)))
             ,@(mapcar (lambda (fv) `(setf (slot-value object ',fv) ,fv)) fvs)
             object))
         (defmethod print-object ((object ,funcallable-class-name) stream)
           (with-slots ,fvs object
             (format stream "#.~s"
                     (cons ',make-function-name
                           ,(lambda-list-serialize-form lambda-list-1)))))
         (defun ,name (object ,@lambda-list-2)
           ,doc
           (with-slots ,fvs object
             ,@decls ,@forms))))))

(defmacro nclo (name lambda-list &body body &environment env)
  "Similar to (lambda LAMBDA-LIST . BODY).

Returns a funcallable object with slots corresponding to free variable
in BODY, has readable print syntax, and if `nclo' with the same NAME
is encountered (for example, if re-evaluated from REPL), the function
definition of all such funcallable objects is updated. Closed
variables with the same names are carried over across update."
  (let ((fvs (walk-fvs `(labels ((,name ,lambda-list ,@body)) #',name) env)))
    #+sbcl
    (sb-kernel::%compiler-defclass (funcallable-class-name name) nil nil fvs)
    `(funcall (make-function-name
               (load-time-value (defnclo ,name (&key ,@fvs) ,lambda-list ,@body)))
              ,@(mapcan (lambda (fv) (list (alexandria:make-keyword fv) fv)) fvs))))
