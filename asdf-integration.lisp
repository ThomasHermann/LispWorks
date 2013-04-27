#|

 LispWorks ASDF Integration

 Copyright (c) 2011-2013, Thomas M. Hermann

 Permission is hereby granted, free  of charge, to any person obtaining a copy
 of this software and associated documentation files (the "Software"), to deal
 in the Software without restriction,  including without limitation the rights
 to use, copy, modify,  merge,  publish,  distribute,  sublicense, and/or sell
 copies of the  Software,  and  to  permit  persons  to  whom  the Software is
 furnished to do so, subject to the following conditions:

 The above copyright notice and  this  permission  notice shall be included in
 all copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED  "AS IS",  WITHOUT  WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT  NOT  LIMITED  TO  THE  WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE  AND  NONINFRINGEMENT. IN NO EVENT SHALL THE
 AUTHORS OR COPYRIGHT  HOLDERS  BE  LIABLE  FOR  ANY  CLAIM,  DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 SOFTWARE.

|#

(in-package :asdf)

;;; Write output to the output buffer collector stream.

(defun lw-collector-stream ()
  "The stream used for the LispWorks output buffer."
  (aif (capi:collect-interfaces (find-class 'lw-tools:listener))
       (capi:map-pane-descendant-children
        (first it)
        (lambda (self)
          (when (typep self 'capi:collector-pane)
            (return-from lw-collector-stream
              (capi:collector-pane-stream self)))))))

(defmethod operate :around (operation system &rest initargs)
  "Set *STANDARD-OUTPUT* in LispWorks to the output buffer."
  (aif (lw-collector-stream)
       (let ((*standard-output* it)
             (*error-output* it))
         (call-next-method)
         (terpri)
         (write-string "---- ASDF Done ----"))
       (call-next-method)))

;;; Add an ASDF namespace for the System Browser

(defun lw-list-system-names ()
  "Return a list of the names of the defined systems."
  (loop for system-name being each hash-key in *defined-systems*
        collect system-name))

(defun lw-list-systems ()
  "Return a list of the defined systems."
  (loop for system being each hash-value in *defined-systems*
        collect (cdr system)))

;;; FIXME : Given a wild filename, generate a list of names to find.
(defun lw-finder (name)
  "Return the system for name."
  (find-system name nil))

(scm:add-system-namespace "asdf"
                          :name-lister 'lw-list-system-names
                          :system-lister 'lw-list-systems
                          :finder 'lw-finder)

;;; Mapping over sources

(defclass map-source-op (load-source-op)
  ((function
    :type function
    :initarg :function
    :documentation
    "Function to map with one argument, a pathname.")
   (source-type
    :type symbol
    :initarg :source-type
    :documentation "Either :source or :object.")
   (must-exist-p
    :type boolean
    :initarg :must-exist-p
    :documentation
    "Only existing pathnames should be passed if non-nil.")
   (skip-binary-p
    :type boolean
    :initarg :skip-binary-p
    :documentation
    "Skip components where the :source is binary if non-nil."))
  (:default-initargs
   :function #'identity
   :source-type :source
   :must-exist-p t
   :skip-binary-p t)
  (:documentation
   "ASDF operation to map over source files applying function."))

(defmethod operation-done-p ((op map-source-op) (co source-file))
  (declare (ignorable op co))
  nil)

(defmethod perform ((op map-source-op) (co cl-source-file))
  "Perform the function on the CL source file."
  (let ((source (component-pathname co)))
    (if (slot-value op 'must-exist-p)
        (when (probe-file source)
          (funcall (slot-value op 'function) source))
        (funcall (slot-value op 'function) source))))

(defmethod scm:map-system-pathnames ((system system) function &key
                                     (type :source) (must-exist t)
                                     skip-binary)
  (operate 'map-source-op system
           :force t
           :function function
           :source-type type
           :must-exist-p must-exist
           :skip-binary-p skip-binary))

(defmethod scm:full-pathname ((object source-file) subject &key)
  "Return the full pathname of the component."
  (declare (ignorable subject))
  (component-pathname object))

;;; Component, module, & system properties

(defmethod scm:module-name ((object component))
  "Return the name of the module."
  (component-name object))

(defmethod scm:module-parent ((object component))
  "Return the parent of a module."
  (component-parent object))

;;; FIXME : Probably should contain at least the version.
(defmethod scm:module-flag-description ((object component))
  "Returns a string or nil."
  "None")

;;; NOTE : What the user sees in the IDE, which is not necessarily the
;;;        same as the name.
(defmethod scm:module-print-name ((object component))
  (component-name object))

;;; NOTE : Must match the name given to scm:add-system-namespace.
(defmethod scm:module-namespace-name ((object component))
  "Name of the namespace for the component."
  (declare (ignorable object))
  "asdf")

(defmethod scm:module-pathname ((object component) parent)
  "Returns the pathname of the module."
  (declare (ignore parent))
  (component-pathname object))

;;; NOTE : The result is a list of 'children'. Each child is a
;;; 'module', but provided there is either SCM:MODULE-PRINT-NAME or
;;; SCM:MODULE-NAME defined for it, it can be anything. There is a
;;; default method on T returning nil.
(defmethod scm:module-children ((object module))
  "Return the children of a module."
  (module-components object))

(defmethod scm:module-is-system-p ((object module))
  "An ASDF module is a 'system'."
  t)

(defmethod scm:system-members ((object system))
  "Return members of the system."
  (module-components object))

(defmethod scm:system-default-pathname ((object system))
  "Returns the pathname of the system."
  (system-source-directory object))

(defmethod scm:system-target-directory ((object system))
  "Return the target directory for the system FASL files."
  (apply-output-translations (system-source-directory object)))

(defmethod scm:system-object-pathname ((object system))
  "Return the pathname for the compiled objects."
  (apply-output-translations (component-pathname object)))

;;; Plan and execute

(defclass asdf-event ()
  ((operation
    :type operation
    :initarg :operation
    :accessor event-operation
    :documentation "The ASDF operation for the event.")
   (component
    :type component
    :initarg :component
    :accessor event-component
    :documentation "The ASDF component for the event."))
  (:documentation
   "An event in the system plan."))

;;; NOTE : The preview tree decides which icon to use based on the
;;; result.
(defmethod scm:event-source-p ((event asdf-event))
  "Returns non-nil if the event is a 'source' event."
  (eq (type-of (event-operation event)) 'load-source-op))

(defmethod scm:event-module ((event asdf-event))
  "Returns the module of the event."
  (event-component event))

(defmethod (setf scm:event-module) ((co component) (event asdf-event))
  "Set the component of the ASDF event."
  (setf (event-component event) co))

(defmethod scm:event-action ((event asdf-event))
  "Returns the action of the event."
  (event-operation event))

(defmethod (setf scm:event-action) ((op operation) (event asdf-event))
  "Set the operation of the ASDF event."
  (setf (event-operation event) op))

(defmethod scm:execute-event ((object module) (event asdf-event))
  "Execute the ASDF event."
  (declare (ignore object))
  (perform-with-restarts (event-operation event)
                         (event-component event)))

;;; NOTE : ACTIONS is a list containing :COMPILE and :LOAD or both. In
;;; ASDF LOAD also COMPILEs, so this maps both '(:LOAD) and '(:COMPILE
;;; :LOAD) to asdf:load-op. FORCE and SOURCE are booleans.
(defmethod scm:system-plan ((object component) actions &key force source)
  "Returns a plan that is interpreted by scm:execute-system-plan."
  (aif (cond (source 'load-source-op)
             ((member :load actions) 'load-op)
             ((member :compile actions) 'compile-op))
       (loop with initargs = (when force '(:force t))
             with op = (apply #'make-instance it
                              :original-initargs initargs
                              initargs)
             for (operation . component) in (traverse op object)
             collect (make-instance 'asdf-event
                                    :operation operation
                                    :component component))))

(defmethod scm:execute-system-plan ((object module) plan)
  "Executes the system plan."
  (declare (ignorable object))
  (with-compilation-unit ()
    (loop for event in plan do
          (perform-with-restarts
           (event-operation event)
           (event-component event)))))


;;; NOTE : This takes the module on which the plan was made (not the
;;; one in the event) and an event in a plan. Needs to return two
;;; values: a string describing the event to show in the preview tree,
;;; and children events.  In ASDF there are no sub-events, so the
;;; second value always NIL.
(defmethod scm:treeify-plan-event ((object module) (event asdf-event))
  "Return a description of the event for the preview tree."
  (values
   (format nil "~A on ~A"
           (type-of (event-operation event))
           (component-name (event-component event)))
   nil))

;;; Editor

(editor:define-file-type-hook ("asd")
    (buffer type)
  (declare (ignore type))
  (setf (editor:buffer-major-mode buffer) "Lisp"))

(defun set-asdf-package (buffer new-mode-p)
  "Use ASDF as the default package for asd files."
  (when new-mode-p
    (let ((pathname (editor:buffer-pathname buffer)))
      (when (and pathname (pathname-match-p pathname "*.asd"))
        ;; FIXME : Accomplish with exported symbols
        (editor::set-buffer-current-package
         buffer (find-package "ASDF"))))))

;;; FIXME : Accomplish with exported symbols
(editor:add-global-hook editor::LISP-MODE-HOOK 'set-asdf-package)
