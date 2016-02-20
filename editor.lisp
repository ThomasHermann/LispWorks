#|

 LispWorks Editor Customizations

 Copyright (c) 2010-2016, Thomas M. Hermann

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

(in-package :cl-user)

;;; Indentation

(editor:setup-indent "defmethod" 2 2 4)
(editor:setup-indent "if" 1 4 1)
(editor:setup-indent "with-buffer-locked" 1 2 4)

(editor:setup-indent "defrule" 3 2 4)
(editor:setup-indent "defrender" 3 2 4)

;;; Key bindings

(editor:bind-key "Indent New Line" #\Return :mode "Lisp")
(editor:bind-key "Insert Parentheses For Selection" #\( :mode "Lisp")
(editor:bind-key "Insert Double Quotes For Selection" #\" :mode "Lisp")

;;; Custom commands and bindings

(editor:defcommand "Move Over ()" (p)
  "Move past the next close parenthesis.
Any indentation preceeding the parenthesis is deleted."
  "Move past the next close parenthesis."
  (declare (ignore p))
  (let ((point (editor:current-point)))
    (editor:with-point ((m point))
      (cond ((editor::forward-up-list m)
	     (editor:move-point point m)
             (editor::point-before point)
             (loop (editor:with-point ((back point))
                     (editor::back-to-indentation back)
                     (unless (editor:point= back point)
                       (return)))
                   (editor::delete-indentation point))
	     (editor::point-after point))
	    (t (editor:editor-error))))))

(editor:bind-key "Move Over ()" #\) :mode "Lisp")

;;; Load the logical host for the editor source code
(load-logical-pathname-translations "EDITOR-SRC")

;;; Configure source finding to know about editor source code:
(setf dspec:*active-finders*
      (append dspec:*active-finders*
              (list "EDITOR-SRC:editor-tags-db")))

;;; Black background with green text
;;; Courtesy of : Raymond C Laning <rclaning@raytheon.com>
(defun set-pane-colors (pane)
  (typecase pane
    (capi:echo-area-pane
     (setf (capi:simple-pane-background pane) :black
           (capi:simple-pane-foreground pane) :red))
    (capi:collector-pane
     (setf (capi:simple-pane-background pane) :blue3
           (capi:simple-pane-foreground pane) :white))
    (capi:listener-pane
     (setf (capi:simple-pane-background pane) :black
           (capi:simple-pane-foreground pane) :green))
    (capi:editor-pane
     (setf (capi:simple-pane-background pane) :black
           (capi:simple-pane-foreground pane) :green))
    (capi:tab-layout
     (mapcar 'set-pane-colors (capi:tab-layout-panes pane)))))

(let ((*handle-warn-on-redefinition* :warn)
      (*redefinition-action* :warn))
  (defmethod capi:interface-display :before ((self lispworks-tools:listener))
    (capi:map-pane-descendant-children self 'set-pane-colors))
  (defmethod capi:interface-display :before ((self lispworks-tools:editor))
    (capi:map-pane-descendant-children self 'set-pane-colors)))
