;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2017 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : mouse.lisp
;;; Version     : 3.2
;;; 
;;; Description : Implements a mouse device.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;; 2017.03.02 Dan [1.0]
;;;             : * First pass at separating the mouse from the internals of the
;;;             :   motor module and device-interface as was done for the keyboard.
;;; 2017.03.07 Dan
;;;             : * Moved the start-hand-at-mouse command here.
;;; 2017.03.08 Dan
;;;             : * The virtual-cursor class isn't something that can be assumed
;;;             :   by the motor module because that couldn't be provided by an
;;;             :   external cursor.  Instead the cursor has to respond to a 
;;;             :   bunch of signals for that info.
;;; 2017.03.10 Dan
;;;             : * Added a remote start-hand-at-mouse command.
;;; 2017.06.08 Dan
;;;             : * Initialize-mouse needs to return a true value.
;;; 2017.08.10 Dan [2.0]
;;;             : * Like the keyboard, install a component to handle the mouse
;;;             :   table instead of using a global variable.  Now, keep old
;;;             :   mouse items around and reuse them when switching windows.
;;; 2017.08.17 Dan
;;;             : * Start-hand-at-mouse needs to speicify a list when creating
;;;             :   a default mouse.
;;; 2017.08.23 Dan
;;;             : * Don't need the dummy functions for the actions now.
;;; 2018.01.29 Dan
;;;             : * Fixed a bug with start-hand-at-mouse because it was assuming
;;;             :   that the mouse was associated with a visual interface, but
;;;             :   the one it creates by default is not. 
;;;             : * Remove-mouse needs to return t to indicate successful removal.
;;; 2018.03.05 Dan
;;;             : * Signal-device was renamed to notify-device.
;;; 2018.03.06 Dan [3.0]
;;;             : * Reworking it so that it's one mouse per model -- not per
;;;             :   window which has issues with models sharing a window.
;;;             : * Also, reset the mouse positions upon init instead of letting
;;;             :   them stay where they were.
;;; 2018.03.07 Dan
;;;             : * Don't directly communicate with the exp-windows.  Instead
;;;             :   create some signals for init, move, click, and remove.
;;; 2018.03.08 Dan
;;;             : * Reworking it yet again after some work on the move-cursor
;;;             :   request from motor.  Actually install a "cursor" and have
;;;             :   the details indicate whether it's a mouse, joystick1, or
;;;             :   joystick2 which is then created.  
;;;             : * Also, don't need the device-hand -- the hand knows where it
;;;             :   is so don't need to consult the device!
;;; 2018.03.13 Dan
;;;             : * Interface fns now get the whole device list and allow a cursor
;;;             :   to be installed for vision or motor!  When installed for vision
;;;             :   it creates a feature in global coordinates.
;;; 2018.03.16 Dan
;;;             : * Warn if start-hand-at-mouse called with :needs-mouse nil.
;;; 2018.03.29 Dan
;;;             : * Don't need to ignore a variable in init-cursor-chunk-type-and-chunks
;;;             :   since it isn't passed in anymore.
;;; 2018.04.26 Dan
;;;             : * Only need to create the cursor chunk-type if it doesn't
;;;             :   already exist.
;;; 2018.05.22 Dan
;;;             : * Update the calls to add-visicon-features since the syntax of
;;;             :   that has changed.
;;; 2018.06.04 Dan
;;;             : * Moved set-cursor-position here from the motor module, fixed a
;;;             :   bug in it, and added remote versions.
;;; 2018.06.13 Dan
;;;             : * Set the cursor-pos slots to lists instead of vectors since
;;;             :   set-hand-location uses lists now too.
;;; 2018.07.26 Dan [3.1]
;;;             : * Don't worry about completing requests.
;;; 2018.10.09 Dan [3.2]
;;;             : * Removed some unnecessary tests for motor module in top level
;;;             :   commands.
;;;             : * Allow for user defined cursors in the same way as keyboards,
;;;             :   and add a set-default-cursor and other-notifications method.
;;;             : * Allow for custom initialization and don't require install
;;;             :   to only be for motor and vision interfaces.
;;; 2018.10.10 Dan
;;;             : * Instead of setting the default cursor, change it to set-default-mouse
;;;             :   since the AGI automatically installs a mouse, and that's 
;;;             :   likely the cursor which will be used most often.  So, it
;;;             :   changes the class used when "mouse" is specified.
;;; 2018.10.11 Dan
;;;             : * Fixed click-mouse because it was accessing the device of a
;;;             :   hand without grabbing the lock.
;;; 2018.10.12 Dan
;;;             : * Because the type of the "mouse" cursor can change need to
;;;             :   make sure it's always referenced by 'mouse in the table.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Create a cursor as its own device under the new interface approach which can
;;; be a mouse, joystick1, or joystick2, and make an instance of an appropriate
;;; cursor device with which a model can interact.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Extend by providing a new class and use that class name as the details.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


;;; The class for a virtual cursor and the provided cursors.
;;; Subclass this to create a custom cursor with the appropriate
;;; values and define a cursor-initialization and other-notifications methods
;;; if needed.

(defclass virtual-cursor ()
  ((cursor-lock :accessor cursor-lock :initform (bt:make-lock "virtual-cursor"))
   (cursor-loc :accessor cursor-loc :initform (list 0 0) :initarg :cursor-loc)
   (cursor-pos :accessor cursor-pos :initform (list 0 0) :initarg :cursor-pos)
   (initial-loc :accessor initial-loc :initform (list 0 0) :initarg :initial-loc)
   (cursor-feature :accessor cursor-feature :initform nil)
   (cursor-ptr :accessor cursor-ptr :initform 'pointer)
   (cursor-name :accessor cursor-name :initarg :cursor-name)
   (cursor-vis-type :accessor cursor-vis-type :initform 'cursor :initarg :cursor-vis-type)
   (device-order :accessor device-order :initform 0 :initarg :device-order)
   (interfaces :accessor interfaces :initform nil)))

(defclass mouse (virtual-cursor)
  ()
  (:default-initargs
    :cursor-name "mouse"
    :cursor-pos (list 28 2)))

(defclass joystick1 (virtual-cursor)
  ()
  (:default-initargs
    :cursor-name "joystick1"
    :cursor-pos (list 28 0)
    :device-order 1))

(defclass joystick2 (virtual-cursor)
  ()
  (:default-initargs
    :cursor-name "joystick2"
    :cursor-pos (list 28 4)
    :device-order 2))

(defmethod cursor-initialization ((c virtual-cursor) device)
  (declare (ignore device)))

(defmethod other-notifications ((c virtual-cursor) device features)
  (print-warning "Cursor device can not process notification from device ~s with features ~s." device features))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The cursor device definition

(defun initialize-cursor (device-list)
  (let* ((model (current-model))
         (interface-name (first device-list))
         (details (string->name (third device-list)))
         (class (cond ((or (null details) (eq details 'mouse))
                       (setf details 'mouse)
                       (default-cursor-class))
                      (t details))))
    
    (unless (subtypep class 'virtual-cursor)
      (print-warning "Invalid cursor type ~s provided for installing a cursor." device-list)
      (return-from initialize-cursor nil))
    
    
    (let* ((key (list model details))
           (cursor (find-model-cursor key)))
      
      (if cursor
          (if (find interface-name (bt:with-lock-held ((cursor-lock cursor)) (interfaces cursor)) :test 'string-equal)
              (print-warning "Cursor ~s already installed for the interface ~s in model ~s and only one is allowed." details interface-name model)
            (progn
              (bt:with-lock-held ((cursor-lock cursor)) (push interface-name (interfaces cursor)))
              
              ;; initialization: custom, then default
              (cursor-initialization cursor device-list)
              
              (when (string-equal interface-name "vision")
                (bt:with-lock-held ((cursor-lock cursor))
                  (setf (cursor-feature cursor) 
                    (first (add-visicon-features `(isa (visual-location ,(cursor-vis-type cursor)) 
                                                       value ,(cursor-name cursor)
                                                       screen-x ,(px (cursor-loc cursor))
                                                       screen-y ,(py (cursor-loc cursor))))))))
              t))
        (progn
          (setf cursor (make-instance class))
          (init-cursor-chunk-types-and-chunks (cursor-name cursor))
          
          (bt:with-lock-held ((cursor-lock cursor))
            (push interface-name (interfaces cursor)))
          
          (add-cursor key cursor)
          
          ;; custom initialization if needed
          
          (cursor-initialization cursor device-list)
          
          ;; signal movement to initial position
          
          (evaluate-act-r-command "move-cursor" model (cursor-name cursor) (initial-loc cursor))
          
          ;; default initialization
          
          (when (string-equal interface-name "vision")
            (bt:with-lock-held ((cursor-lock cursor))
              (setf (cursor-feature cursor) 
                (first (add-visicon-features `(isa (visual-location ,(cursor-vis-type cursor)) 
                                                   value ,(cursor-name cursor)
                                                   screen-x ,(px (initial-loc cursor))
                                                   screen-y ,(py (initial-loc cursor))))))))
          t)))))
              
(add-act-r-command "initialize-cursor" 'initialize-cursor "Function which sets up a cursor when it is installed. Do not call directly.")

(defun uninstall-cursor (device-list) 
  (let* ((model (current-model))
         (interface-name (first device-list))
         (details (aif (string->name (third device-list)) it 'mouse))
         (key (list model details))
         (cursor (find-model-cursor key)))
    
    ;; If it's not in the table or not on the interface list then just ignore it and call
    ;; it a success anyway -- shouldn't have gotten here if
    ;; it wasn't installed...
    
    (when cursor
      (bt:with-lock-held ((cursor-lock cursor))
        (when (find interface-name (interfaces cursor) :test 'string-equal)
          (setf (interfaces cursor) (remove interface-name (interfaces cursor) :test 'string-equal))
        
          (when (string-equal interface-name "vision")
            (when (cursor-feature cursor)
              (delete-visicon-features (cursor-feature cursor))
              (setf (cursor-feature cursor) nil)))
          
          (when (null (interfaces cursor))
            (evaluate-act-r-command "delete-cursor" model (cursor-name cursor))
            (remove-cursor key)))))
    t))
  

(add-act-r-command "uninstall-cursor" 'uninstall-cursor "Function which cleans up a cursor being uninstalled.  Do not call directly.")


(defun internal-cursor-interface (device-list features)
  (let* ((model (current-model))
         (details (aif (string->name (third device-list)) it 'mouse))
         (key (list model details))
         (cursor (find-model-cursor key)))
    
    (if (not cursor)
        (print-warning "Notice sent to cursor ~s but no such cursor could be found for model ~s." device-list model)
      (progn
        ;; The possible signals we'll get are:
        ;;  ((model <>) (style {punch | peck | peck-recoil}) (hand {left | right}) (loc (x,y)))
        ;;   - which indicates a button press
        ;;
        ;;  ((model <>) (style move-cursor) (hand {left | right}) (new-loc (x,y)))
        ;;  
        ;;  (set-position x y)
        ;;  
        ;;  (current-position)
        ;;
        ;;  (device-location)
        ;;  
        ;;  (control-order)
        ;; 
        ;; Do we care where they came from?
        ;; For now doesn't matter which interface, but may want to
        ;; change that at some point.
        
        (cond ((eq (first features) 'current-position)
               (bt:with-lock-held ((cursor-lock cursor))
                 (cursor-loc cursor)))
              ((eq (first features) 'device-location)
               (bt:with-lock-held ((cursor-lock cursor))
                 (coerce (cursor-pos cursor) 'list)))
              ((eq (first features) 'control-order)
               (bt:with-lock-held ((cursor-lock cursor))
                 (device-order cursor)))
              ((eq (first features) 'set-position)
               (bt:with-lock-held ((cursor-lock cursor))
                 (setf (cursor-loc cursor)
                   (list (second features) (third features)))
                 
                 (when (cursor-feature cursor)
                   (modify-visicon-features (list (cursor-feature cursor) 'screen-x (second features) 'screen-y (third features))))
                 
                 (evaluate-act-r-command "move-cursor" model (cursor-name cursor) (cursor-loc cursor))
                 (cursor-loc cursor)))
              (t
               (let ((m (second (find 'model features :key 'first)))
                     (s (second (find 'style features :key 'first)))
                     (h (second (find 'hand features :key 'first)))
                     (l (second (or (find 'loc features :key 'first)
                                    (find 'new-loc features :key 'first)))))
                 
                 (if (and m s h l (find s '(punch peck peck-recoil move-cursor))) ;; the default notifications
            
                     (case s
                       ((punch peck peck-recoil)
                        (let (mx my xyz)
                          (bt:with-lock-held ((cursor-lock cursor))
                            (setf mx (px (cursor-pos cursor))
                              my (py (cursor-pos cursor))
                              xyz (cursor-loc cursor)))
                          
                          (let* ((x (px l)) (y (py l))
                                 (direction (if (eq h 'left) -1 1))
                                 (f (cond ((and (= x mx) (= y my))
                                           'index)
                                          ((and (= x (+ mx direction)) (= y my))
                                           'middle)
                                          ((and (= x (+ mx (* direction 2))) (= y my))
                                           'ring)
                                          ((and (= x (+ mx (* direction 3))) (= y my))
                                           'pinkie)
                                          ((and (= x (- mx direction)) (= y (+ my 2)))
                                           'thumb)
                                          (t
                                           (print-warning "Invalid mouse click finger position ~s." l)))))
                            (when f
                              ;; Is this necessary? shouldn't it always be in the right model?
                              (with-model-eval m
                                (schedule-event-now "click-mouse" :params (list m xyz f) :maintenance t :module 'mouse))))))
                   
                       (move-cursor
                        (bt:with-lock-held ((cursor-lock cursor))
                          (setf (cursor-loc cursor) l)
                          (when (cursor-feature cursor)
                            (modify-visicon-features (list (cursor-feature cursor) 'screen-x (first l) 'screen-y (second l)))))
                        
                        (with-model-eval m
                          (schedule-event-now "move-cursor" :params (list m (cursor-name cursor) l) :maintenance t :module 'mouse))))
                   (other-notifications cursor device-list features)))))))))


(add-act-r-command "cursor-interface-fn" 'internal-cursor-interface "Function which receives the signals for all the cursor devices. Do not call directly.")


(define-device "cursor" "initialize-cursor" "uninstall-cursor" "cursor-interface-fn")


;; These are the commands that it evaluates for monitoring purposes.


(add-act-r-command "click-mouse" nil "Command called when a finger presses a button on the virtual mouse or a real mouse click occurs on a visible exp-window which can be monitored.  It is passed 3 parameters: model name, location list, and finger name. Should not be called directly.")

(add-act-r-command "move-cursor" nil "Command called when a virtual cursor is moved which can be monitored.  It is passed 3 parameters: model name, cursor name, and a list of the new x and y positions. Should not be called directly.")

(add-act-r-command "delete-cursor" nil "Command called when the virtual cursor is uninstalled which can be monitored.  It is passed 2 parameters: model name and cursor name. Should not be called directly.")


;; this is needed for real windows to call

(defun click-the-mouse (model location finger)
  (handle-evaluate-results (evaluate-act-r-command "click-mouse" model location finger)))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; Additional motor actions for a mouse because the general move-cursor
;;;; is used to move it around.  Because these are handled by the internal
;;;; extension commands they get passed the motor module so use it instead
;;;; of interface notifications.

;;; CLICK-MOUSE      [Method]
;;; Date        : 97.02.13
;;; Description : Clicking the mouse is really just a punch with the index finger.  

(defgeneric click-mouse (mtr-mod request)
  (:documentation  "Execute a mouse click operation (a punch with the right hand index finger)"))

(defmethod click-mouse ((mtr-mod motor-module) request)
  (let* ((model (current-model))
         (mouse (find-model-cursor (list model 'mouse))))
    (if mouse
        (let ((hand (right-hand mtr-mod)))
          
          (if (equalp (bt:with-lock-held ((hand-lock hand))(device hand))
                      (list "motor" "cursor" "mouse"))
              (punch mtr-mod :hand 'right :finger 'index :request-spec request)
            (model-warning "CLICK-MOUSE requested when hand not at mouse!")))
      (model-warning "CLICK-MOUSE requested but no mouse device available."))))


(extend-manual-requests (click-mouse) handle-simple-command-request)

(defgeneric hand-to-mouse (mtr-mod request)
  (:documentation  "Moves the right hand to the mouse"))

(defmethod hand-to-mouse ((mtr-mod motor-module) request)
  (cond ((find (list "motor" "cursor" "mouse") (current-devices "motor") :test 'equalp)
         (let* ((mouse (find-model-cursor (list (current-model) 'mouse)))
                (hand (right-hand mtr-mod))
                (pos (bt:with-lock-held ((cursor-lock mouse)) (cursor-pos mouse))))
           (bt:with-lock-held ((hand-lock hand))
             (unless (and (vpt= (loc hand) pos)
                      (equalp (device hand) (list "motor" "cursor" "mouse")))
               (let ((polar (xy-to-polar (loc hand) pos)))
                 (point-hand mtr-mod :hand 'right :r (vr polar) :theta (vtheta polar) :twidth 4.0 
                             :device (list "motor" "cursor" "mouse")
                             :offsets 'standard :request-spec request))))))
        (t
         (model-warning "HAND-TO-MOUSE requested but no mouse device available."))))

(extend-manual-requests (hand-to-mouse) handle-simple-command-request)


;;; Top level commands
;;; use the notifications or available commands for these instead of the motor
;;; module itself.

(defun start-hand-at-mouse ()
  "Starts the right hand on the mouse instead of the 'home row' location"
  (verify-current-model 
   "No current model.  Cannot set hand at mouse."
   (unless (get-parameter-value :needs-mouse)
     (model-warning "Start-hand-at-mouse called but the :needs-mouse parameter is nil."))
   (unless (find (list "motor" "cursor" "mouse") (current-devices "motor") :test 'equalp)
     (model-warning "Installing a default mouse because of start-hand-at-mouse.")
     (install-device (list "motor" "cursor" "mouse")))
   
   (let ((m (find-model-cursor (list (current-model) 'mouse))))
     (set-hand-location-fct 'right (bt:with-lock-held ((cursor-lock m))(cursor-pos m)) (list "motor" "cursor" "mouse"))
     t)))

(add-act-r-command "start-hand-at-mouse" 'start-hand-at-mouse "Have the model place its right hand on the mouse before running. No parameters")


(defmacro set-cursor-position (x y &optional (cursor-name "mouse"))
  "Sets the position of the cursor."
  `(set-cursor-position-fct ',(list x y) ,cursor-name))

(defun set-cursor-position-fct (xyloc &optional (cursor-name "mouse"))
  (verify-current-model 
   "No current model.  Cannot set cursor position."
   (if (and (or (listp xyloc) (vectorp xyloc))
            (= (length xyloc) 2))
       (if (find (list "motor" "cursor" cursor-name) (current-devices "motor") :test 'equalp)
           (let ((result (notify-device (list "motor" "cursor" cursor-name) (list 'set-position (elt xyloc 0) (elt xyloc 1)))))
             (if result
                 result
               (print-warning "Failed to set the position ~s for cursor ~s." xyloc cursor-name)))
         (print-warning "No cursor device named ~s is currently installed for the motor interface." cursor-name))
     (print-warning "Location for set-cursor-position-fct must be a list of two values but ~s provided." xyloc))))

(defun external-set-cursor-position (x y &optional (cursor-name "mouse"))
  (set-cursor-position-fct (list x y) cursor-name))

(add-act-r-command "set-cursor-position" 'external-set-cursor-position "Set the position of the named virtual cursor. Params: x-pos y-pos {cursor-name}")
(add-act-r-command "set-cursor-position-fct" 'set-cursor-position-fct "Set the position of the named virtual cursor. Params: (x-pos y-pos) {cursor-name}")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Create a component for recording the cursor devices which are created
;;; as needed.

(defstruct cursor-component (lock (bt:make-lock "cursor-component")) (table (make-hash-table :test 'equalp)) (default 'mouse))

(defun set-default-mouse (class)
  (let ((c-c (get-component cursor-table)))
    (if c-c
        (cond ((null class) 
               (progn
                 (bt:with-lock-held ((cursor-component-lock c-c))
                   (setf (cursor-component-default c-c) 'mouse))
                 t))
              ((subtypep class 'virtual-cursor)
               (progn
                 (bt:with-lock-held ((cursor-component-lock c-c))
                   (setf (cursor-component-default c-c) class))
                 t))
              (t
               (print-warning "Set-default-mouse requires nil or a subclass of virtual-cursor but given ~s." class)))
      (print-warning "No cursor-table component found when calling set-default-mouse."))))


(defun clear-cursor-component (cursor-component)
  (bt:with-lock-held ((cursor-component-lock cursor-component))
    (clrhash (cursor-component-table cursor-component))))


(defun init-cursor-chunk-types-and-chunks (name)
  (when (and (chunk-type-p visual-object)
             (not (chunk-type-p cursor)))
    (chunk-type (cursor (:include visual-object)) (cursor t)))
  
  (when (stringp name)
    (setf name (string->name name)))
  
  (unless (chunk-p cursor)
    (define-chunks cursor)
    (make-chunk-immutable 'cursor))
  
  (unless (chunk-p-fct name)
    (define-chunks-fct (list name))
    (make-chunk-immutable name)))


(define-component cursor-table :version "3.2" :documentation "Record the cursor devices that are used."
  :creation make-cursor-component
  :clear-all clear-cursor-component
  :delete clear-cursor-component
  :before-reset clear-cursor-component)


;; Accessors for cursor component table

(defun find-model-cursor (key)
  (let ((m-c (get-component cursor-table)))
    (when m-c
      (bt:with-lock-held ((cursor-component-lock m-c))
        (gethash key (cursor-component-table m-c))))))

(defun add-cursor (key cursor)
  (let ((m-c (get-component cursor-table)))
    (when m-c
      (bt:with-lock-held ((cursor-component-lock m-c))
        (setf (gethash key (cursor-component-table m-c)) cursor)))))

(defun remove-cursor (key)
  (let ((m-c (get-component cursor-table)))
    (when m-c
      (bt:with-lock-held ((cursor-component-lock m-c))
        (remhash key (cursor-component-table m-c))))))

(defun default-cursor-class ()
  (let ((m-c (get-component cursor-table)))
    (when m-c
      (bt:with-lock-held ((cursor-component-lock m-c))
        (cursor-component-default m-c)))))


#|
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
|#
