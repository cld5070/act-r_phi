;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell 
;;; Address     : Carnegie Mellon University
;;;             : Psychology Department
;;;             : Pittsburgh,PA 15213-3890
;;;             : db30+@andrew.cmu.edu
;;; 
;;; Copyright   : (c)2002-2005 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : stepper-control.lisp
;;; Version     : 4.0
;;; 
;;; Description : No system dependent code.
;;;             : This file contains the Lisp to support the stepper window.
;;;             : 
;;; Bugs        : 
;;; 
;;; Todo        :
;;; 
;;; ----- History -----
;;;
;;; 05/24/2002  Dan
;;;             : File creation
;;; 10/01/2002  Dan
;;;             : Updated version to 1.1 and fixed the packaging
;;;             : for building a standalone in ACL.
;;;             : Modified the stepper-control-function to use the
;;;             : new uni-wait-for function.
;;; 10/14/2002  Dan
;;;             : Made the changes to implement the tutor mode
;;;             : in the instantiation window instead of the bindings
;;;             : window of the stepper.
;;; 11/11/2002  Dan
;;;             : Modified stepper-instan-info and stepper-control-function
;;;             : so that instantiation picking was possible.  It works
;;;             : off of the step buttons value and the *last-stepper-instantiation*
;;;             : variable.  Requires that the option be set in the
;;;             : environment before it's enabled.
;;; 4/22/2004   Dan [1.5]
;;;             : Added the license info.
;;;             : Updated run-master-process with the RPM 2.2 version.
;;; -----------------------------------------------------------------------
;;; 2005.04.13  Dan
;;;             : * Moved to ACT-R 6.
;;;             : * LOTS of things to change - not included yet.
;;; 2005.04.20  Dan
;;;             : * Updated and added - should work with the tutorial mode.
;;; 2005.05.14 Dan
;;;             : * Fixed a typo in the stepper window - it was printing
;;;             :   +retreval> for retrieval requests...
;;; 2005.08.10 Dan
;;;             : * Minor clean-up to declare event unused in stepper-test.
;;; 2006.03.10 Dan
;;;             : * Calls to get-production no longer need the procedural
;;;             :   module so took that out of stepper-instan-binding.
;;; 2007.07.13 Dan
;;;             : * Added the stepper-stop-button function because the
;;;             :   stop button is being put back on the stepper.
;;; 2007.08.03 Dan
;;;             : * Moved the *stepper-open* defvar to environment-cmds
;;;             :   because it's used there and that's loaded first...
;;; 2007.08.07 Dan
;;;             : * When :esc is set to t the stepper now shows the 
;;;             :   declarative or procedural parameters for the 
;;;             :   item in a new window in the lower left.
;;; 2007.08.08 Dan
;;;             : * Put the "run until" button back into the stepper and
;;;             :   added module as an option now too.
;;; 2007.08.15 Dan
;;;             : * The chunk list shown in the stepper is now sorted
;;;             :   by activation with the chunk being retrieved at the top.
;;; 2011.04.20 Dan
;;;             : * Changed the evt-time call to an evt-mstime one and
;;;             :   converted the user time to ms.
;;; -------------------------------------------------------------------------
;;; 2011.05.20 Dan [3.0]
;;;             : * Start of a complete overhaul to eliminate most of the 
;;;             :   global variable usage and better encapsulate things so
;;;             :   that multiple model support can be added.
;;; 2011.05.25 Dan
;;;             : * Fixed a bug in the tutor-completed function.
;;; 2011.05.26 Dan 
;;;             : * Protected some code that assumed there was a model for
;;;             :   eventual use with multiple models.
;;; 2011.05.31 Dan
;;;             : * Removed a declaration from tutor-completed.
;;;             : * Changed stepper-test so that when it's "until" a production
;;;             :   that name can be from any of the defined models.
;;; 2012.03.01 Dan
;;;             : * Added the macro suppress-declarative-noise which will
;;;             :   temporarily set :ans to nil so that when sdp gets called
;;;             :   from the stepper it doesn't affect the current random
;;;             :   stream because otherwise it changes the results for a
;;;             :   fixed seed model when stepping through it and that makes
;;;             :   debugging more difficult.
;;; 2012.03.21 Dan
;;;             : * Instead of suppress-declarative-noise I can just use 
;;;             :   with-parameters to temporarily set :ans to nil.
;;; 2012.10.25 Dan
;;;             : * Fixed the stepper so it doesn't step when :v is nil, which
;;;             :   is what it's documented as doing i.e. only stepping on
;;;             :   events that show in the trace.
;;; 2015.07.28 Dan
;;;             : * Changed the logical to ACT-R-support in the require-compiled.
;;; 2017.08.11 Dan
;;;             : * Protect access to production-bindings.
;;; 2017.08.28 Dan [4.0]
;;;             : * Complete reworking to not use the hander mechanism and
;;;             :   instead provide functions for the stepper to call and
;;;             :   call stepper side functions as needed.
;;; 2017.08.29 Dan
;;;             : * Continuing the rewrite to complete the stepper tool.
;;;             : * Using a module for it now to more easily handle the
;;;             :   information necessary when multiple models are defined.
;;; 2017.08.31 Dan
;;;             : * Simplified this from all the cross-talk that the old
;;;             :   handler interface had and basically just need the hook
;;;             :   functions, stuff for parsing the events, and things to
;;;             :   deal with multiple models on the Lisp side now.
;;; 2017.10.06 Dan
;;;             : * Need to know if tutor mode is off or on because when it's
;;;             :   on it needs to stop on production-selection events even if
;;;             :   they aren't displayed in the trace.
;;; 2018.08.08 Dan
;;;             : * Have to print the retrieval chunk-spec during the hook
;;;             :   because declaritive frees it when its done.
;;; 2018.12.03 Dan
;;;             : * Need to add a check for whether a varaible is the name of
;;;             :   a buffer for the update when in tutor mode and add extra 
;;;             :   quotes around a binding which is a string.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(defstruct stepper-control
  (lock (bt:make-lock "stepper-lock"))
  open pre-hook post-hook skip-type skip-val tutoring current-event tutor-bindings tutor-responses)

(defvar *stepper* (make-stepper-control))

(defstruct stepper-module rr rs cs)

(defun reset-stepper-module (instance)
  (setf (stepper-module-rr instance) nil)
  (setf (stepper-module-rs instance) nil)
  (setf (stepper-module-cs instance) nil)
  
  (when (bt:with-lock-held ((stepper-control-lock *stepper*)) (stepper-control-open *stepper*))
    
    (no-output
     (unless (find 'stepper-rr-hook (car (sgp :retrieval-request-hook)))
       (sgp :retrieval-request-hook stepper-rr-hook))
     (unless (find 'stepper-rs-hook (car (sgp :retrieval-set-hook)))
       (sgp :retrieval-set-hook stepper-rs-hook))
     (unless (find 'stepper-cs-hook (car (sgp :conflict-set-hook)))
       (sgp :conflict-set-hook stepper-cs-hook)))))

(defun create-stepper-module (name)
  (declare (ignore name))
  (make-stepper-module))

(define-module stepper nil nil 
  :creation create-stepper-module 
  :reset (nil reset-stepper-module) 
  :version 1.0 
  :documentation "Store the model specific information for the Environment's stepper tool.")

(defun check-model-output-for-stepper ()
  (if (bt:with-lock-held ((stepper-control-lock *stepper*)) (stepper-control-open *stepper*))
      "already"
    (if (every (lambda (x) (with-model-eval x (null (car (no-output (sgp :v)))))) (mp-models))
        "none"
      t)))

(add-act-r-command "stepper-status-check" 'check-model-output-for-stepper "Internal command for communicating with the Environment stepper.  Do not call.")

(defun stepper-condition (type val)
  (when (stringp type)
    (let ((real-val (if (stringp val) (read-from-string val nil :stepper-string-read-error) val)))
      (if (eq real-val :stepper-string-read-error)
          (if (zerop (length val))
              (return-from stepper-condition "Run Until entry box does not contain a value.")
            (return-from stepper-condition "Run Until entry box does not contain a useable value."))

        (case (read-from-string type nil :stepper-string-read-error)
          (production 
           (cond ((or (not (symbolp real-val))
                      (notany (lambda (model) 
                                (with-model-eval model 
                                  (find real-val (all-productions))))
                              (mp-models)))
                  (return-from stepper-condition "Run Until Production requires a valid production name."))
                 (t 
                  (setf type 'production val real-val))))
          (time 
           (cond ((not (numberp real-val))
                  (return-from stepper-condition "Run Until Time requires a number in seconds."))
                 ((>= (mp-time) real-val)
                  (return-from stepper-condition "Run Until Time attempted with a time that has already occurred."))
                 (t 
                  (setf type 'time val (seconds->ms real-val)))))
          (module 
           (cond ((not (find val (mapcar 'symbol-name (all-module-names)) :test #'string-equal))
                  (return-from stepper-condition "Run Until Module requires a valid module name."))
                 (t 
                  (setf type 'module val val))))
          (t 
           (print-warning "Unexpected value passed to stepper-condition function.")
           (setf type nil val nil))))))
  
  (bt:with-lock-held ((stepper-control-lock *stepper*))
    (setf (stepper-control-skip-type *stepper*) type)
    (setf (stepper-control-skip-val *stepper*) val))
  1)

(add-act-r-command "stepper-condition" 'stepper-condition "Internal command for communicating with the Environment stepper. Do not call.")


(defun model-trace-enabled (event)
  (with-model-eval (if (evt-model event) (evt-model event) (first (mp-models))) ;; just use the first if there isn't one (a break event)
    (car (no-output (sgp :v)))))


(defun stepper-pre-hook (event)
  (let (type val)
    (bt:with-lock-held ((stepper-control-lock *stepper*))
      (setf type (stepper-control-skip-type *stepper*)
        val (stepper-control-skip-val *stepper*)))
    
    (when (or (and type ;; running until a specific action
                   (event-displayed-p event)
                   (model-trace-enabled event)
                   (case type
                     (production
                      (and (or (eq (evt-action event) 'production-selected)
                               (eq (evt-action event) 'production-fired))
                           (eq val (production-name (car (evt-params event))))))
                     (time
                      (>= (evt-mstime event) val))
                     (module
                      (string-equal val (symbol-name (evt-module event))))))
              (and (null type)
                   (or 
                    (and (event-displayed-p event)
                         (model-trace-enabled event))
                    (and (stepper-control-tutoring *stepper*)
                         (eq (evt-action event) 'production-selected)
                         ))))
      (prog1
          (dispatch-apply "wait_for_stepper" (format-event event))
      
        (bt:with-lock-held ((stepper-control-lock *stepper*))
          (setf (stepper-control-current-event *stepper*) event))))))


(defun stepper-post-hook (event)
  (when (eq event (bt:with-lock-held ((stepper-control-lock *stepper*)) (stepper-control-current-event *stepper*)))
    (let ((details (cond ((or (eq (evt-action event) 'production-selected)
                              (eq (evt-action event) 'production-fired))
                          (list (with-model-eval (evt-model event)
                                  (if (eq (evt-action event) 'production-fired) 
                                      (car (stepper-module-cs (get-module stepper))) 
                                    (stepper-module-cs (get-module stepper))))
                                (if (eq (evt-action event) 'production-selected) 1 0)
                                "Possible Productions" "Production Parameters" "Bindings" "Production"))
                         ((or (eq (evt-action event) 'retrieved-chunk)
                              (eq (evt-action event) 'retrieval-failure))
                          (list (with-model-eval (evt-model event)
                                  (stepper-module-rs (get-module stepper)))
                                0
                                "Possible Chunks" "Chunk Parameters" "Retrieval Request" "Chunk"))
                         (t
                          (list nil 0 "" "" "" "")))))
      (dispatch-apply-list "display_stepper_stepped" (cons (format-event event) details)))))

(defun update-stepper-info (item)
  (if item
      (let ((event (bt:with-lock-held ((stepper-control-lock *stepper*)) (stepper-control-current-event *stepper*))))
        (cond ((or (eq (evt-action event) 'production-selected)
                   (eq (evt-action event) 'production-fired))
               (with-model-eval (evt-model event)
                 (let ((p (string->name item)))
                   (values
                    (if p (capture-command-output (spp-fct (list p))) "")
                                   
                    (let* ((prod (get-production p))
                           (bindings (when prod
                                       (bt:with-recursive-lock-held ((production-lock prod)) 
                                         (mapcar (lambda (x)
                                                   (list (car x) (cdr x) (and (find (car x) (production-lhs-buffers prod) :key 'buffer-name->variable) t)))
                                           (production-bindings prod))))))
                      (mapcar (lambda (x) (list (first x) (if (stringp (second x))
                                                              (format nil "~s" (second x))
                                                            (second x))
                                                (third x))) bindings))
                    ""
                    (if p (printed-production-text p (eq (evt-action event) 'production-fired)) "")
                    (if p (printed-production-text p) "")))))
              
              ((or (eq (evt-action event) 'retrieved-chunk)
                   (eq (evt-action event) 'retrieval-failure))
               
               (with-model-eval (evt-model event)
                 (let ((s (get-module stepper))
                       (c (string->name item)))
                   (values 
                    (if c (with-parameters (:ans nil) (capture-command-output (sdp-fct (list c)))) "")
                    ""
                    (if (stepper-module-rr s) (format nil "+retrieval>~%~a" (stepper-module-rr s)) "")
                    (if c (printed-chunk c) "")
                    ""))))
              (t
               (values "" "" "" "" ""))))
    (values "" "" "" "" "")))

(add-act-r-command "update-stepper-info" 'update-stepper-info "Internal command for communicating with Environment stepper. Do not call.")


(defun install-stepper-hooks ()
  (if (bt:with-lock-held ((stepper-control-lock *stepper*)) (stepper-control-open *stepper*))
      (print-warning "Second stepper was opened but the Environment controls only support a single Stepper at a time.")
    (progn
      (bt:with-lock-held ((stepper-control-lock *stepper*))
        (setf (stepper-control-open *stepper*) t)
        (setf (stepper-control-pre-hook *stepper*)
          (add-pre-event-hook 'stepper-pre-hook))
        (setf (stepper-control-post-hook *stepper*)
          (add-post-event-hook 'stepper-post-hook))
        )
      (dolist (m (mp-models))
        (with-model-eval m
          (let ((s (get-module stepper)))
            (reset-stepper-module s))))
      t)))

(defun stepper-rr-hook (request)
  (setf (stepper-module-rr (get-module stepper)) (printed-chunk-spec request))
  nil)

(defun stepper-rs-hook (set)
  (setf (stepper-module-rs (get-module stepper)) (copy-list set))
  nil)
  
(defun stepper-cs-hook (set)
  (setf (stepper-module-cs (get-module stepper)) (copy-list set))
  nil)
                        
(defun uninstall-stepper-hooks ()
  (bt:with-lock-held ((stepper-control-lock *stepper*))
    (setf (stepper-control-open *stepper*) nil)
    (setf (stepper-control-current-event *stepper*) nil)
    (when (stepper-control-pre-hook *stepper*)
      (delete-event-hook (stepper-control-pre-hook *stepper*))
      (setf (stepper-control-pre-hook *stepper*) nil))
    (when (stepper-control-post-hook *stepper*)
      (delete-event-hook (stepper-control-post-hook *stepper*))
      (setf (stepper-control-post-hook *stepper*) nil)))
        
  (dolist (m (mp-models)) ;; have to put back other hook functions...
    (with-model-eval m
      (no-output
       (when (find 'stepper-rr-hook (car (sgp :retrieval-request-hook)))
         (dolist (x (reverse (remove 'stepper-rr-hook (car (sgp :retrieval-request-hook)))))
           (sgp-fct (list :retrieval-request-hook x))))
       (when (find 'stepper-rs-hook (car (sgp :retrieval-set-hook)))
         (dolist (x (reverse (remove 'stepper-rs-hook (car (sgp :retrieval-set-hook)))))
           (sgp-fct (list :retrieval-set-hook x))))
       (when (find 'stepper-cs-hook (car (sgp :conflict-set-hook)))
         (dolist (x (reverse (remove 'stepper-cs-hook (car (sgp :conflict-set-hook)))))
           (sgp-fct (list :conflict-set-hook x))))))))


(defun turn-on-stepper-tutoring ()
  (setf (stepper-control-tutoring *stepper*) t))

(defun turn-off-stepper-tutoring ()
  (setf (stepper-control-tutoring *stepper*) nil))

(add-act-r-command "turn-on-stepper-tutoring" 'turn-on-stepper-tutoring "Internal command for communicating with the Environment stepper. Do not call.")
(add-act-r-command "turn-off-stepper-tutoring" 'turn-off-stepper-tutoring "Internal command for communicating with the Environment stepper. Do not call.")

(add-act-r-command "install-stepper-hooks" 'install-stepper-hooks "Internal command for communicating with the Environment stepper.  Do not call.")
(add-act-r-command "uninstall-stepper-hooks" 'uninstall-stepper-hooks "Internal command for communicating with the Environment stepper.  Do not call.")


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
