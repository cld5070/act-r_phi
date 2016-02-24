;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2009 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : fatigue.lisp
;;; Version     : 0.7
;;; 
;;; Description : Dampens activations based on a fatigue procedural (fp) parameter.
;;;               The rate of decay is governed by fp-dec, and a stimulus parameter
;;;               restores fp to a percentage of its original state.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;; 2009.01.23 Dan
;;;             : * Initial conversion of the run-for-x function into something
;;;             :   that works off of the conflict-set-hook and is encapsulated
;;;             :   in a module.
;;; 2009.02.17 Rick Moore
;;;             : * Added :stimulus parameter.
;;; 2009.02.27 Dan
;;;             : * Modified things so that it checks the delayed-resolution
;;;             :   status of the procedural before deciding whether or not to
;;;             :   actually decrement.
;;;             : * Also added code to deal with the fact that when the threaded
;;;             :   cognition code is also active there can be multiple calls to
;;;             :   the conflict-set-hook "simultaneously" and it's only really
;;;             :   the last one that matters.
;;; 2009.03.09 Rick Moore
;;;             : * Adeded :ut as an observed parameter to avoid (sgp) calls.
;;; 2009.03.17 Rick Moore
;;;             : * Only compare ut if ut is not nil.
;;; 2009.08.19 Tim Halverson
;;;             : * Added fd and fd-dec parameters, along with supporting code,
;;;                 to allow the fatigue module to affect retrievals.
;;;             : * Added initializers to original-fp and original-fd in the
;;;                 defstruct of fatigue-module. This is required because
;;;                 the :stimuli parameter requires these values to be non-nil
;;;                 and the order in which the parameters get initialized when 
;;;                 the module is instantiated is not defined. Therefore, 
;;;                 depending on the implementation of hash tables in the version
;;;                 of lisp used, original-fp and/or original-fd could be nil
;;;                 when :stimuli is initialized.
;;; 2011.04.19 Rick Moore
;;;             : * Added parameters for utbmc, utmc, utc, fpbmc, fpmc, fpc, and
;;;                 hour Also added calculations for within-task
;;;                 fatigue effects.
;;; 2011.05.13 Rick Moore
;;;             : * Removed original-fp, added fp-percent, and changed fp-dec
;;;                 so it now decrements fp-percent.  Stimulus is simply
;;;                 assigned to fp-percent.
;;; 2011.11.20 Clayton Stanley
;;;             : * Added ability to define a biomath-class, and use it in the fatigue module
;;;		              An implementation for the McCauley 2009 biomath model is included here
;;;                 And set as the default biomath-class to use
;;; 2012.08.05 Rick Moore
;;;             : * Linked fd to biomathematical model of fatigue
;;;             : * Individual mechanisms can now be turned on or off with fp = nil 
;;;                 or not nil, and fd = nil or not nil.
;;;             : * Setting fp or fd does not do anything any more, but they can be 
;;;                 read to find the value that the module last used
;;;             : * Resetting the module causes its own defaults to be used-- not sure 
;;;                 why-- but aligned those with correct defaults.
;;;             : * Set default value for fp-dec.
;;;             : * Fixed more default settings
;;;             : * Updated declarative mechanisms to use biomathematical model
;;;                 calculations
;;;             : * Added simple optimization (a cache) to biomath calculations
;;; 2012.09.27 Rick Moore
;;;             : * Added run-with-microlapses function and removed 
;;;                 conflict-set-hook
;;; 2012.11.06 Rick Moore
;;;             : * Fixed a bug where rounding issues could cause an infinite loop
;;;                 in run-with-microlapses
;;; 2012.11.14 Bella Veksler
;;;             : * Added calculations for initial values of pn and un based 
;;;                 on :tib (time in bed) and :tn (time awake).
;;; 2012.11.19 Bella Veksler
;;;             : * Added set-sleep-history function which modifies pn,un,qn, and vn
;;;                 based on a sleep history of the form '((w1 s1)(w2 s2)...) such that
;;;                 wn is the time awake during day n and sn is time asleep that night
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf *read-default-float-format* 'double-float)

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(defstruct fatigue-module 
  enabled            ; Turns fatigue module off / on
  (fp 1.0)           ; "fatigue parameter" value scales utility calculations
  (fp-percent 1.0)   ; The current percentage of fp after microlapses
  fp-dec             ; Decrease in fp after each microlapse
  ut                 ; Utility threshold (from utilty module)
  dat                ; Dat from whatever module
  stimulus           ; Signals a reset to fp based on specified scalar.  fp <- stimulus.  Resets decrements
  pending 
  last-one-empty 
  (fd 1.0) 
  (fd-percent 1.0)   ; The current percentage of fp after microlapses
  fd-dec 
  utbmc              ; Coefficient relating biomath value to ut
  utmc               ; Coefficient relating minute within session to ut
  utc                ; Constant ut offset
  fpbmc              ; Coefficient relating biomath value to fp
  fpmc               ; Coefficient relating minute within session to fp
  fpc                ; Constant fp offset
  fdbmc              ; Coefficient relating biomath value to fd
  fdc                ; Constant fd offset
  hour               ; Initiates a new session by providing the number of hours since 8:00AM. 
  start-time         ; Records the (mp-time) at the start of a session, so proper within-session offsets can be calculated
  )


; These are used to optimized biomathematical model calculations.  The previously calculated value
; is stored in *last-biomathematical-value* along with the corresponding hour in *last-biomathematical-hour*.
; If the next request uses the same hour, then the previously calculated result is returned.  This optimization
; was necessary for the declarative mechanism because it is called so often.
(defvar *last-biomathematical-hour* nil)
(defvar *last-biomathematical-value* nil)



#|



Biomathematal model section. 




In order to use a biomath model within the fatigue module, a CLOS class is created for the biomath model. 
All slots in that class should be parameters in the biomath model. 

Documentation for the slots in that class will be used as documentation for these additional parameters in the fatigue module. 

All slots for the class will be exposed to the modeler, and he/she can modify them away from defaults, using the standard sgp interface

A single method must be defined for the class, compute-biomath-value, that is given the time (in hours), and returns the human biomathematical performance

Set the dynamic variable *biomath-class* to the name of the class being used (as a symbol) in order to load this particular class for the fatigue module.
|#

;Dynamic variable that stores the symbol for the biomath class being used
;If you want to use another class, you can either change this variable here in the code (after defining the new class in the code here)
;Or, you can define the class in another file that gets loaded, then change the value of this variable, then unload and load the fatigue module
(defvar *biomath-class* 'biomath-model-mccauley-simplified)

;Stores current instantiation of the chosen boimath class. You should not need to manually set/clear this
(defvar *biomath-class-obj* nil)

(defmacro defclass-default (class-name superclasses slots &rest class-options)
  "Shorthand defclass syntax; structure similar to defclass
  Pass three values: slot-name, :initform, and :documentation
  Everything else gets filled in to standard defaults"
  `(defclass 
     ,class-name 
     ,superclasses 
     ,(mapcar (lambda (x) `( ,(first x)
                             :accessor ,(first x)
                             :initarg ,(intern (symbol-name (first x)) "KEYWORD")
                             :initform ,(second x)
                             :documentation ,(third x)))
              slots)
     ,@class-options))

(defclass-default biomath-model-mccauley-simplified ()
                  ((alpha11 	-0.0387		"alpha11 doc")
                   (alpha12 	0.0356	 	"alpha12 doc")
                   (alpha22 	0.0139	 	"alpha22 doc")
                   (kappa1 	0.876	 	"kappa1 doc")
                   (mu1 	0.11	 	"mu1 doc")
                   (phi 	20.96	 	"phi doc")
                   (sigma11 	-0.232 		"sigma11 doc")
                   (sigma12 	0.0695	 	"sigma12 doc")
                   (sigma22 	-0.0774		"sigma22 doc")
                   (kappa2 	0.325	 	"kappa2 doc")
                   (mu2 	0.00211		"mu2 doc")
                   (delta 	7.86 		"delta doc")
                   (tau 	24 		"tau doc")
                   (pn		0		"pnn doc") ;1.6418  BZV set to 0 initially, but can vary tn and tib in order to get the right values
                   (un		0		"un doc") ;9.6307  BZV set to 0 initially, gets reset when tn and tib are changed
                   (tn		8		"tn doc")
                   (tib		10		"tib doc")  ;BZV added, baseline duration of sleep (8-10 hours typically)
                   (qn		0		"vn doc")
                   (vn		0		"qn doc")
                   (wn		0		"wn doc")
                   (awake-p	t		"awake-p doc")
                   ))
 
(defmacro with-functions (functions the-package &body body)
  "Allows functions in the-package to be visible only for body.
  Does this by creating local lexical function bindings that redirect calls
  to functions defined in the-package"
  `(labels
     ,(mapcar (lambda (x) `(,x (&rest args)
                               (apply (find-symbol ,(format nil "~:@(~a~)" x) 
                                                   ,the-package)
                                      args)))
              functions)
     ,@body))

(let ((mop-package-name
        #+allegro   'mop
        #+clisp     'clos
        #+clozure   'ccl
        #+cmu       'clos-mop
        #+ecl       'clos
        #+lispworks 'clos
        #+mcl       'ccl
        #+sbcl      'sb-mop
        #+scl       'clos))
  (with-functions (class-direct-slots slot-definition-name slot-definition-initform) mop-package-name
    (defun class-slots-to-params-list (class-name-as-symbol)
      "Translates slots defined in a CLOS object to params-list entries for an ACT-R module"
      (let* ((slot-objects (class-direct-slots (find-class class-name-as-symbol)))
             (slots (mapcar #'slot-definition-name slot-objects))
             (docs (mapcar (lambda (slot-object) (documentation slot-object t)) slot-objects))
             (initforms (mapcar #'slot-definition-initform slot-objects)))
        (mapcar (lambda (slot doc initform) 
                  ;Assuming only two types of values in a slot; either a number or a list
                  (let ((valid-test 
                          (cond ((listp initform) 'listp)
                                ((numberp initform) 'numberp)
                                (t 'symbolp))))
                    (define-parameter 
                      (intern (symbol-name slot) "KEYWORD")
                      :valid-test valid-test
                      :default-value initform
                      :warning (format nil "calling '~(~a~) on value must return true" valid-test)
                      :documentation doc
                      )))
                slots docs initforms)))))

(defmethod get-omega1 ((obj biomath-model-mccauley-simplified) hour)
  (with-slots (tau alpha11 alpha22 tn kappa1 phi mu1) obj
    (let* ((2pi/tau (/ (* 2 pi) tau))
           (exp11 (exp (* alpha11 (- hour tn))))
           (denom (+ (* 4 pi pi)
                     (* alpha11 alpha11 tau tau))))
      (+ 
        (/   
          (* kappa1 2 pi tau
             (- (* exp11
                   (cos (* 2pi/tau (- tn phi))))
                (cos (* 2pi/tau (- hour phi)))))
          denom)
        (/
          (* kappa1 alpha11 tau tau
             (- (* exp11
                   (sin (* 2pi/tau (- tn phi))))
                (sin (* 2pi/tau (- hour phi)))))
          denom)
        (/
          (* mu1 (- exp11 1))
          alpha11)))))

;BZV fixed bug
(defmethod get-theta1 ((obj biomath-model-mccauley-simplified) hour)
  (with-slots (tau sigma11 sigma22 tn kappa2 phi mu2 wn) obj
    (let* ((2pi/tau (/ (* 2 pi) tau))
           (exp11 (exp (* sigma11 (- hour (+ tn wn)))))
           (denom (+ (* 4 pi pi)
                     (* sigma11 sigma11 tau tau))))
      (+ 
        (/   
          (* kappa2 2 pi tau
             (- (* exp11
                   (cos (* 2pi/tau (- (+ tn wn) phi))))
                (cos (* 2pi/tau (- hour phi)))))
          denom)
        (/
          (* kappa2 sigma11 tau tau
             (- (* exp11
                   (sin (* 2pi/tau (- (+ tn wn) phi))))
                (sin (* 2pi/tau (- hour phi)))))
          denom)
        (/
          (* mu2 (- exp11 1))
          sigma11)))))

(defmethod get-omega2 ((obj biomath-model-mccauley-simplified) hour)
  (declare (ignorable hour))
  0)

(defmethod get-theta2 ((obj biomath-model-mccauley-simplified) hour)
  (declare (ignorable hour))
  0)

(defmethod get-psi11 ((obj biomath-model-mccauley-simplified) hour)
  (with-slots (alpha11 tn) obj
    (exp (* alpha11 (- hour tn)))))

(defmethod get-phi11 ((obj biomath-model-mccauley-simplified) hour)
  (with-slots (sigma11 tn wn) obj
    (exp (* sigma11 (- hour (+ tn wn))))))

(defmethod get-psi12 ((obj biomath-model-mccauley-simplified) hour)
  (with-slots (alpha12 alpha22 alpha11 tn) obj
    (/
      (* alpha12
         (- (exp (* alpha22 (- hour tn)))
            (exp (* alpha11 (- hour tn)))))
      (- alpha22 alpha11))))

(defmethod get-phi12 ((obj biomath-model-mccauley-simplified) hour)
  (with-slots (sigma12 sigma22 sigma11 tn wn) obj
    (/
      (* sigma12
         (- (exp (* sigma22 (- hour (+ tn wn))))
            (exp (* sigma11 (- hour (+ tn wn))))))
      (- sigma22 sigma11))))

(defmethod get-psi21 ((obj biomath-model-mccauley-simplified) hour)
  (declare (ignorable hour))
  0)

(defmethod get-phi21 ((obj biomath-model-mccauley-simplified) hour)
  (declare (ignorable hour))
  0)

(defmethod get-psi22 ((obj biomath-model-mccauley-simplified) hour)
  (with-slots (alpha22 tn) obj
    (exp (* alpha22 (- hour tn)))))

(defmethod get-phi22 ((obj biomath-model-mccauley-simplified) hour)
  (with-slots (sigma22 tn wn) obj
    (exp (* sigma22 (- hour (+ tn wn))))))

(defmethod get-p ((obj biomath-model-mccauley-simplified) hour)
  (with-slots (pn un) obj
    (+ (* (get-psi11 obj hour) pn)
       (* (get-psi12 obj hour) un)
       (get-omega1 obj hour))))

(defmethod get-u ((obj biomath-model-mccauley-simplified) hour)
  (with-slots (pn un) obj
    (+ (* (get-psi21 obj hour) pn)
       (* (get-psi22 obj hour) un)
       (get-omega2 obj hour))))

(defmethod get-q ((obj biomath-model-mccauley-simplified) hour)
  (with-slots (qn vn) obj
    (+ (* (get-phi11 obj hour) qn)
       (* (get-phi12 obj hour) vn)
       (get-theta1 obj hour))))

(defmethod get-v ((obj biomath-model-mccauley-simplified) hour)
  (with-slots (qn vn) obj
    (+ (* (get-phi21 obj hour) qn)
       (* (get-phi22 obj hour) vn)
       (get-theta2 obj hour))))

(defmethod compute-biomath-value ((obj biomath-model-mccauley-simplified) hour)
  "Uses the simplified closed-form version of the biomathematical model in McCauley et al. (2009)"
  (if (awake-p obj)
    (get-p obj hour)
    (get-q obj hour)))

;BZV: added equations 1.11 and 1.12 - some linear algebra to determine p0 and u0
(defmethod get-inv11 (a b c d)
  (* 1.0 (/ d (- (* a d) (* b c))))
)
(defmethod get-inv12 (a b c d)
  (* 1.0 (- (/ b (- (* a d) (* b c)))))
)
(defmethod get-inv21 (a b c d)
  (* 1.0 (- (/ c (- (* a d) (* b c)))))
)
(defmethod get-inv22 (a b c d)
  (* 1.0 (/ a (- (* a d) (* b c))))
)

;eq 1.12 and 1.11
(defmethod get-p0u0 ((obj biomath-model-mccauley-simplified) hour)
  (declare (ignorable hour))
  (with-slots (tn tau delta tib wn) obj
    (let ((oldwn wn)
          (w0 (- tau tib)))
    (setf wn w0)
    (let* ((o1 (get-omega1 obj (+ tn w0)))
           (o2 (get-omega2 obj (+ tn w0)))
           (t1 (get-theta1 obj (+ tn tau)))
           (t2 (get-theta2 obj (+ tn tau)))
           (phi11 (get-phi11 obj (+ tn tau)))
           (phi12 (get-phi12 obj (+ tn tau)))
           (phi21 (get-phi21 obj (+ tn tau)))
           (phi22 (get-phi22 obj (+ tn tau)))
           (part11 (+ (* phi11 o1)
                     (* phi12 o2) t1))
           (part12 (+ (* phi21 o1)
                     (* phi22 o2) t2))
           (part21 (+ (* 0 (- 1 phi11)) (* delta (- 0 phi12)))) 
           (part22 (+ (* 0 (- 0 phi21)) (* delta (- 1 phi22))))
           (f01 (+ part11 part21))
           (f02 (+ part12 part22))

           (psi11 (get-psi11 obj (+ tn w0)))
           (psi12 (get-psi12 obj (+ tn w0)))
           (psi21 (get-psi21 obj (+ tn w0)))
           (psi22 (get-psi22 obj (+ tn w0)))
    
           (big11 (- 1 (+ (* phi11 psi11) (* phi12 psi21))))
           (big12 (- 0 (+ (* phi11 psi12) (* phi12 psi22))))
           (big21 (- 0 (+ (* phi21 psi11) (* phi22 psi21))))
           (big22 (- 1 (+ (* phi21 psi12) (* phi22 psi22))))

           (invbig11 (get-inv11 big11 big12 big21 big22))
           (invbig12 (get-inv12 big11 big12 big21 big22))
           (invbig21 (get-inv21 big11 big12 big21 big22))
           (invbig22 (get-inv22 big11 big12 big21 big22))

           )
      (setf wn oldwn)
      (list (+ (* f01 invbig11) (* f02 invbig12)) (+ (* f01 invbig21) (* f02 invbig22))
)))))

;I had to initialize the object before registering the fatigue module
;Apparently, ACT-R tries to run sgp before calling a reset of the fatigue module.
;Inside reset is where this object gets initialized, but since it isn't called before sgp,
;I'm pretty sure we have to initialize it here - CS
(setf *biomath-class-obj* (make-instance *biomath-class*))

#|


End of biomathematical section


|#


; The "hour" argument specifies the number of hours past 8:00 am after a full nights sleep.
; For example, 2 would be 10:00am. 24 would be 8:00am the following mourning after a full
; night's sleep deprivation.
(defun compute-biomath-value-for-hour( hour )
  (if (equal hour *last-biomathematical-hour*)
    *last-biomathematical-value*
    (progn
      (setf *last-biomathematical-hour* hour)
      (setf *last-biomathematical-value* 	(compute-biomath-value 
                                                 *biomath-class-obj* 
                                                 (+ hour (tn *biomath-class-obj*))))
      )
    )
  )

(defun run-with-microlapses (max-time end-condition &key (real-time nil))
  (verify-current-mp
			"run-with-microlapses called with no current meta-process."
		(if (meta-p-running (current-mp))
				(print-warning "Recursive call to a running function not allowed.  Must wait for a 'run' to complete before 'running' again.")
				(if (not (or (functionp end-condition) (fboundp end-condition)))
						(print-warning "condition must be a function.")
						(let ((start-time (mp-time))
									(run-time max-time)
									end-run-time)
							
							;;Because ACT-R/Phi keeps events in queue, we need to ensure run-until-condition will work correctly
							;; so we schedule a periodic event that allows run-until-condition to check it's conditions every 50ms
							;; independent of the actual timing of the events in queue
							;(schedule-periodic-event 0.005 #'(lambda () t) :module 'procedural :priority :min :output nil)
									
							(loop
								 ; Try to run ACT-R
;(format t "Running for ~a~%" run-time)
								 (when (> run-time 0) 
										(setf end-run-time (+ start-time run-time))
										 ;(run-until-condition #'(lambda() (or (> (mp-time) end-run-time) (funcall end-condition))) :real-time real-time))
										 (run run-time :real-time real-time))

								 ; Calculate remaining maximum run time in seconds, rounded to milliseconds
								 (setf run-time (/ (round (*  (- max-time (- (mp-time) start-time)) 1000)) 1000))

								 ; Abort if we have less than a millisecond left
								 (if (< run-time 0.0005) (return t))
								 ; Abort if end condition is true
								 (if (funcall end-condition) (return t))							 
								 (schedule-event-relative (rand-time (car (no-output (sgp :dat)))) #'(lambda () t) :module 'procedural)
								 ; Schedule a microlapse event
								 ; -This action will trigger a conflict resolution cycle
								(schedule-event-relative (rand-time (car (no-output (sgp :dat))))
																					'decrement-fp-fd
																					:module 'procedural)))))))

(defun decrement-fp-fd ()
  (let* ((module (get-module :fatigue))
		(fp (fatigue-module-fp module)))
;(format t "Microlapse at ~f~%" (mp-time))
  (setf fp (max 0.000001 (* fp (fatigue-module-fp-percent module))))
  (setf (fatigue-module-fp module) fp)
  ;(setf (fatigue-module-fp-percent module) (max .000001 (- (fatigue-module-fp-percent module) (fatigue-module-fp-dec module))))
  (setf (fatigue-module-fd-percent module) (max .000001 (- (fatigue-module-fd-percent module) (fatigue-module-fd-dec module)))))
  )

(defun fatigue-utility-hook ( production )
  (let* ((module (get-module :fatigue))
        (fp (fatigue-module-fp module))
        (ut)
        (minutes-passed)
        (biomath-prediction))

    (if (and (fatigue-module-enabled module) (fatigue-module-fp module))
      (let ((alert-utility)
            (egs (car (no-output (sgp :egs))))
			tempUtil
			tempUtilNoise)
        (sgp-fct (list :utility-hook nil :egs 0))
        (setf alert-utility (compute-utility production))
        (sgp-fct (list :utility-hook 'fatigue-utility-hook :egs egs))

        ; Calculate the biomathematical model prediction
        (setf biomath-prediction (compute-biomath-value-for-hour (fatigue-module-hour module)))
		;(print biomath-prediction)

        ; Calculate the number of minutes that have passed
        (setf minutes-passed (/ (- (mp-time) (fatigue-module-start-time module)) 60))

        ; Calculate ut
        (setf ut (* (fatigue-module-utc module)
				  (+ (* biomath-prediction (fatigue-module-utbmc module)) 
#|(if (zerop minutes-passed) 0 (expt minutes-passed (fatigue-module-utmc module)))|#    ;BZV modified time-on-task function
                    1)))
        (sgp-fct (list :ut ut))
		;(print ut)

        ; Calculate the value for fp
		;-CLD2 Only calculate fp here if it's the first time being changed
		; (otherwise use microlapses to change fp)
        (when (or (not fp) (= fp 1))
;			(print "CHANGING IT")
			(setf fp (+ (* biomath-prediction (fatigue-module-fpbmc module)) 
#|(if (zerop minutes-passed) 0 (expt minutes-passed (fatigue-module-fpmc module)))|#      ;BZV modified time-on-task function
                    1))
			(setf (fatigue-module-fp module) fp))
		(setf tempUtilNoise (act-r-noise egs))
        (setf tempUtil (+ (* alert-utility fp) (if (zerop egs) 0.0 tempUtilNoise)))
;		(format t "fp-pct: ~a time: ~a pn: ~a ut ~a fp ~a alert-utility ~a min: ~a noise ~a ALSO ~a ~%" (fatigue-module-fp-percent module) (mp-time) biomath-prediction ut fp alert-utility minutes-passed tempUtilNoise tempUtil)
		tempUtil)
      nil))
	)



(defun fatigue-bla-hook ( chunk )
  (let ((module (get-module :fatigue))
				(fd)
				(biomath-prediction))
;		(format t "*** Calling bla utility ***~%")
    (if (and (fatigue-module-enabled module)  (fatigue-module-fd module))
				(let ((alert-bla))
					;(format t "*** Adjusting bla ***~%")
					(sgp-fct (list :bl-hook nil))
					(setf alert-bla (base-level-activation (get-module declarative) chunk))
					(sgp-fct (list :bl-hook 'fatigue-bla-hook))

					; Calculate the biomathematical model prediction
					(setf biomath-prediction (compute-biomath-value-for-hour (fatigue-module-hour module)))
;(format t "*** biomath: ~f~%" biomath-prediction )

					; Calculate FD from fatigue module.  This is a rough approxomation of the values fit with the SAST model because
					; SAST was fit using a different biomathematical model
					(setf fd (+ (* biomath-prediction (fatigue-module-fdbmc module)) (fatigue-module-fdc module)))
;(format t "*** fdbmc: ~f fdc: ~f~%"  (fatigue-module-fdbmc module) (fatigue-module-fdc module))
;(format t "*** fd: ~f~%" fd )

					; Add in microlapses
					(setf fd (* fd (fatigue-module-fd-percent module)))
					(setf (fatigue-module-fd module) fd)
;(format t "*** fd: ~f~%" fd )

					(* alert-bla fd))
				nil)))



(defun reset-fatigue-module (module)
  (setf (fatigue-module-pending module) nil)
  (setf (fatigue-module-last-one-empty module) nil)
  (setf (fatigue-module-fp-percent module) 1)
  (setf (fatigue-module-fp module) 1)
  (setf (fatigue-module-fp-dec module) 0.01440861)
  (setf (fatigue-module-fd-percent module) 1)
  (setf (fatigue-module-fd module) nil) 
  (setf (fatigue-module-stimulus module) 1)
  (setf (fatigue-module-fpbmc module) -0.0120716873738272)
  (setf (fatigue-module-fpmc module) -0.00273707862262172)
  (setf (fatigue-module-fpc module) 1.00170859583602)
  (setf (fatigue-module-utbmc module) -0.00991274671415563)
  (setf (fatigue-module-utmc module)  0.00453807249538224)
  (setf (fatigue-module-utc module) 2.0643395401332)
  (setf (fatigue-module-fdbmc module) -0.02681)
  (setf (fatigue-module-fdc module) 0.95743)
  (setf (fatigue-module-hour module) 0)
  (setf (fatigue-module-start-time module) 0)
  (setf (fatigue-module-ut module) (car (no-output (sgp :ut))))
  (setf (fatigue-module-dat module) (car (no-output (sgp :dat))))
  (setf *biomath-class-obj* (make-instance *biomath-class*))
  (reset-p0u0)
  (schedule-event-relative 0.001 'schedule-fatigue-hooks :module 'fatigue :priority :max :details "Schedule Fatigue hooks")
  ;(sgp-fct (list :utility-hook 'fatigue-utility-hook))
  ;(sgp-fct (list :bl-hook 'fatigue-bla-hook)) 
  )
  
;-CLD- Using this so we can either use these fatigue hooks or ACT-R/Phi mechanisms
  (defun schedule-fatigue-hooks ()
	(when (fatigue-module-enabled (get-module :fatigue))
		(print "ENABLED!!!")
		(sgp-fct (list :utility-hook 'fatigue-utility-hook))
		(sgp-fct (list :bl-hook 'fatigue-bla-hook))))

;BZV setting initial conditions for p0 and u0
(defun reset-p0u0()
 (let ((init (get-p0u0 *biomath-class-obj* 0)))
    (setf (pn *biomath-class-obj*) (first init))
    (setf (un *biomath-class-obj*) (second init))
    )
nil)

;BZV takes in tuples of wake/sleep amount per day 
;i.e. '((16 8)(10 8)(10 6)) signifies on first day person was up 16 hours, and slept 8 that night, 
;on second day was up 10 hours and slept 8 the following night, etc.
;assumes :tn and :tib have already been set
;subsequent call to (compute-biomath-value-for-hour) will produce predicted value as per sleep history
;leaves the model in a state of wakefulness, and no need to reset :tn as this function takes care of it
(defun set-sleep-history(history)
  (sgp :qn 0)
  (sgp :vn 0)
  (sgp :wn 0)
  (eval `(sgp :tib ,(second (car history))))
  (reset-p0u0)
  (dolist (h (cdr history))
    (compute-biomath-value-for-hour (first h))
    (sgp :awake-p nil)
    (compute-biomath-value-for-hour (+ (first h) (second h)))
    (sgp :awake-p t))
  )

(defun fatigue-module-params (module param)
  (if (consp param)
    (case (car param)
      ; Set parameter
      (:enable-fatigue (setf (fatigue-module-enabled module) (cdr param)))
      (:stimulus (setf (fatigue-module-stimulus module) (cdr param))
								 (setf (fatigue-module-fp-percent module) (fatigue-module-stimulus module)))
      (:fp-dec (setf (fatigue-module-fp-dec module) (cdr param)))
      (:fp (setf (fatigue-module-fp module) (cdr param)))
      (:fd-dec (setf (fatigue-module-fd-dec module) (cdr param)))
      (:fd (setf (fatigue-module-fd module) (cdr param))) 
      (:ut (setf (fatigue-module-ut module) (cdr param)))
      (:dat (setf (fatigue-module-dat module) (cdr param)))
      (:fpbmc (setf (fatigue-module-fpbmc module) (cdr param)))
      (:fpmc (setf (fatigue-module-fpmc module) (cdr param)))
      (:fpc (setf (fatigue-module-fpc module) (cdr param)))
      (:fdbmc (setf (fatigue-module-fdbmc module) (cdr param)))
      (:fdc (setf (fatigue-module-fdc module) (cdr param)))
      (:utbmc (setf (fatigue-module-utbmc module) (cdr param)))
      (:utmc (setf (fatigue-module-utmc module) (cdr param)))
      (:utc (setf (fatigue-module-utc module) (cdr param)))
(:fp-percent (setf (fatigue-module-fp-percent module) (cdr param)))
(:fd-percent (setf (fatigue-module-fd-percent module) (cdr param)))
      (:hour (setf (fatigue-module-hour module) (cdr param))
       (setf (fatigue-module-start-time module) (mp-time)))
      (:tib (setf (tib *biomath-class-obj*) (cdr param)) (reset-p0u0))
      (:tn (setf (tn *biomath-class-obj*) (cdr param)) (reset-p0u0))
      (:awake-p (setf (awake-p *biomath-class-obj*) (cdr param)) 
                (if (cdr param) (if *last-biomathematical-hour* (progn (setf (pn *biomath-class-obj*) (get-q *biomath-class-obj* (+ (tn *biomath-class-obj*) *last-biomathematical-hour*))) 
                                  (setf (un *biomath-class-obj*) (+ (get-v *biomath-class-obj* (+ (tn *biomath-class-obj*) *last-biomathematical-hour*)) (delta *biomath-class-obj*)))
                                  (setf (tn *biomath-class-obj*) (mod (+ (tn *biomath-class-obj*) (wn *biomath-class-obj*) (- *last-biomathematical-hour* (wn *biomath-class-obj*))) (tau *biomath-class-obj*)))
                                  (setf (tib *biomath-class-obj*) (- *last-biomathematical-hour* (wn *biomath-class-obj*)))
                                  (setf (wn *biomath-class-obj*) 0))
                                  (reset-p0u0))
                  (progn (setf (qn *biomath-class-obj*) (get-p *biomath-class-obj* (+ (tn *biomath-class-obj*) *last-biomathematical-hour*))) 
                    (setf (vn *biomath-class-obj*) (- (get-u *biomath-class-obj* (+ (tn *biomath-class-obj*) *last-biomathematical-hour*)) (delta *biomath-class-obj*)))
                    (setf (wn *biomath-class-obj*) *last-biomathematical-hour*))))
      ;Any keyword that is not in the list above, is assumed to be a slot-name on the biomath class
      ;If this is not the case, then lisp will throw an exception here
      (otherwise (setf (slot-value *biomath-class-obj*
                                   (intern (symbol-name (car param))))
                       (cdr param))))
    (case param
      ; Get parameter
      (:enable-fatigue (fatigue-module-enabled module))
      (:fp-dec (fatigue-module-fp-dec module))
      (:fd-dec (fatigue-module-fd-dec module)) ;; TEH
      (:stimulus (fatigue-module-stimulus module))
      (:fpbmc (fatigue-module-fpbmc module))
      (:fpmc (fatigue-module-fpmc module))
      (:fpc (fatigue-module-fpc module))
      (:utbmc (fatigue-module-utbmc module))
      (:utmc (fatigue-module-utmc module))
      (:utc (fatigue-module-utc module))
      (:fdbmc (fatigue-module-fdbmc module))
      (:fdc (fatigue-module-fdc module))
      (:hour (fatigue-module-hour module))
      (:fp (fatigue-module-fp module))
      (:fd (fatigue-module-fd module))  
(:fp-percent (fatigue-module-fp-percent module))
(:fd-percent (fatigue-module-fd-percent module))
  
      ;Any keyword that is not in the list above, is assumed to be a slot-name on the biomath class
      ;If this is not the case, then lisp will throw an exception here
      (otherwise (slot-value *biomath-class-obj*
                             (intern (symbol-name param)))))))


(define-module-fct :fatigue 
                   nil
                   (append
                     (list (define-parameter :enable-fatigue :valid-test 'tornil :default-value 'nil :warning "T or NIL"  :documentation "Enable the fatigue mechanism")
                           (define-parameter :fp-dec :valid-test 'numberp :default-value 0.01440861 :warning "a number" :documentation "Procedural arousal decrement")
                           (define-parameter :fp :valid-test 'numornil :default-value 1.0 :warning "a number or nil to disable procedural mechanism" :documentation "Current procedural arousal value")
                           (define-parameter :fd-dec :valid-test 'numberp :default-value 0.0 :warning "a number" :documentation "Declarative arousal decrement")
                           (define-parameter :fd :valid-test 'numornil :default-value nil :warning "a number or nil to disable declarative mechansim" :documentation "Current delcarative arousal value") 
                           (define-parameter :stimulus :valid-test 'numberp :default-value 1.0 :warning "a number" :documentation "Most recent stimulus (0-1)")
                           (define-parameter :fpbmc :valid-test 'numberp :default-value -0.0120716873738272 :warning "a number" :documentation "Coefficient relating fp to biomathematical model")
                           (define-parameter :fpmc :valid-test 'numberp :default-value -0.00273707862262172 :warning "a number" :documentation "Coefficient relating fp to minutes on task")
                           (define-parameter :fpc :valid-test 'numberp :default-value 1.00170859583602 :warning "a number" :documentation "Coefficient relating fp to a constant offset")
                           (define-parameter :utbmc :valid-test 'numberp :default-value -0.00991274671415563 :warning "a number" :documentation "Coefficient relating fp to biomathematical model")
                           (define-parameter :utmc :valid-test 'numberp :default-value 0.00453807249538224 :warning "a number" :documentation "Coefficient relating fp to minutes on task")
                           (define-parameter :utc :valid-test 'numberp :default-value 2.0643395401332 :warning "a number" :documentation "Coefficient relating fp to a constant offset")
                           (define-parameter :fdbmc :valid-test 'numberp :default-value -0.02681 :warning "a number" :documentation "Coefficient relating fd to biomathematical model")
                           (define-parameter :fdc :valid-test 'numberp :default-value 0.95743 :warning "a number" :documentation "Coefficient relating fd to a constant offset")
                           (define-parameter :hour :valid-test 'numberp :default-value 0 :warning "a number" :documentation "The number of hours since 8:00AM")
(define-parameter :fp-percent :valid-test 'numberp :default-value 1 :warning "a number" :documentation "FP-Percent")  ;BZV
(define-parameter :fd-percent :valid-test 'numberp :default-value 1 :warning "a number" :documentation "FD-Percent")   ;BZV
                           (define-parameter :ut :owner nil)
                           (define-parameter :dat :owner nil))
                     (class-slots-to-params-list *biomath-class*))
                   :creation (lambda (x) (declare (ignore x)) (make-fatigue-module))
                   :reset (list nil 'reset-fatigue-module)
                   :params 'fatigue-module-params
                   :version "0.7"
                   :documentation "First pass at a module implementation of the fatigue mechanisms, with declarative fatigue added")

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