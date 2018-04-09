;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Model and Batch Run Code for Psychomotor Vigilance Task (PVT)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Author			: Glenn Gunzelmann
;;; Copyright	 : (c)2004 AFRL/HEAS, All Rights Reserved
;;; Availability: tbd
;;; Address		 : AFRL/HEAS
;;;						 : 6030 South Kent St.
;;;						 : Mesa, AZ	85212-6061
;;;						 : glenn.gunzelmann@mesa.afmc.af.mil
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; History
;;;
;;;	2004-07-21 - Glenn Gunzelmann
;;;						 : First version.
;;;						 : Runs PVT model, apparently across variations in any act-r
;;;							 or rpm parameters where real-numbered values are used
;;;
;;;	2004-07-31 - Kevin Gluck
;;;						 : There is a bug preventing variation in the :dat parameter from having the intended effect
;;;						 : The randomize-time parameter is having the opposite effect of that predicted by the documentation
;;;
;;;						 : Dan Bothell provided fix for the randomize-time parameter
;;;						 : Mike Byrne subsequently provided a new master-process file and a new rpm-parameters file
;;;
;;;						 : Dan recommends setting up a list of parameters and having ACT-R read off that list
;;;						 : Important thing is to set :dat after reset and before defining the productions
;;;							 -(i.e., in an sgp after clear-all)
;;;
;;;	2004-08-25 - Glenn Gunzelmann
;;;						 : Added additional keywords to pvt-batch function to make the
;;;							 the system more flexible. The following keywords were added:
;;;							 -:lapse-threshold - rts greater than this are considered lapses
;;;							 -:session-length - number of seconds to run each pvt session
;;;							 -:interval - reporting interval for rts in batch run file
;;;							 -:ind-data - flag; if t, individual data files are created for
;;;								each run of the model, with complete rt & delay legacies
;;;
;;;						 : Changed code for running model once stimulus appears. Model now runs in
;;;							 distinct chunks of time, controled by the :dat parameter. This was needed
;;;							 to force ACT-R to recalculate expected utilities, even after finding no
;;;							 utilities >=0 for a given cycle
;;;
;;;						 : Removed "waiting" production. Delays now occur when no task-related productions
;;;							 rise above threshold, resulting in "blank" cycles.
;;;							 -Depends on previous revision.
;;;
;;;	2004-12-07 - Glenn Gunzelmann
;;;						 : Added functionality to pvt-batch to support comparison to data
;;;							 in batch runs
;;;							 -A third file is created to record statistics and average data
;;;								when multiple iterations are run of a single parameter set
;;;							 -Correlations and mean-deviations are printed out for each
;;;								set of parameters as the model goes through a batch run
;;;								-These statistics are calculated for each day of the experiment
;;;
;;;						 : "Wait" production is back, based on experimenting with the model
;;;
;;;						 : Added probabilities of success for the productions
;;;
;;;						 : Added a keyword in pvt-batch to manipulate a parameter which
;;;							 increments :c for the respond productions each time "Wait" fires
;;;							 -Briefly used to manipulate G, and it may be reverted to that
;;;								purpose
;;;
;;;	2004-06-11 - Glenn Gunzelmann
;;;						 : Added function to support multiple small batch runs
;;;							 -Allows for variation of UT around G, rather than running the
;;;								entire parameter space, where many combinations are nonsensical
;;;
;;; 2005-07-14 JBG Old code version w/out queuing (for performance reasons)
;;;								but w/ correct data
;;;
;;; 2005-08-18 - Glenn Gunzelmann
;;;						: Added total responses, median RT, Standard Deviation to batch-data
;;;							and statistics files written during batch runs
;;;						: Rewrote statistic generation functions for increased efficiency
;;;						: Eliminated comparison to data in batch run (r and RMSD for data
;;;							averaged over entire days
;;;						: Verified accuracy of data recording relative to human data summary
;;;
;;; 2007-10-25 - Rick More
;;;						: Added new utility mechanisms fp and fp-dec
;;;
;;; 2008-4-29 - Rick More
;;;						: Adjusted fatigue mechansim so that noise is not scaled
;;;						: Reset *fp* across trials
;;;
;;; 2009-02-12 - Rick More
;;;						: Changed fatigue mechanisms to Dan Bothel's module
;;;
;;; 2009-05-05 - Rick More
;;;						: Changed run-full-time to run during delay period.
;;;
;;; 2009-05-05 - Rick Moore
;;;						: Added run-model function
;;;
;;; 2012-09-28 - Rick Moore
;;;						: Modified to work with new run-with-microlapses function
;;;
;;; 2013-04-18 - Bella Veksler
;;;						: Combined model and task code into one file
;;;						: New running instructions
;;;						: Modified run-model function and added new write out function
;;;						: to work with MindModeling
;;; 2014-12-07 - Christopher Dancy
;;;				 : Modified model to use ACT-R/Phi mechanisms for Fatigue/Sleep Deprivation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Running Instructions
;;;
;;;	1. Call (run-model numiterations modelduration minper)
;;;						where numiterations is number of times to rerun the model
;;;						modelduration indicates how long the model should run in minutes
;;;						minper indicates how finely to divide the data into blocks (ie 1 min blocks)
;;;		Example: (run-model 10 10 1) will run the model 10 times, for 10 min each
;;;						and report the mean RT, meadian RT, # lapses, # trials, # false starts
;;;						and # sleep attacks for each minute
;;; Note: a warning will appear #|Warning: run-time must be a number greater than zero. |#
;;;			 which can be ignored as far as we can tell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	 From ACT-R/PM website ...
;;;
;;;	 :randomize-time nil or number
;;;	 Default NIL. Normally, completion times for perceptual-motor operations have fixed times.
;;;	 To make those times vary randomly around those fixed values, set this to a positive integer.
;;;	 A value of 2 will produce an output from 1/2 to 3/2 of the input for each time;
;;;	 a value of 4 will produce an output from 3/4 to 5/4, and so forth.
;;;	 If no number is supplied (e.g., this is just set to T, then the value 3 is used, which is the EPIC default.
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Target parameters for simulating effects of sleep restriction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	 cognitive cycle time :dat	**
;;;	 activation noise		 :ans
;;;	 expected gain noise	:egs	*
;;;	 goal value					 :g		**
;;;	 goal activation (W)	:ga
;;;	 latency factor			 :lf
;;;	 motor system params?
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global parameters for running experiment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Window features
(defparameter *w* nil)

;;Other Experiment Parameters
(defparameter *response* nil)

(defparameter *all* (make-hash-table :test #'equalp))

(defparameter *actr-enabled-p* t)

(defparameter *currJ* nil)
(defparameter *currI* nil)

(defparameter *filePathName* (subseq (namestring *LOAD-TRUENAME*) 0 (search (file-namestring *LOAD-TRUENAME*) (namestring *LOAD-TRUENAME*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Experiment Delivery Code
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;=======================
;Added by Chris Dancy
; For running model in mind-modeling like way, but on own machine
;We get beginning, step, and end for both scalar variables (e.g., tSB = threshScalarBegin)
;=======================
(defun run-model-PS (&key (tSB) (tSS) (tSE) (nSB) (nSS) (nSE))
	(let ((DVs (make-hash-table :test 'equal))
			(tempHash (make-hash-table :test 'equal)))
	 (loop
		for i from tSB to tSE by tSS
		do
		 (setf *currI* i)
		 (loop
			for j from nSB to nSE by nSS
		do
		(setf *currJ* j)
		(print (concatenate 'string "ThreshScalar: " (write-to-string i) " NoiseScalar: " (write-to-string j)))
		(setf (gethash (concatenate 'string (write-to-string i) "-" (write-to-string j)) DVs)
				(run-model :outFile (concatenate 'string *HumModDir* "TS-" (write-to-string i) "_NS-" (write-to-string j) ".txt")
				 :threshScalar i :noiseScalar j))
		(setf temphash (gethash (concatenate 'string (write-to-string i) "-" (write-to-string j)) DVs))
		(let ((keys (hash-table-keys tempHash)))

		(setf keys (sort keys #'string-lessp))

		(when (and (= i tSB) (= j nSB))
		 (with-open-file
			 (mStream "C:/Users/Christopher/Dropbox/AFRL-Grant/ICCM 2015/Final_Submission_Data/ParamRun1.txt" :direction :output :if-exists :append :if-does-not-exist :create)
				(format mStream "Parameters~t")
				"C:/Users/Christopher/Dropbox/AFRL-Grant/ICCM 2015/Final_Submission_Data/ParamRun1.txt")
		 (dolist (key keys)
			(with-open-file
			 (mStream "C:/Users/Christopher/Dropbox/AFRL-Grant/ICCM 2015/Final_Submission_Data/ParamRun1.txt" :direction :output :if-exists :append :if-does-not-exist :create)
				(format mStream "~a~t" key)
				"C:/Users/Christopher/Dropbox/AFRL-Grant/ICCM 2015/Final_Submission_Data/ParamRun1.txt")))

		 (with-open-file
			(mStream "C:/Users/Christopher/Dropbox/AFRL-Grant/ICCM 2015/Final_Submission_Data/ParamRun1.txt" :direction :output :if-exists :append :if-does-not-exist :create)
			 (format mStream "~%~a~t" (concatenate 'string (write-to-string i) "-" (write-to-string j)))
				"C:/Users/Christopher/Dropbox/AFRL-Grant/ICCM 2015/Final_Submission_Data/ParamRun1.txt")

		 (dolist (key keys)
			(with-open-file
			 (mStream "C:/Users/Christopher/Dropbox/AFRL-Grant/ICCM 2015/Final_Submission_Data/ParamRun1.txt" :direction :output :if-exists :append :if-does-not-exist :create)
			 (format mStream "~a~t" (gethash key tempHash))
			 "C:/Users/Christopher/Dropbox/AFRL-Grant/ICCM 2015/Final_Submission_Data/ParamRun1.txt"))

		 (with-open-file
			(mStream "C:/Users/Christopher/Dropbox/AFRL-Grant/ICCM 2015/Final_Submission_Data/ParamRun1.txt" :direction :output :if-exists :append :if-does-not-exist :create)
			 (format mStream "~%")
			 "C:/Users/Christopher/Dropbox/AFRL-Grant/ICCM 2015/Final_Submission_Data/ParamRun1.txt"))))
		DVs)
)

;=======================
;Added by Chris Dancy
; For running in mind-modeling environment
;We have to divide by 100 to get the number of decimals we want, while still working for MindModeling
;=======================
(defun run-model (&key (threshScalar 175) (noiseScalar 50) outFile)

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; Run the experiment
	;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	(setf threshScalar (/ threshScalar 100))
	(setf noiseScalar (/ noiseScalar 100))
	; Create a hash table to store the DVs later
	; Run the experiment and collect the dvs in a temporary list
	(let (
#+:Windows (msPID (write-to-string (external-call "GetProcessId" (:* T) (external-process-id *HumModProc*) :UNSIGNED)))
#+:Unix (msPID (write-to-string (external-process-id *HumModProc*)))
		(fileName (concatenate 'string *filePathName* "modelSolver.pid"))
		DVs
		(outFile nil))
	 (handler-case (with-open-file
		(mS fileName :direction :output :if-exists :supersede :if-does-not-exist :create)
		 (format mS "~a" msPID)) (error () nil))
	 (handler-case (with-open-file
		(mS "modelSolver.pid" :direction :output :if-exists :supersede :if-does-not-exist :create)
		 (format mS "~a" msPID)) (error () nil))
	 (setf DVs (run-model-internal 1 10 1 :outFile outFile
				:betweenBlockDelay 110 :totalDuration 5280 :threshScalar threshScalar :noiseScalar noiseScalar))

	#|(when (external-process-id *HumModProc*)
#+:Windows (external-call "TerminateProcess" (:* T) (external-process-id *HumModProc*) :UNSIGNED 1)
#+:Unix (signal-external-process *HumModProc* 9)
		(setf *HumModProc* nil))|#

	DVs)
)

;;;;
(defun pvt (duration &key (visible nil) (sleep-threshold 30000) (betweenBlockDelay 0.001) (totalDuration 0.001))
	(let ((data nil)
				(trial 0)
				(startPVT (if *actr-enabled-p*
												 (get-time)
												 (get-internal-real-time)))
		(start-block (if *actr-enabled-p*
												 (get-time)
												 (get-internal-real-time)))
				(delay 0)
				(start-time nil))
		(setf *w* (open-exp-window "Task Window" :x 200 :y 200 :width 200 :height 200 :visible visible))
	(sgp :enable-fatigue nil)
		(install-device *w*)

	;;Schedule an event to advance the physiology system to 8am (so we can start at the correct point in the circadian cycle)
	(schedule-event-relative 0.015 'advance-phys :module :physio :priority :max :params (list 480))
	;;Turn on daily planner so that sleep schedule and sleep homeostasis can cause deprivation
	(schedule-event 0.022 'set-phys-vals :module 'physio :params (list (list (list "DailyPlannerControl.Switch" 1))) :priority :max :details "Start Daily Control Cycle")
	(schedule-event 0.022 'set-phys-vals :module 'physio :params (list (list (list "DailyPlannerSchedule.Hour12AM-1AM" 1))) :priority :max :details "Start Daily Control Cycle")
	(schedule-event 0.022 'set-phys-vals :module 'physio :params (list (list (list "DailyPlannerSchedule.Hour1AM-2AM" 1))) :priority :max :details "Start Daily Control Cycle")
	(schedule-event 0.022 'set-phys-vals :module 'physio :params (list (list (list "DailyPlannerSchedule.Hour2AM-3AM" 1))) :priority :max :details "Start Daily Control Cycle")
	(schedule-event 0.022 'set-phys-vals :module 'physio :params (list (list (list "DailyPlannerSchedule.Hour3AM-4AM" 1))) :priority :max :details "Start Daily Control Cycle")
	(schedule-event 0.022 'set-phys-vals :module 'physio :params (list (list (list "DailyPlannerSchedule.Hour4AM-5AM" 1))) :priority :max :details "Start Daily Control Cycle")
	(schedule-event 0.022 'set-phys-vals :module 'physio :params (list (list (list "DailyPlannerSchedule.Hour5AM-6AM" 1))) :priority :max :details "Start Daily Control Cycle")
	(schedule-event 0.022 'set-phys-vals :module 'physio :params (list (list (list "DailyPlannerSchedule.Hour6AM-7AM" 1))) :priority :max :details "Start Daily Control Cycle")
	(schedule-event 0.022 'set-phys-vals :module 'physio :params (list (list (list "DailyPlannerSchedule.Hour7AM-8AM" 1))) :priority :max :details "Start Daily Control Cycle")
	(schedule-event 0.022 'set-phys-vals :module 'physio :params (list (list (list "DailyPlannerSchedule.Hour8AM-9AM" 4))) :priority :max :details "Start Daily Control Cycle")
	(schedule-event 0.022 'set-phys-vals :module 'physio :params (list (list (list "DailyPlannerSchedule.Hour9AM-10AM" 1))) :priority :max :details "Start Daily Control Cycle")
	(schedule-event 0.022 'set-phys-vals :module 'physio :params (list (list (list "DailyPlannerSchedule.Hour10AM-11AM" 1))) :priority :max :details "Start Daily Control Cycle")
	(schedule-event 0.022 'set-phys-vals :module 'physio :params (list (list (list "DailyPlannerSchedule.Hour11AM-12PM" 1))) :priority :max :details "Start Daily Control Cycle")
	(schedule-event 0.022 'set-phys-vals :module 'physio :params (list (list (list "DailyPlannerSchedule.Hour1PM-2PM" 1))) :priority :max :details "Start Daily Control Cycle")
	(schedule-event 0.022 'set-phys-vals :module 'physio :params (list (list (list "DailyPlannerSchedule.Hour2PM-3PM" 1))) :priority :max :details "Start Daily Control Cycle")
	(schedule-event 0.022 'set-phys-vals :module 'physio :params (list (list (list "DailyPlannerSchedule.Hour3PM-4PM" 1))) :priority :max :details "Start Daily Control Cycle")
	(schedule-event 0.022 'set-phys-vals :module 'physio :params (list (list (list "DailyPlannerSchedule.Hour4PM-5PM" 1))) :priority :max :details "Start Daily Control Cycle")
	(schedule-event 0.022 'set-phys-vals :module 'physio :params (list (list (list "DailyPlannerSchedule.Hour5PM-6PM" 1))) :priority :max :details "Start Daily Control Cycle")
	(schedule-event 0.022 'set-phys-vals :module 'physio :params (list (list (list "DailyPlannerSchedule.Hour7PM-8PM" 1))) :priority :max :details "Start Daily Control Cycle")
	(schedule-event 0.022 'set-phys-vals :module 'physio :params (list (list (list "DailyPlannerSchedule.Hour8PM-9PM" 1))) :priority :max :details "Start Daily Control Cycle")
	(schedule-event 0.022 'set-phys-vals :module 'physio :params (list (list (list "DailyPlannerSchedule.Hour9PM-10PM" 1))) :priority :max :details "Start Daily Control Cycle")
	(schedule-event 0.022 'set-phys-vals :module 'physio :params (list (list (list "DailyPlannerSchedule.Hour10PM-11PM" 1))) :priority :max :details "Start Daily Control Cycle")
	(schedule-event 0.022 'set-phys-vals :module 'physio :params (list (list (list "DailyPlannerSchedule.Hour11PM-12AM" 1))) :priority :max :details "Start Daily Control Cycle")

	 ;; (set-foreground-window *w*)
		(if *actr-enabled-p*
				(progn
					(sgp :enable-fatigue nil)
					(run-full-time 1))
				(sleep 1))
	;;Changed by CLD2 to have model running for full "totalDuration" time with delays ("betweenBlockDelay") between blocks lasting "duration" time
	(while (< (current-interval startPVT) totalDuration)
		(while (< (current-interval start-block) duration)
				(clear-exp-window)
				;(if *actr-enabled-p* (sgp :enable-fatigue nil))
	;			(format t "~%*** fatigue disabled~%" )
	;(format t "New Trial ~a~%" trial)

				(proc-display)
				(no-output (eval (list 'mod-chunk (goal-focus) 'state 'wait)))
				(setf delay (+ 2 (act-r-random 9)))
	;			(format t "Fatigue module versio n delaying for ~a seconds~%" delay )
				(setf *response* nil)
				(setf start-time (if *actr-enabled-p* (get-time) (get-internal-real-time)))

				;;DELAY
	;			(format t "~%xxxxxxxxxxxxxxxxxxxxx DELAY xxxxxxxxxxxxxxxxxxxxx~%")
				;(eval (list 'sgp ':fp-dec 0))
				;(if *actr-enabled-p* (sgp :enable-fatigue t))
				(clear-buffer 'visual-location)
				(clear-buffer 'visual)
				(clear (get-module :vision))
				(run-with-microlapses delay #'(lambda() (not (equal nil *response*))))
				;(eval (list 'sgp ':fp-dec fpdec))

				;;Handle False Starts
				(when *response* (setf *response* start-time))

				;;STIMULUS PRESENTATION
	;			(format t "~%xxxxxxxxxxxxxxxxxxxxx TRIAL xxxxxxxxxxxxxxxxxxxxx~%")
				(when (null *response*)
					;(if *actr-enabled-p* (sgp :enable-fatigue t))
	;				(format t "~%*** fatigue enabled~%" )
					(setf start-time (if *actr-enabled-p* (get-time) (get-internal-real-time)))

					(add-text-to-exp-window :text (format nil "START") :color 'red :x 100 :y 100 :width 80 :height 80)
					;;		(buffer-contents 'visual-location)
					;;		(buffer-contents 'visual)
					(proc-display)
	;				(format t "~%*** Starting run at ~f for ~f~%" (mp-time) (/ sleep-threshold 1000) );
					(run-with-microlapses (/ sleep-threshold 1000) #'(lambda() (not (equal nil *response*))))
	;				(format t "~%*** Ending run ~f ~%" (mp-time) );
					(clear-exp-window)
					;(if *actr-enabled-p* (sgp :enable-fatigue nil))
	;				(format t "~%*** fatigue disabled~%" )
					(proc-display))

				;;Handle Sleep Attacks
				(if (and (null *response*) *actr-enabled-p*) (setf *response* (get-time)))

				;;Record RT
				(setf data (cons (list delay (round (- *response* start-time)) (get-time)) data))

				;;Feedback
				(add-text-to-exp-window :text (format nil "~a" (round (- *response* start-time)))
																:color 'blue :x 80 :y 90 :width 40 :height 20)
				;; (proc-display)
				(if *actr-enabled-p*
						(run-full-time .5)
								 (sleep .5))
							(setf trial (1+ trial)))
;				(format t "Starting run ~f and bBlock Delay ~f totalTime ~f~%" (current-interval start-block) betweenBlockDelay (current-interval startPVT))
				;We need to advance our physiology too, so we advance the betweenblockdelay - minus any the time it takes to reach any remaining update-phys-vars events updated physiological value that may exist
				(when *actr-enabled-p*
				 (let
					((currPhysTime (read-from-string (cadaar (get-phys-vals nil (list '("System.X")))))))

					;Schedule an advance-phys event either betweenBlockDelay from now (meta-process time) or (betweenBlockDelay + difference between current MP time and phys model time)
					(if (or (null (evt-time *nextUpdateEvent*)) (< (evt-time *nextUpdateEvent*) (/ (get-time) 1000)))
					 (schedule-event-relative 0.001 'advance-phys :module :physio :priority :max :params (list (+ (/ betweenBlockDelay 60) (- (/ (get-time) 60000) currPhysTime))))
					 (schedule-event-relative (+ (- (evt-time *nextUpdateEvent*) (/ (get-time) 1000)) 0.001)
						'advance-phys :module :physio :priority :max
					:params (list (+ (/ (- betweenBlockDelay (- (evt-time *nextUpdateEvent*) (/ (get-time) 1000))) 60) (- (/ (get-time) 60000) currPhysTime)))))
					(run-full-time betweenBlockDelay)))
;				 (print "ending run for 2 hours")
				 (setf start-block
					(if *actr-enabled-p*
									 (get-time)
					 (get-internal-real-time))))
		(clear-exp-window)
		(add-text-to-exp-window :text "Done" :color 'red :x 80 :y 90 :width 40 :height 20)
		;;(pprint (reverse data))
		;;(pprint data)
		(reverse data)
		))


(defun current-interval (start)
	(let ((cur-time (if *actr-enabled-p* (get-time) (get-internal-real-time))))
		(/ (- cur-time start) 1000)
		))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Running Model Code
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;for running with CCL on MM

;these parameter settings are from the original PVT model fitting with linear TOT function
;they can be altered either here or by adding more parameters to run-model to modify them
(defparameter *dat* 0.03817146880148)
(defparameter *iu* 2.2921999934532)
(defparameter *fp-dec* 0.01440861)
(defparameter *fpbmc* -0.0120716873738272)
(defparameter *fpmc* -0.00273707862262172)
(defparameter *fpc* 1.00170859583602)
(defparameter *utbmc* -0.00991274671415563)
(defparameter *utmc* 0.00453807249538224)
(defparameter *utc* 2.0643395401332)
(defparameter *tn* 8)
(defparameter *pn* 0)
(defparameter *tib* 10)
(defparameter *hour* 0)
(defparameter *subject* 0)
(defparameter *fp-dec* 0.01440861)

;run-model runs the model multiple times and reports the results
;specify: number of times to run model (numiterations)
;				 duration of model in minutes (modelduration)
;				 how many minutes per block to write out the summary (minper)


(defun run-model-internal (numiterations modelduration minper &key (threshScalar 1.75) (noiseScalar 0.5) (betweenBlockDelay nil) (totalDuration nil) outFile)
	(let ((run-data (list))
		(indivRunsHash (make-hash-table :test 'equal))
		(keys (list)))

		(dotimes (i numiterations)
			(format t "Iteration: ~s ~%" i)
		(sgp-fct (list :aa-util-thresh-scalar threshScalar :aa-util-noise-scalar noiseScalar))
			(let ((cur-run-data nil))
			(setf cur-run-data (pvt (- (* 60 modelduration) 10)
							:sleep-threshold 30000 :visible nil
							:betweenBlockDelay (* betweenBlockDelay 60) :totalDuration (* totalDuration 60)))
		;(setf (gethash i indivRunsHash) (mm-write-out cur-run-data minper 1))
				(setf run-data (append cur-run-data run-data)))
		;on each model run we need to reset ACT-R/Phi including the timing and give HumMod time to startup
		(reset)
		(sleep 1))
	#|(setf keys (hash-table-keys (gethash 0 indivRunsHash)))
	(setf keys (sort keys #'string-lessp))

	(dolist (key keys)
	 (let (valList '())
		(dotimes (i numiterations)
		 (if (listp (gethash key (gethash i indivRunsHash)))
			(setf valList (append valList (gethash key (gethash i indivRunsHash))))
		(setf valList (append valList (list (gethash key (gethash i indivRunsHash)))))))
		(with-open-file
			 (mStream "C:/Users/Christopher/Dropbox/AFRL-Grant/ICCM 2015/Final_Submission_Data/ParamRun1wVar.txt" :direction :output :if-exists :append :if-does-not-exist :create)
			 (format mStream "~a~t~a~t~5$~t~5$~%" (concatenate 'string (write-to-string *currI*) "-" (write-to-string *currJ*)) (write-to-string key) (get-average valList) (stdev valList))
			 "C:/Users/Christopher/Dropbox/AFRL-Grant/ICCM 2015/Final_Submission_Data/ParamRun1wVar.txt")))|#
	(let (
#+:Windows (msPID (write-to-string (external-call "GetProcessId" (:* T) (external-process-id *HumModProc*) :UNSIGNED)))
#+:Unix (msPID (write-to-string (external-process-id *HumModProc*)))
		(fileName (concatenate 'string *filePathName* "modelSolver.pid")))
	 (handler-case (with-open-file
		(mS fileName :direction :output :if-exists :supersede :if-does-not-exist :create)
		 (format mS "~a" msPID)) (error () nil))
		 (handler-case (with-open-file
		(mS "modelSolver.pid" :direction :output :if-exists :supersede :if-does-not-exist :create)
		 (format mS "~a" msPID)) (error () nil)))
		(mm-write-out run-data minper numiterations outFile)))

;write out results in MindModeling format, includes mean, median, lapses, num trials,
;false starts and sleep attacks per period
(defun mm-write-out(x minper numIter &optional outFile)
	(let ((milper (* 1000 60 minper))	;milliseconds in a period
				 (my-hash-responses (make-hash-table :test 'equal))
		 (totalTrials (list 0 0 0 0))
		 val1
		 val2
		 val3
		 val4
		 (value nil)
		 keys)
		(dolist (i x)
			(let ((d (cond
					((< (third i) 57600000) (write-to-string 0))
					((< (third i) 144000000) (write-to-string 1))
					((< (third i) 230400000) (write-to-string 2))
					(t (write-to-string 3))))
			(mB	(cond
						((loop
					 for j from 150 to 500 by 10
					 do
					 (if (<= (second i) j)
							(if (= j 150)
						 (if (< (second i) 150)
							(return "falseS"))
						 (if (and (<= (second i) 500) (> (second i) 490)) (return (write-to-string j)) (return (write-to-string (+ j 10)))))
						nil)))
				 ((< (second i) 30000) "lapse")
				 ((>= (second i) 30000) "sleepA"))))
					(if (gethash (concatenate 'string "v" d "-" mB) my-hash-responses)
						(setf (gethash (concatenate 'string "v" d "-" mB) my-hash-responses) (append (gethash (concatenate 'string "v" d "-" mB) my-hash-responses) (list (second i))))
						(setf (gethash (concatenate 'string "v" d "-" mB) my-hash-responses) (list (second i))))))

	(when outFile
	 (with-open-file
			(messageStream	outFile :direction :output :if-exists :append :if-does-not-exist :create)
			(format messageStream "Period~tMean~tMedian~tLapses~tSleepAttacks~tFalseStarts~tTrials~&") outFile))

		(setf keys (hash-table-keys my-hash-responses))
	(dolist (key keys)
	 (when outFile
		(setf value (gethash key my-hash-responses))
		(setf val1 (mapcan #'(lambda (i) (if (and (> i 150) (< i 500)) (list i))) value))
		(setf val2 (mapcan #'(lambda (i) (if (and (< i 30000) (> i 500)) (list i))) value))
		(setf val3 (mapcan #'(lambda (i) (if (= i 30000) (list i))) value))
		(setf val4 (mapcan #'(lambda (i) (if (= i 0) (list i))) value))
		(with-open-file
			(messageStream	outFile
			:direction :output :if-exists :append :if-does-not-exist :create)
			(format messageStream "~a~t~a~t~a~t~a~t~a~t~a~t~a~&" key (if (> (length val1) 0) (get-average val1) 0) (if (> (length val1) 0) (median val1) 0) (length val2) (length val3) (length val4) (length value))
			outFile))

	 (setf (gethash key my-hash-responses) (/ (length (gethash key my-hash-responses)) numIter))
	 (cond
		((equal (subseq key 0 2) "v0")
			(setf (first totalTrials) (+ (first totalTrials) (gethash key my-hash-responses))))
		((equal (subseq key 0 2) "v1")
			(setf (second totalTrials) (+ (second totalTrials) (gethash key my-hash-responses))))
		((equal (subseq key 0 2) "v2")
			(setf (third totalTrials) (+ (third totalTrials)(gethash key my-hash-responses))))
		((equal (subseq key 0 2) "v3")
			(setf (fourth totalTrials) (+ (fourth totalTrials) (gethash key my-hash-responses))))))

	;;;Could make faster by moving to code above
	(dolist (key keys)
	;(print (concatenate 'string (write-to-string (gethash key my-hash-responses)) " - " (write-to-string totalTrials)))
	 (cond
		 ((equal (subseq key 0 2) "v0")
			 (setf (gethash key my-hash-responses) (coerce (/ (gethash key my-hash-responses) (first totalTrials)) 'float)))
		 ((equal (subseq key 0 2) "v1")
			 (setf (gethash key my-hash-responses) (coerce (/ (gethash key my-hash-responses) (second totalTrials)) 'float)))
		 ((equal (subseq key 0 2) "v2")
			 (setf (gethash key my-hash-responses) (coerce (/ (gethash key my-hash-responses) (third totalTrials)) 'float)))
		 ((equal (subseq key 0 2) "v3")
			 (setf (gethash key my-hash-responses) (coerce (/ (gethash key my-hash-responses) (fourth totalTrials)) 'float)))))

	;;Set any missing bins to have a total of 0 items in bin
	(loop
		for i from 0 to 3 by 1
	do
	 (loop
		for j from 150 to 500 by 10
		do
		 (if (= j 150)
			(when (not (gethash (concatenate 'string "v" (write-to-string i) "-" (write-to-string (+ j 10))) my-hash-responses))
			 (setf (gethash (concatenate 'string "v" (write-to-string i) "-" (write-to-string (+ j 10))) my-hash-responses) 0))
			(when (not (gethash (concatenate 'string "v" (write-to-string i) "-" (write-to-string j)) my-hash-responses))
			 (setf (gethash (concatenate 'string "v" (write-to-string i) "-" (write-to-string j)) my-hash-responses) 0))))
	 (when (not (gethash (concatenate 'string "v" (write-to-string i) "-lapse") my-hash-responses))
		(setf (gethash (concatenate 'string "v" (write-to-string i) "-lapse") my-hash-responses) 0))
	 (when (not (gethash (concatenate 'string "v" (write-to-string i) "-sleepA") my-hash-responses))
		(setf (gethash (concatenate 'string "v" (write-to-string i) "-sleepA") my-hash-responses) 0))
	 (when (not (gethash (concatenate 'string "v" (write-to-string i) "-falseS") my-hash-responses))
		(setf (gethash (concatenate 'string "v" (write-to-string i) "-falseS") my-hash-responses) 0)))

	my-hash-responses
	))



 #|(defun mm-write-out(x minper)
	(let* ((milper (* 1000 60 minper))	;milliseconds in a period
				 (my-hash-responses (make-hash-table :test 'equal))
				 val1
		 val2
		 val3
		 val4
				 keys
				 value
		 (outFile "testDataPhi.txt"))
		(format t "~s~%" milper)
		(dolist (i x)
			(let ((h (floor (/ (third i) milper))))
		(format t "~f ~f ~%" h (third i))
					(if (gethash h my-hash-responses)
						(setf (gethash h my-hash-responses) (append (gethash h my-hash-responses) (list (second i))))
						(setf (gethash h my-hash-responses) (list (second i))))))
		(setf keys (hash-table-keys my-hash-responses))
		(setf keys (sort keys '<))
	(with-open-file
		(messageStream	outFile :direction :output :if-exists :append :if-does-not-exist :create)
			(format messageStream "Period~tMean~tMedian~tLapses~tSleepAttacks~tFalseStarts~tTrials~&") outFile)
		(dolist (key keys)

			(setf value (gethash key my-hash-responses))
			(setf val1 (mapcan #'(lambda (i) (if (and (> i 150) (< i 500)) (list i))) value))
			(format t "Period ~a MeanRT=~a~%" key (if (> (length val1) 0) (get-average val1) 0))
			(format t "Period ~a MedianRT=~a~%" key (if (> (length val1) 0) (median val1) 0))
			(setf val2 (mapcan #'(lambda (i) (if (and (< i 30000) (> i 500)) (list i))) value))
			(format t "Period ~a Lapses=~a~%" key (length val2))
			(setf val3 (mapcan #'(lambda (i) (if (= i 30000) (list i))) value))
			(format t "Period ~a SleepAttacks=~a~%" key (length val3))
			(setf val4 (mapcan #'(lambda (i) (if (= i 0) (list i))) value))
			(format t "Period ~a FalseStarts=~a~%" key (length val4))
			(format t "Period ~a Trials=~a~%" key (length value))
		(print (length keys))
		(with-open-file
		(messageStream	outFile
		:direction :output :if-exists :append :if-does-not-exist :create)
			(format messageStream "~a~t~a~t~a~t~a~t~a~t~a~t~a~&" key (if (> (length val1) 0) (get-average val1) 0) (if (> (length val1) 0) (median val1) 0) (length val2) (length val3) (length val4) (length value))))
))

|#;returns median of values
(defun median (values)
	(when values
		(let* ((srtd (sort values '<))
					 (middle (/ (length srtd) 2)))
			(if (oddp (length srtd))
					(nth (floor middle) srtd)
				(/ (+ (nth middle srtd) (max (nth (1- middle) srtd) 0)) 2.0))
			)))

;returns standard deviation of values
(defun stdev (values)
	(let ((avgrt (get-average values)) (summation 0))
	 (when (listp values)
		(mapcar #'(lambda (value)
								(setf summation
									(+ summation
										 (* (- value avgrt) (- value avgrt)))))
			values))
		(if (and (listp values) (> (length values) 1)) (sqrt (/ summation (1- (length values)))) 0)
		))

;returns mean of values
(defun get-average (values)
 (if (listp values)
	(let ((sum 0))
		(mapcar #'(lambda (value)
								(setf sum (+ value sum)))
			values)
		(if (> (length values) 0) (/ sum (length values) 1.0) 0))
	 values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ACT-R Code
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod rpm-window-key-event-handler ((win rpm-window) key)
	(declare (ignorable key))
	(setf *response*
		(if *actr-enabled-p*
				(progn
					(get-time))
				(get-internal-real-time)))
	t)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Model Code -- Psychomotor Vigilance Task (PVT) -- ACT-R 6.0 NO_ATTEND
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Author			: Glenn Gunzelmann
;;; Copyright	 : (c)2004 AFRL/HEAS, All Rights Reserved
;;; Availability: tbd
;;; Address		 : AFRL/HEAS
;;;						 : 6030 South Kent St.
;;;						 : Mesa, AZ	85212-6061
;;;						 : glenn.gunzelmann@mesa.afmc.af.mil
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; History
;;;
;;;	2004-07-21 - Glenn Gunzelmann
;;;						 : Created from old combined task/model code file
;;;
;;;	2007-12-10 - Glenn Gunzelmann
;;;						 : Completed careful validation of appropriate behavior
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf *actr-enabled-p* t)

(clear-all)

(define-model fatigue-pvt-ACT-RPhi

	(sgp-fct (list
						:iu 2.5 ;*iu*
			;:ut 2.2
			#|

						:fpbmc 0;*fpbmc*
						:fpmc -.05;*fpmc*
						:fpc 0;*fpc*

						:utbmc 0;*utbmc*
						:utmc -.012;*utmc*
						:utc 2.3;*utc*

						:hour *hour*
						:dat *dat*
						:fp-dec *fp-dec*|#
						))
;; Set PPM according to 2 x IU
(sgp-fct (list :ppm (* (car (no-output(sgp :iu))))))

(sgp-fct (list :dat *dat*))

(sgp :er t :esc t :v nil :trace-detail low :md -3) ;; :era t

(sgp :show-focus nil) ;; :real-time t

(sgp :ncnar nil) ;;controls whether or not the system makes chunks "look" pretty after a run by cleaning up the merged names

(sgp :randomize-time 3)
(sgp :vpft t)	;; this applies the randomize-time function to cognitive cycle times

;(sgp :ans 0.001)
(sgp :phys-delay 299 :phys-enabled t)
(sgp :aa-enabled t :aa-dm-noise-switch nil :aa-util-noise-switch t); :aa-util-thresh-scalar 1.85 :aa-util-noise-scalar 0.5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The following are set to t above.
;;
;; (pm-set-params :real-time t)
;;	 -Causes model to run in real-time
;;
;; (pm-set-params :show-focus t)
;;	 -Will show the current location of the eye's fixation
;;
;; (sgp :v t)
;;	 -Will print the model trace to the active listener
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chunk-types and declarative memory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(chunk-type do-pvt state)

(chunk-type (response-button (:include visual-object)))

(add-dm (trial isa do-pvt state wait)
				(wait isa chunk)
				(look isa chunk)
				(done isa chunk))

(goal-focus trial)
(set-similarities (wait look -.5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Productions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(P wait
	 =goal>
			ISA									 do-pvt
			state								 wait
		- state								 done
	 ?manual>
			state								 free
	 ?visual>
			state								 free
	 ?visual-location>
			buffer								empty
	 ==>
	 #|!eval! (when (<= (mod (get-time) 600000) 100) (print (get-time)) (print (compute-phys-arousal))
			(print (compute-homeostatic-arousal-factor))
			(print (compute-cort)) (print (sgp-fct (list :ut :egs))))|#
 )


(P attend
	 =goal>
			ISA									 do-pvt
			state								 =state
		- state								 done
		- state								 look
	 =visual-location>
			ISA									 visual-location
	 ==>
	 +visual>
			ISA									 move-attention
			screen-pos						=visual-location
	 =goal>
			state								 look

)



(P respond
	 =goal>
			ISA									 do-pvt
			state								 look
		- state								 done
	 ?manual>
			state								 free
	 ?visual>
			state								 free
==>
	 +manual>
			ISA		punch
			hand	 right
			finger index
	 =goal>
			state								 done
	 +visual>	 ISA					 clear

	 )
)



