	#| Copyright 2014 Christopher L. Dancy II
		This program is free software: you can redistribute it and/or modify
		 it under the terms of the GNU General Public License as published by
		 the Free Software Foundation, either version 3 of the License, or
		 (at your option) any later version.

		 This program is distributed in the hope that it will be useful,
		 but WITHOUT ANY WARRANTY; without even the implied warranty of
		 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
		 GNU General Public License for more details.

		 You should have received a copy of the GNU General Public License
		 along with this program.  If not, see <http://www.gnu.org/licenses/>.
|#

;;;By Christopher L. Dancy II
;;;The College of Information Sciences and Technology, The Pennsylvania State University - University Park
;;;Made to be used with ACT-RPhi
;;;12/11/2012 - Module Created
;;;12/17/2012 - Two buffers, sFunction & sValue
;;;03/18/2013 - Two parameters to be used w/ spp-a function:
;;;	:sFun (seeking function), :sVal (seeking Value)

;;;;;;If you reduce the SValue that was previously the focus above a certain different threashold, that value should be propogated back across previous productions and be graded based on how close you are to 10 (or chosen sValue)

;;;;We only get value from visual-location & aural-location buffers & not a function

;;For extending chunks, we should add merge-function, copy-function, and copy-from-chunk function values
;; -Fear Function names are constructed as "sensory-category/category"-"object/survival-category"

(defvar *START-TIME* (write-to-string (get-internal-real-time)))

;(defun add-aa-to-chunks ()
	(suppress-extension-warnings)
		(extend-chunks fValue :default-value 0 :merge-function merge-chunks-fValue)
		(extend-chunks fFunction :default-value nil :merge-function merge-chunks-fFunction)
		(extend-chunks sValue :default-value 0 :merge-function merge-chunks-fValue)
		(extend-chunks sFunction :default-value nil :merge-function merge-chunks-fFunction)
		(extend-chunks arousal-list :default-value nil :copy-function copy-list :merge-function merge-arousal-list)
		(extend-productions sFunction :default-value nil)
		(extend-productions sValue :default-value 0)
		(extend-productions fFunction :default-value nil)
		(extend-productions fValue :default-value 0)
	(unsuppress-extension-warnings)

;(add-aa-to-chunks)
;;Set of functions used when chunks are merged that make sure the correct
(defun merge-chunks-fValue (c1 c2)
 (/ (+ (chunk-fValue c1) (chunk-fValue c2)) 2))

(defun merge-chunks-fFunction (c1 c2)
 (if (> (chunk-fValue c1) (chunk-fValue c2))
	(chunk-fFunction c1)
	(chunk-fFunction c2)))

(defun merge-chunks-sValue (c1 c2)
 (/ (+ (chunk-sValue c1) (chunk-sValue c2)) 2))

(defun merge-chunks-sFunction (c1 c2)
 (if (> (chunk-sValue c1) (chunk-sValue c2))
	(chunk-sFunction c1)
	(chunk-sFunction c2)))

(defun merge-arousal-list (chunk1 chunk2)
	(declare (ignore chunk2))
	(let* ((dm (get-module declarative))
	       (ol (dm-ol dm)))
		(cond ((null ol)
					(cons (compute-arousal-factor) (chunk-arousal-list chunk1)))
				 ((eq ol t)
					nil)
				 (t ;; ol is a number
					(subseq (cons (compute-arousal-factor) (chunk-arousal-list chunk1))
									0 (min ol (1+ (length (chunk-arousal-list chunk1)))))))))

;;;We use the DM noise-hook function to change the noise dependant upon the current state of arousal
(defun dm-noise-aa (chunk)
	(declare (ignore chunk))
	(let* ((aa (get-module Affective-Associations))
			(arous-dm-noise (compute-arousal-factor (AA-pred-error-factor aa)))
			(arous-mid (/ (AA-max-arous aa) 2))
			(noise-val 0))
		(when (= arous-dm-noise 0) (incf arous-dm-noise 0.000001))
		(if (<= arous-dm-noise arous-mid)
			(progn
				(setf noise-val (/ (+ (* arous-dm-noise (AA-nom-dm-noise aa)) (* (- arous-mid arous-dm-noise) (AA-max-dm-noise aa))) arous-mid))
				;We only record every 5 seconds
				(when (eq (mod (mp-time) 2) 0)
					(with-open-file
						(n-stream (format nil "Phys-data/ans-log~a.txt" (phys-module-pipeID (get-module physio))) :direction :output :if-exists :append :if-does-not-exist :create)
						(format n-stream "~5$,~10$~&" (mp-time-ms) noise-val)))
				(setf noise-val (act-r-noise noise-val))
				noise-val)
			(progn
				(setf noise-val (/ (+ (* (- (AA-max-arous aa) arous-dm-noise) (AA-nom-dm-noise aa)) (* (- arous-dm-noise arous-mid) (AA-max-dm-noise aa))) arous-mid))
				;(when (and (>= (mod (mp-time) 5) 0) (<= (mod (mp-time) 5) 1))
				(when (eq (mod (mp-time) 2) 0)
					(with-open-file
						(n-stream (format nil "Phys-data/ans-log~a.txt" (phys-module-pipeID (get-module physio))) :direction :output :if-exists :append :if-does-not-exist :create)
						(format n-stream "~5$,~10$~&" (mp-time-ms) noise-val)))
				(setf noise-val (act-r-noise noise-val))
				noise-val))))

;;We use the utility-hook function to change the current utility noise (egs) value based on current arousal
;;((A-0)*nom + (0.5-A)*max) / 0.5 or ((1-A)*nom + (A-0.5)*max)/0.5
;;if arous <= 0.5, then we decrease utility threshold also
;;Since our nominal utility threshold is negative, we use the eqn below, but should test for it in the future
(defun util-noise-aa (production)
	(let* ((aa (get-module Affective-Associations))
		egs
		ut
		prod-utility
		arous-util-noise
		addNoise
		(arous-mid (/ (AA-max-arous aa) 2)))
	;;Get normal utility of production
	(sgp-fct (list :utility-hook nil :egs 0))
	(setf prod-utility (compute-utility production))
	(sgp-fct (list :utility-hook 'util-noise-aa))
	(setf arous-util-noise (compute-arousal-factor (AA-pred-error-factor aa)))
	#|(with-open-file
		(messageStream  "arousalLevels.txt" :direction :output :if-exists :append :if-does-not-exist :create)
		(format messageStream "~a~&" arous-util-noise))|#
	(when (<= arous-util-noise 0) (setf arous-util-noise 0.000001))
		(if (<= arous-util-noise arous-mid)
			(sgp-fct (list :ut (* (AA-util-thresh-scalar aa) (- (AA-max-util-thresh aa) arous-util-noise));(- (AA-max-util-thresh aa) (* (- 1 (/ arous-util-noise 0.5)) (AA-max-util-thresh aa) ))))
				:egs (* (AA-util-noise-scalar aa) (/ (+ (* arous-util-noise (AA-nom-util-noise aa)) (* (- arous-mid arous-util-noise) (AA-max-util-noise aa))) arous-mid))))
			(sgp-fct (list :egs (* (AA-util-noise-scalar aa) (/ (+ (* (- (AA-max-arous aa) arous-util-noise) (AA-nom-util-noise aa)) (* (- arous-util-noise arous-mid) (AA-max-util-noise aa))) arous-mid)))))

	(setf egs (no-output (sgp-fct (list :egs))))
	(setf egs (car egs))
	(setf ut (no-output (sgp-fct (list :ut))))
	(setf ut (car ut))
	(setf addNoise (act-r-noise egs))
	(when (and (>= (mod (mp-time) 5) 0) (<= (mod (mp-time) 5) 1))
		(with-open-file
		(msgStream "noiseVals.txt"
		:direction :output :if-exists :append :if-does-not-exist :create)
		(format msgStream "~10$~&" arous-util-noise)))
	(setf prod-utility (+ prod-utility (if (zerop egs) 0.0 addNoise)))
;(format t "~a ~a Noise Added ~a egs ~a Threshold ~a arousUtilNoise ~a~%" (production-name (get-production production)) prod-utility addNoise egs ut arous-util-noise)
		prod-utility))
;nom-util-noise
;nominal arousal is assumed to be 0.5 for nominal noise

;;Function that modifies B-L equation to include Affect (an extended version of Cochran, ICCM)
(defun blc-arousal (chunk)
 (let ((base-level nil)
		(dm (get-module declarative)))
		(setf base-level
		 (cond ((dm-bll dm)
			(when (dm-sact dm)
				(setf (sact-chunk-bl-style (dm-current-sact-chunk dm)) :learn)
				(setf (sact-chunk-blc (dm-current-sact-chunk dm)) (dm-blc dm)))
										(+ (progn
												 (when (dm-act-level (dm-act dm) 'medium)
													 (model-output "Starting with blc: ~f" (dm-blc dm)))

												 (dm-blc dm))
											 (cond ((zerop (chunk-reference-count chunk))
															(when (dm-sact dm)
																(setf (sact-chunk-zero-ref (dm-current-sact-chunk dm)) t))
															(model-warning "Cannot compute base-level for a chunk with no references.")
															-999999.0)
														 (t ;; we use the modified computer-references function below to add arousal
															(compute-references-arousal dm (chunk-reference-count chunk)
																									(chunk-reference-list chunk) (chunk-arousal-list chunk) (chunk-creation-time chunk)
																									(- (dm-bll dm)))))))
									 (t ;; bll nil

										(when (dm-sact dm)
											(setf (sact-chunk-bl-style (dm-current-sact-chunk dm)) :simple)
											(setf (sact-chunk-blc (dm-current-sact-chunk dm)) (dm-blc dm))
											(setf (sact-chunk-base-level (dm-current-sact-chunk dm)) (chunk-base-level chunk)))

										(if (chunk-base-level chunk)
												(progn
													(when (dm-act-level (dm-act dm) 'medium)
														(model-output "User provided chunk base-level: ~f" (chunk-base-level chunk)))
													(chunk-base-level chunk))
											(progn
												(when (dm-act-level (dm-act dm) 'medium)
													(model-output "Starting with blc: ~f" (dm-blc dm)))
												(dm-blc dm))))


					 (when (dm-act-level (dm-act dm) 'medium)
						 (model-output "Total base-level: ~f" base-level))))

		(when (dm-sact dm)
			(setf (sact-chunk-bl-result (dm-current-sact-chunk dm)) base-level))

		(setf (chunk-last-base-level chunk) base-level))
)

(defun compute-references-arousal (dm n references arousals creation-time minus-decay)
	"Computes generalized decay formula from number and list of references,
	 creation time and minus the decay rate."

	(when (dm-act-level (dm-act dm) 'medium)
		(model-output "Computing base-level from ~d references ~S" n (mapcar 'ms->seconds references))
		(model-output "  creation time: ~f decay: ~f  Optimized-learning: ~s" (ms->seconds creation-time) (- minus-decay) (dm-ol dm)))

	(let ((value 0.0)
				(last-reference 0))

		;;Calculate base-level activation, now with an arousal term
		(when references
			(loop
		for reference in references
		for arousal in arousals
				do (progn
					(let ((arous-mid (/ (AA-max-arous (get-module Affective-Associations)) 2)))
					(incf value
						(expt-coerced (max .05 (ms->seconds (- (mp-time-ms) reference)))
							(* minus-decay (- 1 (/ arousal arous-mid)))))
					(setf last-reference reference)))))

		(when (dm-ol dm)
			(let ((denominator (+ 1.0 minus-decay)))
				(if (numberp (dm-ol dm))
						(when (> n (dm-ol dm))
							(when (or (< (mp-time-ms) creation-time)
												(< (mp-time-ms) last-reference))
								(print-warning "Activation calculation problem because time has moved backwards.  Assuming a 0 time delay to avoid calculation error."))
							(incf value (/ (* (- n (dm-ol dm))
																(- (expt-coerced (max 0 (ms->seconds (- (mp-time-ms) creation-time))) denominator)
																	 (expt-coerced (max 0 (ms->seconds (- (mp-time-ms) last-reference))) denominator)))
														 (* (max .05 (ms->seconds (- last-reference creation-time))) denominator))))

					(setf value (/ (* n (expt-coerced (max .05 (ms->seconds (- (mp-time-ms) creation-time))) minus-decay))
												 denominator)))))

		(when (dm-sact dm)
			(setf (sact-chunk-base-level (dm-current-sact-chunk dm)) (log-coerced value))
			(setf (sact-chunk-bl-count (dm-current-sact-chunk dm)) n)
			(setf (sact-chunk-bl-refs (dm-current-sact-chunk dm)) (mapcar 'ms->seconds references))
			(setf (sact-chunk-bl-ct (dm-current-sact-chunk dm)) (ms->seconds creation-time))
			(setf (sact-chunk-decay (dm-current-sact-chunk dm)) (- minus-decay))
			(setf (sact-chunk-ol (dm-current-sact-chunk dm)) (dm-ol dm)))

		(when (dm-act-level (dm-act dm) 'medium)
			(model-output "base-level value: ~f" (log-coerced value)))
		(log-coerced value)))


;Function to compute total arousal factor to be used in any arousal-based change in the architecture (pred-error-factor is a term meant to represent an increase in phasic LC activity)
(defun compute-arousal-factor (&optional pred-error-factor test)
	(let ((aa (get-module Affective-Associations))
				(phys (get-module physio)))
		;When physiology is enabled
		(when (and (get-module physio) (phys-module-enabled phys))
			(if test
				(if (probe-file (concatenate 'string "Phys-data/CEC-Arous" (phys-module-pipeID phys) ".txt"))
					(with-open-file
						(msgStream (concatenate 'string "Phys-data/CEC-Arous" (phys-module-pipeID phys) ".txt")
							:direction :output :if-exists :append :if-does-not-exist :create)
						(format msgStream "~&~$,~$,~5$,~5$,~5$,~5$,~5$,~5$,~5$,~5$"
							(mp-time) (mp-time-ms) (compute-homeostatic-arousal-factor)
							(compute-cort test) (compute-epi-arousal test) (compute-crh-arousal test)
							(* (compute-homeostatic-arousal-factor)
								(compute-cort test)
								(+ (* (AA-epi-arous-ratio aa)
										(compute-epi-arousal))
									(* (AA-crh-arous-ratio aa) (compute-crh-arousal))))
							(sgp :ans) (sgp :egs) (sgp :ut)))
					(with-open-file
						(msgStream (concatenate 'string "Phys-data/CEC-Arous" (phys-module-pipeID phys) ".txt")
							:direction :output :if-exists :overwrite :if-does-not-exist :create)
						(format msgStream "time.s,time.ms,Homeostatic-Arousal-Factor,f(Cortisol),g(Epinephrine),h(CRH),Arousal,ans,egs,ut"))))
			(* (compute-homeostatic-arousal-factor) (compute-cort)
				(+
					(if (and (get-module physio) (phys-module-enabled phys))
							(+
								(* (AA-epi-arous-ratio aa) (compute-epi-arousal))
								(* (AA-crh-arous-ratio aa) (compute-crh-arousal)))
						 0)
					(if (and (get-module FEAR) (FEAR-enabled (get-module FEAR)))
						(* (AA-FEAR-arous-ratio aa)
							(/ (FEAR-arousal (get-module FEAR)) (log (exp (FEAR-max-FEAR-val (get-module FEAR))))))
						0)
					(if (and (numberp pred-error-factor) (> pred-error-factor 0))
						(* (AA-pred-error-arous-ratio aa) pred-error-factor)
						0))))))

		;;This function computes a factor to decay arousal non-linearly (logistic function with beta param) based
		;; on how long it has been since the model has slept. By default, the function assumes
		;; an arbitrary max amount of days awake of 4 before hitting a minimal value (0.0001)
		;; The value output is in the range from 0.0001 to 1
		;;Beta-Logistic function - 1/(1+(homArousal^r/(1-homArousal^r))^-beta)
		;; r here tells us roughly where (at what time) we should be in the center (0.5 on y-axis) of
		;;  the non-linear curve descention
		;; homArousal is defined as ((maxDays-timeSinceAsleep)/maxDays)
(defun compute-homeostatic-arousal-factor (&optional test)
	(let* ((LA (if (cadar  (car (get-phys-vals nil (list '("Status.LastAsleep")))))
							(cadar  (car (get-phys-vals nil (list '("Status.LastAsleep")))))
							"0"))
		(currT (if (cadar  (car (get-phys-vals nil (list '("System.X")))))
							(cadar  (car (get-phys-vals nil (list '("System.X")))))
							"0"))
		(ret-arousal 0.0001) ;Arousal variable/value to be returned
		(maxDays 4)
		(r 0.75)
		(beta 2)
		(timeSinceAsleep (- (read-from-string currT) (read-from-string LA))))
		(if (and LA currT (> timeSinceAsleep 0))
			(let ((homArousal (/ (- (* maxDays 1440) timeSinceAsleep) (* maxDays 1440))))
				(if (> homArousal 0)
					(progn
						(setf ret-arousal (/ 1 (+ 1 (expt (/ (expt homArousal r) (- 1 (expt homArousal r))) (* -1 beta))))))))
			(setf ret-arousal 1))
		(if test
			(with-open-file
				(msgStream (concatenate 'string "Phys-data/Homeostatic-Arousal" (phys-module-pipeID phys) ".txt")
					:direction :output :if-exists :append :if-does-not-exist :create)
				(format msgStream "~5$~&" ret-arousal)))
		ret-arousal))


;;We update the pred-error-factor (for LC-based arousal) to an average between the new value (due to a reward) and the old value
;;If all of our utilities are 0, then we just start with a new predicton-error value that is the average reward across all utilities
(defun arous-util-error (rew)
 (let ((total-pred-error 0)
		 (max-pred-error 0)
		 (new-pred-val 0)
		 (prod-count 0))
	(dolist (p-t (utility-history (get-module utility)))
	 (incf total-pred-error (abs (- (production-u (car p-t)) (- rew (ms->seconds (- (mp-time-ms) (cdr p-t)))))))
	 (incf max-pred-error (production-u (car p-t)))
	 (incf prod-count 1))
	(setf new-pred-val (if (= max-pred-error 0) (/ total-pred-error prod-count) (/ total-pred-error max-pred-error)))
	(setf (AA-pred-error-factor (get-module Affective-Associations))
		(if (= (+ (AA-pred-error-factor (get-module Affective-Associations)) new-pred-val) 0) 0 (/ (+ (AA-pred-error-factor (get-module Affective-Associations)) new-pred-val) 2)))
	))

;;Other Affect functions that are not implemented as of yet
(defun vision-spider (FEAR chunk)
 (declare (ignore FEAR))
 (declare (ignore chunk)))

(defun vision-snake (FEAR chunk)
 (declare (ignore FEAR))
 (declare (ignore chunk)))

(defun add-FEAR-to-rew (FEAR)
 (declare (ignore FEAR)))

;;;Decl-Module/Percept-Module functions; these are scheduled to run after procedural module
;;For these modules, when past threshold:
;;;if exists - change associations fear value and schedule an update to the chunk affect value (that will use the then-current fear value)
;;If not in hash, create new w/ learned fear value (high apha)
;;If in hash, update that fear value w/ current fear value
;;add fast-merge check later (assume fast-merge for now)
(defun check-vis-loc-buffer-affect ()
	(let* ((f (get-module FEAR))
			(s (get-module SEEKING))
			(a-a (get-module Affective-Associations))
			(chunk (buffer-read 'visual-location))
			;(val (FEAR-val-FEAR f))
			;(val-s (SEEKING-winning-sValue s))
			(delay 0);(max (FEAR-update-delay-audition f) (FEAR-update-delay-nociception f)))
			(exists (if chunk (gethash (hash-chunk-contents chunk) (AA-vis-FEAR-hash a-a)) nil))
			(exists-s (if chunk (gethash (hash-chunk-contents chunk) (AA-vis-SEEKING-hash a-a)) nil)))
		;(when chunk (get-chunk-warn chunk))
		(if exists
			(let ((chunk-FEAR-val (car exists)))
				(when (AA-fear-vis-affect-hook a-a) (setf chunk-FEAR-val (funcall (AA-fear-vis-affect-hook a-a) chunk (AA-vis-FEAR-hash a-a))))
				(schedule-event-relative delay 'update-vis-affect-val-f :params (list chunk) :module 'Affective-Associations :maintenance t)
				(setf (FEAR-val-association f) chunk-FEAR-val))
			(when chunk
				(when (AA-fear-vis-affect-hook a-a)
					(let ((chunk-FEAR-val (car exists)))
					(progn
						(setf chunk-FEAR-val (funcall (AA-fear-vis-affect-hook a-a) chunk (AA-vis-FEAR-hash a-a)))
						(when (and chunk-FEAR-val (numberp chunk-FEAR-val)) (setf (FEAR-val-association f) chunk-FEAR-val)))
				(schedule-event-relative delay 'update-vis-affect-val-f :params (list chunk) :module 'Affective-Associations :maintenance t)))))
		(if exists-s
			(let ((chunk-SEEKING-val (car exists)))
				(when (AA-seeking-vis-affect-hook a-a) (setf chunk-SEEKING-val (funcall (AA-seeking-vis-affect-hook a-a) chunk (AA-vis-SEEKING-hash a-a))))
				(setf (gethash 'assocVal (SEEKING-S-vals s)) chunk-SEEKING-val)
				(schedule-event-relative delay 'update-vis-affect-val-s :params (list chunk) :module 'Affective-Associations :maintenance t))
			(when chunk
				(if (AA-seeking-vis-affect-hook a-a)
					(let ((chunk-SEEKING-val nil))
						(setf chunk-SEEKING-val (funcall (AA-seeking-vis-affect-hook a-a) chunk (AA-vis-SEEKING-hash a-a)))
						(when (and chunk-SEEKING-val (numberp chunk-SEEKING-val)) (setf (gethash 'assocVal (SEEKING-S-vals s)) chunk-SEEKING-val))
						(schedule-event-relative delay 'update-vis-affect-val-s :params (list chunk) :module 'Affective-Associations :maintenance t)))))))


(defun check-vis-buffer-affect ()
	(let* ((f (get-module FEAR))
			(s (get-module SEEKING))
			(a-a (get-module Affective-Associations))
			(chunk (buffer-read 'visual))
			;(val (FEAR-val-FEAR f))
			;(val-s (SEEKING-winning-sValue s))
			(delay 0)
			(exists (if chunk (gethash (hash-chunk-contents chunk) (AA-vis-FEAR-hash a-a)) nil))
			(exists-s (if chunk (gethash (hash-chunk-contents chunk) (AA-vis-SEEKING-hash a-a)) nil)))
		(if exists
			(let ((chunk-FEAR-val (car exists)))
				(when (AA-fear-vis-affect-hook a-a) (setf chunk-FEAR-val (funcall (AA-fear-vis-affect-hook a-a) chunk (AA-vis-FEAR-hash a-a))))
				(schedule-event-relative delay 'update-vis-affect-val-f :params (list chunk) :module 'Affective-Associations :maintenance t)
				(setf (FEAR-val-association f) chunk-FEAR-val))
			(when chunk
				(if (AA-fear-vis-affect-hook a-a)
					(let ((chunk-FEAR-val (car exists)))
					(progn
						(setf chunk-FEAR-val (funcall (AA-fear-vis-affect-hook a-a) chunk (AA-vis-FEAR-hash a-a)))
						(when (and chunk-FEAR-val (numberp chunk-FEAR-val)) (setf (FEAR-val-association f) chunk-FEAR-val)))
				 (schedule-event-relative delay 'update-vis-affect-val-f :params (list chunk) :module 'Affective-Associations :maintenance t)))))
		(if exists-s
			(let ((chunk-SEEKING-val (car exists)))
				(when (AA-seeking-vis-affect-hook a-a) (setf chunk-SEEKING-val (funcall (AA-seeking-vis-affect-hook a-a) chunk (AA-vis-SEEKING-hash a-a))))
				(setf (gethash 'assocVal (SEEKING-S-vals s)) chunk-SEEKING-val)
				(schedule-event-relative delay 'update-vis-affect-val-s :params (list chunk) :module 'Affective-Associations :maintenance t))
			(when chunk
				(if (AA-seeking-vis-affect-hook a-a)
					(let ((chunk-SEEKING-val nil))
						(setf chunk-SEEKING-val (funcall (AA-seeking-vis-affect-hook a-a) chunk (AA-vis-SEEKING-hash a-a)))
						(when (and chunk-SEEKING-val (numberp chunk-SEEKING-val)) (setf (gethash 'assocVal (SEEKING-S-vals s)) chunk-SEEKING-val))
						(schedule-event-relative delay 'update-vis-affect-val-s :params (list chunk) :module 'Affective-Associations :maintenance t)))))))

;;;;Need to add SEEKING!
(defun check-aur-loc-buffer-affect ()
 (let* ((f (get-module FEAR))
			(a-a (get-module Affective-Associations))
		(chunk (buffer-read 'aural-location))
		(delay (max (FEAR-update-delay-audition f) (FEAR-update-delay-nociception f)))
		(exists (if chunk (gethash (hash-chunk-contents chunk) (AA-aur-FEAR-hash a-a)) nil)))
	 (if exists
	(let ((chunk-FEAR-val (car exists)))
	 (when (AA-fear-aur-affect-hook a-a) (setf chunk-FEAR-val (funcall (AA-fear-aur-affect-hook a-a) chunk (AA-aur-FEAR-hash a-a))))
	 (schedule-event-relative delay 'update-aur-affect-val-f :params (list chunk) :module 'Affective-Associations :maintenance t)
	 (setf (FEAR-val-association f) chunk-FEAR-val))
	(when chunk
		 (if (AA-fear-aur-affect-hook a-a)
	 (let ((chunk-FEAR-val nil))
		(progn
			 (setf chunk-FEAR-val (funcall (AA-fear-aur-affect-hook a-a) chunk (AA-aur-FEAR-hash a-a)))
		 (when (and chunk-FEAR-val (numberp chunk-FEAR-val)) (setf (FEAR-val-association f) chunk-FEAR-val)))
	 (schedule-event-relative delay 'update-aur-affect-val-f :params (list chunk) :module 'Affective-Associations :maintenance t)))))))

(defun check-aur-buffer-affect ()
 (let* ((f (get-module FEAR))
			(a-a (get-module Affective-Associations))
		(chunk (buffer-read 'aural))
		;(val (FEAR-val-FEAR f))
		(delay (max (FEAR-update-delay-audition f) (FEAR-update-delay-nociception f)))
		(exists (if chunk (gethash (hash-chunk-contents chunk) (AA-vis-FEAR-hash a-a)) nil)))
		(if exists
			(let ((chunk-FEAR-val (car exists)))
				(when (AA-fear-aur-affect-hook a-a) (setf chunk-FEAR-val (funcall (AA-fear-aur-affect-hook a-a) chunk (AA-aur-FEAR-hash a-a))))
				(schedule-event-relative delay 'update-aur-affect-val-f :params (list chunk) :module 'Affective-Associations :maintenance t)
				(setf (FEAR-val-association f) chunk-FEAR-val))
			(when chunk
				(if (AA-fear-aur-affect-hook a-a)
					(let ((chunk-FEAR-val nil))
						(progn
							(setf chunk-FEAR-val (funcall (AA-fear-aur-affect-hook a-a) chunk (AA-aur-FEAR-hash a-a)))
							(when (and chunk-FEAR-val (numberp chunk-FEAR-val)) (setf (FEAR-val-association f) chunk-FEAR-val)))
					 (schedule-event-relative delay 'update-aur-affect-val-f :params (list chunk) :module 'Affective-Associations :maintenance t)))))))

(defun check-retr-buffer-affect ()
	(let* ((f (get-module FEAR))
		(s (get-module SEEKING))
			(a-a (get-module Affective-Associations))
		 (chunk (buffer-read 'retrieval))
		;(val (FEAR-val-FEAR f))
		(delay (max (FEAR-update-delay-audition f) (FEAR-update-delay-nociception f)))
		(exists (if chunk (gethash (hash-chunk-contents chunk) (AA-dm-FEAR-hash a-a)) nil))
		(exists-s (if chunk (gethash (hash-chunk-contents chunk) (AA-dm-SEEKING-hash a-a)) nil)))
	 (if exists
	(let ((chunk-FEAR-val (car exists)))
	 (when (AA-fear-dm-affect-hook a-a) (setf chunk-FEAR-val (funcall (AA-fear-dm-affect-hook a-a) chunk (AA-dm-FEAR-hash a-a))))
	 (schedule-event-relative delay 'update-dm-affect-val-f :params (list chunk) :module 'Affective-Associations :maintenance t)
	 (setf (FEAR-val-association f) chunk-FEAR-val))
	(when chunk
		 (if (AA-fear-dm-affect-hook a-a)
			(let ((chunk-FEAR-val nil))
		(progn
			 (setf chunk-FEAR-val (funcall (AA-fear-dm-affect-hook a-a) chunk (AA-dm-FEAR-hash a-a)))
		 (when (and chunk-FEAR-val (numberp chunk-FEAR-val)) (setf (FEAR-val-association f) chunk-FEAR-val)))
	 (schedule-event-relative delay 'update-dm-affect-val-f :params (list chunk) :module 'Affective-Associations :maintenance t)))))
	 (if exists-s
	(let ((chunk-SEEKING-val (car exists)))
	 (when (AA-seeking-dm-affect-hook a-a) (setf chunk-SEEKING-val (funcall (AA-seeking-dm-affect-hook a-a) chunk (AA-dm-SEEKING-hash a-a))))
	 (setf (gethash 'assocVal (SEEKING-S-vals s)) chunk-SEEKING-val)
	 (schedule-event-relative delay 'update-dm-affect-val-s :params (list chunk) :module 'Affective-Associations :maintenance t))
	(when chunk
		(let ((chunk-SEEKING-val nil))
			(if (AA-seeking-dm-affect-hook a-a)
			(progn
				(setf chunk-SEEKING-val (funcall (AA-seeking-dm-affect-hook a-a) chunk (AA-dm-SEEKING-hash a-a)))
				(when (and chunk-SEEKING-val (numberp chunk-SEEKING-val)) (setf (gethash 'assocVal (SEEKING-S-vals s)) chunk-SEEKING-val))
				(schedule-event-relative delay 'update-dm-affect-val-s :params (list chunk) :module 'Affective-Associations :maintenance t))))))))




 ;(schedule-event-after-module 'procedural 'check-retr-buffer-affect :module 'Affective-Associations))

;;;Functions that update the percept/declarative-affect hash-tables
;;;affect-val = curr-val + alpha * (reinforcer + fear-state/(1 + k*t) - curr-val)
;;;Hashing (hash-chunk-contents) these chunks in this equation can sometimes lead to a warning when
;;; the function is scheduled and by the time this is called, the chunk is gone
;;;
(defun update-vis-affect-val-f (chunk)
 (let ((a-a (get-module Affective-Associations))
		(val (if chunk (chunk-fValue chunk) nil)))
	(when (and val (> val (AA-FEAR-affect-thresh a-a)))
		(let* ((f (get-module FEAR))
		 ;Current fear state
		 (fear-val (FEAR-val-FEAR f))
		 ;reinforcer value (from fear state)
		 (r (max (FEAR-val-audition f) (FEAR-val-nociception f)))
		 (alpha (AA-vis-alpha a-a))
		 (v-hash (AA-vis-FEAR-hash a-a))
		 (chunk (hash-chunk-contents chunk))
		 (v-t (if (gethash chunk v-hash) (gethash chunk v-hash) (list 0 (mp-time))))
		 (vis-mult (AA-vis-mult a-a))
		 (k (AA-dm-k a-a)))
		 (let* ((g (/ 1 (+ 1 (* k (- (mp-time) (cadr v-t))))))
			(new-val (* vis-mult (+ (car v-t) (* alpha (+ r (- (* g fear-val) (car v-t))))))))
		(setf (gethash chunk v-hash) (list new-val (mp-time))))))))

 ;;SEEKING
 ;;;Check this again (see if you can figure out the warnings...)later using print statements below
 ;;; (for now, warnings are suppressed in problematic statement below...)
(defun update-vis-affect-val-s (chunk)
	;(print (chunk-p-fct chunk))
	;(print (hash-chunk-contents chunk))
	;(print "467aa")
	;(print (hash-chunk-contents (buffer-read 'visual-location)))
	;(print "471aa")
	(let ((a-a (get-module Affective-Associations))
		(val (if chunk (chunk-sValue chunk) nil)))
	(when (and val (> val (AA-seeking-affect-thresh a-a)))
	 (let* ((s (get-module SEEKING))
		;Current seeking state
		(seeking-val (SEEKING-winning-sValue s))
		;reinforcer value (from SEEKING state)
		(r (max (SEEKING-sThirst-val s) (SEEKING-sHunger-val s) (SEEKING-sSkinTemp-val s)))
			(alpha (AA-dm-alpha a-a))
		(v-hash (AA-vis-SEEKING-hash a-a))
		(chunk (hash-chunk-contents chunk))
		(v-t (if (gethash chunk v-hash) (gethash chunk v-hash) (list 0 (mp-time))))
		(vis-mult (AA-vis-mult a-a))
		(k (AA-dm-k a-a))
				(g (/ 1 (+ 1 (* k (- (mp-time) (cadr v-t))))))
		(new-val (* vis-mult (+ (car v-t) (* alpha (+ r (- (* g seeking-val) (car v-t))))))))
		(setf (gethash chunk v-hash) (list new-val (mp-time)))))))

;;;affect-val = curr-val + alpha * (reinforcer + fear-state/(1 + k*t) - curr-val)
(defun update-aur-affect-val-f (chunk)
 (let ((a-a (get-module Affective-Associations))
		(val (if chunk (chunk-fValue chunk) nil)))
	(when (and val (> val (AA-FEAR-affect-thresh a-a)))
		(let*
		 ((f (get-module FEAR))
		 ;Current fear state
		 (fear-val (FEAR-val-FEAR f))
		 ;reinforcer value (from fear state)
		 (r (max (FEAR-val-audition f) (FEAR-val-nociception f)))
		 (alpha (AA-aur-alpha a-a))
		 (a-hash (AA-aur-FEAR-hash a-a))
		 (chunk (hash-chunk-contents chunk))
		 (v-t (if (gethash chunk v-hash) (gethash chunk v-hash) (list 0 (mp-time))))
		 (aur-mult (AA-aur-mult a-a))
		 (k (AA-aur-k a-a))
		 (g (/ 1 (+ 1 (* k (- (mp-time) (cadr v-t))))))
		 (new-val (* aur-mult (+ (car v-t) (* alpha (+ r (- (* g fear-val) (car v-t))))))))
			(setf (gethash (hash-chunk-contents chunk) a-hash) (list new-val (mp-time)))))))

;;;;;;NEED TO CHECK new-val assignment for other functions!!!. Should be ...(+ car v-t) (* alpha ...
;;;;;; ALSO NEED TO ADD SEEKING TO FUNCTIONS
	;;SEEKING
(defun update-aur-affect-val-s (chunk)
	(let ((a-a (get-module Affective-Associations))
		(val (if chunk (chunk-sValue chunk) nil)))
	(when (and val (> val (AA-seeking-affect-thresh a-a)))
	 (let* ((s (get-module SEEKING))
		;Current seeking state
		(seeking-val (SEEKING-winning-sValue s))
		;reinforcer value (from fear state)
		(r (max (SEEKING-sThirst-val s) (SEEKING-sHunger-val s) (SEEKING-sSkinTemp-val s)))
			(alpha (AA-aur-alpha a-a))
		(a-hash (AA-aur-SEEKING-hash a-a))
		(chunk (hash-chunk-contents chunk))
		(v-t (if (gethash chunk a-hash) (gethash chunk a-hash) (list 0 (mp-time))))
		(aur-mult (AA-aur-mult a-a))
		(k (AA-dm-k a-a))
				(g (/ 1 (+ 1 (* k (- (mp-time) (cadr v-t))))))
		(new-val (* aur-mult (+ (car v-t) (* alpha (+ r (- (* g seeking-val) (car v-t))))))))
	(setf (gethash (hash-chunk-contents chunk) a-hash) (list new-val (mp-time)))))))

;;;affect-val = curr-val + alpha * (reinforcer + fear-state/(1 + k*t) - curr-val)
(defun update-dm-affect-val-f (chunk)
;;FEAR
 (let ((a-a (get-module Affective-Associations))
		(val (if chunk (chunk-fValue chunk) nil)))
	(when (and val (> val (AA-fear-affect-thresh a-a)))
	 (let*
		((f (get-module FEAR))
		;Current fear state
		(fear-val (FEAR-val-FEAR f))
		;reinforcer value (from fear state)
		(r (max (FEAR-val-audition f) (FEAR-val-nociception f)))
			(alpha (AA-dm-alpha a-a))
		(d-hash (AA-dm-FEAR-hash a-a))
		(chunk (hash-chunk-contents chunk))
		(v-t (if (gethash chunk d-hash) (gethash chunk d-hash) (list 0 (mp-time))))
		(dm-mult (AA-dm-mult a-a))
		(k (AA-dm-k a-a))
		(g (/ 1 (+ 1 (* k (- (mp-time) (cadr v-t))))))
		(new-val (* dm-mult (+ (car v-t) (* alpha (+ r (- (* g fear-val) (car v-t))))))))
	(setf (gethash (hash-chunk-contents chunk) d-hash) (list new-val (mp-time)))))))

(defun update-dm-affect-val-s (chunk)
;;SEEKING
	(let
	 ((a-a (get-module Affective-Associations))
	 (val (if chunk (chunk-sValue chunk) nil)))
		(when (and val (> val (AA-seeking-affect-thresh a-a)))
			(let*
			 ((s (get-module SEEKING))
			 ;Current seeking state
			 (seeking-val (SEEKING-winning-sValue s))
			 ;reinforcer value (from fear state)
			 (r (max (SEEKING-sThirst-val s) (SEEKING-sHunger-val s) (SEEKING-sSkinTemp-val s)))
			 (alpha (AA-dm-alpha a-a))
			 (d-hash (AA-dm-SEEKING-hash a-a))
			 (chunk (hash-chunk-contents chunk))
			 (v-t (if (gethash chunk d-hash) (gethash chunk d-hash) (list 0 (mp-time))))
			 (dm-mult (AA-dm-mult a-a))
			 (k (AA-dm-k a-a))
			 (g (/ 1 (+ 1 (* k (- (mp-time) (cadr v-t))))))
			 (new-val (* dm-mult (+ (car v-t) (* alpha (+ r (- (* g seeking-val) (car v-t))))))))
				(setf (gethash (hash-chunk-contents chunk) d-hash) (list new-val (mp-time)))))))

;;Chunk affect activation offset functions
;;We never want to decrease activation values w/ this (just increase) so we give 0 if our activation value is less than 0
(defun FEAR-chunk-activation-offset (chunk)
 (when chunk
	(let* ((f (get-module FEAR))
		(fVal (FEAR-val-FEAR f))
		(cFVal (chunk-fValue chunk))
		(max-fear (FEAR-max-FEAR-val f))
		(f-c-v (* (abs (- max-fear (abs (- cFVal fVal)))) (exp max-fear))))
	 (if (> f-c-v 1)
		(log f-c-v)
		0))))

(defun SEEKING-chunk-activation-offset (chunk)
 (let ((s (get-module SEEKING)))
	(when (and (eq (chunk-sFunction chunk) (SEEKING-winning-sFunction s)) (SEEKING-enabled s))
	 (let* ((sVal (SEEKING-winning-sValue s))
		 (cSVal (chunk-sValue chunk))
		 (max-seek (SEEKING-max-seeking-val s))
		 (s-c-v (* (- max-seek (abs (- cSVal sVal))) (exp max-seek))))
	 (if (> s-c-v (exp 1))
		(log s-c-v)
		0)))))



;;;Procedural Memory Functions
;; New reward function based on work in Zhang, Berridge, et al. (2009) "A Neural Computational Model of Incentive Salience"
;; funcNum 1 is Simple integrator model with Zhang addition
;; funcNum 2 is TD-algorithm (e.g., Fu & Anderson, 2006) with Zhang addition
(defun add-affect-reward-hook (funcNum)
 (when funcNum
	(when (= funcNum 1) (sgp :reward-hook normal-reward-affect) (return-from add-affect-reward-hook))
	(if (= funcNum 2) (sgp :reward-hook TD-reward-affect) (print "Error: sReward-hook parameter is not a valid number")))
)
;;;;This functionality is now depricated (incentive salience is used in utility as opposed to reward and not cached in utility value).
;;;;;We keep these two functions for historical purposes
;;Reward = r-j(n) + log(k) - (t-j(n) - t-i(n))
#|(defun normal-reward-affect (production rewVal timeDiff &optional rewFunc)
	(let ((sVal (chunk-slot-value-fct (buffer-chunk sFunction) sValue))
			(sFunc (chunk-slot-value-fct (buffer-chunk sFunction) sFunName)))
	;We only want to apply the salience reward additive if we have the corresponding reward function
	(if rewFunc
	 (if (equal rewFunc sFunc)
		(setf rewVal (+ (log sVal) (- rewVal timeDiff)))
		(setf rewVal (- rewVal timeDiff)))
	(setf rewVal (- rewVal timeDiff))))
	rewVal
)

;Eventually test this reward equation w/ an eligibility trace (from Walsh, 2011)
(defun TD-reward-affect (production rewVal timeDiff &optional rewFunc)
 (let* ((util-NP 0)
			(k 0.25)
		(sVal (chunk-slot-value-fct (buffer-chunk sFunction) 'sValue))
		(sFun (chunk-slot-value-fct (buffer-chunk sFunction) 'sFunName))
			(time-NP 0)
		(util-hist (reverse (utility-history (get-module utility)))))
	(if rewFunc
	 (if (equal rewFunc sFun)
		(progn (if (not (= timeDiff 0)) (setf rewVal 0) (setf rewVal (production-u production)))
			;Find U-i+1(n-1) *Need to make sure I'm getting the production after (and not the one before) the current production.
			(dolist (p-t util-hist)
				(when (eq production (car p-t))
				 (let ((g (/ 1 (+ 1 (* k (- time-NP (ms->seconds (cdr p-t))))))))
					(setf rewVal (+ rewVal (+ (log sVal) (* g util-NP))))))
				;Save utility in the case this production is the one after our current reward-production
				(setf util-NP (production-u (car p-t)))))
		(progn (if (not (= timeDiff 0)) (setf rewVal 0) (setf rewVal (production-u production)))
			;Find U-i+1(n-1) *Need to make sure I'm getting the production after (and not the one before) the current production.
			(dolist (p-t util-hist)
				(when (eq production (car p-t))
				 (let ((g (/ 1 (+ 1 (* k (- time-NP (ms->seconds (cdr p-t))))))))
					(setf rewVal (+ rewVal (* g util-NP)))))
				;Save utility in the case this production is the one after our current reward-production
				(setf util-NP (production-u (car p-t))))))
	 (progn
			;Find U-i+1(n-1) *Need to make sure I'm getting the production after (and not the one before) the current production.
			(dolist (p-t util-hist)
				(when (and (eq production (car p-t)) (= (+ timeDiff (ms->seconds (cdr p-t))) (mp-time)))
				 (let ((g (/ 1 (+ 1 (* k (- time-NP (ms->seconds (cdr p-t))))))))
					(model-output "~S: ~f - ~f" production rewVal util-NP)
					(setf rewVal (- (+ rewVal (* g util-NP)) (production-u production)))))
				;Save utility in the case this production is the one after our current reward-production
				(setf util-NP (production-u (car p-t)))
				(setf time-NP (ms->seconds (cdr p-t)))))))
	rewVal
)|#

;;;We want SEEKing production relations to happen from 0-1 where 1 means the production is directly related to the sFun
;;AA functions
(defmacro spp-a (&rest params)
`(spp-a-fct ',params))

;;New SPP like function that allows an affective component (spp-a = spp-affect)
(defun spp-a-fct (listVals)
	(let ((sppList '())
			(keywordFlag nil)
			(prodName (first listVals)))
		(if (get-production prodName)
			(progn
			 (dolist (pORv listVals)
				(if (keywordp pORv)
				 (if (get-spp-a-param pORv)
					(if (not keywordFlag)
						(setf keywordFlag pORv)
						(print-prod-affect-param prodName keywordFlag))
					(setf sppList (append sppList (list pORv))))
				 (if keywordFlag
						(progn (set-prod-affect-param prodName keywordFlag pORv) (setf keywordFlag nil))
						;(print-warning (concatenate 'string "Error: Value "(write-to-string pORv) " given without parameter"))
					 (setf sppList (append sppList (list pORv))))))
			(if keywordFlag
				(if sppList
					(print-prod-affect-param prodName keywordFlag)
					(progn (spp-fct sppList) (print-prod-affect-param prodName keywordFlag)))
			(spp-fct sppList)))
			(print-warning (concatenate 'string (write-to-string prodName) " is an invalid production name"))))
)

(defun print-prod-affect-param (production param)
 (print param)
 (case param
	;eventually change this to check if these params exist (don't assume)
	(:sfun (command-output " :sfun ~A" (production-sFunction production)))
	(:sval (command-output " :sval ~A" (production-sValue production)))
	(:ffun (command-output " :ffun ~A" (production-fFunction production)))
	(:fval (command-output " :fval ~A" (production-fValue production)))
	(t (print-warning "~A is not a valid parameter" param)))
)

;;Prods with these values are more likely to be called (U+log(k-ss))
(suppress-extension-warnings)

(unsuppress-extension-warnings)

;;Set affect production parameter
(defun set-prod-affect-param (prod paramName paramVal)
	 (let ((p (get-production prod)))
	(if p
	 (if (keywordp paramName)
		(progn
		(case paramName
		 (:sfun (setf (production-sFunction prod) paramVal))
		 (:sval (setf (production-sValue prod) paramVal))
		 (:ffun (setf (production-fFunction prod) paramVal))
		 (:fval (setf (production-fValue prod) paramVal))
		 (t (print-warning
							 "NO PARAMETER ~A DEFINED FOR PRODUCTIONS." paramName)
							:error)))
		(print-warning (concatenate 'string (write-to-string paramName) " is an invalid parameter name")))
	 (print-warning (concatenate 'string (write-to-string production) " is an invalid production name"))))
)

(defun get-spp-a-param (param)
 (let ((tempParam nil))
	(if (keywordp param)
		(dolist (a-a-param (AA-spp-a-params (get-module Affective-Associations)))
		 (when (eq param a-a-param) (setf tempParam a-a-param))))
	tempParam))

(defun add-spp-a-param (param)
 (if (keywordp param)
	(if (member param (AA-spp-a-params (get-module Affective-Associations)))
		(progn (print "spp-a parameter already in list") nil)
		(append param (AA-spp-a-params (get-module Affective-Associations))))
	(progn (print "spp-a parameter name must be a keyword") nil)))

;;;Commented out the aural buffer checks, need to add back later
(defun schedule-aa-events (AA)
	(when (AA-enabled AA)
		;(add-aa-to-chunks)
		(when (AA-dm-noise-switch AA)
			(sgp-fct (list :noise-hook 'dm-noise-aa)))
		(when (AA-util-noise-switch AA)
			(sgp-fct (list :utility-hook 'util-noise-aa)))
		(when (AA-arous-util-error-switch AA)
			(sgp-fct (list :reward-notify-hook 'arous-util-error)))
		;;;This definitely isn't working as well as needed (completely screwed up Subtraction model and had to disable, NEED TO CHECK THE FUNCTION!!!!
		(when (AA-chunk-arousal-switch AA)
			(sgp-fct (list :bl-hook 'blc-arousal)))
		(when (AA-chunk-affect-switch AA)
			(sgp-fct (list :activation-offsets 'FEAR-chunk-activation-offset))
			(sgp-fct (list :activation-offsets 'SEEKING-chunk-activation-offset))
			(schedule-periodic-event 0.016 'check-aur-loc-buffer-affect :module 'Affective-Associations :maintenance t :priority :max)
			(schedule-periodic-event 0.016 'check-vis-loc-buffer-affect :module 'Affective-Associations :maintenance t :priority :max)
			(schedule-periodic-event 0.100 'check-aur-buffer-affect :module 'Affective-Associations :maintenance t :priority :max)
			(schedule-periodic-event 0.100 'check-vis-buffer-affect :module 'Affective-Associations :maintenance t :priority :max)
			(schedule-periodic-event 0.016 'check-retr-buffer-affect :module 'Affective-Associations :maintenance t :priority :max))
			t))

;;ACT-R Module functions
(defstruct AA
	busy

	;Hash table to hold DM - Affect/Emotion Pairs
	;key - chunk contents; val - '(value time)
	(dm-FEAR-hash nil)

	;Hash table to hold visual - Affect/Emotion Pairs
	;key - chunk contents; val - '(value time)
	(vis-FEAR-hash nil)

	;Hash table to hold aural - Affect/Emotion Pairs
	;key - chunk contents; val - '(value time)
	(aur-FEAR-hash nil)

	;Hash table to hold DM - Affect/Emotion Pairs
	;key - chunk contents; val - '(value time)
	(dm-SEEKING-hash nil)

	;Hash table to hold visual - Affect/Emotion Pairs
	;key - chunk contents; val - '(value time)
	(vis-SEEKING-hash nil)

	;Hash table to hold aural - Affect/Emotion Pairs
	;key - chunk contents; val - '(value time)
	(aur-SEEKING-hash nil)

	(spp-a-params (list :sfun :sval :ffun :fval))

	;Function called to change fear value based on (vis,aur,ret...) buffer
	(fear-vis-affect-hook nil)
	(fear-aur-affect-hook nil)
	(fear-dm-affect-hook nil)

	;Function called to change fear value based on (vis,aur,ret...) buffer
	(seeking-vis-affect-hook nil)
	(seeking-aur-affect-hook nil)
	(seeking-dm-affect-hook nil)

		;Model parameter to turn on use of arousal-based decay
	(chunk-arousal-switch nil)

	;Model parameter to turn on use of affect that is attached to DM/Percep modules (e.g., FEAR-visual)
	(chunk-affect-switch nil)

	;threshold to determine whether to save perceptual/dm-affect pairs
	(fear-affect-thresh 0.1)

	;threshold to determine whether to save perceptual/dm-affect pairs for SEEKING
	(seeking-affect-thresh 0.1)

	;current association-fear value (found by using current max fear-value from Perc/Decl buffer chunk associations)
	(assoc-fear-val 1)

	;alphas for chunk-affect update eqns
	(vis-alpha 0.25)
	(aur-alpha 0.25)
	(dm-alpha 0.25)

	;k-values used in update eqns
	(vis-k	0.25)
	(aur-k	0.25)
	(dm-k	0.25)

	;Multipliers for all affective associations may be useful for general neural modulation (e.g., catecolamines or hormones)
	(vis-mult 1)
	(aur-mult 1)
	(dm-mult 1)

	;Used for utility threshold change function (based on curent arousal)
	(max-util-thresh 0)
	(nom-util-thresh -0.5)
	(util-thresh-scalar 1)

	;Switch for turning on connection between arousal (meant to represent LC activity) and utlity noise (from Aston-Jones 2005)
	(arous-util-error-switch nil)

	;Prediction error factor that is updated everytime a reward is propogated (it likely should be called something different
	;given it is related to LC activation due to a new/unexpected reward, but for now we'll call it this)
	(pred-error-factor 0)

	;Nominal utility noise value
	(nom-util-noise 0.01)

	;Max utility noise a model can have
	(max-util-noise 0.4)

	(util-noise-scalar 1)

	(util-noise-switch nil)

		;Nominal dm noise value
	(nom-dm-noise 0.01)

	;Max dm noise a model can have
	(max-dm-noise 0.4)

	;Maximum arousal value possible/expected
	(max-arous 3.5)

	(dm-noise-switch nil)

	(enabled nil)

	;;Ratios used for figuring out noise modulation due to arousal
	;; epi, crh, FEAR, pred-error
	(epi-arous-ratio 0.5)
	(crh-arous-ratio 0.5)
	(FEAR-arous-ratio 0)
	(pred-error-arous-ratio 0)
)

(defun create-AA-module (model-name)
	(declare (ignore model-name))
	(make-AA))

(defun delete-AA-module (AA)
	(declare (ignore AA)))

(defun reset-AA-module (AA)
 (setf (AA-dm-FEAR-hash AA) (make-hash-table))
 (setf (AA-vis-FEAR-hash AA) (make-hash-table))
 (setf (AA-aur-FEAR-hash AA) (make-hash-table))
 (setf (AA-dm-SEEKING-hash AA) (make-hash-table))
 (setf (AA-vis-SEEKING-hash AA) (make-hash-table))
 (setf (AA-aur-SEEKING-hash AA) (make-hash-table))
 (schedule-event 0.001 'schedule-AA-events :params (list AA) :priority :max :module 'Affective-Associations :maintenance t :output nil)
 nil)

(defun AA-module-query (AA buff slot val)
 (case slot
	(state
	 (case val
		(error nil)
		(busy (AA-busy AA))
		(free (not (AA-busy AA)))
		(t (print-warning "Bad state query to ~s buffer" buff))))
	(t (print-warning "Invalid slot ~s with query to buffer ~s" slot buff))))

(defun AA-module-requests (AA buff spec)
 (case buff
	(sFunction
	 (AA-module-function-request AA spec))
		(sValue
	 (AA-module-value-request AA spec))
	(t
	 (let*
		((chunkType (chunk-spec-chunk-type spec))
		 (hasVal (slot-in-chunk-spec-p spec 'value))
		 (val (when (chunk-spec-slot-spec spec 'value))))
		(if (eq chunkType 'AA-output)
		 (if hasVal
			(if (= (length val) 1)
			 (if(eq (caar val) '=)
				(model-output "Value: ~s" (caddar val))
				(model-warning "Invalid slot modifier ~s" (caar val)))
			 (model-warning "Value slot specified multiple times"))
			(model-warning "Value slot missing in output request"))
		 (model-warning "Bad chunk-type in request to output"))))))

(defun AA-module-function-request (AA spec)
	(declare (ignore AA))
	(declare (ignore spec))
	nil)

(defun AA-module-value-request (AA spec)
	(declare (ignore AA))
	(declare (ignore spec))
	nil)

;;;;;;!!!!Need to come back to this and make sure the sFunction and fFunction are working as intended!!!!
;;Use this function to take whatever buffer chunk is being cleared and pair it with the affective value currently in the SEEKING module
;;For now, fast-merge must be enabled
;;We access the Affective function and value buffers
(defun AA-clear-buffers (AA buffer chunk)
 (let* ((dm (get-module declarative))
			 ;(AA (get-module Affective-Associations))
			 ;(sFunction (buffer-read 'sFunction))
			 ;(fFunction (buffer-read 'fFunction))
			 (f (get-module FEAR))
			 (s (get-module SEEKING))
			 (delay (max (FEAR-update-delay-audition f) (FEAR-update-delay-nociception f))))
	(when (AA-chunk-affect-switch AA)
	;Pair chunk being cleared with affective functions and values
	(when (eq buffer 'retrieval)
	 (when (dm-fast-merge dm)
		(when (and f (FEAR-enabled f) (not (chunk-fValue chunk)))
		 (setf (chunk-fValue chunk) (FEAR-val-FEAR f))
	 (setf (chunk-fFunction chunk) (FEAR-func f)))
	(when (and s (SEEKING-enabled s) (not (chunk-sValue chunk)))
		 (setf (chunk-sValue chunk) (SEEKING-winning-sValue s))
	 (setf (chunk-sFunction chunk) (SEEKING-winning-sFunction s)))
	(schedule-event-relative delay 'update-dm-affect-val-s :params (list chunk) :module 'Affective-Associations :maintenance t)))
	(when (or (eq buffer 'visual-location) (eq buffer 'visual))
		;Chunk only gets function name if the value comes from the visual buffer but gets function value from both "what" and "where" buffers
		(when (not (chunk-fValue chunk))
			(when (and f (FEAR-enabled f) (not (chunk-fValue chunk)))
				(setf (chunk-fValue chunk) (FEAR-val-FEAR f)))
			(when (and s (SEEKING-enabled s) (not (chunk-sValue chunk)))
				(setf (chunk-sValue chunk) (SEEKING-winning-sValue s))
				(when (eq buffer 'visual)
					(setf (chunk-fFunction chunk) (FEAR-func f))
					(setf (chunk-sFunction chunk) (SEEKING-winning-sFunction s)))))
		(schedule-event-relative delay 'update-vis-affect-val-s :params (list chunk) :module 'Affective-Associations :maintenance t)
		(schedule-event-relative delay 'update-vis-affect-val-f :params (list chunk) :module 'Affective-Associations :maintenance t))
	(when (or (eq buffer 'aural-location) (eq buffer 'aural))
	 (when (not (chunk-fValue chunk))
		(when (and f (FEAR-enabled f) (not (chunk-fValue chunk)))
		 (setf (chunk-fValue chunk) (FEAR-val-FEAR f)))
	(when (and s (SEEKING-enabled s) (not (chunk-sValue chunk)))
		 (setf (chunk-sValue chunk) (SEEKING-winning-sValue s))
		(when (eq buffer 'aural)
		 (when (and f (FEAR-enabled f) (not (chunk-fValue chunk)))
		(setf (chunk-fFunction chunk) (FEAR-func f)))
	 (when (and s (SEEKING-enabled s) (not (chunk-sValue chunk)))
		(setf (chunk-sFunction chunk) (SEEKING-winning-sFunction s))))))
	 (schedule-event-relative delay 'update-aur-affect-val-s :params (list chunk) :module 'Affective-Associations :maintenance t)
	 (schedule-event-relative delay 'update-aur-affect-val-f :params (list chunk) :module 'Affective-Associations :maintenance t)))))

(defun AA-module-params (AA param)
 (if (consp param)
	(case (car param)
	 (:AA-chunk-affect-switch
		(setf (AA-chunk-affect-switch AA) (cdr param)))
	 (:AA-chunk-arousal-switch
	(setf (AA-chunk-arousal-switch AA) (cdr param)))
	 (:AA-fear-vis-affect-hook
		(setf (AA-fear-vis-affect-hook AA) (cdr param)))
	 (:AA-seeking-vis-affect-hook
		(setf (AA-seeking-vis-affect-hook AA) (cdr param)))
	 (:AA-arous-util-error-switch
	(setf (AA-arous-util-error-switch AA) (cdr param)))
	 (:AA-max-util-noise
	(setf (AA-max-util-noise AA) (cdr param)))
	 (:AA-max-util-thresh
	(setf (AA-max-util-thresh AA) (cdr param)))
	 (:AA-max-dm-noise
	(setf (AA-max-dm-noise AA) (cdr param)))
	 (:AA-nom-util-noise
	(setf (AA-nom-util-noise AA) (cdr param)))
	 (:AA-nom-util-thresh
	(setf (AA-nom-util-thresh AA) (cdr param)))
	 (:AA-nom-dm-noise
	(setf (AA-nom-dm-noise AA) (cdr param)))
	 (:AA-dm-noise-switch
	(setf (AA-dm-noise-switch AA) (cdr param)))
	(:AA-util-noise-switch
		(setf (AA-util-noise-switch AA) (cdr param)))
	(:AA-util-thresh-scalar
		(setf (AA-util-thresh-scalar AA) (cdr param)))
	(:AA-util-noise-scalar
		(setf (AA-util-noise-scalar AA) (cdr param)))
	(:AA-max-arous
		(setf (AA-max-arous AA) (cdr param)))
	 (:AA-enabled
		(setf (AA-enabled AA) (cdr param))))
	(case param
	 (:AA-chunk-affect-switch
		(AA-chunk-affect-switch AA))
	 (:AA-fear-vis-affect-hook
		(AA-fear-vis-affect-hook AA))
	 (:AA-seeking-vis-affect-hook
		(AA-seeking-vis-affect-hook AA))
	 (:AA-chunk-arousal-switch
	(AA-chunk-arousal-switch AA))
	 (:AA-arous-util-error-switch
	(AA-arous-util-error-switch AA))
	 (:AA-max-util-noise
	(AA-max-util-noise AA))
	 (:AA-max-dm-noise
	(AA-max-dm-noise AA))
	 (:AA-nom-util-noise
	(AA-nom-util-noise AA))
	 (:AA-nom-util-thresh
	(AA-nom-util-thresh AA))
	 (:AA-nom-dm-noise
	(AA-nom-dm-noise AA))
	 (:AA-dm-noise-switch
	(AA-dm-noise-switch AA))
	 (:AA-util-noise-switch
	(AA-util-noise-switch AA))
	 (:AA-util-thresh-scalar
	(AA-util-thresh-scalar AA))
	 (:AA-util-noise-scalar
	(AA-util-thresh-scalar AA))
			(:AA-max-arous
				(AA-max-arous AA))
	 (:AA-enabled
	(AA-enabled AA)))))

(define-module-fct 'Affective-Associations
	'(AAFunction AAValue)
	(list
	 (define-parameter
		 :AA-chunk-affect-switch
		 :default-value nil)
		(define-parameter
		 :AA-fear-affect-thresh
		 :default-value 1)
		(define-parameter
		 :AA-fear-vis-affect-hook
		 :default-value nil
		 :valid-test (lambda (x) (symbolp x)))
		(define-parameter
		 :AA-seeking-vis-affect-hook
		 :default-value nil
		 :valid-test (lambda (x) (symbolp x)))
		(define-parameter
		 :AA-arous-util-error-switch
		 :default-value nil)
		(define-parameter
		 :AA-max-util-noise
		 :default-value 1)
		(define-parameter
		 :AA-max-util-thresh
		 :default-value 0.5)
		(define-parameter
		 :AA-nom-util-noise
		 :default-value 0.01)
		(define-parameter
		 :AA-nom-util-thresh
		 :default-value -0.5)
		(define-parameter
		 :AA-max-dm-noise
		 :default-value 0.4)
		(define-parameter
		 :AA-nom-dm-noise
		 :default-value 0.01)
		(define-parameter
		 :AA-chunk-arousal-switch
		 :default-value nil)
		(define-parameter
		 :AA-dm-noise-switch
		 :default-value nil)
		(define-parameter
		 :AA-util-noise-switch
		 :default-value nil)
		(define-parameter
		 :AA-util-thresh-scalar
		 :default-value 1
		 :valid-test (lambda (x) (numberp x)))
		(define-parameter
 		 :AA-enabled
 		 :default-value nil
 		 :valid-test (lambda (x) (symbolp x)))
		(define-parameter
		 :AA-util-noise-scalar
		 :default-value 1
		 :valid-test (lambda (x) (numberp x)))
		(define-parameter
		 :AA-max-arous
		 :default-value 2
		 :valid-test (lambda (x) (numberp x))))
	:version "0.1"
	:documentation "Affective Associations Module"
	:creation 'create-AA-module
	:delete 'delete-AA-module
	:reset 'reset-AA-module
	:request 'AA-module-requests
	:params 'AA-module-params
	:query 'AA-module-query
		:notify-on-clear 'AA-clear-buffers)
