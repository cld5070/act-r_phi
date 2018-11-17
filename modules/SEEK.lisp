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
;;;Based on SEEKing affective system (Panksepp, J. and L. Biven (2012). The Archeology of Mind: Neuroevoloutionary Origins of Human Emotions. New York, NY, W.W. Norton & Company.)
;;;12/11/2012 - Module Created
;;;12/17/2012 - Two buffers, sFunction & sValue
;;; S-vals
;;;-If a homeostatic SEEKING function, give respective name to sFunction buffer. If the goal function is in sFunction, use goal name (from goal buffer)
;;;-Figure out how to compute "Goal" value in the Update-goalVal function
;;; - May want to change something in goal buffer when big loss hits. imaginal-action buffer seems like it may be correct since parietal activity is seen during outcome phase
;;; - Make it so "Goal" value can be determined using "hook"

;;SEEKING value system (Default) goes [0,1



;;SEEKing functions
(defun update-SEEKING (SEEKING)
	;;;Should alter this to function like FEAR module and have all of these scheduled
	;;as periodic functions that operate in parallel
	(update-hunger SEEKING)
	(update-thirst SEEKING)
	(update-skinTemp SEEKING)
	(let*
		((winnerVal (highest-SEEKING SEEKING))
		(sFChunk (if (chunk-p-fct (car winnerVal))
			(mod-chunk-fct (car winnerVal) (list 'sFun (car winnerVal) 'sValue (cadr winnerVal)))
			(car (define-chunks-fct (list (list (car winnerVal) 'isa 'sChunk 'sFun (car winnerVal) 'sValue (cadr winnerVal))))))))
		(setf (SEEKING-winning-sFunction SEEKING) (car winnerVal))
		(setf (SEEKING-winning-sValue SEEKING) (* (cadr winnerVal) (exp (SEEKING-max-seeking-val SEEKING))))
	 (schedule-overwrite-buffer-chunk 'sFunction sFChunk (SEEKING-internal-delay SEEKING) :module 'SEEKING :output nil)))

(defun highest-SEEKING (S)
	(let ((curr-val nil)
	 (curr-func nil))
		(maphash
			#'(lambda (k v)
				(when (and (not (eq v nil)) (or (not curr-val) (> (abs v) (abs curr-val))))
				 (setf curr-val v)
				 (setf curr-func k)))
			(SEEKING-S-vals S))
		(list curr-func curr-val)))

(defun update-hunger (S)
	(setf (gethash 'hungerVal (SEEKING-S-vals S)) (SEEKING-max-neg-incentive S)))

;;Thirst Value (tVal) = (osmo(mOsm/L) - base-osmo(mOsm/L)) / ((base-osmo(mOsm/L) - Thirstmin) or (ThirstMax - base-osmo(mOsm/L)))
;;tVal = ((cube-root(10) * tVal)^3) + NOISE
(defun update-thirst (S)
 (if (not (phys-module-enabled (get-module physio))) 0
	 (let*
		((checkOsmoVal (car(get-phys-vals nil (list '("OsmBody.CellWall(mOsm/L)")))))
		;(checkOsmoVal (car(get-phys-vals nil (list '("OsmBody.[Osm(mOsm/L)]-CellWall")))))
		(osmoVal (if (or (eq checkOsmoVal nil)
					(eq checkOsmoVal 0) (eq (cadar checkOsmoVal) nil)) 0
					(read-from-string (cadar checkOsmoVal))))
		(checkOsmoVal-base (car(get-phys-vals t (list '("OsmBody.CellWall(mOsm/L)")))))
		;(checkOsmoVal-base (car(get-phys-vals nil (list '("OsmBody.[Osm(mOsm/L)]-CellWall")))))
		(base-osmoVal (if (or (eq checkOsmoVal-base 0)
			(eq checkOsmoVal-base nil) (eq (cadar checkOsmoVal-base) nil)) 0
			(read-from-string (cadar checkOsmoVal-base))))
		(tVal nil)
		(subjMult (SEEKING-subj-thirst S))
			(tMult (if (SEEKING-thirst-multiplier S) (SEEKING-thirst-multiplier S) 1))
		(max-reward (if (SEEKING-max-seeking-val S) (SEEKING-max-seeking-val S) 1))
		(noise-val (if (SEEKING-hom-noise S) (act-r-noise (* (SEEKING-hom-noise S) max-reward)) 0)))

		(declare (ignore tMult))
		(declare (ignore subjMult))
		;(print checkOsmoVal)
		(if (> osmoVal (SEEKING-thirst-max S))
			(setf tVal 1)
			(if (< osmoVal (SEEKING-thirst-min S))
				(setf tVal -1)
				(if (<= osmoVal base-osmoVal)
					(setf tVal (/ (- osmoVal base-osmoVal) (- base-osmoVal (SEEKING-thirst-min S))))
					(setf tVal (/ (- osmoVal base-osmoVal) (- (SEEKING-thirst-max S) base-osmoVal))))))
		;(print (list base-osmoVal osmoVal (* subjMult tVal)))

		;;Could use log-type curve to convert tVal to thirst, however I will just make a linear conversion w/ added noise for now
		;;(setf tVal (* tMult (+ (expt  (* 2.1544 tVal) 3) noise-val)))
		;;Since BICA (2013) we've changed this to include an offset of 1/e^max_reward so a value of 0 serves as the line between positive & negative affect (in terms of value)
		;(setf tVal (* tMult (+ (* subjMult (+ (* tVal max-reward) (exp max-reward))) noise-val)))

		(setf tVal (+ tVal noise-val))

		;(print tVal)

		(setf tVal (exp (* (- tval 1) max-reward)))

		(when (> tVal max-reward) (setf tVal max-reward))
		(when (< tVal (SEEKING-max-neg-incentive S)) (setf tVal (SEEKING-max-neg-incentive S)))

		(setf (gethash 'thirstVal (SEEKING-S-vals S)) tVal)
		tVal
	 )))

(defun update-skinTemp (S)
	(setf (gethash 'skinTempVal (SEEKING-S-vals S)) (SEEKING-max-neg-incentive S)))

;;Goal functions must return numerical value
(defun update-goalVal ()
 (let* ((S (get-module SEEKING))
		 (fcn-list (SEEKING-goal-fcns S))
		 (fcn-name-list (get-fcn-names fcn-list))
		 (fcn-id-list (get-fcn-ids fcn-list))
		 (winning-val nil)
		 (curr-val nil))
 (when fcn-list
	(progn
		(dotimes (i (length fcn-name-list))
			(setf curr-val (funcall (nth (- i 1) fcn-name-list) S))
			(setf (gethash (nth (- i 1) fcn-id-list) (SEEKING-S-vals S)) curr-val)
			(when (or (not winning-val) (> curr-val winning-val))
				 (setf winning-val curr-val)))))))

;;;;;;!!!Need to check this function later, it seems we probably should
;;;;;; be using the time-np and func-np vars
;;Update the SEEKING value associated with a production
(defun update-prod-SEEKING-val (prod)
 (let* ((s-val (SEEKING-winning-sValue (get-module SEEKING)))
		 (val 0)
		 (val-NP 0)
		 (time-NP 0)
		 (func-NP nil)
		 (k (SEEKING-k (get-module SEEKING)))
		 (alpha (SEEKING-alpha (get-module SEEKING)))
		 (curr-prod-val (if (numberp (production-sValue prod)) (production-sValue prod) 0))
		 (curr-prod-func (production-sFunction prod))
		 (util-hist (reverse (utility-history (get-module utility)))))
	(dolist (p-t util-hist)
	 (when (eq prod (car p-t))
	(let ((g (/ 1 (+ 1 (* k (- time-NP (ms->seconds (cdr p-t))))))))
	(setf val (+ (log s-val) (* g val-np))))
	(return))
	 (if (and (numberp (production-sValue (car p-t))) (not (<= (production-sValue (car p-t)) 0)))
		(progn (setf val-NP (production-sValue (car p-t))) (setf func-NP (production-sFunction (car p-t))))
		(setf val-NP 0))
	 (setf time-np (ms->seconds (cdr p-t))))
	 (when (not curr-prod-func) (setf (production-sFunction prod) (SEEKING-winning-sFunction (get-module SEEKING))))
	 (setf (production-sValue prod) (+ curr-prod-val (* alpha (- val curr-prod-val))))))

;;(Utility offset) Function that adds representation of modulating affect.
;Thus, here we dynamically adjust the rule utility based on the current state of the SEEKING system.
;we want log(k-s) <= max-reward so k-s <= e^(max-reward). So k-s=sValue*e^(max-reward) s.t. sValue [0,1]
;Think about making floor of this outcome ln(0.01)
;Why not just add incentive salience to every production with (all/or) SEEKING functions; why is SEEKING winner-take-all?...probably shouldn't be
;We should just go through list of sFunctions and do this
(defun add-incent-sal (production)
	(let* ((s (get-module SEEKING))
			(affect-val 0)
			(k-s (if (SEEKING-winning-sValue s)
				(SEEKING-winning-sValue s) (/ 1 (exp 1)))))
	 (when (and (production-sFunction production) (equal (production-sFunction production) (SEEKING-winning-sFunction s)))
		(update-prod-SEEKING-val production)
		(if (<= k-s (SEEKING-max-neg-incentive s))
		 (+ affect-val (log (* (SEEKING-max-neg-incentive s) (exp (SEEKING-max-seeking-val s))))) (+ affect-val (log k-s))))))


;;intialize hash-table for SEEKING variables
(defun initialize-SEEKING-hash (nLHash nameList)
 (loop
	for nL in nameList
	do (progn ;(print nL)
			(setf (gethash nL nLHash) 0))))

(defun schedule-SEEKING-events ()
	;This needs to go later and phys-module-enabled should only control homeostatic-based affects (and if not enabled, basically just assign value of 0 to those SEEKING functions)
	;(when (phys-module-enabled (get-module physio))
	(schedule-event 0.01 'initialize-SEEKING :priority :max :details "Initialize SEEKING module" :module 'SEEKING :maintenance t :maintenance t)
)

;;Gets list of function-ids from list of functions (used to affective attachment of proc/decl mems)
(defun get-fcn-ids (fcns)
 (let ((id-list nil))
	(dotimes (i (length fcns))
	 (when (oddp (- i 1))
		(setf id-list (append id-list (list (nth i fcns))))))))

;;Gets list of function names from list of functions (used to affective attachment of proc/decl mems)
(defun get-fcn-names (fcns)
 (let ((name-list nil))
	(dotimes (i (length fcns))
	 (when (evenp (- i 1))
		(setf name-list (append name-list (list (nth i fcns))))))))

;;ACT-R Module functions
(defstruct SEEKING

 (busy nil)
 esc

 ;These add to the corresponding functional values
 (hunger-multiplier nil)
 (thirst-multiplier nil)
 (skinTemp-multiplier nil)
 (goal-multiplier nil)

 ;Values for physiology-based drives (used in equations to determine the highest potential change before physiological collapse)
 (thirst-max 336)
 (thirst-min 245)

 (winning-sFunction nil)
 (winning-sValue nil)

 (sThirst-val 0)
 (sHunger-val 0)
 (sSkinTemp-val 0)

 ;Noise values for SEEKING-values (paramaters)
 (hom-noise nil)
 (goal-noise nil)

 ;Used to hold SEEKING values from previous states and baseline phys values
 (S-vals (make-hash-table))
 (base-S-vals (make-hash-table))

 ;Some parameter values
 (delay 0.1)
 (util-offset nil)
 (max-seeking-val 1)
 (subj-thirst 1.451)
 (max-neg-incentive 0.001)

 ;This value is used to determine the delay for the SEEKing buffers to be updated after the update SEEKing function has completed
 ;(we use a variable here on the off chance it someone may wish to change it)
 (internal-delay 0.05)

 ;parameters used to update a production's SEEKING value
 (k 0.2)
 (alpha 0.01)

 ;SEEKING goal functions
 (goal-fcns nil)

 ;SEEKING module switch
 (enabled nil)
)

(defun create-SEEKING-module (model-name)
 (declare (ignore model-name))
 (make-SEEKING))

(defun initialize-SEEKING ()
 (when (SEEKING-enabled (get-module SEEKING))
	(chunk-type sChunk sFun sValue)
	;Use different Utility Hook
	(let* ((S (get-module SEEKING
		))
		(goal-fcns (SEEKING-goal-fcns S)))
	 (if goal-fcns
	 (initialize-SEEKING-hash (SEEKING-S-vals S) (flatten (list 'hungerVal 'thirstVal 'skinTempVal 'assocVal (get-fcn-ids goal-fcns))))
	 (initialize-SEEKING-hash (SEEKING-S-vals S) (list 'assocVal 'hungerVal 'thirstVal 'skinTempVal 'assocVal)))
	 (initialize-SEEKING-hash (SEEKING-base-S-vals S) (list 'bL-assocVal 'bL-hungerVal 'bL-thirstVal 'bL-skinTempVal 'bL-goalVal))
	 (schedule-periodic-event (SEEKING-delay S) 'update-SEEKING :details "Keep the SEEKING module updated" :params (list S) :module 'SEEKING :initial-delay 0.05 :priority 5 :maintenance t)
	 (when (SEEKING-util-offset S) (sgp :utility-offsets add-incent-sal))))
)

(defun delete-SEEKING-module (SEEKING)
 (declare (ignore SEEKING))
)

(defun reset-SEEKING-module (SEEKING)
	(declare (ignore SEEKING))
	(schedule-event 0.01 'schedule-SEEKING-events :priority :max :details "Schedule any (initial) SEEKING events" :module 'SEEKING :maintenance t)
)

(defun SEEKING-query (SEEKING buff slot val)
 (case slot
	(state
	 (case val
		(error nil)
		(busy (SEEKING-busy SEEKING))
		(free (not (SEEKING-busy SEEKING)))
		(t (print-warning "Bad state query to ~s buffer" buff)))
	 )
	(t (print-warning "Invalid slot ~s with query to buffer ~s" slot buff))))

;;Requests current 'winning' function in SEEKING module
(defun SEEKING-function-request (SEEKING spec)
 (declare (ignore SEEKING))
 (declare (ignore spec)))

;;Used to request value for specific function
(defun SEEKING-request (SEEKING spec)
 (declare (ignore SEEKING))
 (declare (ignore spec)))

(defun free-SEEKING-module (SEEKING)
 (setf (SEEKING-busy SEEKING) nil))

(defun SEEKING-requests (SEEKING buff spec)
 (case buff
	(sFunction
	 (SEEKING-function-request SEEKING spec))
		(sRequest
	 (SEEKING-request SEEKING spec))
	(t
	 (let*
		((chunkType (chunk-spec-chunk-type spec))
		 (hasVal (slot-in-chunk-spec-p spec 'value))
		 (val (when (chunk-spec-slot-spec spec 'value))))
		(if (eq chunkType 'SEEKING-output)
		 (if hasVal
				(if (= (length val) 1)
		 (if (eq (caar val) '=)
			(model-output "Value: ~s" (caddar val))
			(model-warning "Invalid slot modifier ~s" (caar val)))
				(model-warning "Value slot specified multiple times"))
			 (model-warning "Value slot missing in output request"))
			(model-warning "Bad chunk-type in request to output")))))
)

(defun SEEKING-params (SEEKING param)
 (if (consp param)
	(case (car param)
		(:SEEK-hom-noise (setf (SEEKING-hom-noise SEEKING) (cdr param)))
		(:SEEK-util-offset (setf (SEEKING-util-offset SEEKING) (cdr param)))
		(:SEEK-max-rew (setf (SEEKING-max-seeking-val SEEKING) (cdr param)))
		(:SEEK-delay (setf (SEEKING-delay SEEKING) (cdr param)))
		(:SEEK-st-mult (setf (SEEKING-subj-thirst SEEKING) (cdr param)))
		(:SEEK-k (setf (SEEKING-k SEEKING) (cdr param)))
		(:SEEK-alpha (setf (SEEKING-alpha SEEKING) (cdr param)))
		(:SEEK-goal-list (setf (SEEKING-goal-fcns SEEKING) (cdr param)))
		(:SEEK-enabled (setf (SEEKING-enabled SEEKING) (cdr param))))
	(case param
		(:SEEK-hom-noise (SEEKING-hom-noise SEEKING))
		(:SEEK-util-offset (SEEKING-util-offset SEEKING))
		(:SEEK-max-rew (SEEKING-max-seeking-val SEEKING))
		(:SEEK-delay (SEEKING-delay SEEKING))
		(:SEEK-st-mult (SEEKING-subj-thirst SEEKING))
		(:SEEK-k (SEEKING-k SEEKING))
		(:SEEK-alpha (SEEKING-alpha SEEKING))
		(:SEEK-goal-list (SEEKING-goal-fcns SEEKING))
		(:SEEK-enabled (SEEKING-enabled SEEKING)))
))

(define-module-fct 'SEEKING
	'(sFunction sRequest)
	(list
	 (define-parameter
		:hungerMult
		:default-value nil)
	 (define-parameter
		:thirstMult
		:default-value nil)
	 (define-parameter
		:skinTempMult
		:default-value nil)
	 (define-parameter
		:goalMult
		:default-value nil)
	 (define-parameter
		:esc
		:owner nil)
	 (define-parameter
		:SEEK-hom-noise
		:valid-test (lambda (x) (or (numberp x) (not x)))
		:default-value nil)
	 (define-parameter
		:SEEK-util-offset
		:valid-test (lambda (x) (or (eq x t) (not x)))
		:default-value nil)
	 (define-parameter
		:SEEK-max-rew
		:valid-test (lambda (x) (or (numberp x) (not x)))
		:default-value 2)
	 (define-parameter
		:SEEK-delay
		:valid-test (lambda (x) (numberp x))
		:default-value 0.1)
	 (define-parameter
		:SEEK-st-mult
		:valid-test (lambda (x) (numberp x))
		:default-value 1.451)
	 (define-parameter
		:SEEK-k
		:valid-test (lambda (x) (numberp x))
		:default-value 0.2)
	 (define-parameter
		:SEEK-alpha
		:valid-test (lambda (x) (numberp x))
		:default-value 0.01)
	 (define-parameter
		:SEEK-goal-list
		:valid-test (and (lambda (x) (listp x)) (lambda (x) (evenp (length x))))
		:default-value nil)
	 (define-parameter
		:SEEK-enabled
		:default-value nil))
	:version "0.1"
	:documentation "SEEKING Module (from Primary-Process Affect theory)"
	:creation 'create-SEEKING-module
	:delete 'delete-SEEKING-module
	:reset 'reset-SEEKING-module
	:request 'SEEKING-requests
	:params 'SEEKING-params
	:query 'SEEKING-query)
