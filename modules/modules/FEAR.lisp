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
;;;Based on FEAR affective system (Panksepp, J. and L. Biven (2012). The Archeology of Mind: Neuroevoloutionary Origins of Human Emotions. New York, NY, W.W. Norton & Company.)
;;;12/11/2012 - Module Created
;;;12/17/2012 - Two buffers, Function & Value 

(suppress-extension-warnings)

;;Prods with these values are more likely to be called (U+log(k-f))
;;Currently expected to be 'cond, 'uncond, or 'noupdate

(unsuppress-extension-warnings)

;;FEAR functions

;Checks affective-associations module for FEAR related stimuli
;The value in-turn should be associated to declarative memories via a hebbian learning algorithm (using fFunction and fValue)
;;Now we can make this decay over time
(defun update-association-FEAR (FEAR)
 (let ((val 0))
  (setf val (- (+ (FEAR-val-offset FEAR) (FEAR-val-association FEAR)) (* (FEAR-decay-association FEAR) (FEAR-val-association FEAR))))
  (when (< val (FEAR-max-neg-FEAR-val FEAR)) (setf val (FEAR-max-neg-FEAR-val FEAR)))
  (setf (FEAR-val-association FEAR) val))) 

;Update FEAR value related to visual stimuli
; -haven't found evidence for this direct connection, but included as a 0 function in the case there are direct functional/structural connections I've missed
(defun update-vision-FEAR (FEAR)
 (let ((val 0))
  (setf val (- (+ (FEAR-val-offset FEAR) (FEAR-val-vision FEAR)) (* (FEAR-decay-vision FEAR) (FEAR-val-vision FEAR))))
  (when (< val (FEAR-max-neg-FEAR-val FEAR)) (setf val (FEAR-max-neg-FEAR-val FEAR)))
  (setf (FEAR-val-vision FEAR) val)))

;Update FEAR value related to aural stimuli
(defun update-audition-FEAR (FEAR)
 (let ((val 0))
  (setf val (- (+ (FEAR-val-offset FEAR) (FEAR-val-audition FEAR)) (* (FEAR-decay-audition FEAR) (FEAR-val-audition FEAR))))
  (when (< val (FEAR-max-neg-FEAR-val FEAR)) (setf val (FEAR-max-neg-FEAR-val FEAR)))
  (setf (FEAR-val-audition FEAR) val)))

;;This function probably should be moved to physio module (will in future once we get good representation for tissue damage)
(defun nociception-shock (FEAR shock-val)
 (setf (FEAR-val-nociception FEAR) shock-val))

;Checks internal physio module for (innate) FEAR stimuli (i.e., immediate potential tissue damage)
(defun update-nociception-FEAR (FEAR)
 (let ((val 0))
  (setf val (- (+ (FEAR-val-offset FEAR) (FEAR-val-nociception FEAR)) (* (FEAR-decay-nociception FEAR) (FEAR-val-nociception FEAR))))
  (when (< val (FEAR-max-neg-FEAR-val FEAR)) (setf val (FEAR-max-neg-FEAR-val FEAR)))
  (setf (FEAR-val-nociception FEAR) val)))

(defun update-FEAR-vals (f)
 (update-nociception-FEAR f)
 (update-vision-FEAR f)
 (update-audition-FEAR f)
 (update-association-FEAR f))

 
 ;;;;Need to check with this later to make sure it works correctly w/ new Stress system
;Function to update the state (value) of the FEAR system (module)
(defun update-FEAR (FEAR)
 (let ((val-FEAR (FEAR-val-nociception FEAR))
	  (func-FEAR 'nociception-defensive))
	  
  (when (< val-FEAR (FEAR-val-audition FEAR))
   (setf val-FEAR (FEAR-val-audition FEAR))
   (setf func-FEAR 'audition-defensive))
  (when (< val-FEAR (FEAR-val-vision FEAR))
   (setf val-FEAR (FEAR-val-vision FEAR))
   (setf func-FEAR 'vision-defensive))
  (when (< val-FEAR (FEAR-val-association FEAR))
   (setf val-FEAR (FEAR-val-association FEAR))
   (setf func-FEAR 'association-defensive))
 
  (if (<= (FEAR-val-FEAR FEAR) (FEAR-max-neg-FEAR-val FEAR)) 
   (progn (setf (FEAR-val-FEAR FEAR) (* (FEAR-max-neg-FEAR-val FEAR) (exp (FEAR-max-FEAR-val FEAR))))
    (setf (FEAR-arousal FEAR) 0)
	(setf (FEAR-func FEAR) nil))
   (progn (setf (FEAR-val-FEAR FEAR) (* val-FEAR (exp (FEAR-max-FEAR-val FEAR))))
    (setf (FEAR-arousal FEAR) (log (FEAR-val-FEAR FEAR)))
    (setf (FEAR-func FEAR) func-FEAR)))
  (when (and (FEAR-epi-conn FEAR) (phys-module-enabled (get-module physio)))
   (set-phys-vals 
    (list (list "SympsCNS.ClampSwitch" 1)
		  (list "SympsCNS.ClampLevel" (* val-FEAR (phys-module-max-integ-nerve-activation (get-module physio)))))))))

(defun initialize-FEAR ()
 (let ((FEAR (get-module FEAR)))
  (when (FEAR-enabled (get-module FEAR))
	  ;(schedule-periodic-event (FEAR-update-delay FEAR) 'update-FEAR-vals :priority :max :details "Update internal FEAR values" :params (list FEAR) :module 'FEAR :output nil)
	  (schedule-periodic-event (FEAR-update-delay-association FEAR) 'process-run-function :priority :max :details "Update association fear value" :params (list "Update FEAR association value" 'update-association-FEAR FEAR) :module 'FEAR :output nil :maintenance t)
	  (schedule-periodic-event (FEAR-update-delay-vision FEAR) 'process-run-function :priority :max :details "Update vision fear value" :params (list "Update FEAR vision value" 'update-vision-FEAR FEAR) :module 'FEAR :output nil :maintenance t)
	  (schedule-periodic-event (FEAR-update-delay-audition FEAR) 'process-run-function :priority :max :details "Update audition fear value" :params (list "Update FEAR audition value" 'update-audition-FEAR FEAR) :module 'FEAR :output nil :maintenance t)
	  (schedule-periodic-event (FEAR-update-delay-nociception FEAR) 'process-run-function :priority :max :details "Update nociception fear value" :params (list "Update FEAR nociception value" 'update-nociception-FEAR FEAR) :module 'FEAR :output nil :maintenance t)
	  (schedule-periodic-event (FEAR-delay FEAR) 'update-FEAR :priority :max :details "Update FEAR module" :params (list FEAR) :module 'FEAR :output nil :maintenance t)
	  (when (FEAR-util-offset FEAR)
		 (sgp :utility-offsets add-FEAR-util)))))

;;Update the FEAR value associated with a production
(defun update-prod-FEAR-val (prod)
 (let* ((f (get-module FEAR))
       (f-val (FEAR-val-FEAR f))
	   (val 0)
	   (val-NP 0)
	   (func-NP nil)
	   (time-NP 0)
	   (k (FEAR-k f))
	   (alpha (FEAR-alpha f))
	   (curr-prod-val (if (numberp (production-fValue prod)) (production-fValue prod) 0))
	   (curr-prod-func (production-fFunction prod))
	   (util-hist (reverse (utility-history (get-module utility)))))
  (dolist (p-t util-hist)
   (when (eq prod (car p-t))
	(let ((g (/ 1 (+ 1 (* k (- time-NP (ms->seconds (cdr p-t))))))))
	(setf val (+ (log f-val) (* g val-np))))
	(return))
   (if (and (numberp (production-fValue (car p-t))) (not (= (production-fValue (car p-t)) 0))) 
    (progn (setf val-NP (production-fValue (car p-t))) (setf func-NP (production-fFunction (car p-t))))
    (progn (setf val-NP 0) (setf func-NP nil)))
    (setf time-NP (ms->seconds (cdr p-t))))
   (setf (production-fValue prod) (+ curr-prod-val (* alpha (- val curr-prod-val))))
   (when (not curr-prod-func) (setf (production-fFunction prod) (FEAR-func f)))
   (if (and func-NP (< (production-fValue prod) val-np)) (setf (production-fFunction prod) func-NP))))

;;Add FEAR system utility offset
;we want log(k-f) <= max-reward so k-f <= e^(max-reward). So k-f=fValue*e^(max-reward) s.t. sValue [1/e,1]
(defun add-FEAR-util (production)
 (let* ((affect-val 0)
		  (f (get-module FEAR))
		  (k-f (if (FEAR-val-FEAR f) 
			    (FEAR-val-FEAR f) (/ 1 (exp 1)))))
	 (when (production-fFunction production)
	  (when (not (or (search "uncond" (symbol-name (production-fFunction production))  :test #'char-equal) (search "noupdate" (symbol-name (production-fFunction production)) :test #'char-equal))) (update-prod-FEAR-val production))
	  (+ affect-val (* (FEAR-mult f) (log k-f))))))

;;ACT-R Module functions
(defstruct FEAR
	(update-delay-association 0.016)
	(update-delay-vision 0.016)
	(update-delay-audition 0.008)
	(update-delay-nociception 0.008)
    (update-delay 0.008)
    (delay 0.004)

	(arousal 0)
  
    (busy nil)
	
	(val-offset 0)

    (mult 1)
	
	(decay-association 0.2)
	(decay-vision 0.2)
	(decay-nociception 0.2)
	(decay-audition 0.2)

    ;Times used to upddate values whenever value is used
    (val-vision-update-time)
    (val-audition-update-time)
    (val-nociception-update-time)

	;Switch for the fear utility-offset
	(util-offset nil)


	(val-FEAR 0)
	(func nil)

    (max-FEAR-val 1)
	
	(val-association (/ 1 (exp 1)))
	(val-vision (/ 1 (exp 1)))
	(val-audition (/ 1 (exp 1)))
	(val-nociception (/ 1 (exp 1)))

	;ln(max-neg-fear-val)
    (max-neg-FEAR-val (/ 1 (exp 1)));(eval (/ 1 (exp max-FEAR-val))))

	;parameters for FEAR-production update value
	(k 0.2)
    (alpha 0.01)

	;Threshold for FEAR system to cause physiological change
	(phys-thresh nil)

    ;Determines whether we update ANS activation based on FEAR system value
    (epi-conn nil)
	
	;module enabled switch
	(enabled nil))

(defun create-FEAR-module (model-name)
 (declare (ignore model-name))
 (make-FEAR))

(defun delete-FEAR-module (FEAR)
 (declare (ignore FEAR)))

(defun reset-FEAR-module (FEAR)
	(declare (ignore FEAR))
	(schedule-event 0.01 'initialize-FEAR :details "Initialize FEAR module" :module 'FEAR :priority :max))

(defun FEAR-module-query (FEAR buff slot val)
 (case slot
	(state
	 (case val
	  (error nil)
	  (busy (FEAR-busy FEAR))
	  (free (not (FEAR-busy FEAR)))
	  (t (print-warning "Bad state query to ~s buffer" buff))))
	(t (print-warning "Invalid slot ~s with query to buffer ~s" slot buff))))

(defun FEAR-module-value-request (FEAR spec)
 (declare (ignore FEAR))
 (declare (ignore spec)))

(defun FEAR-module-function-request (FEAR spec)
 (declare (ignore FEAR))
 (declare (ignore spec)))

(defun FEAR-module-requests (FEAR buff spec)
 (case buff 
	(fFunction
	 (FEAR-module-function-request FEAR spec))
	(fValue 
	 (FEAR-module-value-request FEAR spec))
	(t
	 (let* 
	  ((chunkType (chunk-spec-chunk-type spec))
		 (hasVal (slot-in-chunk-spec-p spec 'value))
		 (val (when (chunk-spec-slot-spec spec 'value))))
	  (if (eq chunkType 'FEAR-output)
		 (if hasVal
      (if (= (length val) 1)
			 (if(eq (caar val) '=)
			  (model-output "Value: ~s" (caddar val))
				(model-warning "Invalid slot modifier ~s" (caar val)))
       (model-warning "Value slot specified multiple times"))
      (model-warning "Value slot missing in output request"))
     (model-warning "Bad chunk-type in request to output"))))
 )
)

(defun FEAR-module-params (FEAR param)
 (if (consp param)
  (case (car param)
   (:FEAR-util-offset (setf (FEAR-util-offset FEAR) (cdr param)))
   (:FEAR-k (setf (FEAR-k FEAR) (cdr param)))
   (:FEAR-alpha (setf (FEAR-alpha FEAR) (cdr param)))
   (:FEAR-mult (setf (FEAR-mult FEAR) (cdr param)))
   (:FEAR-epi-conn (setf (FEAR-epi-conn FEAR) (cdr param)))
   (:FEAR-enabled (setf (FEAR-enabled FEAR) (cdr param))))
  (case param
   (:FEAR-util-offset (FEAR-util-offset FEAR))
   (:FEAR-k (FEAR-k FEAR))
   (:FEAR-alpha (FEAR-alpha FEAR))
   (:FEAR-mult (FEAR-mult FEAR))
   (:FEAR-epi-conn (FEAR-epi-conn FEAR))
   (:FEAR-enabled (FEAR-enabled FEAR)))))

(define-module-fct 'FEAR
	'(fFunction fValue)
	(list
	 (define-parameter
	   :association-delay
	 )
	 (define-parameter
	   :vision-delay
	 )
	 (define-parameter
	   :audition-delay
	 )
	 (define-parameter
	   :nociception-delay
	 )
	 (define-parameter
	   :FEAR-offset
	 )
	 (define-parameter
	   :FEAR-util-offset
	   :default-value nil)
	 (define-parameter
	   :FEAR-k
	   :default-value 0.2)
     (define-parameter
	   :FEAR-alpha
	   :default-value 0.01)
	 (define-parameter
	   :FEAR-mult
	   :default-value 1)
     (define-parameter
	   :FEAR-epi-conn
	   :default-value nil)
	 (define-parameter
	   :FEAR-enabled
	   :default-value nil))
	:version "0.1"
	:documentation "FEAR Module (from Jaak Panksepp)"
	:creation 'create-FEAR-module
	:delete 'delete-FEAR-module
	:reset 'reset-FEAR-module
	:request 'FEAR-module-requests
	:params 'FEAR-module-params
	:query 'FEAR-module-query)

