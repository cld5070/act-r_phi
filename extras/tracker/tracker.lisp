;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : John Anderson and Dan Bothell
;;; Copyright   : (c) 2017
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : tracker.lisp
;;; Version     : 2.1
;;; 
;;; Description : Module to create "trackers" which can learn a mapping of values
;;;             : in monitored slots for "good" and/or "bad" events to an output
;;;             : value that is updated in another slot. 
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;; 2017.06.27 Dan [1.0a]
;;;             : * First pass at the module.
;;; 2017.06.29 Dan
;;;             : * Moved the module-definition below the request function to
;;;             :   avoid a problem on first loading.
;;; 2017.07.03 John
;;;             : * Fix potential rounding bug in generate-tracker-choices.
;;;             : * Update the trackers at random intervals.
;;;             : * Separately track good and bad events to suport recalculating
;;;             :   with changing weights.
;;;             : * It's possible to discount past experiences by their delay.
;;; 2017.07.05 Dan
;;;             : * Remove control-buffer from the chunk-type and add delay.
;;;             : * Add a global parameter for the delay setting :update-delay.
;;;             : * Added parameters :tracker-decay-method and :tracker-decay
;;;             :   to handle the discouting.
;;;             : * Added get-tracker and print-tracker-stats commands.
;;;             : * Started work on a modification request to allow one to change
;;;             :   a tracker's current settings.
;;; 2017.07.06 Dan
;;;             : * Switching the buffer to a :search-copy buffer to allow the
;;;             :   model to be able to access tracker info and to provide a 
;;;             :   way to specify a tracker to modify with a * action (the one
;;;             :   that's in the buffer).
;;; 2017.07.07 Dan
;;;             : * Need to separate the weight from the scaleing function for
;;;             :   good and bad because they're applied at different times now.
;;;             : * The modification requests should work now.
;;; 2017.07.10 Dan
;;;             : * Fixed some issues that tripped up the older CCL compiler, but
;;;             :   interestingly didn't cause run-time errors in ACL or newer
;;;             :   CCLs for the test model.
;;;             : * Fixed a bug in the parameter handler and a typo in the docs
;;;             :   for :tracker-decay-method.
;;; 2017.08.14 Dan
;;;             : * Protect the exp calculation against over/under flow.
;;; 2017.09.11 Dan
;;;             : * Make sure good/bad weights get the default value even if 
;;;             :   there isn't a slot for the action because the current
;;;             :   calculation requires it.
;;; 2018.03.12 Dan
;;;             : * The tracker requests schedule the schedule-mod-buffer-chunk
;;;             :   action so that buffers (like goal) can put the chunk to be
;;;             :   modified in place first.
;;; 2018.04.04 Dan  [1.1]
;;;             : * Update to use with ACT-R 7.x.  The only change necessary is
;;;             :   to remove the current-meta-process calls to determine if there
;;;             :   is an event hook since there's only one now, but still having
;;;             :   it do the lookup for a constant name since that may come back
;;;             :   at some point.
;;;             :   May need to adjust the event detection code at some point
;;;             :   since the event, buffer, or chunk/mod details may contain
;;;             :   string based names instead of symbols down the line.
;;; 2018.04.10 Dan
;;;             : * Added some comments.
;;;             : * Cleaned up the update-tracker event so it has a module and
;;;             :   prints something reasonable in the trace (high detail only).
;;;             : * Allow all the scale values to be remote functions.
;;; 2018.04.13 Dan
;;;             : * Include the provide so it can be loaded using require-extra.
;;;             : * Change default for :control-count to 21.
;;;             : * Fix a bug in how it creates the dummy chunk when the control
;;;             :   buffer is empty at update time.
;;;             : * Allow print-tracker-stats to take a string and provide an
;;;             :   external command for it.
;;;             : * Print the values and probabilities with the stats.
;;; 2018.06.12 Dan
;;;             : * Only the external commands decode the string names.
;;; 2018.06.22 Dan
;;;             : * Call to purge-chunk should be purge-chunk-fct.
;;; 2018.06.25 Dan [2.1]
;;;             : * To differentiate the trackers for different ACT-R versions
;;;             :   switching this one to be v. 2 (and assume that the other
;;;             :   won't ever need to be changed from v. 1).
;;;             : * The update-event has to stay with the tracker instead of the
;;;             :   module since there can be more than one.
;;; 2018.06.26 Dan
;;;             : * Added a handler-case in tracker-calc-quad to avoid issues
;;;             :   with a divide-by zero which seems to happen more often than
;;;             :   it should because things shouldn't round that much -- this
;;;             :   shouldn't be zero but is in one case where it was caught:
;;;             :   (- 38.707016 (/ (* 28.701324 28.701324) 21.293)).
;;; 2018.08.21 Dan [2.2]
;;;             : * Actually make it thread safe with the simple approach of 
;;;             :   wrapping everything in a recursive lock (including all
;;;             :   access to any tracker structure values) except for the 
;;;             :   event counter since that's a between module value that 
;;;             :   needs its own lock.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; A tracker collects occurrences of good and/or bad events which are detected
;;; from slots in buffers (good and bad occur in different slots) and uses those
;;; to compute the best value for a control slot by estimating a quadratic 
;;; function from the occurrences and then using a softmax rule to pick the 
;;; next value.
;;;
;;; See the tracker documentation for details on how that process occurs.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; There is a tracker buffer associated with the module.  It is a searchable
;;; multi-buffer which holds all of the trackers which have been created.  It
;;; takes requests to create new trackers and modification requests to change
;;; the parameters of an existing tracker.
;;;
;;; A tracker request is made using the slots of this chunk-type:
;;; TRACK-OUTCOME <- TRACKER-PARAMS
;;;    CONTROL-SLOT
;;;    GOOD-BUFFER
;;;    BAD-BUFFER
;;;    CONTROL-SCALE
;;;    GOOD-SLOT
;;;    BAD-SLOT
;;;    GOOD-WEIGHT
;;;    BAD-WEIGHT
;;;    GOOD-SCALE
;;;    BAD-SCALE
;;;    MIN
;;;    MAX
;;;    CONTROL-COUNT
;;;    TEMP
;;;    SCALE
;;;    DELAY
;;;
;;; It must include a control-slot (which is unique among all the trackers
;;; which have been created) and at least one of good-slot or bad-slot (it
;;; may specify both).  All of the other values are optional and have default
;;; values which are set with parameters for the module.
;;;
;;; For each tracker created a chunk is added to the tracker buffer, and the 
;;; best way to access that in a production is by using the control-slot to find
;;; the appropriate chunk in the set.
;;;
;;; A tracker can be modified after its creation using a modification request
;;; with the slots from this chunk-type:
;;;
;;; TRACKER-MODIFICATION <- TRACKER-PARAMS
;;;    CONTROL-SCALE
;;;    GOOD-SLOT
;;;    BAD-SLOT
;;;    GOOD-WEIGHT
;;;    BAD-WEIGHT
;;;    GOOD-SCALE
;;;    BAD-SCALE
;;;    MIN
;;;    MAX
;;;    CONTROL-COUNT
;;;    TEMP
;;;    SCALE
;;;    DELAY
;;; 
;;; The tracker which is currently in the buffer will be updated with the new
;;; values provided.
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

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


(require-compiled "GOAL-STYLE-MODULE" "ACT-R-support:goal-style-module")

(defstruct tracker 
  control-buffer
  control-slot
  control-scale
  good-buffer
  good-slot
  good-weight
  (good-value 0)
  good-scale
  bad-buffer
  bad-slot
  bad-weight
  (bad-value 0)
  bad-scale
  min
  max
  delay
  setting
  choices
  statistics
  last-update
  init-time
  init-temp
  temperature
  scale-temp
  chunk
  update-event
  params ;; params are c,b,a for c + bx + ax^2
  )
  

(defclass tracker-module nil
  ((trackers :accessor trackers :initform nil)
   (initial-temp :accessor initial-temp)
   (temp-scale :accessor temp-scale)
   (default-control :accessor default-control)
   (default-control-scale :accessor default-control-scale)
   (default-values :accessor default-values)
   (default-count :accessor default-count)
   (default-delay :accessor default-delay :initform 10)
   (default-min :accessor default-min :initform 0)
   (default-max :accessor default-max :initform 1)
   (default-good-weight :accessor default-good-weight)
   (default-bad-weight :accessor default-bad-weight)
   (default-good-scale :accessor default-good-scale)
   (default-bad-scale :accessor default-bad-scale)
   (tracker-trace :accessor tracker-trace)
   (decay-method :accessor decay-method)
   (decay-param :accessor decay-param)
   (tracker-lock :accessor tracker-lock :initform (bt:make-recursive-lock "tracker"))
   (tracker-event-lock :accessor tracker-event-lock :initform (bt:make-lock "tracker-event") :allocation :class)
   (event-trackers :accessor event-trackers :initform nil :allocation :class)))


(defun create-tracker (model-name)
  (declare (ignore model-name))
  (let ((module (make-instance 'tracker-module)))
    (bt:with-lock-held ((tracker-event-lock module))
      (aif (find 'current-mp (event-trackers module) :key 'first)
           (incf (second it))
           (push (list 'current-mp 1 (add-post-event-hook 'tracker-check-for-buffer-changes)) (event-trackers module))))
    module))

(defun delete-tracker (module)
  (bt:with-lock-held ((tracker-event-lock module))
    (let ((event-tracker (find 'current-mp (event-trackers module) :key 'first)))
      (decf (second event-tracker))
      (when (zerop (second event-tracker))
        (delete-event-hook (third event-tracker))
        (setf (event-trackers module) (remove event-tracker (event-trackers module)))))))

(defun reset-tracker (module)
  (chunk-type tracker-params control-scale good-slot bad-slot good-weight bad-weight 
              good-scale bad-scale min max control-count temp scale delay)
  
  (suppress-warnings (chunk-type (tracker-modification (:include tracker-params))))
  
  (chunk-type (track-outcome (:include tracker-params)) control-slot good-buffer bad-buffer)
  
  (bt:with-recursive-lock-held ((tracker-lock module))
    (setf (trackers module) nil)))

(defun params-tracker (module param)
    (bt:with-recursive-lock-held ((tracker-lock module))
      (if (consp param)
          (case (car param)
            (:initial-temp (setf (initial-temp module) (cdr param)))
            (:temp-scale (setf (temp-scale module) (cdr param)))
            (:control-buffer (setf (default-control module) (cdr param)))
            (:control-scale (setf (default-control-scale module) (cdr param)))
            (:values-buffer (setf (default-values module) (cdr param)))
            (:control-count (setf (default-count module) (cdr param)))
            (:control-min (setf (default-min module) (cdr param)))
            (:control-max (setf (default-max module) (cdr param)))
            (:good-weight (setf (default-good-weight module) (cdr param)))
            (:bad-weight (setf (default-bad-weight module) (cdr param)))
            (:good-scale (setf (default-good-scale module) (cdr param)))
            (:bad-scale (setf (default-bad-scale module) (cdr param)))
            (:trace-tracker (setf (tracker-trace module) (cdr param)))
            (:update-delay (setf (default-delay module) (cdr param)))
            (:tracker-decay-method (setf (decay-method module) (cdr param))
                                   (when (cdr param)
                                     (unless (numberp (decay-param module))
                                       (sgp-fct (list :tracker-decay (case (cdr param) (exponential .99) (power .5))))))
                                   (cdr param))
            (:tracker-decay (setf (decay-param module) (cdr param))))
        (case param
          (:initial-temp (initial-temp module))
          (:temp-scale (temp-scale module))
          (:control-buffer (default-control module))
          (:control-scale (default-control-scale module))
          (:values-buffer (default-values module))
          (:control-count (default-count module))
          (:control-min (default-min module))
          (:control-max (default-max module))
          (:good-weight (default-good-weight module))
          (:bad-weight (default-bad-weight module))
          (:good-scale (default-good-scale module))
          (:bad-scale (default-bad-scale module))
          (:trace-tracker (tracker-trace module))
          (:update-delay (default-delay module))
          (:tracker-decay-method (decay-method module))
          (:tracker-decay (decay-param module))))))
  

#-(or :allegro :ccl) (defun cl-user::tracker-error-message (stream error colon-p atsign-p &optional (width 0) padchar commachar)
                       (declare (ignore colon-p atsign-p width padchar commachar))
                       (format stream "~s" error))

#+(or :allegro :ccl) (defun cl-user::tracker-error-message (stream error colon-p atsign-p &optional (width 0) padchar commachar)
                       (declare (ignore colon-p atsign-p width padchar commachar))
                       (format stream "#<~a " (type-of error))
                       (write error :stream stream :escape nil)
                       (format stream  ">"))


(defun tracker-apply-scale (setting scale)
  (if scale
      (handler-case (dispatch-apply scale setting)
        (error (x) (print-warning "Tracker module encountered error ~/tracker-error-message/ while trying to apply the function ~s to value ~s" x scale setting)))
    setting))


(defun tracker-request-checker (spec slot default)
  (if (null (chunk-spec-slot-spec spec slot)) 
      default
    (verify-single-explicit-value spec slot :tracker 'track-outcome)))

; generate n choices uniformly spaced from min to max
(defun generate-tracker-choices (n min max)
  (do* ((interval (- max min))
        (i 0 (1+ i))
        (step (/ interval (1- n)))
        (val min (min max (+ val step)))
        (choices nil))
       ((= i n) choices)
    (push-last val choices)))

;convert choices into an initial set of experiences
(defun tracker-prior-statistics (choices)
  (let ((statistics (list 0 0 0 0 0)))
    (dolist (val choices)
      (setf statistics (mapcar '+ statistics (list 1 val (* val val) (* val val val) (* val val val val)))))
    (append statistics (list 0 0 0 0 0 0))))

(defun tracker-update-time (delay)
  (handler-case  (min 60 (max 1 (- (* (log (act-r-random 1.0)) delay))))
    (error () 60)))

(defun create-update-tracker-event (delay module tracker)
  (bt:with-recursive-lock-held ((tracker-lock module))
    (schedule-event-relative (tracker-update-time delay) 'update-tracker :params (list module tracker) :module :tracker :maintenance t :output 'high :details (format nil "update-tracker ~a" (tracker-control-slot tracker)))))


(defun request-tracker (module buffer chunk-spec)
  (declare (ignore buffer)) ;; only have one
  (bt:with-recursive-lock-held ((tracker-lock module))
    
    (flet ((failure (text)
                    (model-warning "Tracker request failed because ~a" text)))
      
      (let ((control-buffer (default-control module))
            (good-buffer (tracker-request-checker chunk-spec 'good-buffer (default-values module)))
            (bad-buffer (tracker-request-checker chunk-spec 'bad-buffer (default-values module)))
            (control-slot (tracker-request-checker chunk-spec 'control-slot nil))
            (good-slot (tracker-request-checker chunk-spec 'good-slot nil))
            (bad-slot (tracker-request-checker chunk-spec 'bad-slot nil))
            (control-scale (tracker-request-checker chunk-spec 'control-scale (default-control-scale module)))
            (good-weight (tracker-request-checker chunk-spec 'good-weight (default-good-weight module)))
            (bad-weight (tracker-request-checker chunk-spec 'bad-weight (default-bad-weight module)))
            (good-scale (tracker-request-checker chunk-spec 'good-scale (default-good-scale module)))
            (bad-scale (tracker-request-checker chunk-spec 'bad-scale (default-bad-scale module)))
            (delay (tracker-request-checker chunk-spec 'delay (default-delay module)))
            (min (tracker-request-checker chunk-spec 'min (default-min module)))
            (max (tracker-request-checker chunk-spec 'max (default-max module)))
            (control-count (tracker-request-checker chunk-spec 'control-count (default-count module)))
            (temp (tracker-request-checker chunk-spec 'temp (initial-temp module)))
            (scale (tracker-request-checker chunk-spec 'scale (temp-scale module))))
        
        (cond ((null control-buffer)
               (failure "control-buffer not specified."))
              ((null control-slot)
               (failure "control-slot not specified."))
              ((not (or (and good-buffer good-slot)
                        (and bad-buffer bad-slot)))
               (failure (cond ((and (null good-buffer) (null bad-buffer))
                               "neither a good or bad buffer is specified.")
                              (good-buffer
                               "The good buffer's slot is not specified.")
                              (t
                               "The bad buffer's slot is not specified."))))
              ((not (local-or-remote-function-or-nil control-scale))
               (failure (format nil "invalid control-scale ~a provided." control-scale)))
              ((and good-weight good-buffer good-slot (not (numberp good-weight)))
               (failure (format nil "invalid good-weight ~a provided." good-weight)))
              ((and good-buffer good-slot (not (local-or-remote-function-or-nil good-scale)))
               (failure (format nil "invalid good-scale ~a provided." good-scale)))
              ((and bad-weight bad-buffer bad-slot (not (numberp bad-weight)))
               (failure (format nil "invalid bad-weight ~a provided." bad-weight)))
              ((and bad-buffer bad-slot (not (local-or-remote-function-or-nil bad-scale)))
               (failure (format nil "invalid bad-scale ~a provided." bad-scale)))
              ((not (numberp min))
               (failure "min must be a number."))
              ((not (numberp max))
               (failure "max must be a number."))
              ((not (and (posnum control-count) (integerp control-count)))
               (failure "prior-count must be a positive integer."))
              ((not (numberp temp))
               (failure "temp must be a number."))
              ((not (posnum scale))
               (failure "scale must be a number."))
              ((not (posnum delay))
               (failure "delay must be a number."))
              (t
               (awhen (find control-slot (trackers module) :key 'tracker-control-slot)
                      (model-warning "Tracker for control-slot ~a already exists and is being replaced with a new tracker." control-slot)
                      (setf (trackers module) (remove it (trackers module)))
                      (remove-m-buffer-chunk 'tracker (tracker-chunk it))
                      (delete-event (tracker-update-event it))
                      (purge-chunk-fct (tracker-chunk it)))
               
               (let* ((choices (generate-tracker-choices control-count min max))
                      (params (list 0 0 0))
                      (chunk (car (define-chunks-fct (list (mapcan (lambda (x) (if x x nil))
                                                             (list (list 'control-slot control-slot)
                                                                   (list 'min min) (list 'max max)
                                                                   (if (and good-buffer good-slot) (list 'good-slot good-slot) nil)
                                                                   (if (and bad-buffer bad-slot) (list 'bad-slot bad-slot) nil)
                                                                   (if control-scale (list 'control-scale control-scale) nil)
                                                                   (list 'good-weight good-weight)
                                                                   (if (and good-buffer good-slot good-scale) (list 'good-scale good-scale) nil)
                                                                   (list 'bad-weight bad-weight)
                                                                   (if (and bad-buffer bad-slot bad-scale) (list 'bad-scale bad-scale) nil)))))))
                      (tracker (make-tracker 
                                :control-buffer control-buffer
                                :control-slot control-slot
                                :control-scale control-scale
                                :choices choices
                                :statistics (tracker-prior-statistics choices)
                                :min min
                                :max max
                                :params params
                                :init-temp temp
                                :temperature temp
                                :delay delay
                                :scale-temp (seconds->ms scale)
                                :init-time (mp-time-ms)
                                :chunk chunk
                                :good-buffer (when (and good-buffer good-slot)
                                               good-buffer)
                                :good-slot (when (and good-buffer good-slot)
                                             good-slot)
                                :good-weight good-weight
                                :good-scale (when (and good-buffer good-slot)
                                              good-scale)
                                :bad-buffer (when (and bad-buffer bad-slot)
                                              bad-buffer)
                                :bad-slot (when (and bad-buffer bad-slot)
                                            bad-slot)
                                :bad-weight bad-weight
                                :bad-scale (when (and bad-buffer bad-slot)
                                             bad-scale)))
                      (setting (tracker-genQuadValue module tracker))
                      (result (tracker-apply-scale setting control-scale)))
                 
                 (push tracker (trackers module))
                 (setf (tracker-update-event tracker) (create-update-tracker-event delay module tracker))
                 
                 (schedule-event-now 'schedule-mod-buffer-chunk  :priority -2000 :params (list control-buffer (list control-slot result) 0 :module :tracker :output 'medium) :module :tracker :output nil)
                 
                 
                 (store-m-buffer-chunk 'tracker chunk)
                 
                 (when (and (tracker-trace module) (not (equalp result setting)))
                   (model-output " Result scaled to value: ~a" result)))))))))

(defun mod-request-tracker (module buffer mods)
  (bt:with-recursive-lock-held ((tracker-lock module))
    (let* ((current-chunk (buffer-read buffer))
           (current-control-slot (when current-chunk (chunk-slot-value-fct current-chunk 'control-slot)))
           (current-tracker (find current-control-slot (trackers module) :key 'tracker-control-slot)))
      
      (flet ((failure (text)
                      (model-warning "Tracker modification request failed because ~a" text)
                      (return-from mod-request-tracker)))
        
        (unless current-tracker
          (failure "no matching tracker was found for a modification request."))
        
        (let ((slots (chunk-spec-slots mods))
              (valid-slots (chunk-type-possible-slot-names-fct 'tracker-modification)))
          (flet ((get-value (slot test)
                            (when (find slot slots)
                              (let ((val (slot-spec-value (first (chunk-spec-slot-spec mods slot)))))
                                (if (funcall test val)
                                    val
                                  (failure (format nil "value for the ~s slot is ~s which does not satisfy ~s."
                                             slot val test)))))))
            
            (unless (every (lambda (x) (find x valid-slots)) slots)
              (let ((invalid (remove-if (lambda (x) (find x valid-slots)) slots)))
                (if (> (length invalid) 1)
                    (failure (format nil "slots ~s are not valid for tracker modification." invalid))
                  (failure (format nil "slot ~s is not valid for tracker modification." (first invalid))))))
            
            (let* ((tracker-chunk (tracker-chunk current-tracker))
                   changes
                   (re-size (list nil nil nil)))
              
              (unless mods
                (failure "the modification request did not correspond to valid changes as indicated in the chunk-spec-to-chunk-def warnings."))
              
              ;; first pass just to verify everything is of the right type
              
              (dolist (x slots) 
                (push-last x changes)
                
                (let ((val (case x
                             (good-slot (get-value 'good-slot 'symbolp))
                             (bad-slot (get-value 'bad-slot 'symbolp))
                             (control-scale (get-value 'control-scale 'local-or-remote-function-or-nil))
                             (good-weight (get-value 'good-weight 'numberp))
                             (bad-weight (get-value 'bad-weight 'numberp))
                             (good-scale (get-value 'good-scale 'local-or-remote-function-or-nil))
                             (bad-scale (get-value 'bad-scale 'local-or-remote-function-or-nil))
                             (delay (get-value 'delay 'posnum))
                             (min (setf (first re-size) (get-value 'min 'numberp)))
                             (max (setf (second re-size) (get-value 'max 'numberp)))
                             (control-count (get-value 'control-count 'integerp)
                                            (setf (third re-size) (get-value 'control-count 'plusp)))
                             (scale (get-value 'scale 'posnum)))))
                  (push-last val changes)))
              
              ;; verify that we still have a valid tracker
              
              ;; test min and max 
              
              (unless (< (if (find 'min slots) (get-value 'min 'identity) (tracker-min current-tracker))
                         (if (find 'max slots) (get-value 'max 'identity) (tracker-max current-tracker)))
                (failure "the modification would make the trackers min value greater than the max value."))
              
              ;; make sure there's at least one event to track
              (let ((the-good-slot (if (find 'good-slot slots) (get-value 'good-slot 'symbolp) (tracker-good-slot current-tracker)))
                    (the-bad-slot (if (find 'bad-slot slots) (get-value 'bad-slot 'symbolp) (tracker-bad-slot current-tracker))))
                (unless (or (and (tracker-good-buffer current-tracker) the-good-slot)
                            (and (tracker-bad-buffer current-tracker) the-bad-slot))
                  (failure "the modification leaves the tracker with neither a good or bad event to track.")))
              
              ;; update the slots
              
              ;; new choices
              
              (when (some 'identity re-size)
                (when (tracker-trace module)
                  (model-output "Tracker modification request changing current choices from:")
                  (let ((w (apply 'max (mapcar (lambda (x) (length (format nil "~5,3f" x))) (tracker-choices current-tracker)))))
                    (model-output (format nil "  ~~{~~~d,3f~~^ ~~}" w) (tracker-choices current-tracker))))
                
                (when (first re-size)
                  (setf (tracker-min current-tracker) (first re-size)))
                (when (second re-size)
                  (setf (tracker-max current-tracker) (second re-size)))
                (setf (tracker-choices current-tracker)
                  (generate-tracker-choices (if (third re-size) (third re-size) (length (tracker-choices current-tracker)))
                                            (tracker-min current-tracker) (tracker-max current-tracker)))
                (when (tracker-trace module)
                  (let ((w (apply 'max (mapcar (lambda (x) (length (format nil "~5,3f" x))) (tracker-choices current-tracker)))))
                    (model-output " to")
                    (model-output (format nil "  ~~{~~~d,3f~~^ ~~}" w) (tracker-choices current-tracker)))))
              
              ;; all the other slots
              
              (dolist (x slots)
                (case x
                  (good-slot (setf (tracker-good-slot current-tracker) (get-value 'good-slot 'symbolp)))
                  (bad-slot (setf (tracker-bad-slot current-tracker)(get-value 'bad-slot 'symbolp)))
                  (control-scale (setf (tracker-control-scale current-tracker)(get-value 'control-scale 'local-or-remote-function-or-nil)))
                  (good-weight (setf (tracker-good-weight current-tracker)(get-value 'good-weight 'numberp)))
                  (bad-weight (setf (tracker-bad-weight current-tracker)(get-value 'bad-weight 'numberp)))
                  (good-scale (setf (tracker-good-scale current-tracker)(get-value 'good-scale 'local-or-remote-function-or-nil)))
                  (bad-scale (setf (tracker-bad-scale current-tracker)(get-value 'bad-scale 'local-or-remote-function-or-nil)))
                  (delay (setf (tracker-delay current-tracker)(get-value 'delay 'posnum)))
                  (scale (setf (tracker-scale-temp current-tracker) (seconds->ms (get-value 'scale 'posnum))))))
              
              ;; set weights to defaults if necessary
              
              (when (and (tracker-good-buffer current-tracker) (tracker-good-slot current-tracker))
                (unless (numberp (tracker-good-weight current-tracker))
                  (setf (tracker-good-weight current-tracker) (default-good-weight module))
                  (push-last 'good-weight changes)
                  (push-last (default-good-weight module) changes)))
              
              (when (and (tracker-bad-buffer current-tracker) (tracker-bad-slot current-tracker))
                (unless (numberp (tracker-bad-weight current-tracker))
                  (setf (tracker-bad-weight current-tracker) (default-bad-weight module))
                  (push-last 'bad-weight changes)
                  (push-last (default-bad-weight module) changes)))
              
              (mod-chunk-fct tracker-chunk changes)
              
              (when (tracker-trace module)
                (model-output "Changing tracker for ~s slot values: ~{~s ~^~}" current-control-slot changes))
              
              
              (delete-event (tracker-update-event current-tracker))
              
              ;; get rid of the copied chunk in the buffer
              ;; since the underlying chunk is the one that's been updated
              (erase-buffer buffer)
              
              (update-tracker module current-tracker))))))))

               
               
(define-module-fct :tracker '((tracker nil nil nil nil :search-copy))
  (list
   (define-parameter :initial-temp :valid-test 'numberp :default-value 1 :warning "must be a number" :documentation "Default initial temperature")
   (define-parameter :temp-scale :valid-test 'posnum :default-value 180 :warning "must be a positive number" :documentation "Default scale for temperature")
   (define-parameter :control-buffer :valid-test (lambda (x) (find x (buffers))) :default-value 'goal :warning "must be a valid buffer name"
     :documentation "The default buffer to use for the control slot.")
   (define-parameter :control-scale :valid-test 'local-or-remote-function-or-nil :default-value nil :warning "must be a function or nil"
     :documentation "The default scaling function for the result in the control slot.")
   (define-parameter :values-buffer :valid-test (lambda (x) (find x (buffers))) :default-value 'imaginal :warning "must be a valid buffer name"
     :documentation "The default buffer to use for the good and bad value slots.")
   (define-parameter :control-count :valid-test (lambda (x) (and (posnum x) (integerp x))) :default-value 21 :warning "must be a positive integer"
     :documentation "The default number of possible control results between min and max (inclusive of both).")
   (define-parameter :control-min :valid-test 'numberp :default-value 0 :warning "must be a number" :documentation "Default mininum value for the control range")
   (define-parameter :control-max :valid-test 'numberp :default-value 1 :warning "must be a number" :documentation "Default maximum value for the control range")
   (define-parameter :good-weight :valid-test 'numberp :default-value 1 :warning "must be a number"
     :documentation "The default weighting value for good events.")
   (define-parameter :good-scale :valid-test 'local-or-remote-function-or-nil :default-value nil :warning "must be a function or nil"
     :documentation "The default scaling function for good events.")
   (define-parameter :bad-weight :valid-test 'numberp :default-value -1 :warning "must be a number"
     :documentation "The default weighting value for bad events.")
   (define-parameter :bad-scale :valid-test 'local-or-remote-function-or-nil :default-value nil :warning "must be a function or nil"
     :documentation "The default scaling function for bad events.")
   (define-parameter :trace-tracker :valid-test 'tornil :default-value nil :warning "T or nil" :documentation "Show tracker updates in the model trace")
   (define-parameter :update-delay :valid-test 'posnum :default-value 10 :warning "must be a positive number"
     :documentation "The average update time in seconds.")
   (define-parameter :tracker-decay :valid-test 'posnumornil :default-value nil :warning "a positive number or nil" :documentation "The decay parameter for the chosen decay method.")
   (define-parameter :tracker-decay-method :valid-test (lambda (x) (or (null x) (eq x 'power) (eq x 'exponential))) :warning "nil, power, or exponential"
     :documentation "Whether to use temporal discounting of past statistics, and if so, whether to use an exponential decay or power law decay."))
  :creation 'create-tracker
  :reset 'reset-tracker
  :params 'params-tracker
  :buffer-mod 'mod-request-tracker
  :request 'request-tracker
  :delete 'delete-tracker
  :query 'goal-style-query
  :version "2.2"
  :documentation "Module which can learn an outcome value based on one or two events monitored in buffer slots.")

(defun updated-tracker-value (module tracker buffer slot value scale which)
  (bt:with-recursive-lock-held ((tracker-lock module))
    (let ((outcome (tracker-apply-scale value scale)))
      (cond ((not (numberp outcome))
             (print-warning "Tracker for ~s detected a change in the ~s slot of the ~s buffer but the resulting value ~s is not a number." (tracker-control-slot tracker) slot buffer outcome)
             (print-warning " Cannot update the value for the ~s results." which)
             0)
            (t
             (when (tracker-trace module)
               (model-output "Tracker for ~s updating the current ~s value" (tracker-control-slot tracker) which)
               (model-output " Updating event occurred in the ~s slot of the ~s buffer" slot buffer)
               (if (equalp outcome value)
                   (model-output "  updating with value ~f" value)
                 (model-output "  slot value ~s was scaled to update value ~f" value outcome)))
             outcome)))))
           
           
(defun tracker-check-for-buffer-changes (event)
  (cond ((eq (evt-action event) 'set-buffer-chunk)
         (let ((module (get-module :tracker))
               (changed-buffer (first (evt-params event))))
             (bt:with-recursive-lock-held ((tracker-lock module))
               (dolist (x (trackers module))
                 (when (eq changed-buffer (tracker-good-buffer x))
                   (let* ((chunk (buffer-read changed-buffer))
                          (slot (tracker-good-slot x))
                          (slots (and chunk (chunk-filled-slots-list-fct chunk))))
                     (when (find slot slots)
                       (incf (tracker-good-value x) (updated-tracker-value module x changed-buffer slot (fast-chunk-slot-value-fct chunk slot) (tracker-good-scale x) 'good)))))
                 (when (eq changed-buffer (tracker-bad-buffer x))
                   (let* ((chunk (buffer-read changed-buffer))
                          (slot (tracker-bad-slot x))
                          (slots (and chunk (chunk-filled-slots-list-fct chunk))))
                     (when (find slot slots)
                       (incf (tracker-bad-value x) (updated-tracker-value module x changed-buffer slot (fast-chunk-slot-value-fct chunk slot) (tracker-bad-scale x) 'bad)))))))))
        ((eq (evt-action event) 'mod-buffer-chunk)
         (let* ((module (get-module :tracker))
                (changed-buffer (first (evt-params event)))
                (mods (second (evt-params event)))
                (slots (if (listp mods)
                           (do ((m mods (cddr m))
                                (s nil))
                               ((null m) s)
                             (push (car m) s))
                         (chunk-spec-slots mods))))
           (bt:with-recursive-lock-held ((tracker-lock module))
             (dolist (x (trackers module))
               (when (eq changed-buffer (tracker-good-buffer x))
                 (let ((slot (tracker-good-slot x)))
                   (when (find slot slots)
                     (let ((chunk (buffer-read changed-buffer)))
                       (when (fast-chunk-slot-value-fct chunk slot)
                         (incf (tracker-good-value x) (updated-tracker-value module x changed-buffer slot (fast-chunk-slot-value-fct chunk slot) (tracker-good-scale x) 'good)))))))
               (when (eq changed-buffer (tracker-bad-buffer x))
                 (let ((slot (tracker-bad-slot x)))
                   (when (find slot slots)
                     (let ((chunk (buffer-read changed-buffer)))
                       (when (fast-chunk-slot-value-fct chunk slot)
                         (incf (tracker-bad-value x) (updated-tracker-value module x changed-buffer slot (fast-chunk-slot-value-fct chunk slot) (tracker-bad-scale x) 'bad)))))))))))))

(defun safe-exp (x temp)
  (handler-case (exp x)
            (floating-point-underflow () 0)
            (floating-point-overflow () ;; could happen for low temperatures
                                     (print-warning "Math overflow during tracker calculation with temperature ~f using most-positive-double-float instead" temp)
                                     most-positive-double-float)))

;select a value from choices reflecting the current quadratic parameters and the current temperature
(defun tracker-genQuadValue (module tracker)
  (bt:with-recursive-lock-held ((tracker-lock module))
    (let* ((params (tracker-params tracker))
           (choices (tracker-choices tracker))
           (temperature (tracker-temperature tracker))
           (Ys  (mapcar (lambda (x) (+ (first params) (* x (second params)) (* x x (third params)))) choices))
           (max (apply 'max Ys))
           (vals  (mapcar (lambda (x) (safe-exp (/ (- x max) temperature) temperature)) Ys))
           (sum  (apply '+ vals))
           (probs (mapcar (lambda (x) (/ x sum)) vals))
           (p (act-r-random 1.0)))
    
      (when (tracker-trace module)
        (model-output "Tracker generating value for buffer ~s slot ~s with temperature ~f:" 
                      (tracker-control-buffer tracker) (tracker-control-slot tracker) temperature)
        (let ((w (apply 'max (mapcar (lambda (x) (length (format nil "~5,3f" x))) choices))))
          (model-output (format nil "  Choices: ~~{~~~d,3f~~^ ~~}" w) choices)
          (model-output (format nil "  Probs:   ~~{~~~d,3f~~^ ~~}" w) probs)))
    
      (let ((result (do ((temp1 (cdr probs) (cdr temp1))
                         (cumsum (car probs) (+ cumsum (car temp1)))
                         (temp2 choices (cdr temp2)))
                        ((or (< p cumsum) (null (cdr temp2))) (car temp2)))))
        (when (tracker-trace module)
          (model-output " Chosen result is ~f" result))
        
        (setf (tracker-last-update tracker) (mp-time-ms))
        (setf (tracker-setting tracker) result)))))


(defun update-tracker (module tracker)
  (bt:with-recursive-lock-held ((tracker-lock module))
    (tracker-update-stats module tracker)
    (tracker-calc-quad tracker)
    
    (setf (tracker-update-event tracker) (create-update-tracker-event (tracker-delay tracker) module tracker))
    
    (when (tracker-trace module)
      (model-output "Tracker for ~s slot updating the equation" (tracker-control-slot tracker))
      (model-output "  new equation is ~s + ~sx + ~sx^2" (first (tracker-params tracker)) (second (tracker-params tracker)) (third (tracker-params tracker))))
    
    (let* ((setting (tracker-genQuadValue module tracker))
           (result (tracker-apply-scale setting (tracker-control-scale tracker))))
      
      (when (and (tracker-trace module) (not (equalp result setting)))
        (model-output " Result scaled to value: ~a" result))
      
      (if (buffer-read (tracker-control-buffer tracker))
          (schedule-mod-buffer-chunk (tracker-control-buffer tracker) (list (tracker-control-slot tracker) result) 0 :module :tracker :output 'medium)
        (progn
          (model-warning "Tracker update occurred with no chunk in the control buffer ~s." (tracker-control-buffer tracker))
          (model-warning "Creating a new chunk with only the ~s slot." (tracker-control-slot tracker))
          (let ((chunk-name (car (define-chunks-fct (list (list (tracker-control-slot tracker) result))))))
            (schedule-set-buffer-chunk (tracker-control-buffer tracker) chunk-name 0 :module :tracker :output 'medium)
            ;; because the chunk is only being created to be copied into the buffer
            ;; just get rid of it after that happens to keep the chunk count down 
            (schedule-event-relative 0 'clean-up-goal-chunk :module :tracker :output nil 
                                     :priority :min :params (list chunk-name)
                                     :details "Clean-up unneeded chunk" :maintenance t)))))))


(defun tracker-update-stats (module tracker)
  (let* ((old-stats (tracker-statistics tracker))
         (weight  (max .05 (ms->seconds (- (mp-time-ms) (tracker-last-update tracker)))))
         (weight2 (tracker-discount-past module tracker (length (tracker-choices tracker)) weight))
         (X (tracker-setting tracker))
         (X2 (* X X))
         (good-Y (/ (tracker-good-value tracker)  weight))
         (bad-Y (/ (tracker-bad-value tracker)  weight))
         (current-stats (list 1 X X2 (* X X2) (* X2 X2) good-y (* X good-Y) (* X2 good-y) bad-y (* X bad-Y) (* X2 bad-y)))
         (new-stats (mapcar (lambda (x y) (+ (* weight x) (* weight2 y))) current-stats old-stats)))
    (setf (tracker-good-value tracker) 0)
    (setf (tracker-bad-value tracker) 0)
    (setf (tracker-temperature tracker) (/ (tracker-init-temp tracker) (1+ (/ (- (mp-time-ms) (tracker-init-time tracker)) (tracker-scale-temp tracker)))))
    (setf (tracker-statistics tracker) new-stats)))


(defun tracker-discount-past (module tracker prior delay)
  (if (null (decay-method module))
      1
    (case (decay-method module)
      (exponential (expt (decay-param module) delay))
      (power (let* ((tot (+ prior (ms->seconds (- (mp-time-ms) (tracker-init-time tracker)))))
                    (pastweight (* 2 tot (/ (- (expt tot (- 1 (decay-param module))) (expt delay (- 1 (decay-param module)))) (- tot delay))))
                    (currentweight (expt delay (decay-param module))))
               (/ pastweight (+ pastweight currentweight)))))))

(defun tracker-calc-quad (tracker)
  (let (params)
    (handler-case
        (let* ((stats  (tracker-statistics tracker))
               (wg (tracker-good-weight tracker))
               (wb (tracker-bad-weight tracker))
               (N (nth 0 stats))
               (SX1  (nth 1 stats))
               (SX2 (nth 2 stats))
               (SX3 (nth 3 stats))
               (SX4 (nth 4 stats))
               (SY (+ (* wg (nth 5 stats)) (* wb (nth 8 stats))))
               (SX1Y (+ (* wg (nth 6 stats)) (* wb (nth 9 stats))))
               (SX2Y (+ (* wg (nth 7 stats)) (* wb (nth 10 stats))))
               (lx2 (- SX2 (/ (* SX1 SX1) N)))
               (lx3 (- SX3 (/ (* SX1 SX2) N)))
               (lx4 (- SX4 (/ (* SX2 SX2) N)))
               (lx1y (- SX1Y (/ (* SX1 SY) N)))
               (lx2y (- SX2Y (/ (* SX2 SY) N)))
               (p2 (/ (- (* lx4 lx1y) (* lx3 lx2y)) (- (* lx2 lx4) (* lx3  lx3))))
               (p3 (/ (- (* lx2 lx2y) (* lx3 lx1y)) (- (* lx2 lx4) (* lx3  lx3))))
               (p1 (/ (- SY (* p2 SX1) (* p3 SX2)) N)))
          (setf params (list p1 p2 p3)))
      (error () (model-warning "Error updating tracker params for tracker ~s.  Using previous values." (tracker-control-slot tracker)))
      (:no-error (&rest r) (setf (tracker-params tracker) params)))))


(defmacro get-tracker (control-slot)
  `(get-tracker-fct ',control-slot))

(defun get-tracker-fct (control-slot)
  (let ((module (get-module :tracker)))
    (when module
      (bt:with-recursive-lock-held ((tracker-lock module))
        (find control-slot (trackers module) :key 'tracker-control-slot)))))

(defmacro print-tracker-stats (&optional control-slot)
  `(print-tracker-stats-fct ',control-slot))

(defun print-tracker-stats-fct (&optional control-slot)
  (verify-current-model 
   "No current model available for printing tracker stats."
   (let ((module (get-module :tracker)))
     (if module
         (bt:with-recursive-lock-held ((tracker-lock module))
           (if control-slot
               (let ((tracker (get-tracker-fct control-slot)))
                 (if tracker
                     (output-tracker-stats tracker)
                   (print-warning "There is no tracker associated with the ~s slot." control-slot)))
             (let (r)
               (dolist (x (trackers module) r)
                 (push-last (list (tracker-control-slot x) (output-tracker-stats x)) r)))))
       (print-warning "No tracker module available.")))))

(defun external-print-tracker-stats (&optional control-slot)
  (print-tracker-stats-fct (string->name control-slot)))

(add-act-r-command "print-tracker-stats" 'external-print-tracker-stats "Print the details for a tracker or all trackers and return the equation coefficients. Params: {control-slot}")

(defun output-tracker-stats (tracker)
  (command-output "Tracker for slot ~s" (tracker-control-slot tracker))
  (when (and (tracker-good-buffer tracker) (tracker-good-slot tracker))
    (command-output " Good slot is ~s in buffer ~s with weight ~s" (tracker-good-slot tracker) (tracker-good-buffer tracker) (tracker-good-weight tracker)))
  (when (and (tracker-bad-buffer tracker) (tracker-bad-slot tracker))
    (command-output " Bad slot is ~s in buffer ~s with weight ~s" (tracker-bad-slot tracker) (tracker-bad-buffer tracker) (tracker-bad-weight tracker)))
  (command-output "  Current temperature is: ~f" (tracker-temperature tracker))
  (command-output "  Current equation is: ~s + ~sx + ~sx^2" (first (tracker-params tracker)) (second (tracker-params tracker)) (third (tracker-params tracker)))
  
  (let* ((params (tracker-params tracker))
         (choices (tracker-choices tracker))
         (temperature (tracker-temperature tracker))
         (Ys  (mapcar (lambda (x) (+ (first params) (* x (second params)) (* x x (third params)))) choices))
         (max (apply 'max Ys))
         (vals  (mapcar (lambda (x) (safe-exp (/ (- x max) temperature) temperature)) Ys))
         (sum  (apply '+ vals))
         (probs (mapcar (lambda (x) (/ x sum)) vals)))
    (let ((w (apply 'max (mapcar (lambda (x) (length (format nil "~5,3f" x))) choices))))
      (command-output (format nil "  Choices: ~~{~~~d,3f~~^ ~~}" w) choices)
      (command-output (format nil "  Probs:   ~~{~~~d,3f~~^ ~~}" w) probs)))
  
  (let* ((setting (tracker-setting tracker))
         (result (tracker-apply-scale setting (tracker-control-scale tracker))))
    (command-output "  Current setting is: ~s" setting)
    (unless (equalp setting result)
      (command-output "  Scaled to result: ~s by scale: ~s" result (tracker-control-scale tracker))))
  (tracker-params tracker))
  
     
  
(provide "tracker")

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
