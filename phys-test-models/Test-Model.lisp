(clear-all)

(defun run-test-model (run-time)
;;Schedule event to change physiology
	;(schedule-event 0.022 'set-phys-vals :module 'physio :params (list (list (list "IVDrip.Switch" 1) (list "IVDrip.H2OSetting" 25) (list "IVDrip.ClinicalSaline" 860))) :priority :max :details "Start hypertonic saline IV")
	(schedule-periodic-event 1 'test-record-arousal
		:initial-delay 2 :module :physio :output nil)
	(run run-time))

(defun run-graded-stress1 (&optional (length 20) (perc-inc 100) (num-steps 20))
	"Runs stress in a graded manner w/ instant CRF stress reaction
	(i.e., slowly increases, the decreases, activation of physiological systems)
	Inputs:
		length - time to run incerase and decrease in minutes (int)
		perc-inc - percent of maximum to increase (double 0-100)
		num-steps - number of steps to increase from base to percentage of max"

	(let* ((num-steps-side (ceiling (/ num-steps 2)))
				 (perc-per (/ (/ perc-inc num-steps-side) 100))
				 (time-per (/ length num-steps))
				 (END-SYMP-ACT 1))
		(sgp :AA-max-arous 3)
		(schedule-periodic-event 1 'test-record-arousal
			:module :physio :output nil :initial-delay 2)
		;Affect epinephrine/adrenaline & CRF
		(schedule-event-relative 0.020 'set-phys-vals :module 'physio
			:params (list (list (list "Sympathetics-Adrenal.ClampLevel" 2)
													(list "Sympathetics-Adrenal.ClampSwitch" 1)
													(list "CorticotropinReleasingFactor.Stress" 4)))
			:priority :max :details "Graded stress increase adrenal & CRF")
		(dotimes (i num-steps-side)
			(let* ((step (+ i 1))
						 (symp-act (* perc-per step END-SYMP-ACT)))
				;General sympathetic arousal
				(schedule-event-relative 0.021 'set-phys-vals :module 'physio
					:params (list (list (list "Sympathetics-General.EssentialEffect" symp-act)))
					:priority :max :details "Graded stress increase Symp Essential")
				(run (* time-per 60))))

				;;Now move stress back towards normal (adrenaline & CRF)
				(schedule-event-relative 0.020 'set-phys-vals :module 'physio
					:params (list (list (list "Sympathetics-Adrenal.ClampSwitch" 0)
															(list "CorticotropinReleasingFactor.Stress" 2)))
					:priority :max :details "Graded stress reduction adrenal & CRF")
				(dotimes (i num-steps-side)
					(let* ((step (+ i 1))
								 (symp-act (* perc-per step END-SYMP-ACT)))
						(if (> symp-act END-SYMP-ACT) (setf symp-act END-SYMP-ACT))
						(format t "Sleep ~a ~a ~a~%" perc-per step symp-act)
						;General sympathetic arousal
						(schedule-event-relative 0.021 'set-phys-vals :module 'physio
							:params (list (list (list "Sympathetics-General.EssentialEffect" (- END-SYMP-ACT symp-act))))
							:priority :max :details "Graded stress reduction symp essential")
						(run (* time-per 60))))))

(defun run-graded-stress2 (&optional (length 20) (perc-inc 100) (num-steps 20))
	"Runs stress in a graded manner w/ graded CRF stress reaction
	(i.e., slowly increases, the decreases, activation of physiological systems)
	Inputs:
		length - time to run incerase and decrease in minutes (int)
		perc-inc - percent of maximum to increase (double 0-100)
		num-steps - number of steps to increase from base to percentage of max"

	(let* ((num-steps-side (ceiling (/ num-steps 2)))
				 (perc-per (/ (/ perc-inc num-steps-side) 100))
				 (time-per (/ length num-steps))
				 (END-SYMP-ACT 1)
				 (crh2 nil)
				 (crh4 nil))
		(schedule-periodic-event 1 'test-record-arousal
			:module :physio :output nil :initial-delay 2)
		;Affect epinephrine/adrenaline
		(schedule-event-relative 0.020 'set-phys-vals :module 'physio
			:params (list (list (list "Sympathetics-Adrenal.ClampLevel" 2)
				(list "Sympathetics-Adrenal.ClampSwitch" 1)))
			:priority :max :details "Graded stress")
		(dotimes (i num-steps-side)
			(let* ((step (+ i 1))
						 (symp-act (* perc-per step END-SYMP-ACT)))
				;General sympathetic arousal
				(schedule-event-relative 0.021 'set-phys-vals :module 'physio
					:params (list (list (list "Sympathetics-General.EssentialEffect" symp-act)))
					:priority :max :details "Graded stress")
				;Increase CRH-based stress
				(if (and (not crh2) (> (* step time-per) (* length 0.375)))
					(progn
						(setf crh2 t)
						(schedule-event-relative 0.022 'set-phys-vals :module 'physio
						:params (list (list (list "CorticotropinReleasingFactor.Stress" 4)))
						:priority :max :details "Graded stress"))
					(if (and (not crh4) (> (* step time-per) (* length 0.125)))
						(progn
							(setf crh4 t)
							(schedule-event-relative 0.022 'set-phys-vals :module 'physio
								:params (list (list (list "CorticotropinReleasingFactor.Stress" 3)))
								:priority :max :details "Graded stress"))))
				(run (* time-per 60))))
				;;Now move stress back towards normal
				(schedule-event-relative 0.020 'set-phys-vals :module 'physio
					:params (list (list (list "Sympathetics-Adrenal.ClampSwitch" 0)))
					:priority :max :details "Graded stress reduction Adrenal")
				(setf crh2 nil)
				(setf crh4 nil)
				(dotimes (i num-steps-side)
					(let* ((step (+ i 1))
						    (symp-act (* perc-per step END-SYMP-ACT)))
						(if (> symp-act END-SYMP-ACT) (setf symp-act END-SYMP-ACT))
						;General sympathetic arousal
						(schedule-event-relative 0.021 'set-phys-vals :module 'physio
							:params (list (list (list "Sympathetics-General.EssentialEffect" (- END-SYMP-ACT symp-act))))
							:priority :max :details "Graded stress reduction Symp Essential")
						;Increase CRH-based stress
						(if (and (not crh2) (> (* step time-per) (* length 0.375)))
							(progn
								(setf crh2 t)
								(schedule-event-relative 0.022 'set-phys-vals :module 'physio
								:params (list (list (list "CorticotropinReleasingFactor.Stress" 2)))
								:priority :max :details "Graded stress reduction CRF"))
							(if (and (not crh4) (> (* step time-per) (* length 0.125)))
								(progn
									(setf crh4 t)
									(schedule-event-relative 0.022 'set-phys-vals :module 'physio
										:params (list (list (list "CorticotropinReleasingFactor.Stress" 3)))
										:priority :max :details "Graded stress reduction"))))
						(run (* time-per 60))))))

(defun test-record-arousal ()
	(let* ((aa (get-module Affective-Associations))
	      (arous-dm-noise (compute-arousal-factor (AA-pred-error-factor aa) t))
	      (arous-mid (/ (AA-max-arous aa) 2))
	      (noise-val 0))
		(when (= arous-dm-noise 0) (incf arous-dm-noise 0.000001))
		(if (<= arous-dm-noise arous-mid)
			(setf noise-val (/ (+ (* arous-dm-noise (AA-nom-dm-noise aa)) (* (- arous-mid arous-dm-noise) (AA-max-dm-noise aa))) arous-mid))
			(setf noise-val (/ (+ (* (- (AA-max-arous aa) arous-dm-noise) (AA-nom-dm-noise aa)) (* (- arous-dm-noise arous-mid) (AA-max-dm-noise aa))) arous-mid)))
		(with-open-file
			(n-stream (concatenate 'string "arous-ans-log" *START-TIME* ".txt") :direction :output :if-exists :append :if-does-not-exist :create)
			(format n-stream "~0$,~5$,~5$~&" (mp-time) arous-dm-noise noise-val))))


(define-model ACT-Phi2_Test1

	(sgp :esc t :lf .05 :trace-detail high)
	(sgp :phys-delay 2 :phys-enabled t);:recorded-phys ("OsmBody.[Osm(mOsm/L)]-CellWall"))
	(sgp :AA-enabled t)
	(sgp :AA-dm-noise-switch t)
	(sgp :AA-util-noise-switch nil)
	;(sgp :AA-chunk-arousal-switch nil)
	(sgp :AA-max-dm-noise 1)


	(chunk-type chunk1 myslot1)
	(chunk-type chunk2 myslot2)
	(chunk-type goal-chunk goal-slot)


	(add-dm (blah1 isa chunk1 myslot1 2) (blah2 isa chunk2 myslot2 1) (goal1 isa goal-chunk goal-slot 1) (goal2 isa goal-chunk goal-slot 2))




	(p rule1
		=goal>
			isa	goal-chunk
			goal-slot =num
		?imaginal>
			state	free
	==>
		=goal>
			goal-slot 2

		+imaginal>
			isa chunk1
			myslot1 =num)

	(p rule2
		=goal>
			isa goal-chunk
			goal-slot 2
		=imaginal>
			isa chunk1
	==>
		=goal>
			goal-slot 1)

	(goal-focus goal1))
