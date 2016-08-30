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
;;;Made to be used with HumMod v1.6.2 - Modular 
;;;---------------
;;; - New thread version, This version needs tested more
;;;---------------
;;; ---For any nerve activity related to heart-rate, HR shouldn't go above : 208-(0.7*age) : (from H Tanaka, KD Monahan 2001).

;;;*This version is more messy than I'd like, but some have requested it, so I'm releasing the code so it can be used and improved by others besides me*


;;LISP Version dependent variables
#+:ccl (defvar *s-file-error* `ccl::simple-file-error)
#+:sbcl (defvar *s-file-error* `sb-impl::simple-file-error)

;;So we can use basic windows api functions
#+:Windows (open-shared-library "kernel32.dll")


;;Thread library
;(eval-when (:compile-toplevel :load-toplevel :execute)
;  (let ((*compile-file-pathname* nil))
;    (asdf:load-system :bordeaux-threads)))
	
;;Function to test for non ACT-R/Phi based events in queue (modified run function in scheduling.lisp to use)
(defun non-phi-events ()
 (or 
  (mp-modules-events 'procedural)
  (mp-modules-events 'declarative)
  (mp-modules-events :fatigue)
  (mp-modules-events 'vision)
  (mp-modules-events 'audio)
  (mp-modules-events 'blending)
  (mp-modules-events 'temporal)
  (mp-modules-events 'goal)
  (mp-modules-events 'imaginal)
  (mp-modules-events 'motor)
  (mp-modules-events 'speech))
)

;;;Custom Physiology Function Section
;;HumMod Directory variable
(defvar *HumModDir* (subseq (namestring *LOAD-TRUENAME*) 0 (search (file-namestring *LOAD-TRUENAME*) (namestring *LOAD-TRUENAME*))))

(defvar *HumModProc* nil)
;;We can leave it to this for now, because this should only come into question when multiple solver are started from the same directory
(defvar *pipeID* (write-to-string (random(get-universal-time))))

;;;Print out all key-value pairs: used with
(defun print-hash-table (key value)
	(format t "~s - ~s" key value)
)

;;;clear out any unwanted files
(defun clear-phys-files ()
	(sleep 0.5)
	(dolist (fileName (directory (concatenate 'string *HumModDir* "SolverOut*")))
	(handler-case (delete-file fileName)
	(error () nil)))
	(dolist (fileName (directory (concatenate 'string *HumModDir* "SolverIn*")))
	(handler-case (delete-file fileName)
	(error () nil)))
	(dolist (fileName (directory (concatenate 'string *HumModDir* "*.tem")))
	(handler-case (delete-file fileName)
	(error () nil))))

;;;Initialize Variable and Variable Order Hash-Table for physiological variable order
(defun init-list-to-hash (myList)
 (handler-case (let ((phys (get-module physio)))
  (loop
	 for k in myList
	  do (if (not(equal ':|solverout| k))
	   (loop
	    for k2 in k
		  do (if (not (equal ':|varroster| k2))
			  (setf (gethash (cadr k2) (phys-module-vars phys)) 0)))))) (error (e) (progn (print e) nil (return-from init-list-to-hash))))
t)

;;;Advance the HumMod model a specified amount of time (mins)
(defun advance-phys (timeSlice)
 (let ((advanceMessage (concatenate 'string "\"<solverin><gofor><solutionint>" (write-to-string timeSlice) 
         "</solutionint><displayint>" (write-to-string timeSlice)	"</displayint></gofor></solverin>\""))
			 (solverInputFile (concatenate 'string *HumModDir* "SolverIn" (phys-module-pipeID (get-module physio))))
			 (solverOutputFile (concatenate 'string *HumModDir* "SolverOut" (phys-module-pipeID (get-module physio)))))
	(clear-phys-files)
	(tagbody resetAdvance
	 (handler-case
	  (with-open-file
		 (messageStream  solverInputFile
		 :direction :output :if-exists :overwrite :if-does-not-exist :create)
		 	(format messageStream advanceMessage)) ((or simple-error ccl::simple-file-error) () (go resetAdvance)))
		(while (probe-file solverInputFile))
		 (let ((currTime (get-universal-time)))
			 (handler-case (while (and (not (probe-file solverOutputFile)) (< (- (get-universal-time) currTime) 480))) (error (e) (print e))))
		(when (not (probe-file solverOutputFile)) (go resetAdvance)))

	(handler-case (delete-file solverOutputFile)
			(error () nil)))
)

;;;Set HumMod Vars using specified list
(defun set-phys-vals (varValList)
  
  (tagbody
	resetSetVal
	(clear-phys-files)
  (model-output "Changing the following physiology: ~A" varValList)
	(let ((phys (get-module physio))
				(setPhysMessage "\"<solverin>")
				(timeOut 240))
		;;Construct message to be sent to new HumMod Solver Process
		;; -We must find the chunk in the hash-table because the request to HumMod is case sensitive
		(loop 
		 for v in varValList
		 do
		  (loop 
		   for k being the hash-keys in (phys-module-vars phys)
			 do
				(if (equal (string-upcase (car v)) (string-upcase k))
				 (setq setPhysMessage 
				  (concatenate 'string 
				   setPhysMessage "<setvalue><var>" k "</var><val>" (write-to-string (cadr v)) "</val></setvalue>")))))

		 (setf setPhysMessage (concatenate 'string setPhysMessage "</solverin>\""))
		  (let ((solverOutputFile (concatenate 'string *HumModDir* "SolverOut" (phys-module-pipeID phys)))
			    (solverInputFile (concatenate 'string *HumModDir* "SolverIn" (phys-module-pipeID phys))))

		 (tagbody
		  startGetVals
		  ;;Set HumMod variables
		  ;(print setPhysMessage)
		  (handler-case
		   (with-open-file
			  (messageStream solverInputFile
			  :direction :output :if-exists :supersede :if-does-not-exist :create)
				 (format messageStream setPhysMessage)) ((or simple-error ccl::simple-file-error) () (go startGetVals)))
		(while (probe-file solverInputFile))
		 (let ((currTime (get-universal-time)))
			 ;;We only wait so long for the file to be created
			 (handler-case (while (and (not (probe-file solverOutputFile)) (< (- (get-universal-time) currTime) timeOut))) 
				(error (e) (print e))))
		 (when (not (probe-file solverOutputFile)) (go startGetVals)))

		 (let ((checkError nil))

		 (handler-case (setf checkError (s-xml:parse-xml-file solverOutputFile)) 
				((or file-error s-xml::xml-parser-error type-error) () nil))
		(while (probe-file solverOutputFile) (handler-case (delete-file solverOutputFile)
			(error () nil)))

		(loop
		for v in checkError
					do
					 ;;If we have an error, then we reset the entire physiology system and (try to) update it back to its last state
					 ;; We will need to find a way to improve this in the future so that all phys variable changes and advances 
					 ;; can be accomplished. For this we may need a internal list of functions (& params) run within the struct
					 (if (equal ':|parameterchangefailed| v)
							(progn
							;#+:Windows (close-HumMod '(#_TerminateProcess (ccl::external-process-id *HumModProc*) 1))
							;#+:Unix (close-HumMod '(signal-external-process *HumModProc* 9))
							(create-phys-vars)
							 
							 (dotimes (i (floor (/ (mp-time) (phys-module-delay phys))))
								(update-phys-vars))
							 (go resetSetVal)))))))))

(define-condition modelSolver-error (error)
 ((text :initarg :text :reader text)))
							 
(defun start-HumMod ()

 (let (oldDir)
   (setf oldDir 
#+:ccl   (ccl::current-directory-name)
#+:sbcl	 (sb-posix:getcwd)
	)
#+:ccl	 (ccl::cwd *HumModDir*)
#+:sbcl	 (sb-posix:chdir *HumModDir*)
   (setf *HumModProc* 
	(run-program "ModelSolver.exe"
	 (list (concatenate 'string
	  "\"<root><model>" *HumModDir* "HumMod.DES</model><pipeid>"
	  *pipeID* "</pipeid></root>\"")) 
	  :wait nil :output t))
	(sleep 4)
#+:ccl (ccl::cwd oldDir)
#+:sbcl (sb-posix:chdir oldDir)
  )
	;;(For mind Modeling) Create pid file to close a leftover modelsolver process (if it isn't killed by model file)
	;;This now is offloaded to the model file so that each model created in parallel can use their specific ModelSolver PID
	#|(let (
#+:Windows (msPID (write-to-string (external-call "GetProcessId" (:* T) (external-process-id *HumModProc*) :UNSIGNED)))
#+:Unix (msPID (write-to-string (external-process-id *HumModProc*)))
		  (fileName (concatenate 'string (subseq *HumModDir* 0 (search "/" (subseq *HumModDir* 0 (- (length *HumModDir*) 9)) :from-end t)) "/modelSolver.")))
	 (with-open-file
	  (mS (concatenate 'string fileName "pid") :direction :output :if-exists :supersede :if-does-not-exist :create)
	   (format mS "~a" msPID))
	   
	 (with-open-file
	  (mS (concatenate 'string fileName (write-to-string (parse-integer (subseq msPID 22 (- (length msPID) 1)) :radix 16))) :direction :output :if-exists :overwrite :if-does-not-exist :create)
	   (format mS "~a~%" (write-to-string (parse-integer (subseq msPID 22 (- (length msPID) 1)) :radix 16)))))|#

	;;Try to clear any left around files in the stream (e.g., from an error)
	(clear-phys-files)
)

;;Can be used to close HumMod from terminal
(defmacro close-HumMod (fun) (eval fun))
	
;;Can be used to reset HumMod variables to initial model
(defun reset-HumMod ()
 (tagbody
	 resetCreate
(clear-phys-files)
  (let*
    ((phys (get-module physio))
	(pipeID (phys-module-pipeID phys))
	(solverInputFile (concatenate 'string *HumModDir* "SolverIn" pipeID))
	(solverOutputFile (concatenate 'string *HumModDir* "SolverOut" pipeID))
	;Reset/Restart Utility Message (needed to start getting values from HumMod model solver)
	(resetMessage "\"<solverin><restart/></solverin>\""))
	
	 (handler-case
	  (with-open-file
		(messageStream  solverInputFile
		:direction :output :if-exists :overwrite :if-does-not-exist :create)
			(format messageStream resetMessage)) ((or simple-error ccl::simple-file-error) () (go resetCreate)))
			
	 (while (probe-file solverInputFile))
	 (let ((currTime (get-universal-time)))
	  (while (and (not (probe-file solverOutputFile)) (< (- (get-universal-time) currTime) 5))))
	  (when (not (probe-file solverOutputFile)) (go resetCreate))
	  (while (and (probe-file solverOutputFile) (not (handler-case (delete-file solverOutputFile)
			(error () nil)))))))
)

;;Generate hash-table of physiological variables & Hash-Table of the order of the variables
;; and default values
(defun create-phys-vars () 
 (tagbody
	 resetCreate
	(clear-phys-files)
	;;Set Physiology Substrate periodic communication time (HumMod)
	(let* 
	  ((phys (get-module physio))
	  (pipeID (phys-module-pipeID phys))
	  ;;Set the name of the files used to input to model solver stream and to which solver outputs results
	  (solverInputFile (concatenate 'string *HumModDir* "SolverIn" pipeID))
	  (solverOutputFile (concatenate 'string *HumModDir* "SolverOut" pipeID))
	  ;Reset/Restart Utility Message (needed to start getting values from HumMod model solver)
	  (resetMessage "\"<solverin><restart/></solverin>\"")
	  ;Get Variables Message
	  (getVarsMessage "\"<solverin><requestvarroster/></solverin>\"")
	  ;We have the simulation go for X mins to stabilize some
	  ; -variables in HumMod (e.g. osmoreceptors), this also returns values
	  (getValsMessage 
		(concatenate 'string "\"<solverin><gofor><solutionint>1</solutionint><displayint>"
							"1</displayint></gofor></solverin>\""))
	  (physVarList nil)
	  (physValueList nil))
	  (setf (phys-module-init phys) t)
	  (setf (phys-module-HProc phys) *HumModProc*)

	;;Send reset & restart message to solver
	;; *This is needed for solver to correctly process messages sent*
	(handler-case
	  (with-open-file
		(messageStream  solverInputFile
		:direction :output :if-exists :overwrite :if-does-not-exist :create)
			(format messageStream resetMessage)) ((or simple-error ccl::simple-file-error) () (go resetCreate)))
	(while (probe-file solverInputFile))
	(let ((currTime (get-universal-time)))
	 (while (and (not (probe-file solverOutputFile)) (< (- (get-universal-time) currTime) 5))))
	 (when (not (probe-file solverOutputFile)) (go resetCreate))
	 (while (and (probe-file solverOutputFile) (not (handler-case (delete-file solverOutputFile)
			(error () nil)))))
 ;Create input file for hummod to give us a list of variables and digest the output file created by HumMod (w/ the variables). Loop back around if there is an error
 (tagbody getVars
	(handler-case (delete-file solverOutputFile)
			(error () nil))
   (when (not (phys-module-vars-baseLine-init phys))
	(handler-case 
	 (with-open-file
		(messageStream  solverInputFile
		:direction :output :if-exists :overwrite :if-does-not-exist :create)
			(format messageStream getVarsMessage))
		((or simple-error ccl::simple-file-error) () (go getVars)))

	(while (probe-file solverInputFile))

	(let ((currTime (get-universal-time)))
		(while (and (not (probe-file solverOutputFile)) (< (- (get-universal-time) currTime) 8))))

	(when (not (probe-file solverOutputFile)) (handler-case (delete-file solverOutputFile) (error () nil))  (go getVars))


(let ((parseStart (get-universal-time)))
  (tagbody parseVarList
	(when (> (- (get-universal-time) parseStart) 8) (go getVars))
	(handler-case (setf physVarList (s-xml:parse-xml-file solverOutputFile)) 
		((or file-error s-xml::xml-parser-error type-error) () 
			(go parseVarList)))
	;;To handle the times when the solver is on a slower machine and the files are output slower than expected at the initial setup
	 (when (equal (cadr physVarList) ':|valuesframe|)
	  (handler-case (delete-file solverOutputFile) (error () nil))
	  (handler-case (setf physVarList (s-xml:parse-xml-file solverOutputFile)) 
		((or file-error s-xml::xml-parser-error type-error) () 
			(go parseVarList))))
	 (setf (phys-module-physVarList phys) physVarList)
	 (when (not (init-list-to-hash physVarList)) (print "problem with init") (go getVars))
	(when (not physVarList) (go parseVarList))))))

	;Delete output file when finished with it
	(handler-case (delete-file solverOutputFile)
			(error () nil))

	;;Advance Simulator X mins (done to get level osmo-receptor values at a steady state)
   (let ((timeOut 10))	
	(tagbody
		 startGetVals
		(handler-case (delete-file solverOutputFile)
			(error () nil))
		 ;;Get new HumMod value list
		 (handler-case
		  (with-open-file
			 (messageStream solverInputFile
			 :direction :output :if-exists :overwrite :if-does-not-exist :create)
				(format messageStream getValsMessage)) ((or simple-error ccl::simple-file-error) () 
					(progn (handler-case (progn (delete-file solverInputFile) (delete-file solverOutputFile))
							(error () nil)) 
						   (go startGetVals))))

		(while (probe-file solverInputFile))
			(let ((currTime (get-universal-time)))
			 ;;We only wait so long for the file to be created
			 (handler-case (while (and (not (probe-file solverOutputFile)) (< (- (get-universal-time) currTime) timeOut))) (error () nil)))
			(when (not (probe-file solverOutputFile)) (go startGetVals))
	(let ((parseStart (get-universal-time)))
      (tagbody parseValList
		(when (> (- (get-universal-time) parseStart) 4) (go startGetVals)) 
			(handler-case (setf physValueList (s-xml:parse-xml-file solverOutputFile)) 
				((or file-error s-xml::xml-parser-error type-error) () 
					 (go parseValList)))
			(when (not physValueList) 
				(go parseValList))
			(when (equal (caadr physValueList) ':|varroster|)
				(while (not (handler-case (delete-file solverOutputFile)
					(error () nil))))
				(go parseValList))))))

			 (while (not (handler-case (delete-file solverOutputFile)
					(error () nil))))
			
				(setf (phys-module-physValList (get-module physio)) physValueList)
				(let ((varCount 0))

				;;Go through lists and replace matching values in hash table
				;; This is done because HumMod outputs values without corresponding
				;; variable names, instead the original order is maintained
				(loop
					for k in (phys-module-physVarList phys)
					do (if (not(equal ':|solverout| k))
							(loop
								for k2 in k
								do (progn
										(if (equal ':|valuesframe| k2) (go resetCreate))
										(when (and (not (equal ':|varroster| k2)) (not (equal ':|solutiondone| k2)))
											(setf varCount (+ varCount 1))
											(when (not (phys-module-vars-baseLine-init phys))
											  (setf (gethash (cadr k2) (phys-module-vars phys))
												 varCount)))))))
				;;Record values requested by model to list (to be analyzed later)
				(setf (phys-module-vals-analysis phys) (record-phys-vals (phys-module-recordedPhys phys) physValueList (phys-module-vals-analysis phys)))
				;;Record total number of variables
				(setf (phys-module-varCount phys) varCount))
	(when (eq (hash-table-count (phys-module-vars (get-module physio))) 0) (go resetCreate))
	(setf (phys-module-physValList phys) physValueList)
	(setf (phys-module-physValList-baseline phys) physValueList)
	(setf (phys-module-vars-baseLine-init phys) t)))
	(clear-phys-files)
)

;;Record values for variables specified by model
(defun record-phys-vals (recVarList allValList savedValList)
	(let ((valNumbers nil)
		 (tempValList nil))
	;Get value places
	 (dolist (recVar recVarList)
		(let ((valNum (gethash recVar (phys-module-vars (get-module physio)))))
			(when valNum
				(setf tempValList (if tempValList 
									(append tempValList (list (list recVar (cadar (nthcdr valNum (cadr allValList))))))
									(list (list recVar (cadar (nthcdr valNum (cadr allValList))))))))))
	 (if savedValList (append savedValList (list tempValList))
					  (list tempValList))))
					  
;;;find the time (in seconds) of the last event within the specified modules
(defun find-last-event-time (moduleList)
 (verify-current-mp 
   "mp-show-queue called with no current meta-process."
   (let ((events (meta-p-events (current-mp)))
		(lastTime nil))
    (dolist (event events (length events))
	(dolist (mod moduleList (length moduleList))
	 (if (or (eq (evt-module event) mod)
		  (eq (evt-module event) 'NONE))
			(if (or (eq lastTime nil) 
				  (> (evt-time event) lastTime))
					(setf lastTime (evt-time event))
					))))
	 (if lastTime
	  lastTime
	  0))))

;;;
(defun schedule-update-phys (delay)
 ;;if no events are currently scheduled, 
 ;; schedule for update function to be run again later if there is a conflict resolution event eventually scheduled
 ;; Else check to see if last event is after the current time + delay and if it isn't, schedule next update and continue
 (if
  (not (or 
	  (mp-modules-events 'procedural)
	  (mp-modules-events 'declarative)
	  (mp-modules-events :fatigue)
	  (mp-modules-events 'vision)
	  (mp-modules-events 'audio)
	  (mp-modules-events 'blending)
	  (mp-modules-events 'temporal)
	  (mp-modules-events 'goal)
	  (mp-modules-events 'imaginal)
	  (mp-modules-events 'motor)
	  (mp-modules-events 'speech)))
	(progn 
	 (schedule-event-after-change 'update-phys-vars :module 'physio :delay t :output t :maintenance t)
	 (return-from schedule-update-phys t))
	(progn
	 (if (> (+ (phys-module-lastUpdate (get-module physio)) delay) (find-last-event-time (list 'procedural 'declarative :fatigue 'vision 'audio 'blending 'temporal 'goal 'imaginal 'motor 'speech)))
	  (progn 
	   (schedule-event-after-change 'update-phys-vars :module 'physio :delay t :output t :maintenance t)
	   (if (phys-module-physValList (get-module physio))
		(return-from schedule-update-phys nil)
		(return-from schedule-update-phys t)))
	  (progn 
	   (schedule-event-relative delay 'update-phys-vars :module 'physio :priority :max :output t :maintenance t)
	   (return-from schedule-update-phys t)))))
)
					  
;;;Update Phys Variable Hash Table with variable info from
;;; HumMod *updateDelay should be obtained from buffer parameter and be in ms*
(defun update-phys-vars ()
	(let* 
	 ((phys (get-module physio))
	 ;;Convert update delay to HumMod format (mins) and then to string
	 (physUpdateDelay (float (/ (phys-module-delay phys) 60)))
	 (pipeID (phys-module-pipeID phys)))

	(let*
	 ((physUpdateDelay (remove #\" (write-to-string (format nil "~,10f" physUpdateDelay))))
	 ;;Set the name of the files used to input to model solver stream and to which solver outputs results
	 (solverInputFile (concatenate 'string *HumModDir* "SolverIn" pipeID))
	 (solverOutputFile (concatenate 'string *HumModDir* "SolverOut" pipeID))
	 (physValueList nil)
	 ;;Utility Messages that can be sent to solver
	 (getValsMessage 
			 (concatenate 'string "\"<solverin><gofor><solutionint>" physUpdateDelay
			 "</solutionint><displayint>" physUpdateDelay "</displayint></gofor></solverin>\"")))
			 
	  ;;
	  (when (null (schedule-update-phys (phys-module-delay phys))) (return-from update-phys-vars))
	  (while (probe-file solverOutputFile)
	  (handler-case (delete-file solverOutputFile)  (error () nil)))
	  (while (probe-file solverInputFile))
		(tagbody
		 startGetVals
		 ;;Get new HumMod value list
		 (handler-case
		  (with-open-file
			 (messageStream solverInputFile
			 :direction :output :if-exists :overwrite :if-does-not-exist :create)
				(format messageStream getValsMessage)) ((or simple-error (or file-error)) () (go startGetVals)))

		;wait for HumMod to digest input file
		(while (probe-file solverInputFile))
		
		;wait for HumMod to output corresponding file and if it takes too long, reset
		(let ((startTime (get-universal-time)))
		(while (and (not (probe-file solverOutputFile)) (< (- (get-universal-time) startTime) 5)))
		(when (not (probe-file solverOutputFile)) (go startGetVals))))

		 (tagbody parseValList
			(handler-case (setf physValueList (s-xml:parse-xml-file solverOutputFile)) 
				((or file-error s-xml::xml-parser-error type-error) () 
					 (go parseValList)))
			(when (not physValueList) 
				(go parseValList)))

			 (while (and (probe-file solverOutputFile) (not (handler-case (delete-file solverOutputFile)
					(error () nil)))))

	(setf (phys-module-physValList phys) physValueList)
	;;Record values requested by model to list (to be analyzed later)
	(let ((newVals (record-phys-vals (phys-module-recordedPhys phys) physValueList (phys-module-vals-analysis phys))))
		(when (car newVals) (setf (phys-module-vals-analysis phys) newVals)))
				
	(incf (phys-module-timeSliceCount phys))))
)

;;;Generate chunks from list of physiological variables
;;; and corresponding values
(defun phys-chunk-list (physVarList)
	(let ((newList '(ISA phys-var)))
	;;for each variable in our list
	(dolist (currSlot physVarList)
		;;append the slot (or slot value) to the list 
		(setf newList (append newList (list (read-from-string
				(string-upcase (remove #\space (car currSlot))))))))
	;make a chunk from our list
	(define-chunks-fct (list newList)))
)

(defun schedule-phys-events ()
	(let ((phys (get-module physio)))
	(when (phys-module-enabled phys)
		(schedule-event-relative 0.005 'create-phys-vars :module 'physio :priority :max :details "initialize physiological simulation system" :maintenance t)
		;;Schedule the function to keep these physiological values updated
		;(schedule-periodic-event (phys-module-delay phys) 'process-run-function :params `("update-phys-vars" update-phys-vars) :initial-delay 0.011 :module 'physio :priority :max :output t :maintenance t)
		;(schedule-periodic-event (phys-module-delay phys) 'update-phys-vars :initial-delay 0.006 :module 'physio :priority :max :output t :maintenance t)
		(schedule-event-relative 0.006 'update-phys-vars :module 'physio :priority :max :output t :maintenance t)
		#|(schedule-event 0.015 'set-food-intake :module 'physio :priority :min :params (list 0) :details "Make ACT-R control food intake")
		(schedule-event 0.015 'set-liquid-intake :module 'physio :priority :min :params (list 0) :details "Make ACT-R control liquid intake")|#))
)


;;;;;Cognition - Physiology Connection Function Section
;;;Set food intake (eating) variable to a value
(defun set-food-intake (value)
	(set-phys-vals (list (list "DietIntakeNutrition.Fixed?" 1) (list "DietIntakeNutrition.FixedIntake(xGoal)" value)))
)

;;;Set liquid intake (drinking) variable to a value
(defun set-liquid-intake (value)
	(set-phys-vals (list (list "DietIntakeH2O.Fixed?" 1) (list "DietGoalH2O.Rate(L/Day)" value)))
)

;Arousal = Arousal(fear) + Arousal(Epi,CRH)
;Arousal(Epi,CRH) = CRH-base/(3*((CRH-High-Perc*CRH-MAX)-base)) + EPI-base/(3*((EPI-High-Perc*EPI-MAX)-base))
;;;Compute current arousal level
(defun compute-phys-arousal ()
;If we havent had a chance to get phys variable values return 1, else compute
(if (< (mp-time) 0.006)
 1
 (let* ((physio (get-module physio))
	   (epi (cadar (car (get-phys-vals nil (list '("EpiPool.[Epi(pG/mL)]"))))))
	   (crh (cadar (car (get-phys-vals nil (list '("CorticotropinReleasingFactor.StressFactor"))))))
	   (epi-base (cadar (car (get-phys-vals t (list '("EpiPool.[Epi(pG/mL)]"))))))
	   (crh-base (cadar  (car (get-phys-vals t (list '("CorticotropinReleasingFactor.StressFactor"))))))
	   (arousal 0)
	   (crh-max (phys-module-crh-max (get-module physio)))
	   (crh-high-perc (phys-module-crh-high-perc (get-module physio)))
	   (epi-max (phys-module-epi-max (get-module physio)))
	   (epi-high-perc (phys-module-epi-high-perc (get-module physio)))
	   (crh-val (if (and crh crh-base)
				 (calc-crh-arousal (read-from-string crh) (read-from-string crh-base) crh-high-perc crh-max)
				 (/ 1 2)))
	   (epi-val (if (and epi epi-base)
				 (calc-epi-arousal (read-from-string epi) (read-from-string epi-base) epi-high-perc epi-max)
				 (/ 1 2))))
   (setf arousal (+ crh-val epi-val))
   arousal
  )))

;;Compute crh arousal value
(defun calc-crh-arousal (crh crh-base crh-high-perc crh-max)
 (/ crh (* crh-base 3)));(+ (/ (- crh crh-base) (* 6 (- (* crh-high-perc crh-max) crh-base))) (- 1 crh-high-perc)))

;;Compute epinephrine arousal value
(defun calc-epi-arousal (epi epi-base epi-high-perc epi-max)
 (/ epi (* epi-base 3)));(+ (/ (- epi epi-base) (* 6 (- (* epi-high-perc epi-max) epi-base))) (- 1 epi-high-perc)))

;;Compute cortisol factor
(defun compute-cort ()
 (let* ((physio (get-module physio))
	   (cort (cadar (car (get-phys-vals nil (list '("Cortisol.[Conc(uG/dL)]"))))))
	   (cort-base (cadar (car (get-phys-vals t (list '("Cortisol.[Conc(uG/dL)]"))))))
	   (cort-max (phys-module-cort-max physio))
	   (cort-high-perc (phys-module-cort-high-perc physio))
	   (cort-norm 0))
  (setf cort-norm (if (and cort cort-base)
					(/ (read-from-string cort) (read-from-string cort-base)) 0));(/ (- cort cort-base) (* cort-high-perc (- cort-max cort-base))))
  (if (<= cort-norm 0) 0 cort-norm)
 ))

;;Function to retrieve phys values for specified list
(defun get-phys-vals (baseLine physVals)
 (let ((myHash (phys-module-vars (get-module physio)))
	   (varValList nil)
	   (valList nil))
	 (if baseLine
	  (setf valList (phys-module-physValList-baseline (get-module physio)))
	  (setf valList (phys-module-physValList (get-module physio))))
	 (dolist (var physVals)
		(let ((valNum (gethash (car var) myHash)))
			(setf varValList (list (if varValList (append varValList (list (append (list (car var)) (list (cadar (nthcdr valNum (cadr valList)))))))
												  (list (append (list (car var)) (list (cadar (nthcdr valNum (cadr valList)))))))))))
		varValList))
;;;
;;;;Helpful functions for examining physiological data
(defun get-recorded-vals ()
	(phys-module-vals-analysis (get-module physio)))


;;;
;;;;;ACT-R Module Section
(defstruct phys-module
	;;Delay to update HumMod variables
	(delay)

	;;Events (recorded so that we can delete them if we need to)
	(create-phys-event)
	(set-food-event)
	(set-drink-event)
  
	;;Initial time to advance model
	(initial-advance 1440)
	esc
	busy

	(vars-baseLine-init nil)
	;;List used to record values and conduct data analysis
	(vals-analysis nil)

	(enabled nil)

	;;Number of times we skip updating values due to errors (Used for debugging)
	(skipCount 0)

	;;Number of times we successfully update the values (Used for debugging)
	(timeSliceCount 0)

	;;Used to communicate with HumMod Process
	(pipeID *pipeID*)
	(init nil)

	;;Holds our physiological variables and place in value list (e.g., 1, 2, 3, etc.)
	(vars (make-hash-table :test #'equal))
	;(vars-order (make-hash-table :test #'equal))
	
	;List form of physiological variables
	(physVarList nil)

	;List that holds current physiological values
	(physValList nil)

	;List that holds baseline physiological values
	(physValList-baseLine nil)

	;;HumMod Process Identifier
	(HProc nil)

	;;Switch for continuous startle reflex effect on memory noise (:ans)
	epi-ans
	
	;;Parameter set by user to record certain variables
	(recordedPhys nil);

	;;Debug param used to count num of times going thru vars analysis loop
	(debugMe 0)

	(changeVar-List nil)
	
	;Total number of physiological variables
	(varCount 0)

   ;;;Arousal Related vars
   ;;Internal arousal value (linked to epi & CRF and used by DM)
	(arousal 0)

	;;Obtained by setting integration nerve to value that results in max HR from (from Nater, La Marca 2006 - Stress-induced changes in human salivary alpha-amylase activity—associations with adrenergic activity)
	(epi-max 67)

    (crh-max 5)

	;;Found using highest value from Kirschbaum (1993) study (TSST)
    (cort-max 28)

	(epi-high-perc 0.75)
	(crh-high-perc 0.75)
	(cort-high-perc 0.75)

	(max-integ-nerve-activation 2.7)
	
	;;Holds the time of the (in ms) that the update-phys-vars function was run
	(lastUpdate 0)
)

;Create module everytime new model is defined
(defun create-phys-module (model-name)
	 (declare (ignore model-name))
	 (make-phys-module))

;Delete module when model is deleted (not needed right now)
(defun delete-phys-module (phys)
	(declare (ignore phys)))
;(start-HumMod)
;Reset module after model is reset or after initial creation of model
(defun reset-phys-module (phys)
	;;Close any previous versions of HumMod
	(when (and *HumModProc* (external-process-id *HumModProc*))
	 #+:Windows (external-call "TerminateProcess" (:* T) (external-process-id *HumModProc*) :UNSIGNED 1)
	 #+:Unix (signal-external-process *HumModProc* 9)
	 (setf *HumModProc* nil)
	 )
	;;Start HumMod
	(start-HumMod)
	;;All chunk-types used in the efferent buffer should be a subtype of phys-var
	(chunk-type phys-var)
	(setf (phys-module-physValList phys) nil)
	(setf (phys-module-physValList-baseLine phys) nil)
	(clear-phys-files)
	(schedule-event-relative 0.005 'schedule-phys-events :module 'physio :priority :max :details "Schedule all physio module-based setup events")
)

;Define our parameters created in define-module-fct
(defun phys-module-params (phys param)
	(if (consp param)
	 (case (car param)
	  (:phys-delay
	   (setf (phys-module-delay phys) (cdr param) ))
	  (:epi-ans
	   (setf (phys-module-epi-ans phys) (cdr param) ))
	  (:phys-enabled
	   (setf (phys-module-enabled phys) (cdr param) ))
	  (:esc
	   (setf (phys-module-esc phys) (cdr param) ))
	  (:recorded-phys
		 (setf (phys-module-recordedPhys phys) (cdr param))))

	 (case param
	  (:phys-delay
		(phys-module-delay phys))
	  (:epi-ans
	   (phys-module-epi-ans phys))
	  (:phys-enabled
	   (phys-module-enabled phys))
	  (:esc
	   (phys-module-esc phys))
	  (:recorded-phys
		 (phys-module-recordedPhys phys)))))

;Define query function for module (b = buffer)
(defun phys-module-query (phys b slot value)
	(case slot
	 (state
	  (case value
	   (error nil)
	   (busy (phys-module-busy phys))
	   (free (not (phys-module-busy phys)))
	   (t (print-warning
		   "Bad state query to ~s buffer" b))))
	 (t (print-warning
		 "Invalid slot ~s in query to buffer ~s" slot b)))
)

(defun free-phys-module (phys)
	(setf (phys-module-busy phys) nil)
)

;;;Define request function for module (slides 53-54 in extending-actr.ppt)
(defun phys-module-requests (phys buffer spec)
	;;start underlying substrate 
	(if (not (phys-module-init phys))
	 (progn

		;(phys-create-chunk phys spec)
		#|(schedule-periodic-event (phys-module-delay phys) 'update-phys-vars
		:module 'physio :priority :max :output nil :maintenance t)|#

		;;Schedule periodic updating of ans parameter (eventually I need to make this only happen if call to phys-substrate buffer is made)
		(if (phys-module-epi-ans phys)
		(schedule-periodic-event (phys-module-delay phys) 'do-epi-ans2
		:priority :max :initial-delay 1 :module :none :output nil :maintenance t))

		;;Make it so cognition controlls eating (as opposed to HumMod)
		(set-food-intake 0)))
	(if (eq buffer 'efferent)
		(phys-create-chunk phys spec))
)

;;;;For now only explicit changes of the value will be parsed, later we will add relative changes (i.e. original value +- chunk slot value)
(defun phys-create-chunk (phys spec)
		(progn	(if (or (eq (chunk-spec-chunk-type spec) 'phys-var)
						(eq (cadr (chunk-type-supertypes-fct (chunk-spec-chunk-type spec))) 'phys-var))
				  (let ((delay 0))
					(dolist (changeVar (chunk-spec-slot-spec spec))
					 (schedule-event-relative delay 'set-phys-vals
						:module 'physio :priority :min :params (list (list (list (write-to-string (cadr changeVar)) (caddr changeVar))))
						:details "Setting physiological variables"))
						 (setf (phys-module-busy phys) t)
						;;Put chunk into buffer and set module back to free after delay seconds
						;;Use schedule event to update variables every delay
						(schedule-set-buffer-chunk 'efferent (define-chunks-fct (list (chunk-spec-to-chunk-def spec))) delay 
							:module 'physio :priority :min))
				  (progn (print (chunk-spec-chunk-type spec))
					 (print "Warning - invalid chunk-type in efferent buffer")))))


(define-module-fct 'physio
				   '(phys-substrate efferent output)
	(list 
		(define-parameter 
			:phys-delay
			:documentation "Delta for HumMod updates"
			:default-value .03
			:valid-test (lambda (x) (and (numberp x) (>= x 0.002)))
			:warning "Non-negative number greater than 2ms"
			:owner t)
		(define-parameter
			:esc
			:owner nil)
		(define-parameter
			:epi-ans
			:documentation "Switch to turn on/off Epinephrine effect on memory noise (:ans)"
			:default-value nil
			:valid-test (lambda (x) (or (typep x 't) (typep x 'nil)))
			:owner t)
		
		(define-parameter
		:recorded-phys
		:documentation "Physiological (HumMod) variables to be recorded during the simulation"
		:default-value nil
		:valid-test (lambda (x) (listp x))
		:owner t)

		(define-parameter
		:phys-enabled
		:documentation "Swicth variable for physiology to continue to run w/ model"
		:default-value nil
		:valid-test (lambda (x) (or (typep x 't) (typep x 'nil)))
		:owner t)
	)
	:version "1.0"
	:documentation
		"Module for underlying physiological substrate (via HumMod mechanisms)"
	:creation 'create-phys-module
	:reset 'reset-phys-module
	:delete 'delete-phys-module
	:params 'phys-module-params
	:query 'phys-module-query
	:request 'phys-module-requests
)