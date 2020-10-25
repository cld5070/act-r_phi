	#| Copyright 2017 Christopher L. Dancy II
		This program is free software: you can redistribute it and/or modify
		 it under the terms of the GNU General Public License as published by
		 the Free Software Foundation, either version 3 of the License, or
		 (at your option) any later version.

		 This program is distributed in the hope that it will be useful,
		 but WITHOUT ANY WARRANTY; without even the implied warranty of
		 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the
		 GNU General Public License for more details.

		 You should have received a copy of the GNU General Public License
		 along with this program.	If not, see <http://www.gnu.org/licenses/>.
|#

;;;By Christopher L. Dancy II
;;;Dept of Computer Science, Bucknell University
;;;Made to be used with HumMod v1.6.2 - Modular
;;;---------------
;;; ---For any nerve activity related to heart-rate, HR shouldn't go above : 208-(0.7*age) : (from H Tanaka, KD Monahan 2001).

;;; Tested w/ CCL
;;; Should theoretically work with sbcl
;;;	(*features* stil need to be tested for all implementation specific functions)


;;Thread library
;(eval-when (:compile-toplevel :load-toplevel :execute)
;	(let ((*compile-file-pathname* nil))
;		(asdf:load-system :bordeaux-threads)))

#|(load (merge-pathnames "sxml/package.lisp" *LOAD-TRUENAME*))
(load (merge-pathnames "sxml/dom.lisp" *LOAD-TRUENAME*))
(load (merge-pathnames "sxml/lxml-dom.lisp" *LOAD-TRUENAME*))
(load (merge-pathnames "sxml/sxml-dom.lisp" *LOAD-TRUENAME*))
(load (merge-pathnames "sxml/xml.lisp" *LOAD-TRUENAME*))
(load (merge-pathnames "sxml/xml-struct-dom.lisp" *LOAD-TRUENAME*))|#

;;;Function to test for non ACT-R/Phi based events in queue (modified run function in scheduling.lisp to use)
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

(defun wait-delete-output (out-file &optional (time-out 500))
	(let ((timed-out nil)
		    (end-time (+ (get-universal-time) time-out)))
		(while (and (not (probe-file out-file)) (< (get-universal-time) end-time)))
		(when (not (probe-file out-file)) (setf timed-out t))
		(handler-case
			(delete-file out-file)
			(error () nil))
		timed-out))

(defvar *HumModOutStream* (make-string-output-stream))

(defvar *HumModInStream* nil)
;;;Custom Physiology Function Section
;;HumMod Directory variable
#+:windows	(defvar *HumModDir* (subseq (namestring *LOAD-TRUENAME*) 0 (search (file-namestring *LOAD-TRUENAME*) (namestring *LOAD-TRUENAME*))))
#+:linux	(defvar *HumModDir* (substitute #\\ #\/ (subseq (namestring *LOAD-TRUENAME*) 0 (search (file-namestring *LOAD-TRUENAME*) (namestring *LOAD-TRUENAME*))) :from-end t :count 1));(subseq (namestring *LOAD-TRUENAME*) 0 (search (file-namestring *LOAD-TRUENAME*) (namestring *LOAD-TRUENAME*))))
#+:darwin	 (defvar *HumModDir* (substitute #\\ #\/ (subseq (namestring *LOAD-TRUENAME*) 0 (search (file-namestring *LOAD-TRUENAME*) (namestring *LOAD-TRUENAME*))) :from-end t :count 1))

(format t "+++++++++~&~s~&++++++----+++++" (substitute #\ #\/ (subseq (namestring *LOAD-TRUENAME*) 0 (search (file-namestring *LOAD-TRUENAME*) (namestring *LOAD-TRUENAME*))) :from-end t :count 1))

;;We use this so we have a reference to the next update-event that is scheduled to be run
(defvar *nextUpdateEvent* nil)

;;;Print out all key-value pairs: used with
(defun print-hash-table (key value)
	(format t "~s - ~s" key value))

;;;clear out any unwanted files
(defun clear-phys-files ()
	(sleep 0.005)
	(dolist (fileName (directory (concatenate 'string *HumModDir* "SolverOut" (phys-module-pipeID (get-module physio)))))
		(handler-case (delete-file fileName) (error () nil)))
	(dolist (fileName (directory (concatenate 'string *HumModDir* "SolverIn" (phys-module-pipeID (get-module physio)))))
		(handler-case (delete-file fileName) (error () nil)))
	(dolist (fileName (directory (concatenate 'string *HumModDir* (phys-module-pipeID (get-module physio)) ".tem")))
		(handler-case (delete-file fileName) (error () nil))))

;;;Initialize Variable and Variable Order Hash-Table for physiological variable order
(defun init-var-list-to-hash (myList)
	(handler-case
		(let ((phys (get-module physio)))
			(loop
				for k in myList
				do (when (not (equal ':|solverout| k))
					(loop
						for k2 in k
						do (when (not (equal ':|varroster| k2))
							(setf (gethash (cadr k2) (phys-module-vars phys)) 0))))))
		(error (e) (progn (print e) nil (return-from init-var-list-to-hash))))
t)

;;;Request values from Model Solver
(defun set-as-init-cond ()
	"Sets current state as initial conditions for model"
	(let* ((phys (get-module physio))
				 (solver-input-file (concatenate 'string *HumModDir* "SolverIn" (phys-module-pipeID phys)))
				 (solver-in-msg "<solverin><newics/></solverin>")
				 (solver-out-file (concatenate 'string *HumModDir* "SolverOut" (phys-module-pipeID phys))))
		(with-open-file
			(messageStream	solver-input-file
				:direction :output :if-exists :overwrite :if-does-not-exist :create)
			(format messageStream solver-in-msg))
		(while (not (probe-file solver-out-file)))))

(defun create-ics-from-out (out-file ics-file-name phys &optional (delete-solverout? nil))
	"Creates a suitable initial conditions file from the given output file"
	(let ((out-vals (cdadr (s-xml:parse-xml-file out-file)))
				(init-str "<ics>"))
		(loop for val in out-vals and var in (cdadr (phys-module-physVarList phys))
			do (progn
				(setf init-str
					(concatenate 'string init-str "~&<var>~&<name>" (cadr var) "</name>~&<val>"
					 (cadr val) "</val>~&</var>"))))
		(setf init-str (concatenate 'string init-str "</ics>"))
		(with-open-file
			(msg-stream ics-file-name :direction :output :if-exists :overwrite
				:if-does-not-exist :create)
			(format msg-stream init-str)))
		(when delete-solverout?
			(delete-file out-file)))

;;;Advance the HumMod model a specified amount of time (mins)
(defun advance-phys (timeSlice)
	"Advance the modelsolver forward in time
		timeSlice - The time in minutes"
	(let*	((advanceMessage
				 (concatenate 'string
						"\"<solverin><gofor><solutionint>"
						(format nil "~10,$" timeSlice)
						"</solutionint><displayint>"
						(format nil "~10,$" timeSlice)
						"</displayint></gofor></solverin>\""))
				 (phys (get-module physio))
				 (solverInputFile
					(concatenate 'string
						*HumModDir* "SolverIn" (phys-module-pipeID phys)))
				 (solverOutputFile
					(concatenate 'string
						*HumModDir* "SolverOut" (phys-module-pipeID phys)))
				 (max-wait
					(cond
						((<= timeSlice 2) 1)
						((< timeSlice 10) 6)
						((< timeSlice 60) 12)
						((< timeSlice 600) 24)
						((< timeSlice 3600) 48)
						(t 480))))
		(clear-phys-files) ;;Make sure we don't have any files left over for some reason

	(tagbody resetAdvance
		(model-output "Advancing: ~S~&" timeSlice)
		;;Create new input file for modelsolver to digest w/ advance time message
		;; (or keep trying until we don't get an error)
		(handler-case
			(with-open-file
				(messageStream	solverInputFile
					:direction :output :if-exists :overwrite :if-does-not-exist :create)
				(format messageStream advanceMessage))
			((or
					#+:ccl ccl::simple-file-error
					#+:sbcl sb-impl::simple-file-error
					simple-error) () (go resetAdvance)))
		;;Wait for file to be digested by ModelSolver
		(while (probe-file solverInputFile))
		;;Wait for ModelSolver to advance and output data from advancing specified time
		(let ((currTime (get-universal-time)))
			(handler-case
				(while (and (not (probe-file solverOutputFile)) (< (- (get-universal-time) currTime) max-wait)))
				(error (e) (print (concatenate 'string "Error near line 139 in Phys Module - " (write-to-string e)))))

		;;If we didn't get the output file, go back and do this all over again
		(when (not (probe-file solverOutputFile)) (print "Issue @ 141 - PT") (print (- (get-universal-time) currTime)) (go resetAdvance))))

	;;Translate those output data (which are in an xml file) into a list
	(tagbody parseValList
		(handler-case (setf physValueList (s-xml:parse-xml-file solverOutputFile))
			((or file-error s-xml::xml-parser-error type-error) ()
			(go parseValList)))
		(when (not physValueList)
			(go parseValList))
		;;If the modelsolver gave us a variable roster instead of the corresponding value list
		;; (When does this happen? Keeping it, but should investigate at some point)
		(when (equal (caadr physValueList) ':|varroster|)
			(while (not (handler-case (delete-file solverOutputFile)
				(error () nil))))
			(go parseValList)))
	;;Delete the output file after we've harvested those data
	(while (probe-file solverOutputFile)
		(handler-case
			(delete-file solverOutputFile)
			(error () nil)))

	;;Save the new list of values
	(setf (phys-module-physValList phys) physValueList)
	;;Record values requested by model to list (so that they can be analyzed later)
	(let ((newVals (record-phys-vals (phys-module-recordedPhys phys) physValueList (phys-module-vals-analysis phys))))
		(when (car newVals) (setf (phys-module-vals-analysis phys) newVals)))

		;;Record advance-phys to internal event trace
		(push (list #'advance-phys (list timeSlice)) (phys-module-solverActionTrace phys))))

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
				(setf varValList
					(list
						(if varValList
							(append varValList (list (append (list (car var)) (list (cadar (nthcdr valNum (cadr valList)))))))
							(list (append (list (car var)) (list (cadar (nthcdr valNum (cadr valList)))))))))))
		varValList))

;;;Set HumMod Vars using specified list
(defun set-phys-vals (varValList)
	"Set HumMod variables using the input list
		varValList - a list of (variable value) pairs"
	(tagbody resetSetVal
		(clear-phys-files)
		(model-output "Changing the following physiology: ~a~&" varValList)
		;(format t "Changing the following physiology: ~a~&" varValList)
		(let ((phys (get-module physio))
					(setPhysMessage "<solverin>")
					(timeOut 6)
					(num-param-changes 0))
		;;Construct message to be sent to new HumMod Solver Process
		;; -We must find the chunk in the hash-table because the request to HumMod is case sensitive
		(dolist (v varValList)
			(loop
				for k being the hash-keys in (phys-module-vars phys)
				do
					(when (equal (string-upcase (car v)) (string-upcase k))
						(setq setPhysMessage
							(concatenate 'string
								setPhysMessage
								"<setvalue><var>" k "</var><val>"
								(format nil "~10,$" (cadr v)) "</val></setvalue>"))
						(incf num-param-changes))))
		(setf setPhysMessage (concatenate 'string setPhysMessage "</solverin>"))
		(let ((solverOutputFile (concatenate 'string *HumModDir* "SolverOut" (phys-module-pipeID phys)))
					(solverInputFile (concatenate 'string *HumModDir* "SolverIn" (phys-module-pipeID phys))))
			(tagbody startGetVals
				;;Set HumMod variables (by writing file to be digested by HumMod)
				(handler-case
					(with-open-file
						(messageStream solverInputFile
							:direction :output :if-exists :supersede :if-does-not-exist :create)
						(format messageStream setPhysMessage))
					((or
							#+:ccl ccl::simple-file-error
							#+:sbcl sb-impl::simple-file-error
							simple-error) () (go startGetVals)))
				(while (probe-file solverInputFile))
				(let ((currTime (get-universal-time)))
					;;We only wait so long for the file to be created
					(handler-case
						(while
							(and (not (probe-file solverOutputFile))
								(< (- (get-universal-time) currTime) timeOut)))
						(error (e) (format t "Error 200 Phys - ~a~%" (write-to-string e)))))
				;;If we ran out of time waiting for the file, start over
				(when (not (probe-file solverOutputFile)) (go startGetVals)))
				(let ((checkError nil))
					(handler-case
						(setf checkError (s-xml:parse-xml-file solverOutputFile))
						((or file-error s-xml::xml-parser-error type-error) () nil))
					;;The model solver may output multiple files (one for each parameter changed)
					(let ((currTime (get-universal-time)))
						;;We only wait so long for the file to be created
						(handler-case
							(while
								(and (not (probe-file solverOutputFile))
									(< (- (get-universal-time) currTime) timeOut)))
							(error (e) (format t "Error 214 Phys - ~a~%" (write-to-string e)))))
					;;The multiple output files don't occur for the prebuilt (Windows) Model Solver
					(wait-delete-output solverOutputFile)
					;;How many output files we should expect depends upon the number of param changes
					#|(while (> num-param-changes 0)
						(wait-delete-output solverOutputFile)
						(print "236 PT")
						(wait-delete-output solverOutputFile)
						(decf num-param-changes)
						(print "239 PT"))|#
					(dolist (v checkError)
						;;If we have an error, then we reset the entire physiology system and
						;; (try to) update it back to its last state
						;;This has been improved so that we now should be able to update to
						;; our state before the parameter change issue
						(when (equal ':|parameterchangefailed| v)
							(format t "236 PhysThread")
							(create-phys-vars)
							;;Go through list of actions affecting modelsolver and complete all of the actions
							;; (we reverse the list so we start from the beginning of the model run)
							(dolist (msAction (reverse (phys-module-solverActionTrace phys)))
								(apply (car msAction) (cdr msAction)))
							(go resetSetVal)))))
	;;Record set-phys-vals call to internal event trace
	(push (list #'set-phys-vals (list varValList)) (phys-module-solverActionTrace phys)))))

(define-condition modelSolver-error (error)
 ((text :initarg :text :reader text)))

(defun start-HumMod (phys)

	;;So we can use basic windows api functions to restart a process if we're on a windows platform
	;; This doesn't seem to be needed anymore, but keeping here for legacy purposes for now
	;#+:Windows (open-shared-library "kernel32.dll")
 "Starts HumMod (the ModelSolver)"
	(let* ((old-dir)
				(get-module-physio)
				(progName
					#+:windows	"ModelSolver.exe"
					#+:darwin		"./ModelSolverMac"
					#+:linux		"./ModelSolverLinux"
					)
				(pipeID (phys-module-pipeID phys))
				(solverOutputFile (concatenate 'string *HumModDir* "SolverOut" pipeID))
				(model (concatenate 'string
				  "\"<root><model>" *HumModDir* "HumMod.DES</model><pipeid>"
				  pipeID "</pipeid></root>\""))
				(hide-gui " \"True\"")
				(non-gui-version
					(if (phys-module-non-gui phys)
						" \"True\""
						" \"False\""))
				(activity-timeout " 60"))

		(setf old-dir
			#+:ccl	(ccl::current-directory-name)
			#+:sbcl	(sb-posix:getcwd)
			)
		#+:ccl	(ccl::cwd *HumModDir*)
		#+:sbcl	(sb-posix:chdir *HumModDir*)
		(setf (phys-module-HProc phys)
			(run-program progName
				(list (concatenate 'string model hide-gui non-gui-version activity-timeout))
								:wait nil :output *HumModOutStream*))
		;;This gives the modelsolver time to process the initial HumMod model
		#+:ccl	(ccl::cwd old-dir)
		#+:sbcl	(sb-posix:chdir old-dir)
		;;Wait for the output file from starting HumMod, then delete it
		(while (not(probe-file solverOutputFile)))

		(handler-case (delete-file solverOutputFile)	(error () nil))))

;;Can be used to close HumMod from terminal
(defun close-HumMod (phys)
	(when (and (phys-module-HProc phys) (external-process-id (phys-module-HProc phys)))
		#+:Windows (external-call "TerminateProcess" (:* T) (external-process-id (phys-module-HProc phys)) :UNSIGNED 1)
		#+:Unix (signal-external-process (phys-module-HProc phys) 9)
		(setf (phys-module-HProc phys) nil)))

(setf ccl:*break-hook*
	(lambda (cond hook)
		(declare (ignore cond hook))
		(format t "Cleaning up ...")
		(close-HumMod (get-module physio))
		(ccl:quit)))

;;;Can be used to reset HumMod variables to initial model
(defun reset-HumMod ()
	(tagbody resetCreate
		(clear-phys-files)
		(let* ((phys (get-module physio))
					(pipeID (phys-module-pipeID phys)) ;Used by model solver as UID for in/out files
					(solverInputFile (concatenate 'string *HumModDir* "SolverIn" pipeID)) ;Filename for file that will be digested by modelsolver
					(solverOutputFile (concatenate 'string *HumModDir* "SolverOut" pipeID)) ;Filename for file that will be output by modelsolver
					;Reset/Restart Utility Message (needed to start getting values from HumMod model solver)
					(resetMessage "\"<solverin><restart/></solverin>\""))
			;;Create input file & place reset message in file
			;; This will tell the modelsolver to restart (needed to work correctly)
			(handler-case
				(with-open-file
					(messageStream	solverInputFile
					:direction :output :if-exists :overwrite :if-does-not-exist :create)
					(format messageStream resetMessage))
				((or
					#+:ccl ccl::simple-file-error
					#+:sbcl sb-impl::simple-file-error
					simple-error) () (go resetCreate)))

			(while (probe-file solverInputFile))
			(let ((currTime (get-universal-time)))
				(while (and (not (probe-file solverOutputFile)) (< (- (get-universal-time) currTime) 5))))
			(when (not (probe-file solverOutputFile)) (go resetCreate))
		(while (and (probe-file solverOutputFile) (not (handler-case (delete-file solverOutputFile)
			(error () nil))))))))

(defun load-HumMod-ICs (init-filename)
	"Used to load initial conditions based on ICS file saved from HumMod
		Inputs:
			filename - Name of file to be read in and parsed"

	(let* ((phys (get-module physio))
				 (solverInputFile (concatenate 'string *HumModDir* "SolverIn" (phys-module-pipeID phys)))
				 (solverOutputFile (concatenate 'string *HumModDir* "SolverOut" (phys-module-pipeID phys)))
				 (init-vals-msg "<solverin>")
				 ordered-val-list
				 old-dir
				 ics-val-list)
		(setf old-dir
			#+:ccl	(ccl::current-directory-name)
			#+:sbcl	(sb-posix:getcwd)
			)
		#+:ccl	(ccl::cwd *HumModDir*)
		#+:ccl	(ccl::cwd "../")
		#+:sbcl	(sb-posix:chdir *HumModDir*)
		#+:sbcl	(sb-posix:chdir "../")
		(setf ics-val-list (cdr (s-xml:parse-xml-file init-filename)))
		(setf init-vals-msg (concatenate 'string init-vals-msg (list #\newline) "<sending_current_values>" (list #\newline)))
		;Go through list returned by parsing and pull vals out
		; and put into msg (assuming 1st 2 elements are ICS & time)
		(dolist (var-val ics-val-list)
			(setf init-vals-msg
				(concatenate 'string init-vals-msg "<val>" (remove #\Space (cadr (caddr var-val))) "</val>" (list #\newline) )))
		;Add closing tag
		(setf init-vals-msg (concatenate 'string init-vals-msg "</sending_current_values>~&"))
		(setf init-vals-msg (concatenate 'string init-vals-msg "</solverin>"))

		(with-open-file
			(messageStream	(concatenate 'string init-filename ".ICSconv")
			:direction :output :if-exists :overwrite :if-does-not-exist :create)
			(format messageStream init-vals-msg))

		;;Send new values to model solver
		(handler-case
			(with-open-file
				(messageStream	solverInputFile
				:direction :output :if-exists :overwrite :if-does-not-exist :create)
				(format messageStream init-vals-msg))
			((or
				#+:ccl ccl::simple-file-error
				#+:sbcl sb-impl::simple-file-error
				simple-error) (e) (format t "Error loading ICS file!~&" e)))
			;Wait for solverInput file to be digested (and get of Output files if there are any holdin up the ModelSolver digesting the input files
			(while (probe-file solverInputFile)
				(handler-case (delete-file solverOutputFile)
					(error () nil)))
			#+:ccl	(ccl::cwd old-dir)
			#+:sbcl	(sb-posix:chdir old-dir)); (sleep 20)
			)

;;Generate hash-table of physiological variables & Hash-Table of the order of the variables
;; and default values
(defun create-phys-vars ()
	(let ((phys (get-module physio)))
		(tagbody resetCreate
			(clear-phys-files)
			(setf (phys-module-init phys) t)
			;;Set Physiology Substrate periodic communication time (HumMod)
			(let*	((pipeID (phys-module-pipeID phys))
						 ;;Set the name of the files used to input to model solver stream and to which solver outputs results
						 (solverInputFile (concatenate 'string *HumModDir* "SolverIn" pipeID))
						 (solverOutputFile (concatenate 'string *HumModDir* "SolverOut" pipeID))
						 (initial-advance-time (format nil "~10,$" (phys-module-initial-advance phys)))

						 ;Restart Utility Message (needed to start getting values from HumMod model solver)
						 (resetMessage "<solverin><restart/></solverin>~&")

						 ;Get Variables Message
						 (getVarsMessage "<solverin><requestvarroster/></solverin>~&")

						 (getValsMessage
							(concatenate 'string
								"<solverin><gofor><solutionint>" initial-advance-time
								"</solutionint><displayint>" initial-advance-time
								"</displayint></gofor></solverin>~&"))
						 (physVarList nil)
						 (physValueList nil))
				;;Send restart message to solver
				;; *This is needed for solver to correctly process messages sent*
				(clear-phys-files)
				(handler-case
					(with-open-file
						(messageStream	solverInputFile
							:direction :output :if-exists :overwrite :if-does-not-exist :create)
							(format messageStream resetMessage))
					((or
							#+:ccl ccl::simple-file-error
							#+:sbcl sb-impl::simple-file-error
							simple-error) () (go resetCreate)))
				;Wait for solverInput file to be digested (and get of Output files if there are any holdin up the ModelSolver digesting the input files
				(while (probe-file solverInputFile))
				(when (wait-delete-output SolverOutputFile 5) (go resetCreate))
				;; Initialize with stable values (default is obtained from running sim 1 week)
				(format t "Loading ICS!~&")
				(load-HumMod-ICs (phys-module-ics-file phys))
				(format t "Done!~&")

;(sleep 0.05)
				;Create input file for hummod to give us a list of variables and digest the output file created by HumMod (w/ the variables). Loop back around if there is an error
				(tagbody getVars
					(handler-case
						(delete-file solverOutputFile)
						(error () nil))
					(when (not (phys-module-vars-baseLine-init phys))
					(handler-case
						(with-open-file
							(messageStream	solverInputFile
								:direction :output :if-exists :overwrite :if-does-not-exist :create)
							(format messageStream getVarsMessage))
						((or
								#+:ccl ccl::simple-file-error
								#+:sbcl sb-impl::simple-file-error
								simple-error) () (go getVars)))
					;;Wait for input file to get digested by ModelSolver
					(let ((currTime (get-universal-time)))
						(while (and (not (probe-file solverOutputFile)) (< (- (get-universal-time) currTime) 10))))
					;;If we ran out of time for some reason, start the process all over again
					(when (not (probe-file solverOutputFile))
						(handler-case
							(delete-file solverOutputFile)
							(error () nil))
						(go getVars))
					;;Parse the list of variables output by the ModelSolver
					(let ((parseStart (get-universal-time)))
						(tagbody parseVarList
							(when (> (- (get-universal-time) parseStart) 10)
								(go getVars))
							(handler-case
								(setf physVarList (s-xml:parse-xml-file solverOutputFile))
								((or file-error s-xml::xml-parser-error type-error) () (go parseVarList)))
							;;To handle the times when the solver is on a slower machine
							;; and the files are output slower than expected at the initial setup
							(when (equal (cadr physVarList) ':|valuesframe|)
								(handler-case
									(delete-file solverOutputFile)
									(error () nil))
								(handler-case
									(setf physVarList (s-xml:parse-xml-file solverOutputFile))
									((or file-error s-xml::xml-parser-error type-error) () (go parseVarList))))
							(setf (phys-module-physVarList phys) physVarList)
							(when (not (init-var-list-to-hash physVarList)) (print "problem with init")
								(go getVars))
							(when (not physVarList) (go parseVarList))))))
				;Delete output file when finished with it
				(handler-case (delete-file solverOutputFile)
						(error () nil))
				(let ((timeOut 120))
					(tagbody startGetVals
						;;Get new value list output by the ModelSolver
						(handler-case
							(with-open-file
								(messageStream solverInputFile
									:direction :output :if-exists :overwrite :if-does-not-exist :create)
								(format messageStream getValsMessage))
							((or
									#+:ccl ccl::simple-file-error
									#+:sbcl sb-impl::simple-file-error
									simple-error) ()
								(progn
									(handler-case
										(progn (delete-file solverInputFile) (delete-file solverOutputFile))
										(error () nil))
									(go startGetVals))))

						(while (probe-file solverInputFile)) ;Wait for input file to be digested
						(let ((currTime (get-universal-time)))
							;;We only wait so long for the file to be created
							(handler-case
								(while (and (not (probe-file solverOutputFile)) (< (- (get-universal-time) currTime) timeOut)))
								(error () nil)))
						;;When we ran out of time, start this section of the code over
						(when (not (probe-file solverOutputFile)) (go startGetVals))
						;;Parse the list of values output by ModelSolver
						(let ((parseStart (get-universal-time)))
							(tagbody parseValList
								(when (> (- (get-universal-time) parseStart) 4) (go startGetVals))
								(handler-case
									(setf physValueList (s-xml:parse-xml-file solverOutputFile))
									((or file-error s-xml::xml-parser-error type-error) () (go parseValList)))
								;;If parsing the output file didn't error, but physValList is still nil
								;; (Can this actually happen?
								;; May want to explore in the future to see if I need to do this)
								(when (not physValueList)
									(go parseValList))

								;;If the modelsolver happens to give us a variable roster
								;; instead of list, delete and redo this section of code
								(when (equal (caadr physValueList) ':|varroster|)
									(while (not (handler-case (delete-file solverOutputFile)
										(error () nil))))
									(go parseValList))))))

				(while (not (handler-case (delete-file solverOutputFile) (error () nil))))
				;;Set our current values
				(setf (phys-module-physValList (get-module physio)) physValueList)
				;;Go through lists and replace matching values in hash table
				;; This is done because HumMod outputs values without corresponding
				;; variable names, instead the original order is maintained
				(let ((varCount 0))
					(dolist (k (phys-module-physVarList phys))
						(if (not(equal ':|solverout| k))
							(dolist (k2 k)
								(if (equal ':|valuesframe| k2) (go resetCreate))
								(when (and (not (equal ':|varroster| k2)) (not (equal ':|solutiondone| k2)))
									(setf varCount (+ varCount 1))
									(when (not (phys-module-vars-baseLine-init phys))
										(setf (gethash (cadr k2) (phys-module-vars phys))
										 varCount))))))
					;;Record total number of variables
					(setf (phys-module-varCount phys) varCount))
				;;Record values requested by model to list (to be analyzed later)
				(setf (phys-module-vals-analysis phys) (record-phys-vals (phys-module-recordedPhys phys) physValueList (phys-module-vals-analysis phys)))
			;;If our variable hash-table didn't get filled, run code again
			(when (eq (hash-table-count (phys-module-vars phys)) 0)
				(go resetCreate))
		;; If we have initial conditions for an experiment, load those initial conditions
		(when (phys-module-ics-exp-file phys)
			(load-HumMod-ICs (phys-module-ics-exp-file phys)))
		(setf (phys-module-physValList phys) physValueList)
		(setf (phys-module-physValList-baseline phys) physValueList)
		(setf (phys-module-vars-baseLine-init phys) t))))
	(clear-phys-files))

;;Record values for variables specified by model
(defun record-phys-vals (recVarList allValList savedValList)
	(let ((valNumbers nil)
				(tempValList nil))
	;Get value places and use those places to find value in allValList
		(dolist (recVar recVarList)
			(let ((valNum (gethash recVar (phys-module-vars (get-module physio)))))
				(when valNum
					(setf tempValList
						(if tempValList
							(append tempValList (list (list recVar (cadar (nthcdr valNum (cadr allValList))))))
							(list (list recVar (cadar (nthcdr valNum (cadr allValList))))))))))
		;;;Return values either as new list, or appended to existing list
		(if savedValList
			(append savedValList (list tempValList))
			(list tempValList))))

;;;find the time (in seconds) of the last event within the specified modules
(defun find-last-event-time (moduleList)
	(verify-current-mp
	"mp-show-queue called with no current meta-process."
		(let ((events (meta-p-events (current-mp)))
					(lastTime nil))
			(dolist (event events (length events))
				(dolist (module moduleList (length moduleList))
					(when (or (eq (act-r-event-module event) module)
						(eq (act-r-event-module event) 'NONE))
						(if (or (eq lastTime nil)
								(> (* (act-r-event-mstime event) 0.001) lastTime))
								(setf lastTime (* (act-r-event-mstime event) 0.001))
								))))
	 (if lastTime
		lastTime
		0))))

;;;
(defun schedule-update-phys (delay)
 ;;if no events are currently scheduled,
 ;; schedule for update function to be run again later if there is a conflict resolution event eventually scheduled
 ;; Else check to see if last event is after the current time + delay and if it isn't, schedule next update and continue
 (if (not (non-phi-events))
	(progn
	 (setf *nextUpdateEvent* (schedule-event-after-change 'update-phys-vars :module 'physio :delay t :output t :maintenance t))
	 (return-from schedule-update-phys t)
	 (setf (phys-module-updateSwitch (get-module physio)) nil))
	(progn
	 (if (and
			(not (phys-module-updateSwitch (get-module physio)))
			(> (+ (phys-module-lastUpdate (get-module physio)) delay) (find-last-event-time (list 'procedural 'declarative :fatigue 'vision 'audio 'blending 'temporal 'goal 'imaginal 'motor 'speech))))
		(progn
			(setf *nextUpdateEvent* (schedule-event-after-change 'update-phys-vars :module 'physio :delay t :output t :maintenance t))
			(if (phys-module-physValList (get-module physio))
				(return-from schedule-update-phys nil)
				(return-from schedule-update-phys t))
			(setf (phys-module-updateSwitch (get-module physio)) t))
		(progn
		 (setf *nextUpdateEvent* (schedule-event-relative delay 'update-phys-vars :module 'physio :priority :max :output t :maintenance t))
		 (setf (phys-module-updateSwitch (get-module physio)) nil)
		 (return-from schedule-update-phys t)))))
)

;;;Update Phys Variable Hash Table with variable info from
;;; HumMod *updateDelay should be obtained from buffer parameter and be in ms*
(defun update-phys-vars ()
 (tagbody startUpdate
	(let*
	 ((phys (get-module physio))
	 (start-update-time (get-internal-real-time))
	 ;;Convert update delay to HumMod format (mins) and then to string
	 (physUpdateDelay (float (/ (phys-module-delay phys) 60)))
	 (pipeID (phys-module-pipeID phys)))
		(let*
		 ((physUpdateDelay (format nil "~10,$" physUpdateDelay))
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

			(tagbody startGetVals
				(while (probe-file solverOutputFile)
					(handler-case (delete-file solverOutputFile)	(error () nil)))
				 ;;Get new HumMod value list
				(handler-case
					(with-open-file
						(messageStream solverInputFile
							:direction :output :if-exists :overwrite :if-does-not-exist :create)
							(format messageStream getValsMessage)) ((or simple-error (or file-error))
							() (go startGetVals)))

				;wait for HumMod to digest input file
				(let ((startTime (get-universal-time)))
					(while (probe-file solverInputFile)
						(when (and (> (- (get-universal-time) startTime) 2) (probe-file solverOutputFile))
							(handler-case (delete-file solverOutputFile)	(error () nil)))))
				;wait for HumMod to output corresponding file and if it takes too long, reset
				(let ((startTime2 (get-universal-time)))
					(while (and (not (probe-file solverOutputFile)) (< (- (get-universal-time) startTime2) 2)))
					(when (not (probe-file solverOutputFile)) (model-output "Issue at line 617 in physiology module") (go startGetVals)))
				 (tagbody parseValList
					(handler-case (setf physValueList (s-xml:parse-xml-file solverOutputFile))
						((or file-error s-xml::xml-parser-error type-error) ()
							 (progn (print "ISSUE 2")(go startGetVals))))
					(when (not physValueList)
						(go parseValList))
					(when (equal (caadr physValueList) ':|varroster|)
						(while (not (handler-case (delete-file solverOutputFile)
							(error () nil))))
						(go parseValList))))

			(handler-case (delete-file solverOutputFile)
				 (error () nil))

		(setf (phys-module-physValList phys) physValueList)
		;;Record values requested by model to list (to be analyzed later)
		(let ((newVals (record-phys-vals (phys-module-recordedPhys phys) physValueList (phys-module-vals-analysis phys))))
			(when (car newVals) (setf (phys-module-vals-analysis phys) newVals)))

		(incf (phys-module-timeSliceCount phys)))
		;;Record update-phys to internal event trace
		(push (list #'update-phys-vars '()) (phys-module-solverActionTrace phys))
	)))

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
	(define-chunks-fct (list newList))))

(defun schedule-phys-events ()
	(let ((phys (get-module physio)))
		(if (phys-module-enabled phys)
			(progn
				(if (not (phys-module-HProc phys))
					;;Start HumMod
					(progn
						(start-HumMod phys)
						(setf (phys-module-first-run phys) t))
					(setf (phys-module-first-run phys) nil))
				;;All chunk-types used in the efferent buffer should be a subtype of phys-var
				(chunk-type phys-var)
				(setf (phys-module-physValList phys) nil)
				(setf (phys-module-physValList-baseLine phys) nil)
				(schedule-event-relative 0.005 'create-phys-vars :module 'physio :priority :max :details "initialize physiological simulation system" :maintenance t)
				;;Schedule the function to keep these physiological values updated
				(setf *nextUpdateEvent* (schedule-event-relative 0.006 'update-phys-vars :module 'physio :priority :max :output t :maintenance t))
				#|(schedule-event 0.015 'set-food-intake :module 'physio :priority :min :params (list 0) :details "Make ACT-R control food intake")
				(schedule-event 0.015 'set-liquid-intake :module 'physio :priority :min :params (list 0) :details "Make ACT-R control liquid intake")|#)
			(close-hummod (get-module physio)))))

#|Begin phys change functions|#
;;;;;Cognition - Physiology Connection Function Section

(defvar *stress-on* nil)

(defun create-high-stress ()
	"Simulate the physiology side of (high) stress"
	(setf *stress-on* t)
	(cleanup-de-stress)
	(set-phys-vals
		(list (list "Sympathetics-Adrenal.ClampLevel" 2)
			(list "Sympathetics-Adrenal.ClampSwitch" 1)))
	(set-phys-vals
		(list (list "Sympathetics-General.EssentialEffect" 1)))
	(set-phys-vals
		(list (list "CorticotropinReleasingFactor.Stress" 4))))

(defun create-mid-stress ()
	"Simulate the physiology side of (medium) stress"
	(setf *stress-on* t)
	(cleanup-de-stress)
	(set-phys-vals
		(list (list "Sympathetics-General.EssentialEffect" 0.5)
			(list "CorticotropinReleasingFactor.Stress" 3)
			(list "Sympathetics-Adrenal.ClampSwitch" 1)
			(list "Sympathetics-Adrenal.ClampLevel" 1.5))))

(defun create-graded-symp-stress (&optional (length 20) (perc-inc 100) (num-steps 10))
	"Function to allowed a graded increase in sympathetic activity
	(presumably due to stress)
	Inputs:
		length: length of time in mins for (graded) stress increase (int)
		perc-inc: percentage of 'stress' that we move towards (double 0-100)
			(for now, we assume a max value for relevant phys vars)
		num-steps: number of iterations to increase towards increase-perc (int)"
	(let ((perc-per (/ perc-inc num-steps))
				(time-per (/ length num-steps))
				(+END-SYMP-ACT+ 1.1))
		(dotimes (i num-steps)
			(let ((symp-act (* perc-per i +END-SYMP-ACT+)))
				(schedule-event-relative 0.001 'set-phys-vals :module 'physio
					:params (list (list (list "Sympathetics-General.EssentialEffect" ())))
					:priority :max :details "Graded stress")
				(schedule-event-relative 0.002 'advance-phys
					:module :physio :priority :max :params (list time-per))))))

(defun de-stress ()
	"Turn stress-based params back to normal"
	(setf *stress-on* nil)
	(schedule-event-relative 0.001 'set-phys-vals :module 'physio
		:params
			(list (list	(list "Sympathetics-Adrenal.ClampSwitch" 0)))
		:priority :max :details "Turn stress off")
	(schedule-event-relative 0.002 'set-phys-vals :module 'physio
		:params
			(list (list (list "Sympathetics-General.EssentialEffect" 0)))
		:priority :max :details "Turn stress off")
	(schedule-event-relative 0.003 'set-phys-vals :module 'physio
		:params
			(list (list (list "CorticotropinReleasingFactor.Stress" 2)))
		:priority :max :details "Turn stress off"))

(defun de-stress-graded (&optional (ds-steps 5) (ds-len 60))
	"Turn stress back to normal in a graded manner
		Inputs:
			ds-steps - de-stress steps, provides number of steps to be used for grading de-stress [OPTIONAL]
			ds-len - de-stress length, provides length (seconds) of graded de-stress [OPTIONAL]
			*note* the above are only used if module ds-state is inactive (i.e., ==nil)"
	(setf *stress-on* nil)
	;ds-state will hold all of the state information we need
	(let* ((phys (get-module physio))
				 (ds-state (phys-module-de-stress phys))
				 (ds-steps (if ds-state (elt ds-state 0) ds-steps))
				 (ds-len (if ds-state (elt ds-state 1) ds-len))
				 (perc-per (/ 1 ds-steps))
				 (END-SYMP-ACT 1))
		;;If we haven't turned off the ClampSwitch, do so now
		(when (or (not ds-state) (not (= (elt ds-state 3) 0)))
			(schedule-event-relative 0.020 'set-phys-vals :module 'physio
				:params (list (list (list "Sympathetics-Adrenal.ClampSwitch" 0)))
				:priority :max :details "Graded stress reduction Adrenal")
			(if ds-state
				(setf (elt ds-state 3) 0)
				(setf ds-state (list ds-steps ds-len (mp-time) 0 nil nil))))

			;If we've already done as much graded de-stress as needed, reset state variable, else continue
			(if (> (- (mp-time) (elt ds-state 2)) ds-len)
				(setf (phys-module-de-stress phys) nil)
				(let* ((step (floor (* (/ (- (mp-time) (elt ds-state 2)) ds-len) ds-steps)))
							 (symp-act (- END-SYMP-ACT (* perc-per step END-SYMP-ACT))))

					(setf (elt ds-state 4) symp-act) ;Update our saved sympathetic activity state
					;General sympathetic arousal
					(schedule-event-relative 0.021 'set-phys-vals :module 'physio
						:params (list (list (list "Sympathetics-General.EssentialEffect"  symp-act)))
						:priority :max :details "Graded stress reduction Symp Essential")

					(print ds-state)
					;Reduce CRH-based stress
					(if (and (elt ds-state 5) (= (elt ds-state 5) 3) (> (- (mp-time) (elt ds-state 2)) (* ds-len 0.75)))
						(progn

							(setf (elt ds-state 5) 2)
							(schedule-event-relative 0.022 'set-phys-vals :module 'physio
								:params (list (list (list "CorticotropinReleasingFactor.Stress" 2)))
								:priority :max :details "Graded stress reduction CRF/CRH")
							(setf (phys-module-de-stress-evt phys)
								(schedule-event-relative (* (/ ds-steps ds-len) ds-len) 'de-stress-graded :module 'physio
									:priority :max :details "Global graded stress reduction")))
						(when (and (or (not (elt ds-state 5)) (= (elt ds-state 5) 4)) (> (- (mp-time) (elt ds-state 2)) (* ds-len 0.375)))

							(setf (elt ds-state 5) 3)
							(schedule-event-relative 0.022 'set-phys-vals :module 'physio
							 :params (list (list (list "CorticotropinReleasingFactor.Stress" 3)))
							 :priority :max :details "Graded stress reduction")
							(setf (phys-module-de-stress-evt phys)
								(schedule-event-relative (* (/ ds-steps ds-len) ds-len) 'de-stress-graded :module 'physio
									:priority :max :details "Global graded stress reduction"))))
					;If our sympathetic actitity is 0, then we no longer need to de-stress
					; cleanup any vars
					(when (= symp-act 0)
						(cleanup-de-stress))))))

(defun cleanup-de-stress ()
	"Used to cleanup any on-going (i.e., graded) de-stress process
	Also may be used to stop de-stress process when a new stressfule event occurs"
	(let ((phys (get-module physio)))
		(setf (phys-module-de-stress phys) nil)
		(when (phys-module-de-stress-evt phys)
			(delete-event (phys-module-de-stress-evt phys))
			(setf (phys-module-de-stress-evt phys) nil))))

(defun start-slow-breathing ()
	"Start deep slow breaths"
	(set-phys-vals
		(list (list " ControlledBreathing.RespRate" 6)
			(list "ControlledBreathing.TidalVolumeMax" 3000)
			(list "ControlledBreathing.TidalVolumeMin" 1200)
			(list "ControlledBreathing.ControlledBreathing" 1)))
		(de-stress))

(defun stop-controlled-breathing ()
	"Move to spontaneous breath"
	(set-phys-vals (list (list "ControlledBreathing.ControlledBreathing" 0))))

;;;Set food intake (eating) variable to a value
(defun set-food-intake (value)
	(set-phys-vals (list (list "DietIntakeNutrition.Fixed?" 1) (list "DietIntakeNutrition.FixedIntake(xGoal)" value)))
)

;;;Set liquid intake (drinking) variable to a value
(defun set-liquid-intake (value)
	(set-phys-vals (list (list "DietIntakeH2O.Fixed?" 1) (list "DietGoalH2O.Rate(L/Day)" value)))
)

#|End phys change functions|#

#|Begin phys state functions|#
;;;Compute CRH-based arousal
;; See epinephrine comments below for current state update
(defun compute-crh-arousal (&optional test)
	"crh(t_n)/crh(t_0)"
	(let* ((crh (read-from-string (cadar (car (get-phys-vals nil (list '("CorticotropinReleasingFactor.[CRF(pG/mL)]")))))))
	       (crh-stress (read-from-string (cadar (car (get-phys-vals nil (list '("CorticotropinReleasingFactor.StressEffect")))))))
	       (crh-base (read-from-string (cadar	(car (get-phys-vals t (list '("CorticotropinReleasingFactor.[CRF(pG/mL)]")))))))
	       (phys (get-module physio))
	       (crh-factor (/ crh crh-base)));(+ (- crh crh-base) (* (- (phys-module-max-crh phys) crh-base) 0.25)) (- (phys-module-max-crh phys) crh-base)))
		;Write to output file if we are testing things
		(if test
			(with-open-file
				(msgStream (concatenate 'string "CRH-Raw" *START-TIME* ".txt")
					:direction :output :if-exists :append :if-does-not-exist :create)
				(format msgStream "~S~T~10$~T~10$~&" crh-stress crh crh-base)))
		;Return the CRH factor or 0 if less than 0
		(if (< crh-factor 0) 0 crh-factor)))

;;;Compute Epinephrine-based arousal
;; At the moment, we're using a simple ratio (despite what the function comment
;; indicates). This is meant to keep things more simple right now.
;; Will reconsider the eqn in the future - ((epi-base)+((max-base)*0.25))/(max-base)
(defun compute-epi-arousal (&optional test)
	"epi(t_n)/epi(t_0)"
	(let* ((epi-base (read-from-string (cadar (car (get-phys-vals t (list '("EpiPool.[Epi(pG/mL)]")))))))
	       (epi (read-from-string (cadar (car (get-phys-vals nil (list '("EpiPool.[Epi(pG/mL)]")))))))
	       (phys (get-module physio))
	       (epi-factor (/ epi epi-base)));(+ (- epi epi-base) (* (- (phys-module-max-epi phys) epi-base) 0.25)) (- (phys-module-max-epi phys) epi-base)))))
		;Write to output file if we are testing things
		(if test
			(with-open-file
				(msgStream (concatenate 'string "EPI-RAW" *START-TIME* ".txt")
					:direction :output :if-exists :append :if-does-not-exist :create)
				(format msgStream "~10$~T~10$~&" epi epi-base)))
		;Return the epi factor or 0 if less than 0
		(if (< epi-factor 0) 0 epi-factor)))

#| Keeping these for historical purposes, but now just using
		compute-***-arousal functions above in Affective-Associations
		Could use some of this in new format in the future
;Arousal = Arousal(fear) + Arousal(Epi,CRH)
;Arousal(Epi,CRH) = CRH-base/(3*((CRH-High-Perc*CRH-MAX)-base)) + EPI-base/(3*((EPI-High-Perc*EPI-MAX)-base))
;;;Compute current arousal level
(defun compute-phys-arousal ()
	;If we havent had a chance to get phys variable values return 1, else compute
	(if (< (mp-time) 0.006)
	 1
	 (let* ((physio (get-module physio))
			 (epi (cadar (car (get-phys-vals nil (list '("EpiPool.[Epi(pG/mL)]"))))))
			 (crh (cadar (car (get-phys-vals nil (list '("CorticotropinReleasingFactor.StressEffect"))))))
			 ;(crh (cadar (car (get-phys-vals nil (list '("CorticotropinReleasingFactor.StressFactor"))))))
			 (epi-base (cadar (car (get-phys-vals t (list '("EpiPool.[Epi(pG/mL)]"))))))
			 (crh-base (cadar	(car (get-phys-vals t (list '("CorticotropinReleasingFactor.StressEffect"))))))
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
 (* 0.5 (/ crh crh-base));(+ (/ (- crh crh-base) (* 6 (- (* crh-high-perc crh-max) crh-base))) (- 1 crh-high-perc)))

;;Compute epinephrine arousal value
(defun calc-epi-arousal (epi epi-base epi-high-perc epi-max)
 (* 0.5 (/ epi epi-base ));(+ (/ (- epi epi-base) (* 6 (- (* epi-high-perc epi-max) epi-base))) (- 1 epi-high-perc)))

|#

;;Compute cortisol factor
(defun compute-cort (&optional test)
	"(cort(t_n)/cort(t_0) + cortisol[uG/dL](t_n)/cortisol[uG/dL](t_0))/2
	cort(t_n) == (cort.gain(t_n)/cort.loss(t_n)
	That is, the average of the Cortisol circulating and the more rapid change in cortisol"
	(let* ((cort (read-from-string (cadar (car (get-phys-vals nil (list '("Cortisol.[Conc(uG/dL)]")))))))
				 (cort-base (read-from-string (cadar (car (get-phys-vals t (list '("Cortisol.[Conc(uG/dL)]")))))))
				 (cort-gain (read-from-string (cadar (car (get-phys-vals nil (list '("Cortisol.Gain")))))))
				 (cort-gain-base (read-from-string (cadar (car (get-phys-vals t (list '("Cortisol.Gain")))))))
				 (cort-loss (read-from-string (cadar (car (get-phys-vals nil (list '("Cortisol.Loss")))))))
				 (cort-loss-base (read-from-string (cadar (car (get-phys-vals t (list '("Cortisol.Loss")))))))
				 (cort-ratio (if (and cort-gain cort-loss) (/ cort-gain cort-loss) nil))
				 (cort-ratio-base (if (and cort-gain-base cort-loss-base) (/ cort-gain-base cort-loss-base) nil)))
		(setf cort-norm
			(if (and cort-ratio cort-ratio-base cort cort-base)
				(/ (+ (/ cort-ratio cort-ratio-base) (/ cort cort-base)) 2) 0))
		;Write to output file if we are testing things
		(if test
			(with-open-file
				(msgStream (concatenate 'string "CORT-RAW" *START-TIME* ".txt")
					:direction :output :if-exists :append :if-does-not-exist :create)
				(format msgStream "~5$~T~5$~T~5$~T~5$~&" cort cort-base cort-ratio cort-ratio-base)))
		;Return the cort factor or 0 if less than 0
		(if (<= cort-norm 0) 0 cort-norm)))

#|End phys state functions|#

#|---Begin Physiological Initial Condition Functions
		Provides some useful functions that can create initial condition files that can be used to run simulations under various conditions
		Initial condition files help us avoid having to take the time to get to some stable state related to a particular physiological state
|#
(defun make-Racinais08-ICS (&optional (new-ics-file-name "Racinais-2008_Hot.ICS") (phys (get-module physio)))
	"Sets up physiology in a scenario that matches:
		Racinais, S., Gaoua, N., & Grantham, J. (2008).
		Hyperthermia impairs short-term memory and peripheral motor drive transmission.
		The Journal of Physiology, 586(19), 4751-4762.
		This needs to be run every time the Phys Model is changed
		@param new-ics-file-name: [string] The output file name for the ICS file. \"Racinais-2008_Hot.ICS\" by default
		@param phys: [ACT-R module] should be ACT-R module (what you get from call to get-module).
									(get-module physio) by default"
	(schedule-event-relative 0.020 'set-phys-vals :module 'physio
		:params (list (list (list "PostureControl.Request" 3)))
		:priority :max :details "Change to standing posture")
	(schedule-event-relative 0.021 'set-phys-vals :module 'physio
		:params (list (list (list "Exercise-Control.Request" 2)))
		:priority :max :details "Use the treadmill")
	(schedule-event-relative 0.022 'set-phys-vals :module 'physio
		:params (list (list (list "AmbientTemperature.Temp(F)" 122)
												(list "RelativeHumidity.Percent" 50)))
		:priority :max :details "Change temperature and humidity")
	(schedule-event-relative 0.023 'advance-phys :module 'physio
		:module :physio :priority :max :params '(15)
		:details "Advance physiology 15 minutes")
	(schedule-break-relative 0.024)
	(run 0.5)
	(set-as-init-cond)
	(let ((solver-out-file (concatenate 'string *HumModDir* "SolverOut" (phys-module-pipeID phys)))
		    (ics-out-file (concatenate 'string *HumModDir* "../ICS/" new-ics-file-name)))
	(create-ics-from-out solver-out-file ics-out-file phys)))

(defun make-Default-ICS (&optional (new-ics-file-name "1wkNormal.ICS") (phys (get-module physio)))
	"Make default ICS file that moves model to stable position (1 week)
		This needs to be run every time the Phys Model is changed
		@param new-ics-file-name: [string] The output file name for the ICS file. \"1wkNormal.ICS\" by default
		@param phys: [ACT-R module] should be ACT-R module (what you get from call to get-module).
									(get-module physio) by default"
	(schedule-event-relative 0.023 'advance-phys :module 'physio
		:module :physio :priority :max :params '(10080)
		:details "Advance physiology 1 week")
	(schedule-break-relative 0.024)
	(run 0.5)
	(set-as-init-cond)
	(let ((solver-out-file (concatenate 'string *HumModDir* "SolverOut" (phys-module-pipeID phys)))
		    (ics-out-file (concatenate 'string *HumModDir* "../ICS/" new-ics-file-name)))
	(create-ics-from-out solver-out-file ics-out-file phys)))

#|---End Physiological Initial Condition Functions---|#
#|---Start Physiological data help functions---|#
;;;
;;;;Helpful functions for examining physiological data
(defun get-recorded-vals ()
	(phys-module-vals-analysis (get-module physio)))

(defun output-phys-var (var-name &key (out-file-name "phys-var.txt") (phys (get-module physio)))
	"Output physiological variable state (value) to given file
		@param var-name: [string] Name of physiological variable who's value we will output
		@key out-file-name: [string] name of output file
		@key phys: Physiological module to use for output (our own physio by default)"
		(let ((var-val (read-from-string (cadar (car (get-phys-vals nil (list (list var-name))))))))
			(with-open-file
				(out-file out-file-name
					:direction :output :if-exists :append :if-does-not-exist :create)
				(format out-file "~3$,~3$~&" (mp-time) var-val))))

#|---End Physiological data help functions---|#
;;;
;;;;;ACT-R Module Section
(defstruct phys-module
	;;Delay to update HumMod variables
	(delay)

	;Switch that allows us to schedule an update-phys event x seconds(S.T. 0 <= x <= delay) past the end of any particular run
	(updateSwitch nil)

	;;Events (recorded so that we can delete them if we need to)
	(create-phys-event)
	(set-food-event)
	(set-drink-event)

	;;Initial time to advance model
	(initial-advance 1)

	;File to be used for initial conditions
	ics-file

	;Flag used to tell us whether the ICS file is one
	; created by HumMod (thus needing to read in and converted in a way suitable for the Model Solver)
	ics-exp-file

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
	(pipeID (write-to-string (random (get-universal-time))))
	(init nil)

	;;Holds our physiological variables (key) and place in value list (val; e.g., 1, 2, 3, etc.)
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

	;;Obtained by setting integration nerve to value that results in max HR from
	;; (from Nater, La Marca 2006 - Stress-induced changes in human salivary
	;;	alpha-amylase activity-associations with adrenergic activity)
	(epi-max 67)

	(crh-max 5)

	;;Found using highest value from Kirschbaum (1993) study (TSST)
	(cort-max 28)

	(epi-high-perc 0.6)
	(crh-high-perc 0.75)
	(cort-high-perc 0.75)

	(max-integ-nerve-activation 2.7)

	;;Holds the time of the (in ms) that the update-phys-vars function was run
	(lastUpdate 0)

	;;A trace of all previous functions called w/ their parameters
	;; (used if we get an error that forces us to restart)
	(solverActionTrace '())

	;[NOT IMPLEMENTED YET] Use this to save what physiological events we have in queue
	; an attempt to stop wierd overriding behavior by deleting events before
	(phys-sched '())

	;Used to save current state of de-stress
	; If active then == `(steps len[secs] last-time[secs] symp-adrenal symp-act crh)
	; else (inactive) then == nil
	(de-stress nil)
	;Hold the next de-stress event (if there is one in the mp queue)
	(de-stress-evt nil)
	;Tells us whether this is the 1st run of the module (i.e., it was reset once)
	(first-run nil)

	;Tells us whether the model solver should be run in non-gui mode (only really useful currently for Ubuntu/Redhat)
	; (For modelsolver that can be made to use only console [and no GUI])
	(non-gui nil)
)

;Create module everytime new model is defined
(defun create-phys-module (model-name)
	 (declare (ignore model-name))
	 (make-phys-module))

;Delete module when model is deleted (not needed right now)
(defun delete-phys-module (phys)
	(close-hummod phys))
;(start-HumMod)
;Reset module after model is reset or after initial creation of model
(defun reset-phys-module (phys)
	;;Close any previous versions of HumMod
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
				(setf (phys-module-recordedPhys phys) (cdr param)))
			(:phys-ics-file
				(setf (phys-module-ics-file phys) (cdr param)))
			(:phys-ics-exp-file
				(setf (phys-module-ics-exp-file phys) (cdr param)))
			(:phys-pipe-id
				(setf (phys-module-pipeID phys) (cdr param)))
			(:phys-non-gui
				(setf (phys-module-non-gui phys) (cdr param))))

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
				(phys-module-recordedPhys phys))
			(:phys-ics-file
				(phys-module-ics-file phys))
			(:phys-ics-exp-file
				(phys-module-ics-exp-file phys))
			(:phys-pipe-id
				(phys-module-pipeID phys))
			(:phys-non-gui
				(phys-module-non-gui phys)))))


;;;Deprecated, keeping for now for records
#|
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
|#

(defun free-phys-module (phys)
	(setf (phys-module-busy phys) nil)
)

;;;Deprecated, keeping for now for records
#|
;;;Define request function for module (slides 53-54 in extending-actr.ppt)
(defun phys-module-requests (phys buffer spec)
	;;start underlying substrate
	(if (not (phys-module-init phys))
	 (progn
		;;Schedule periodic updating of ans parameter (eventually I need to make this only happen if call to phys-substrate buffer is made)
		(if (phys-module-epi-ans phys)
			(schedule-periodic-event (phys-module-delay phys) 'do-epi-ans2
			:priority :max :initial-delay 1 :module :none :output nil :maintenance t))

		;;Make it so cognition controlls eating (as opposed to HumMod)
		(set-food-intake 0)))
	(if (eq buffer 'efferent)
		(phys-create-chunk phys spec))
)|#

;;;Deprecated, keeping for now for records
#|
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
|#

(define-module-fct 'physio
	'();'(phys-substrate efferent output)
	(list
		(define-parameter
			:phys-delay
			:documentation "Delta for HumMod updates"
			:default-value .05
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
		:documentation "Switch variable for physiology to continue to run w/ model"
		:default-value nil
		:valid-test (lambda (x) (or (typep x 't) (typep x 'nil)))
		:owner t)

		(define-parameter
		:phys-ics-file
		:documentation "File used to load initial conditions"
		:default-value "ICS/1wkNormal.ICS"
		:valid-test (lambda (x) (or (typep x 'string) ))
		:owner t)

		(define-parameter
		:phys-ics-exp-file
		:documentation "File used to load initial conditions for an experiment"
		:default-value nil
		:valid-test (lambda (x) (or (typep x 'string) (equal x nil)))
		:owner t)

		(define-parameter
		:phys-pipe-id
		:documentation "Used by HumMod to determine solver{input/output} file name for communication"
		:default-value (write-to-string (random (get-universal-time)))
		:valid-test (lambda (x) (or (typep x 'string) (equal x nil)))
		:owner t)

		(define-parameter
		:phys-non-gui
		:documentation "Used by HumMod to determine whether we should try to use model-solver with no actual GUI"
		:default-value nil
		:valid-test (lambda (x) (or (typep x 't) (typep x 'nil)))
		:owner t))
	:version "1.0"
	:documentation
		"Module for underlying physiological substrate (via HumMod mechanisms)"
	:creation 'create-phys-module
	:reset 'reset-phys-module
	:delete 'delete-phys-module
	:params 'phys-module-params
	:query nil;'phys-module-query
	:request nil;'phys-module-requests
)
