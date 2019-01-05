#|
Developed by Chris Dancy at Bucknell University

Controls communication with Apache ActiveMQ via sockets
Made with the help of various tutorials as well as using
 JNI Module (https://github.com/RyanHope/json-network-interface) as example

Requirements:
quicklisp, so that relevant libraries can be loaded.
The Apache ActiveMQ server running


For viewing, the spacing assumes a tab counts as two spaces (w/ atom.io as an editor)
***NOTES***

Next todo:
	Schedule periodic event that gets world info every delay (and save event to class instance)
|#

#-comm-system
(eval-when (:compile-toplevel :load-toplevel :execute)
	(let ((*compile-file-pathname* nil))
		(asdf:load-system :usocket)
		(asdf:load-system :bordeaux-threads)
		(asdf:load-system :jsown)))

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

;;;Inital Endpoint pulled from the server
(defvar *initial-endpoint* nil)

(defvar *mouse-pos* (vector 0 0))

#|
Begin AMQ Client & Agent class sections
|#

;;Class that holds all client info (and is used as "module" info
(defclass AMQ-client ()
	((address :accessor address :initform "localhost")
	 (port :accessor port :initform 61613)
	 (socket :accessor socket :initform nil)
	 (stream :accessor stream :initform nil)
	 (pull-thread :accessor pull-thread :initform nil)
	 (queue-name :accessor queue-name :initform nil)
	 (AMQ-conn :accessor AMQ-conn :initform nil)
	 (agent :accessor agent :initform (make-instance 'AMQ-agent))
	 (other-agents :accessor other-agents :initform nil)
	 (busy :accessor busy :initform nil)
	 (world-delay :accessor world-delay :initform nil)
	 (self-delay :accessor self-delay :initform nil)
	 (self-info-event :accessor self-info-event :initform nil)
	 (world-info-event :accessor world-info-event :initform nil)
	 (vis-chunks-lst :accessor vis-chunks-lst :initform nil)
	 (ext-info-lock :accessor ext-info-lock :initform (bordeaux-threads:make-lock))
	 (ext-info-var :accessor ext-info-var :initform (bordeaux-threads:make-condition-variable))))

;;Holds the ground truth of agent-sim values
(defclass AMQ-agent ()
	((guiid :accessor guiid :initform "1")
	 (squad-id :accessor squad-id :initform "1")
	 (loc-east :accessor loc-east :initform "200")
	 (loc-north :accessor loc-north :initform "0")
	 (heading :accessor heading :initform "0")
	 (posture :accessor posture :initform "Standing")
	 (color :accessor color :initform "Blue")
	 (run-speed :accessor run-speed :initform "2000")
	 (walk-speed :accessor walk-speed :initform "1000")
	 (speed :accessor speed :initform "0")
	 (firing :accessor is-firing :initform nil)))

(defun create-AMQ-client (address port)
	"Create an AMQ-client instance"
	(let ((a-client(make-instance 'AMQ-client)))
		(setf (port a-client) port)
		(setf (address a-client) address)
		a-client))

(defmethod start-client ((a-client AMQ-client))
	"Start the AMQ-Client w/ built-in address and port"
	(handler-case
		(progn
			;;Connect to AMQC & JNI streams
			(setf (socket a-client)
				(usocket:socket-connect (address a-client) (port a-client)))
			(setf (stream a-client)
				(usocket:socket-stream (socket a-client)))
			;;Start separate thread to pull messages & communicate w/ JNI Module
			(initialize-agent a-client)
			(bordeaux-threads:with-lock-held ((ext-info-lock a-client))
				(setf (pull-thread a-client)
					(bordeaux-threads:make-thread #'(lambda () (pull-message a-client))))
			(format t  "95 - Waiting~&")
			(bordeaux-threads:condition-wait (ext-info-var a-client) (ext-info-lock a-client)))
			(format t "96 - Done waiting~&")
			;Install the AMQC instance as an ACT-R device
			(install-device a-client)
			;;Sleep needed to allow setup on sim side (otherwise it doesn't respond as it should)
			(sleep .25)
			;Schedule events to get agent info every delay secs
			(aif (self-delay a-client)
				(progn
					;(get-agent-info a-client)
					(setf (self-info-event a-client)
						(schedule-periodic-event it 'get-agent-info
							:params (list a-client) :maintenance t
							:details "Get info about myself" :output t :module 'AMQC
							:initial-delay it))))
			;Schedule events to get world info every delay secs
			(aif (world-delay a-client)
				#|(setf (world-info-event a-client)
					(schedule-periodic-event it 'get-world-info
						:params (list a-client) :maintenance t
						:details "Get info from environment" :output t  :module 'AMQC
						:initial-delay it))|#
						(print it)))
		(usocket:connection-refused-error ()
			(progn
				(print "Connection to ActiveMQ refused or connection to JSON stream refused")
				(finish a-client)
				(return-from start-client)))
		(usocket:timeout-error ()
			(progn
				(print "Connection to ActiveMQ timed out or connection to JSON stream timed out")
				(finish a-client)
				(return-from start-client)))))

#|
End AMQ Client & Agent class sections
|#

#|
Begin SkirmishSim Command section
|#

(defmethod initialize-agent ((a-client AMQ-client))
	"Initialize the agent when module is created"
	(let* ((agent (agent a-client))
				 (guiid (guiid agent))
				 (sid (squad-id agent))
				 (loc-east (loc-east agent))
				 (loc-north (loc-north agent))
				 (heading (heading agent))
				 (posture (posture agent))
				 (color (color agent)))
		(push-message a-client
			(jsown:to-json
				(jsown:new-js
					("ActionID" "qinitialize") ("Guid" guiid)
					("Content" (format nil "~a,~a,~a,~a,~a,~a,~a" guiid sid loc-east loc-north heading posture color)))))))

(defmethod update-agent-info ((agent AMQ-agent) key val)
	"Update agent information based on the JSON object given
	Inputs:
		agent - agent that has info to be changed (AMQ-Agent)
		key - JSON Object key, used for field name (string)
		val - JSON Object value, new value for field name"
		(cond
			((string-equal key "id")
				(setf (guiid agent) (write-to-string val)))
			((string-equal key "team")
				(setf (squad-id agent) (write-to-string val)))
			((string-equal key "color")
				(setf (color agent) (write-to-string val)))
			((string-equal key "bodyPos")
				(jsown:do-json-keys (k v) val
					(update-agent-info agent k v)))
			((string-equal key "bodyPose")
				(setf (posture agent) (write-to-string val)))
			((string-equal key "bodyOrientation")
				(setf (heading agent) (write-to-string val)))
			((string-equal key "bodyVelocity")
				(setf (speed agent) (write-to-string val)))
			((string-equal key "bodyAngVelocity")
				)
			((string-equal key "currActs")
				)
			((string-equal key "bodyx")
				(setf (loc-east agent) (write-to-string val)))
			;We ignore y for now (which is up & down)
			((string-equal key "bodyy"))
			((string-equal key "bodyz")
				(setf (loc-north agent) (write-to-string val)))
			(t
				(format t "Method update-agent-info: No match for ~a - ~a~&" key val))
		))

(defmethod get-agent-info ((a-client AMQ-client) &optional (in-agent nil))
	"Get current information about agent from SkirmishSim
	Inputs:
		in-agent: Instance of the AMQ-Agent class that"
	(format t "192 about to lock it up~&")
	(bordeaux-threads:with-lock-held ((ext-info-lock a-client))
		(let* ((agent (if in-agent in-agent (agent a-client)))
					(guiid (guiid agent))
					(sid (squad-id agent)))
			(format t "Are we stuck?!?!~&")
			(push-message a-client
				(jsown:to-json
					(jsown:new-js
						("ActionID" "qsenseself") ("Guid" guiid)
						("Content" (format nil "~a,~a" guiid sid))))))
			(bordeaux-threads:condition-notify (ext-info-var a-client))
			(bordeaux-threads:condition-wait (ext-info-var a-client) (ext-info-lock a-client))
			(format t "204 - We outchea~&"))
		(format t "NERP SKIRP SKIRP~&"))

(defmethod get-world-info ((a-client AMQ-client) &optional (in-agent nil))
"Get current information about things agent can see in SkirmishSim"
	(let* ((agent (agent a-client))
				(guiid (guiid agent))
				(sid (squad-id agent)))
		(format t "Are we stuck AGAIN?!?!~&")
		;;Threading to ensure that pull-message is only called when needed
		(bordeaux-threads:acquire-lock (ext-info-lock a-client))
		(format t "NOOOOOPE~&")
		(push-message a-client
			(jsown:to-json
				(jsown:new-js
					("ActionID" "qsee") ("Guid" guiid)
					("Content" (format nil "~a,~a" guiid sid)))))
		;;Threading to ensure that pull-message is only called when needed
		(bordeaux-threads:release-lock (ext-info-lock a-client))
		(format t "224 lock-released~&")
		(bordeaux-threads:condition-notify (ext-info-var a-client))
		(format t "226 Thread-Notified-released~&")))

(defmethod change-agent-location-run ((a-client AMQ-client) east north &optional (in-agent nil))
"Move the agent to the specified location east (X) & north (Y) @ running speed"
	(let* ((agent (agent a-client))
				 (speed (run-speed agent)))
		(change-agent-location a-client east north speed)))

(defmethod change-agent-location-walk ((a-client AMQ-client) east north &optional (in-agent nil))
"Move the agent to the specified location east (X) & north (Y) @ walking speed"
	(let* ((agent (agent a-client))
				 (speed (walk-speed agent)))
		(change-agent-location a-client east north speed)))

(defmethod change-agent-location ((a-client AMQ-client) east north speed &optional (in-agent nil))
"Move the agent to the specified location east (X) & north (Y) with a specified speed"
	(let* ((agent (agent a-client))
				(guiid (guiid agent))
				(sid (squad-id agent)))
		(format t "Moving to ~a East, ~a North~%" east north)
		(push-message a-client
			(jsown:to-json
				(jsown:new-js
					("ActionID" "qmoveto2d") ("Guid" guiid)
					("Content" (format nil "~a,~a,~a,~a,~a" guiid sid east north speed)))))
		(setf (speed agent) speed)))

(defmethod change-agent-heading ((a-client AMQ-client) heading &optional (in-agent nil))
	(let* ((agent (agent a-client))
				 (guiid (guiid agent))
				 (sid (squad-id agent)))
		(format t "Changing heading to ~a~%" heading)
		(push-message a-client
			(jsown:to-json
				(jsown:new-js
					("ActionID" "qchangeface") ("Guid" guiid)
					("Content" (format nil "~a,~a,~a" guiid sid heading)))))
		(setf (heading agent) (mod (+ heading (heading agent)) 360))))

(defmethod change-agent-pose ((a-client AMQ-client) pose &key (in-agent nil))
	"Change the current pose of the agent. Can be one of the following:
		Inputs:
			a-client - Instance of AMQ-Client class
			pose - String contained in {'Standing', 'Crouching', 'Prone'}
			"
	(let* ((agent (agent a-client))
				 (guiid (guiid agent))
				 (sid (squad-id agent)))
		(format t "Changing posture to ~a~%" pose)
		(push-message a-client
			(jsown:to-json
				(jsown:new-js
					("ActionID" "qchangepose") ("Guid" guiid)
					("Content" (format nil "~a,~a,~a" guiid sid pose)))))))

(defmethod stop-agent-movement ((a-client AMQ-client) post &optional (in-agent nil))
	(let* ((agent (agent a-client))
				 (guiid (guiid agent))
				 (sid (squad-id agent)))
		(format t "Stopping my current movement~%")
		(push-message a-client
			(jsown:to-json
				(jsown:new-js
					("ActionID" "qstopmoveto2d") ("Guid" guiid)
					("Content" (format nil "~a,~a" guiid sid)))))
		(setf (speed agent) "0")))

#|
End SkirmishSim Command section
|#

#|
Begin redefinitions for Visual Module methods/functions section
|#

;;Define xz-loc function so that we can just use an x & z instead of screen-x & screen-y
(defun xz-loc (chunk)
	(progn
		(setf x (chunk-slot-value-fct chunk 'x))
		(setf z (chunk-slot-value-fct chunk 'z))
		(list x z)))

;;Redefine get object at location so that we use the attributes correctly
(defmethod get-obj-at-location ((vis-mod vision-module) loc scale)
	;Modification for SkirmishSim
	(if (chunk-slot-value-fct chunk 'x)
		(let ((xz-loc (xz-loc loc)))
			(let ((feat-lis (within-move vis-mod xz-loc)))
				(when feat-lis
					(featlis-to-focus vis-mod xz-loc feat-lis))))
		(let ((xy-loc (xy-loc loc)))

			(cond ((eq scale 'PHRASE)
						 (get-phrase-at vis-mod loc))
						((and (eq scale 'WORD) (not (optimize-p vis-mod)))
						 (get-word-at-noopt vis-mod loc))
						(t
						 (let ((feat-lis (within-move vis-mod xy-loc)))
							 (when (eq scale 'WORD)
								 (setf feat-lis (text-feats feat-lis)))
							 (when feat-lis
								 (featlis-to-focus vis-mod loc feat-lis))))))))

(defmethod within-move ((vis-mod vision-module) (xz-loc list))
	(if (= (move-allowance vis-mod) 0)
		(feat-match-xy (visicon-chunks vis-mod) xz-loc)

		(let* ((amqc-mod (get-module AMQC))
		       (max (angle-to-cart (agent amqc-mod) move-allowance xz-loc))
		       (accum nil))
			(maphash (lambda (key value)
								 (declare (ignore key))
								 (when (>= max (dist (xy-loc value) xy-loc))
									 (push value accum)))
							 (visicon vis-mod))
			accum)))

(defmethod angle-to-cart ((agent AMQ-agent) allowance xz-loc)
	"Gives us the maximum amount we can move in our cartesian grid given the
	max angle and the distance between us and the object
		Inputs:
			agent - Instance of AMQ-Agent class
			allowance - angle of allowance represented by float
			xz-loc - coords of objects being used to calculate allowance
				represented by list of floats
		Outputs:
			distance - distance (in cartesian grid) that object can be away from previous
				position to be considered the same"
		(let ((distance 0))
			(setf distance
				(* (abs (- (loc-north agent) (cadr xz-loc))) (tan (deg->rad allowance))))
			distance))

(defmethod feat-match-xy (feat-list (xz-loc list))
	(let ((outlis nil)
	      (x (car xz-loc))
	      (z (cadr xz-loc)))
		(dolist (chunk feat-list)
			(when (and (= x (fast-chunk-slot-value-fct chunk 'x))
						     (= z (fast-chunk-slot-value-fct chunk 'z)))
				(push chunk outlis)))
		outlis))

#|
End redefinitions for Visual Module methods/functions section
|#


#|
Begin JSON & chunk section
|#

(defun parse-json-slot (chunk-expr slot-name slot-val)
	"Return an expanded chunk def given the values supplied
	Inputs:
		chunk-expr - original chunk expression to be expanded (list)
		slot-name - name of the slot to be supplied for chunk def (string)
		slot-val - value to be applied w/ the given slot-name (string XOR JSOWN-object)
	Outputs:
		chunk-expr - extended chunk expression
	"
	(cond
		;add slot and value to chunk expression
		((stringp slot-val)
			(let* ((slot-name (read-from-string slot-name))
						 (slot-val (read-from-string slot-val))) ;(process-slot slot-symbol (jsown:val slots slot-name) :visual visual)))
				(append chunk-expr (list slot-name slot-val))))

		;Create a person chunk and add it to type slot
		((equal slot-name 'color)
			(let ((person-chunk nil)
						(uniform-chunk nil))
				(cond
					((not (stringp slot-val))
						;(model-warning "SkirmishSim supplied invalid value for color, assuming external agent is a foe~%")
						(setf uniform-chunk 'foe))

					((string-equal slot-val "blue")
						(setf uniform-chunk 'friend))

					((string-equal slot-val "red")
						(setf uniform-chunk 'foe))

					(t
						;(model-warning "SkirmishSim supplied invalid value for color, assuming external agent is a foe~%")
						(setf uniform-chunk 'foe)))
				(when (not uniform-chunk)
					(setf person-chunk (define-chunks-fct
						(list (list 'isa 'person 'uniform uniform-chunk)))))
				(append chunk-expr (list 'type (car person-chunk)))))

		((numberp slot-val)
			(let* ((slot-name (read-from-string slot-name))
						 (slot-val slot-val)) ;(process-slot slot-symbol (jsown:val slots slot-name) :visual visual)))
				(append chunk-expr (list slot-name slot-val))))

		;Create a visual-location chunk and add it to screen-pos slot
		((and (listp slot-val) (equal :OBJ (car slot-val)))
			(let* ((slot-name (read-from-string slot-name))
						 (slot-val
								(let ((pos-chunk-expr (list 'isa 'obj-SALUTE-primitive)))
									(jsown:do-json-keys (k v) slot-val
										(cond
											((string-equal k "bodyx")
												(setf pos-chunk-expr (parse-json-slot pos-chunk-expr "x" v)))
											((string-equal k "bodyy")
												(setf pos-chunk-expr (parse-json-slot pos-chunk-expr "y" v)))
											((string-equal k "bodyz")
												(setf pos-chunk-expr (parse-json-slot pos-chunk-expr "z" v)))
											(t
												(model-warning "Warning: ~a may be an invalid slot for chunk~%" k)
												(setf pos-chunk-expr
													(parse-json-slot pos-chunk-expr (read-from-string k) v)))))
									(define-chunks-fct (list pos-chunk-expr)))))
				;(format t "388 - ~a~&" slot-val)
				(append chunk-expr (list slot-name (car slot-val)))))

		((eq slot-val nil)
			(let* ((slot-name (read-from-string slot-name)))
				(append chunk-expr (list slot-name slot-val))))
		(t
			(model-warning "Issue parsing json slot for ~a with value ~a~%" slot-name slot-val))))

(defun jobj->chunkpair (jobj)
	(let* ((chunk-type "obj-salute")
				 (chunk-expr (list 'isa (read-from-string chunk-type))))
		(jsown:do-json-keys (k v) jobj
			(cond
				((string-equal k "bodyPos")
					;Here we set screen-pos, x, y, and z slots
					(setf chunk-expr (parse-json-slot chunk-expr "screen-pos" v))
					;(format t "405 ~a~t~a~%" chunk-expr (car (last chunk-expr)))
					(setf loc-chunk (car (last chunk-expr)))
					(setf chunk-expr
						(parse-json-slot chunk-expr "x" (chunk-slot-value-fct loc-chunk 'x)))
					(setf chunk-expr
						(parse-json-slot chunk-expr "y" (chunk-slot-value-fct loc-chunk 'y)))
					(setf chunk-expr
						(parse-json-slot chunk-expr "z" (chunk-slot-value-fct loc-chunk 'z))))

				((string-equal k "bodyPose")
					(setf chunk-expr (parse-json-slot chunk-expr "pose" v)))

				((string-equal k "color")
					(setf chunk-expr (parse-json-slot chunk-expr 'color v))0)

				((string-equal k "bodyOrientation")
					(setf chunk-expr (parse-json-slot chunk-expr "orientation" v)))

				((string-equal k "bodyVelocity")
					(setf chunk-expr (parse-json-slot chunk-expr "velocity" v)))

				((string-equal k "bodyAngVelocity")
					)

				((string-equal k "currActs")
					(setf chunk-expr (parse-json-slot chunk-expr "activity" v)))

				;Want to ignore these ids for now, but may use in future
				((or (string-equal k "id") (string-equal k "team"))
					nil)

				(t
					(model-output "Warning: ~a may be an invalid slot for chunk~%" k)
					(setf chunk-expr
						(parse-json-slot chunk-expr (read-from-string k) v)))))
		(car (define-chunks-fct (list chunk-expr)))))
#|
End JSON & chunk section
|#

#|
Start visual module display function section
|#
(defun update-display-chunks (chunks)
	(loop for chunk in chunks do
				(let ((name (read-from-string (jsown:val chunk "name")))
							(slots (jsown:val chunk "slots")))
					(loop for slot-name in (jsown:keywords slots) do
								(let* ((slot-symbol (read-from-string slot-name))
											 (slot-value (process-slot slot-symbol (jsown:val slots slot-name) :visual t)))
									(set-chunk-slot-value-fct name slot-symbol slot-value))))))

#|
End visual module display function section
|#

#|
Start AMQ Server communication functions
|#

;;;Pulls message from Apache AMQ Server port/stream
(defmethod pull-message ((client AMQ-client))
	(let ((a-stream (stream client))
				(in-msg-header nil)
				(test-var 1))
		(bordeaux-threads:acquire-lock (ext-info-lock client))
		(format t "521 - Acquire Lock~&")
		(handler-case
			(progn
				;Skip initial connected message frame
				(loop for line = (read-line a-stream) do (progn (format t "~a~%" line)) until (string= line #\Null))
				(format a-stream "SUBSCRIBE~%destination:Sim2Controller~%ack:auto~%~%~C" #\Null)
				(finish-output a-stream)
				(format t "533 Condition notify & wait @ The initial message....~&")
				(bordeaux-threads:condition-notify (ext-info-var client))
				(bordeaux-threads:condition-wait (ext-info-var client) (ext-info-lock client))
				(format t "535 Done waiting, now to look for our 1st message....~&")
				(bordeaux-threads:release-lock (ext-info-lock client))
				(loop
						(let ((curr-line (read-line a-stream))
								  (parsed-line nil)
									(other-agents (other-agents client))
								  (vis-objs nil)
								  (aur-objs nil)
								  (sa-objs nil)
								  (vis-str nil)
								  (aur-str nil)
								  (sa-str nil))
							(when curr-line (format t "546 - ~a~&" curr-line))
							(when (and (string-equal curr-line "MESSAGE") (not in-msg-header))
								(setf in-msg-header t)
								(bordeaux-threads:acquire-lock (ext-info-lock client))
								(format t "547 MSG Header~&"))
							;For the aural objects (future), we may use the new-other-sound construct
							(if in-msg-header
								(when (string= curr-line "")
									(setf in-msg-header nil)
									(model-output "Trying to finish this shit"))
								(progn
									(format t "553 - ~a~%" curr-line)
									;;;--Begin pull and parse objects from stream
									(setf parsed-line
										(jsown:parse curr-line))

									;Tests whether parsed json was one object (e.g., from get-agent-info)
									; or multiple objs (e.g., from get-world-info)
									(if (equal :OBJ (car parsed-line))
										;One JSON Object
										(format t "~a~%" (jobj->chunkpair (cdr parsed-line)))
										;(format t "~a~&" parsed-line)
										;(jsown:do-json-keys (k v) parsed-line
										;	(update-agent-info (agent client) k v))
										;Multiple JSON Objects+-
										(progn
											(dolist (jobj parsed-line)
												(let ((vis-chunk nil)
												      (vis-loc-chunk nil))
													(setf vis-chunk (jobj->chunkpair (cdr jobj)))
													(setf vis-loc-chunk (chunk-slot-value-fct vis-chunk 'screen-pos))
													(if (not (vis-chunks-lst client))
														(setf (vis-chunks-lst client) (list (cons vis-loc-chunk vis-chunk)))
														(nconc (vis-chunks-lst client) (list (cons vis-loc-chunk vis-chunk)))))
												(let ((agent-pos nil)
															(new-other-agent nil))
													(jsown:do-json-keys (k v) jobj
														(if (string-equal k "id")
															;If we can find the agent guiid in the other-agents list, record that index
															; (we have a JSON-Object that represents an agent)
															(aif (position (write-to-string v) other-agents
																		:test #'(lambda (x y) (string-equal x (guiid y))))
																(setf agent-pos it)
																(setf new-other-agent (make-instance 'AMQ-agent))))
														;If we have an object that represents an agent update that agent info
														;else we'll have a new agent, so update that agent's info
														(if agent-pos
															(update-agent-info (elt other-agents agent-pos) k v)
															(update-agent-info new-other-agent k v))
														#|(format t "~a~T~a~%" k v)|#)
													;Reset agent position variable
													(if agent-pos
														(setf agent-pos nil)
														(progn
															;If list is not  empty, append new agent to list
															;else create it w/ agent as 1st instance in list
															(if other-agents
																	(append other-agents (list new-other-agent))
																	(setf other-agents (list new-other-agent)))
															(setf new-other-agent nil)))))))
											(format t "543 Thread waiting...~&")
											(bordeaux-threads:condition-notify (ext-info-var client))
											(format t "608 Going to sleep...~&")
											(bordeaux-threads:condition-wait (ext-info-var client) (ext-info-lock client))
											(bordeaux-threads:release-lock (ext-info-lock client))
											(format t "545 Thread started...~&")))
							;;;--End push objects to JSON Module
							#|(finish-output a-stream)|#)));)
			(usocket:connection-aborted-error () (return-from pull-message "Connection aborted"))
			(usocket:connection-reset-error () (return-from pull-message "Connection reset"))
			(usocket:bad-file-descriptor-error () (return-from pull-message "Bad file descriptor"))
			(usocket:socket-error () (return-from pull-message "Socket error"))
			(end-of-file () (return-from pull-message "Socket stream closed")))))

;;;Push message to Apache AMQ Server port/stream
(defmethod push-message ((client AMQ-client) msg-str &key sync)
	(progn
		;Connect to stream
		(when (not (AMQ-conn client))
			(format (stream client) "CONNECT~%~%~C" #\Null)
			(finish-output (stream client))
			(setf (AMQ-conn client) t))
		(format (stream client) "SEND~%destination:Controller2Sim~%~%~a~%~C" msg-str #\Null)
		(finish-output (stream client))))

(defmethod disconnect ((a-client AMQ-client))
	(when (stream a-client)
		(format (stream a-client) "DISCONNECT~%~%~C" #\Null)
		(force-output (stream a-client))
		(close (stream a-client)))
	(when (socket a-client) (usocket:socket-close (socket a-client))))

#|
End AMQ Server communication functions
|#

#|
ACT-R device methods
|#

;;;;Mouse Section (Defaults)
(defmethod device-handle-keypress ((client AMQ-client) keychar)
	(model-output "Model pressed key ~c" keychar))

(defmethod device-handle-click ((client AMQ-client))
	(model-output "Model clicked the mouse"))

(defmethod device-move-cursor-to ((client AMQ-client) xyvec)
	(model-output "Model moved mouse to ~a" loc)
	(setf *mouse-pos* loc))

(defmethod get-mouse-coordinates ((client AMQ-client))
	*mouse-pos*)

;;;;Speech section (Defaults)
(defmethod device-speak-string ((client AMQ-client) string)
	(model-output "Model said ~s" string))


;;;;Vision section
(defmethod build-vis-locs-for ((client AMQ-client) vis-mod)
	(declare (ignore vis-mod))
	(if (vis-loc-lst client)
		(mapcar 'car (vis-chunks-lst client))))

(defmethod cursor-to-vis-loc ((client AMQ-client))
	nil)

(defmethod vis-loc-to-obj ((client AMQ-client) vis-loc)
	(cdr (assoc vis-loc client)))
#|
ACT-R device methods
|#

(defmethod finish ((a-client AMQ-client))
	(disconnect a-client)
	(setf (pull-thread a-client) nil)
	(setf (stream a-client) nil)
	(setf (socket a-client) nil)
	;(when (sync-event instance) (delete-event (sync-event a-client)))
	)

(defun params-AMQC (amqc param)
	(if (consp param)
		(case (car param)
			(:AMQ-address
				(setf (address amqc) (cdr param)))
			(:AMQ-port
				(setf (port amqc) (cdr param)))
			(:AMQ-world-delay
				(setf (world-delay amqc) (cdr param)))
			(:AMQ-self-delay
				(setf (self-delay amqc) (cdr param))))
		(case param
			(:AMQ-address
				(address amqc))
			(:AMQ-port
				(address amqc))
			(:AMQ-world-delay
				(address amqc))
			(:AMQ-self-delay
				(address amqc)))))

(defun create-AMQC (model-name)
	(declare (ignore model-name))
	(make-instance 'AMQ-Client)
)

(defun reset-AMQC (AMQC)
	(finish AMQC)
	;(start-client AMQC)
	(schedule-event 0 'start-client :params (list AMQC) :module 'AMQC :output t)
	)

(defun delete-AMQC (AMQC)
	(finish AMQC))

#|
Buffer functions
|#

;;;Define request function for module (slides 53-54 in extending-actr.ppt)
(defun AMQC-requests (amqc buffer spec)
	(declare (ignore amqc))
	(declare (ignore buffer))
	(declare (ignore spec))
	(let ((delay 0))
	(cond
		((eq buffer 'move)
			#|(if (or (eq (chunk-spec-chunk-type spec) 'move)
			 (eq (cadr (chunk-type-supertypes-fct (chunk-spec-chunk-type spec))) 'move))
				(schedule-set-buffer-chunk 'move define-chunks-fct (list (chunk-spec-to-chunk-def spec)))
				(progn
					(print (chunk-spec-chunk-type spec))
					(print "Warning - invalid chunk-type in efferent buffer")))|#
					nil)
		((eq buffer 'heading)
			nil))))
;Define query function for module (b = buffer)
(defun AMQC-query (AMQC b slot value)
	(declare (ignore AMQC))
	(declare (ignore b))
	(declare (ignore slot))
	(declare (ignore value))
	nil)

#|
End Buffer functions
|#
(define-module-fct 'AMQC
	'(move heading)
	(list
		(define-parameter
			:AMQ-address
			:documentation "Address of the AMQ Server"
			:default-value nil
			:owner t)
		(define-parameter
			:AMQ-port
			:documentation "Port number to communicate with AMQ server"
			:valid-test (lambda (x) (or (numberp x) (eq x nil)))
			:default-value nil
			:owner t)
		(define-parameter
			:AMQ-world-delay
			:documentation "Delay to get world information"
			:valid-test (lambda (x) (or (numberp x)(eq x nil)))
			:default-value nil
			:owner t)
		(define-parameter
			:AMQ-self-delay
			:documentation "Delay to get self information"
			:valid-test (lambda (x) (or (numberp x) (eq x nil)))
			:default-value nil
			:owner t))
	:version "0.1"
	:documentation "Module to manage communication between ACT-R & SkrimishSim"
	:creation 'create-AMQC
	:reset 'reset-AMQC
	:delete 'delete-AMQC
	:params 'params-AMQC
	:request 'AMQC-requests
	:query 'AMQC-query)
