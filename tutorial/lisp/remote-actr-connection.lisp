;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell & Mike Byrne
;;; Copyright   : (c) 2017 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : dispatcher-client.lisp
;;; Version     : 1.1
;;; 
;;; Description : * Code to connect to the remote interface ACT-R and provide the
;;;             :   modeling commands necessary to run the remote tutorial model tasks.
;;; 
;;; Bugs        : * 
;;;
;;; To do       : * they're macros but they do result in evaluating the parameters.
;;; 
;;; ----- History -----
;;; 2016.11.22 Dan
;;;             : * To pass symbols back and forth for now going to use an object
;;;             :   in the JSON arrays of the form {"name":"<symbol-name>"}.
;;;             :   Adding modifications to the encoding and decoding to handle
;;;             :   that automatically.
;;; 2016.11.23 Dan
;;;             : * Added a format function for printing the error messages so
;;;             :   that ACL and CCL will show all the details (may need to add
;;;             :   additional Lisps as well as needed).
;;;             : * Added the cleaner encode-json symbol method which only does
;;;             :   the symbol->object when a variable is set.
;;; 2016.11.29 Dan
;;;             : * The start-echoing-all-act-r-output command now makes sure
;;;             :   the client has the "echo" command set because if it has to
;;;             :   restart for some reason it won't still be there.
;;; 2016.12.01 Dan
;;;             : * Don't attempt to do any fancy encoding/decoding for symbols
;;;             :   now -- let it use the default camel case stuff for decoding
;;;             :   because at this point the only thing converted to symbols 
;;;             :   will be the object slot names which have fixed values. 
;;;             :   However, the lisp symbol to string needs to not use the
;;;             :   camel case converter otherwise something like visual-location
;;;             :   gets converted to "visualLocation" which is bad...
;;; 2016.12.02 Dan
;;;             : * Changed how the print-error-message function handles things
;;;             :   for ACL and CCL to just write the error message because 
;;;             :   that seems to work better across different error/condition
;;;             :   types.
;;; 2016.12.09 Dan
;;;             : * Use the new check method to determine if the remote-echo
;;;             :   command exists and whether or not this client owns it.
;;; 2017.01.23 Dan
;;;             : * Add functions for the ACT-R commands which send the evaluate
;;;             :   action out and then parse the return results based on success.
;;; 2017.01.24 Dan
;;;             : * Echo the output by default.
;;; 2017.01.25 Dan
;;;             : * Added act-r-random and print-visicon functions.
;;; 2017.01.30 Dan
;;;             : * Permute-list actually permutes the indices and then creates
;;;             :   a new list using those because the list itself could contain
;;;             :   Lisp objects that can't be passed through the JSON serializer.
;;; 2017.02.02 Dan
;;;             : * Added the run-full-time function.
;;; 2017.02.03 Dan
;;;             : * Added penable and pdisable.
;;; 2017.02.06 Dan
;;;             : * Fixed goal-focus because it didn't need to be applied.
;;; 2017.02.07 Dan
;;;             : * Added buffer-read and clear-buffer.
;;; 2017.02.08 Dan
;;;             : * Better catch errors with encoding a result so that it still
;;;             :   sends the message with an error notice.
;;;             : * Change the output monitor since it's now a simple monitor.
;;;             : * Added define-chunks, pprint-chunks, chunk-slot-value, mod-focus,
;;;             :   mod-chunk, and set-chunk-slot-value commands.
;;; 2017.02.09 Dan
;;;             : * Added set-buffer-chunk and add-line-to-exp-window.
;;; 2017.02.15 Dan
;;;             : * For the remote functions that differentate a macro from a 
;;;             :   function in the ACT-R code, include a -fct version which
;;;             :   takes the same form as the real -fct.
;;;             : * Added chunk-p and chunk-p-fct.
;;;             : * Added extend-possible-slots.
;;;             : * Added add-dm.
;;; 2017.02.16 Dan
;;;             : * Added add-dm-fct.
;;;             : * Evaluate incoming commands in separate threads because otherwise
;;;             :   there's a nasty deadlock if a command results in calling other
;;;             :   commands in this client (which can easily happen for something like
;;;             :   an ACT-R warning being printed during evaluation of a command).
;;;             : * Added model-output.
;;; 2017.02.22 Dan
;;;             : * Added the schedule-simple-event and schedule-simple-event-relative
;;;             :   commands.
;;; 2017.02.23 Dan
;;;             : * Added whynot-dm and whynot-dm-fct commands.
;;;             : * Added mp-show-queue command.
;;;             : * Added print-dm-finsts.
;;; 2017.02.24 Dan
;;;             : * Added run-until-condition.
;;; 2017.03.10 Dan
;;;             : * Added start-hand-at-mouse, modify-line-for-exp-window,
;;;             :   add-button-to-exp-window, remove-items-from-exp-window.
;;;             : * Fixed a bug with sending requests because the incf of the
;;;             :   id value wasn't protected by a lock.
;;; 2017.03.13 Dan
;;;             : * Added an spp command.
;;;             : * Added no-output and hide-output macros along with the underlying
;;;             :   start-echoing-act-r-output/stop-echoing-act-r-output and
;;;             :   show-act-r-output/hide-act-r-output pairs.
;;;             : * Added a copy-chunk and copy-chunk-fct.
;;; 2017.03.14 Dan
;;;             : * Have the remote spp-fct not use apply so things are taken out
;;;             :   of the list so the dispatcher's one collects them again...
;;;             : * Added encode-keyword-names because the default encoding will
;;;             :   strip the : off in what it sends which isn't good for something
;;;             :   like spp.
;;; 2017.04.06 Dan [1.1]
;;;             : * The only method that comes over now is evaluate and its
;;;             :   first two parameters are the command name and the model name,
;;;             :   and when sending an evaluate the second parameter needs to be
;;;             :   a model name or nil.
;;; 2017.07.12 Dan
;;;             : * Adding the while macro so it'll work in other than ACL.
;;; 2017.07.13 Dan
;;;             : * Added the general-output trace monitoring and the act-r-output
;;;             :   command.
;;; 2017.12.05 Dan
;;;             : * Updated with more of the remote commands.
;;;             : * Added the check for quicklisp instead of always trying to
;;;             :   load the setup file.
;;;             : * Fixed some things that should be macros to line up with the
;;;             :   server so that the tutorial tasks can be run from the client.
;;;             : * Convert string return values back to symbols for everything.
;;; 2017.12.06 Dan
;;;             : * Added the chunk-spec related commands.
;;; 2017.12.18 Dan
;;;             : * Added mp-time and mp-time-ms.
;;; 2018.03.14 Dan
;;;             : * Use get-new-command-name to set the echo command's name.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#-:QUICKLISP (eval-when (:compile-toplevel :load-toplevel :execute)
               (when (probe-file "~/quicklisp/setup.lisp")
                 (load "~/quicklisp/setup.lisp"))
               (unless (find :quicklisp *features*)
                 (error "This version of ACT-R requires Quicklisp to load the components necessary for the remote connection.")))



(ql:quickload :bordeaux-threads)

(ql:quickload :usocket)

(ql:quickload :cl-json)

(defparameter *default-package* *package*)

(defvar *dispatcher*)

;;; push-last
;;;
;;; (defmacro push-last (item place)
;;; 
;;; item anything
;;; place a Lisp place
;;;
;;; push-last postpends item to the list that is stored in place and stores the 
;;; resulting list in place.
;;;
;;; returns place. 
;;;


(define-modify-macro  act-r_nconcf (&rest args) nconc)

(defmacro push-last (item place)
  `(act-r_nconcf ,place (list ,item)))



#-(or :allegro-ide (and :allegro-version>= (version>= 6)))
(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ACL and CCL don't seem to print the text provided in an error
;;; call normally so create a special format function so that the
;;; created error strings can be more informative.



#-(or :acl :ccl) (defun cl-user::print-error-message (stream error colon-p atsign-p &optional (width 0) padchar commachar)
                   (declare (ignore colon-p atsign-p width padchar commachar))
                   (format stream "~s" error))

#+(or :acl :ccl) (defun cl-user::print-error-message (stream error colon-p atsign-p &optional (width 0) padchar commachar)
         (declare (ignore colon-p atsign-p width padchar commachar))
         (format stream "#<~a " (type-of error))
         (write error :stream stream :escape nil)
         (format stream  ">"))




(defun decode-string (s)
  (if (stringp s)
      (if (char= #\' (char s 0) (char s (1- (length s))))
          (subseq s 1 (1- (length s)))
        (string->name s))
    s))


(defun string->name (s)
  (if (stringp s)
      (if (eq (char s 0) #\:)
          (intern (string-upcase (subseq s 1)) :keyword)
        (let ((n (ignore-errors (read-from-string s))))
          (if (numberp n)
              n
            (intern (string-upcase s)))))
    s))

(defun string->name-recursive (s)
  (if (stringp s)
      (if (eq (char s 0) #\:)
          (intern (string-upcase (subseq s 1)) :keyword)
        (let ((n (ignore-errors (read-from-string s))))
          (if (numberp n)
              n
            (intern (string-upcase s)))))
    (if (listp s)
        (mapcar 'string->name-recursive s)
      s)))


(setf json:*lisp-identifier-name-to-json* 'identity)



;;; Client side connection
;;; Basically the same setup as server, but here the command table is referenced directly
;;; when an evaluate method is received.

(defstruct client-dispatcher
  (action-lock (bt:make-lock "client-action-lock"))
  (action-cv (bt:make-condition-variable :name "client-cv"))
  (stream-lock (bt:make-lock "stream-lock"))
  stream
  socket
  (command-lock (bt:make-lock "dispatcher-command-lock"))
  (command-table (make-hash-table :test 'equalp))
  action-list
  action-thread
  input-thread
  (requests-lock (bt:make-lock "dispatcher-requests-lock"))
  pending-requests
  (id-lock (bt:make-lock "dispatcher-id-lock"))
  (id 0))

(defstruct client-action name parameters id model)
(defstruct client-request (lock (bt:make-lock)) (cv (bt:make-condition-variable)) id success result complete)
  

(defun add-client-command (name function)
  (handler-case
          (progn
            (unless (and (stringp name)
                         (or (functionp function)
                             (and (symbolp function)
                                  (fboundp function))))
              (progn
                (format *error-output* "Invalid parameters when trying to add commmand ~s for function ~s" name function)
                (return-from add-client-command nil)))
            (bt:with-lock-held ((client-dispatcher-command-lock *dispatcher*))
              (if (gethash name (client-dispatcher-command-table *dispatcher*))
                  (format *error-output* "~s already names a command in the table" name)
                (setf (gethash name (client-dispatcher-command-table *dispatcher*)) function))))
        ((or error condition) (x) 
         (format *error-output* "Error ~/print-error-message/ occurred while trying to add command ~s" x name))))


(defvar *current-act-r-model* nil)

(defun send-command-to-act-r (command-name &rest parameters)
  (if (stringp command-name)
      (let ((p (make-client-request)))
        (bt:with-lock-held ((client-dispatcher-id-lock *dispatcher*))
          (setf (client-request-id p) (incf (client-dispatcher-id *dispatcher*)))
          (push p (client-dispatcher-pending-requests *dispatcher*)))
        
        (bt:acquire-lock (client-request-lock p))
        
        
        (handler-case 
            (progn
              (bt:with-lock-held ((client-dispatcher-stream-lock *dispatcher*))
                (format (client-dispatcher-stream *dispatcher*) "{\"method\": ~s, \"params\": ~a, \"id\": ~d}~c" command-name
                  (json:encode-json-to-string parameters) (client-request-id p) (code-char 4))
                (force-output (client-dispatcher-stream *dispatcher*)))
              (loop
                (when (client-request-complete p)
                  (bt:with-lock-held ((client-dispatcher-requests-lock *dispatcher*))
                    (setf (client-dispatcher-pending-requests *dispatcher*) (remove p (client-dispatcher-pending-requests *dispatcher*))))
                  (return-from send-command-to-act-r (values-list (append (list (client-request-success p)) (if (listp (client-request-result p))
                                                                                                                (client-request-result p)
                                                                                                              (list (client-request-result p)))))))
                (bt:condition-wait (client-request-cv p) (client-request-lock p))))
          (error (x)
            (format *error-output* "Error ~/print-error-message/ while trying to send command." x))))
    (values nil (format nil "Command-name for send-command-to-act-r must be a string but given ~s" command-name))))


(defun evaluate-act-r-command (&rest parameters)
  (let ((result (multiple-value-list (apply 'send-command-to-act-r "evaluate" (cons (first parameters) (cons *current-act-r-model* (cdr parameters)))))))
    (if (first result)
        (values-list (mapcar 'string->name-recursive (rest result)))
      (dolist (x (rest result) nil)
        (format *error-output* x)))))


(defun start-des-client (&optional (host "127.0.0.1") (remote-port 2650))
  (let ((socket (handler-case (usocket:socket-connect host remote-port :nodelay :if-supported)
                (error (x) (progn
                             (format *error-output* "Error ~/print-error-message/ occurred while trying to open a socket connection to host ~s port ~s" x host remote-port)
                             nil)))))
    (if socket
        (let* ((dispatcher
                (make-client-dispatcher 
                 :socket socket
                 :stream (usocket:socket-stream socket))))
          
          (setf *dispatcher* dispatcher)
          
          (setf (client-dispatcher-input-thread dispatcher)
            (bt:make-thread  (lambda ()
                               (let ((*package* *default-package*))
                                 (unwind-protect
                                     (process-client-input dispatcher)
                                   
                                   (progn 
                                     ;; Things we've received and not initiated should just be removed
                                     (bt:with-lock-held ((client-dispatcher-action-lock dispatcher))
                                       (setf (client-dispatcher-action-list dispatcher) nil))
                                     ;; things we've sent but haven't received need to return an error result
                                     (dolist (x (client-dispatcher-pending-requests dispatcher))
                                       (bt:with-lock-held ((client-request-lock x))
                                         (setf (client-request-success x) nil)
                                         (setf (client-request-result x) "Connection terminated")
                                         (setf (client-request-complete x) t))
                                       (bt:condition-notify (client-request-cv x)))
                                     (ignore-errors (usocket:socket-close (client-dispatcher-socket dispatcher)))))))))
          
          (setf (client-dispatcher-action-thread dispatcher)
            (bt:make-thread (lambda ()
                              (let ((*package* *default-package*))
                                (dispatcher-process-client-actions dispatcher)))))
          t)
      nil)))


(defun process-action-thread (dispatcher command a)
  (let ((result (if command
                    (multiple-value-list (handler-case
                                             (values-list (cons t (let ((*current-act-r-model* (client-action-model a)))(multiple-value-list (apply command (client-action-parameters a))))))
                                           (error (e) (let ((m (format nil "Error ~/print-error-message/ while applying ~s to ~s." e (client-action-name a) (client-action-parameters a))))
                                                        (format *error-output* m)
                                                        (values nil m)))
                                           (condition (c) (let ((m (format nil "Condition ~/print-error-message/ while applying ~s to ~s." c (client-action-name a) (client-action-parameters a))))
                                                            (format *error-output* m)
                                                            (values nil m)))))
                  (list nil "Invalid command name"))))
        
        (when (client-action-id a)
          
          (let* ((success (car result))
                 (res (handler-case
                          (json:encode-json-to-string (cdr result))
                        (error ()
                          (format nil "Unable to encode return result ~s in JSON" (cdr result))
                          (setf success nil))))
                 (json-result (if success
                                  (format nil "{\"result\": ~a, \"error\": null, \"id\": ~s}~c" res (client-action-id a) (code-char 4))
                                (format nil "{\"result\": null, \"error\": {\"message\": ~a}, \"id\": ~s}~c" res (client-action-id a) (code-char 4)))))
            (handler-case
                (bt:with-lock-held ((client-dispatcher-stream-lock dispatcher))
                  (format (client-dispatcher-stream dispatcher) json-result)
                  (force-output (client-dispatcher-stream dispatcher)))
              ((or error condition) (x)
               (format *error-output* "Error ~/print-error-message/ while trying to return results of evaluating a command." x)))))))



(defun dispatcher-process-client-actions (dispatcher)
  (bt:acquire-lock (client-dispatcher-action-lock dispatcher) t)
  (loop 
    (dolist (a (client-dispatcher-action-list dispatcher))
      (let ((action a)
            (command (bt:with-lock-held ((client-dispatcher-command-lock dispatcher)) 
                       (gethash (client-action-name a) (client-dispatcher-command-table dispatcher)))))
        (bt:make-thread (lambda ()
                          (process-action-thread dispatcher command action)))))
                                
    (setf (client-dispatcher-action-list dispatcher) nil)
    (bt:condition-wait (client-dispatcher-action-cv dispatcher) (client-dispatcher-action-lock dispatcher))))


(defun process-client-input (dispatcher)
  (handler-case
      (let ((s (make-array 40
                           :element-type 'character
                           :adjustable T
                           :fill-pointer 0)))
        (loop
          (let ((char (read-char (client-dispatcher-stream dispatcher) nil :done)))
            (cond ((eq char :done)
                   (format *error-output* "External connection closed.")
                   (return))
                  ((eq (char-code char) 4)
                   (let ((message (handler-case (json:decode-json-from-string s)
                                    ((or error condition) (x)
                                     (format *error-output* "Problem encountered decoding JSON string ~s: ~/print-error-message/. Connection terminated" s x)
                                     (return-from process-client-input nil)))))
                     
                     (cond ((and (= (length message) 3)
                                 (assoc :method message)
                                 (assoc :params message)
                                 (assoc :id message)
                                 (string-equal (cdr (assoc :method message)) "evaluate"))
                            (let* ((params (cdr (assoc :params message)))
                                   (action (make-client-action :name (first params) :model (second params) :parameters (cddr params) :id (cdr (assoc :id message)))))
                              (bt:with-lock-held ((client-dispatcher-action-lock dispatcher))
                                (push-last action (client-dispatcher-action-list dispatcher)))
                              (bt:condition-notify (client-dispatcher-action-cv dispatcher))))
                           ((and (= (length message) 3)
                                 (assoc :result message)
                                 (assoc :error message)
                                 (assoc :id message))
                            (let ((id (cdr (assoc :id message)))
                                  (result (cdr (assoc :result message)))
                                  (error (cdr (assoc :error message))))
                              (let (p)
                                (bt:with-lock-held ((client-dispatcher-requests-lock dispatcher)) 
                                  (setf p (find id (client-dispatcher-pending-requests dispatcher) :key 'client-request-id :test 'equalp))
                                  (when p (setf (client-dispatcher-pending-requests dispatcher) (remove p (client-dispatcher-pending-requests dispatcher)))))
                                (if p
                                    (progn
                                      (bt:with-lock-held ((client-request-lock p))
                                        (setf (client-request-success p) (if result t nil))
                                        (setf (client-request-result p) (if result result (let ((messages (cdr (assoc :message error))))
                                                                                            (if messages
                                                                                                messages
                                                                                              (list "No error messages received.")))))
                                        (setf (client-request-complete p) t))
                                      (bt:condition-notify (client-request-cv p)))
                                  (progn
                                    (format *error-output* "Returned result has an id that does not match a pending request ~s" s)
                                    (return-from process-client-input nil))))))
                           (t
                            (format *error-output* "Invalid message received ~s terminating connection" s)
                            (return-from process-client-input nil))))
                   (setf (fill-pointer s) 0))
                  (t (vector-push-extend char s))))))
    ((or error condition) (x)
     (usocket:socket-close (client-dispatcher-socket dispatcher))
     (format *error-output* "Error ~/print-error-message/ occurred in process-client-input.  Connection terminated." x))))

(start-des-client)


(defvar *current-stream* nil)

(defvar *trace-command-name* nil)

(defun echo-trace-stream (string)
  (format *current-stream* string))

(add-client-command "echo" 'echo-trace-stream)

(defun start-echoing-act-r-output ()
  (setf *current-stream* *standard-output*)
  
  (if *trace-command-name*
      (format *error-output* "ACT-R output is already being displayed.")
    (let ((name (evaluate-act-r-command "get-new-command-name" "remote-echo")))
      
      (unless (gethash "echo" (client-dispatcher-command-table *dispatcher*))
        (add-client-command "echo" 'echo-trace-stream))
      (if name
          (progn
            (setf *trace-command-name* name)
      
            (send-command-to-act-r "add" *trace-command-name* "echo")
            
            (send-command-to-act-r "monitor" "model-trace" *trace-command-name*)
            (send-command-to-act-r "monitor" "command-trace" *trace-command-name*)
            (send-command-to-act-r "monitor" "warning-trace" *trace-command-name*)
            (send-command-to-act-r "monitor" "general-trace" *trace-command-name*)
            t)
        (progn
          (setf *trace-command-name* nil)
          (format *error-output* "Could not get ACT-R command information and cannot echo trace.")
          nil)))))

(defun stop-echoing-act-r-output ()
  (setf *current-stream* nil)
  
  (if *trace-command-name*
      (progn
        (send-command-to-act-r "remove-monitor" "model-trace" *trace-command-name*)
        (send-command-to-act-r "remove-monitor" "command-trace" *trace-command-name*)
        (send-command-to-act-r "remove-monitor" "warning-trace" *trace-command-name*)
        (send-command-to-act-r "remove-monitor" "general-trace" *trace-command-name*)
        (send-command-to-act-r "remove" *trace-command-name*)
        (setf *trace-command-name* nil)
        t)
    (format *error-output* "ACT-R trace not currently being echoed so it cannot be stopped.")))

(defun hide-act-r-output ()
  (if *current-stream*
      (progn
        (setf *current-stream* nil)
        t)
    (format *error-output* "ACT-R output already being hidden.")))

(defun show-act-r-output ()
  (if *current-stream*
      (format *error-output* "ACT-R output is not currently hidden.")
    (progn
      (setf *current-stream* *standard-output*)
      t)))


;;; Macros to handle turning off/hiding output

(defmacro no-output (&body commands)
  `(unwind-protect 
       (progn 
         (stop-echoing-act-r-output)
         ,@commands)
     (start-echoing-act-r-output)))

(defmacro hide-output (&body commands)
  `(unwind-protect 
       (progn 
         (hide-act-r-output)
         ,@commands)
     (show-act-r-output)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Echo the output by default

(start-echoing-act-r-output)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Anything that needs to pass keywords has to be in a list first because the
;;; default JSON encoding will mess that up e.g. (spp :u) wouldn't work as-is.
;;; So, for commands where that's important, like spp, do a quick re-encoding
;;; to convert keywords to strings (leave everything else alone).

(defun encode-keyword-names (sn)
  (cond ((keywordp sn)
         (write-to-string sn))
        ((listp sn)
         (mapcar 'encode-keyword-names sn))
        (t
         sn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Add functions for the ACT-R commands instead of always calling evaluate.

(defun reset ()
  (evaluate-act-r-command "reset"))

(defun reload (&optional compile)
  (evaluate-act-r-command "reload" compile))

(defun run (time &optional real-time)
  (evaluate-act-r-command "run" time real-time))

(defun run-full-time (time &optional real-time)
  (evaluate-act-r-command "run-full-time" time real-time))

(defun run-until-condition (condition &optional real-time)
  (evaluate-act-r-command "run-until-condition" condition real-time))

(defun buffer-chunk (&rest buffers)
  (apply 'evaluate-act-r-command "buffer-chunk" buffers))

(defun buffer-chunk-fct (buffers)
  (apply 'evaluate-act-r-command "buffer-chunk" buffers))

(defun whynot (&rest productions)
  (apply 'evaluate-act-r-command "whynot" productions))

(defun whynot-fct (productions)
  (apply 'evaluate-act-r-command "whynot" productions))

(defun whynot-dm (&rest productions)
  (apply 'evaluate-act-r-command "whynot-dm" productions))

(defun whynot-dm-fct (productions)
  (apply 'evaluate-act-r-command "whynot-dm" productions))

(defun penable (&rest productions)
  (apply 'evaluate-act-r-command "penable" productions))

(defun penable-fct (productions)
  (apply 'evaluate-act-r-command "penable" productions))

(defun pdisable (&rest productions)
  (apply 'evaluate-act-r-command "pdisable" productions))

(defun pdisable-fct (productions)
  (apply 'evaluate-act-r-command "pdisable" productions))

(defun load-act-r-model (file-name)
  (evaluate-act-r-command "load-act-r-model" file-name))

(defmacro goal-focus (&optional chunk-name)
  `(evaluate-act-r-command "goal-focus" ',chunk-name))

(defun goal-focus-fct (&optional chunk-name)
  (evaluate-act-r-command "goal-focus" chunk-name))

(defun clear-exp-window (&optional window)
  (evaluate-act-r-command "clear-exp-window" window))
   
(defun permute-list (list)
  (let ((indexes (make-list (length list)))
        (result (make-list (length list))))
    (dotimes (i (length list))
      (setf (nth i indexes) i))
    (let ((new-indexes (evaluate-act-r-command "permute-list" indexes)))
      (dotimes (i (length list))
        (setf (nth i result) (nth (nth i new-indexes) list))))
    result))

(defun open-exp-window (title &rest details)
  (apply 'evaluate-act-r-command "open-exp-window" title details))

(defun add-text-to-exp-window (window text &rest details)
  (apply 'evaluate-act-r-command "add-text-to-exp-window" window text details))

(defun add-button-to-exp-window (window text &rest details)
  (apply 'evaluate-act-r-command "add-button-to-exp-window" window text details))

(defun remove-items-from-exp-window (window &rest items)
  (apply 'evaluate-act-r-command "remove-items-from-exp-window" (cons window items)))

(defun process-events ()
  (bt:thread-yield))

(defun install-device (device-list)
  (evaluate-act-r-command "install-device" device-list))

(defmacro print-warning (warning-string &rest params)
  `(evaluate-act-r-command "print-warning" (format nil "~@?" ,warning-string ,@params)))

(defun model-output (model-string)
  (evaluate-act-r-command "model-output" model-string))

(defun act-r-output (output-string)
  (evaluate-act-r-command "act-r-output" output-string))


(defun add-act-r-command (name function &optional (documentation "No documentation provided.") (single-instance t))
  (let ((current (gethash name (client-dispatcher-command-table *dispatcher*))))
    (if (and current (eq current function))
        (print-warning (format nil "Command ~s already exists locally and is being reused." name))
      (if current
          (progn
            (print-warning (format nil "Command ~s exists locally, but with a different function.  Replacing prior function." name))
            (setf (gethash name (client-dispatcher-command-table *dispatcher*)) function))
        (setf (gethash name (client-dispatcher-command-table *dispatcher*)) function)))
    (let ((result (multiple-value-list (send-command-to-act-r "add" name name documentation single-instance))))
      (if (first result)
          (values-list (rest result))
        (dolist (x (rest result) nil)
          (format *error-output* x))))))

(defun monitor-act-r-command (act-r-command local-command)
  (let ((result (multiple-value-list (send-command-to-act-r "monitor" act-r-command local-command))))
    (if (first result)
        (values-list (rest result))
      (dolist (x (rest result) nil)
        (format *error-output* x)))))

#-(or :allegro-ide (and :allegro-version>= (version>= 6)))
(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defun remove-act-r-command-monitor (act-r-command local-command)
  (let ((result (multiple-value-list (send-command-to-act-r "remove-monitor" act-r-command local-command))))
    (if (first result)
        (values-list (rest result))
      (dolist (x (rest result) nil)
        (format *error-output* x)))))

(defun remove-act-r-command (name)
  (let ((current (gethash name (client-dispatcher-command-table *dispatcher*))))
    (if current
        (remhash name (client-dispatcher-command-table *dispatcher*)) 
      (print-warning (format nil "Command ~s does not exist locally." name)))
    (let ((result (multiple-value-list (send-command-to-act-r "remove" name))))
      (if (first result)
          (values-list (rest result))
        (dolist (x (rest result) nil)
          (format *error-output* x))))))


(defun act-r-random (value)
  (evaluate-act-r-command "act-r-random" value))

(defun print-visicon ()
  (evaluate-act-r-command "print-visicon"))

(defun mean-deviation (results data &optional (output t))
  (evaluate-act-r-command "mean-deviation" results data output))

(defun correlation (results data &optional (output t))
  (evaluate-act-r-command "correlation" results data output))

(defun get-time (&optional (model-time t))
  (evaluate-act-r-command "get-time" model-time))


(defun buffer-status (&rest buffers)
  (apply 'evaluate-act-r-command "buffer-status" buffers))

(defun buffer-status-fct (buffers)
  (apply 'evaluate-act-r-command "buffer-status" buffers))

(defun buffer-read (buffer-name)
  (evaluate-act-r-command "buffer-read" buffer-name))

(defun clear-buffer (buffer-name)
  (evaluate-act-r-command "clear-buffer" buffer-name))


(defun new-tone-sound (freq duration &optional onset time-in-ms)
  (evaluate-act-r-command "new-tone-sound" freq duration onset time-in-ms))

(defun new-word-sound (word &optional onset (location 'external) time-in-ms)
  (evaluate-act-r-command "new-word-sound" word onset location time-in-ms))

(defun new-digit-sound (digit &optional onset time-in-ms)
  (evaluate-act-r-command "new-digit-sound" digit onset time-in-ms))

(defun define-chunks (&rest chunk-definitions)
  (apply 'evaluate-act-r-command "define-chunks" chunk-definitions))

(defun define-chunks-fct (chunk-definitions)
  (apply 'evaluate-act-r-command "define-chunks" chunk-definitions))

(defun add-dm (&rest chunk-definitions)
  (evaluate-act-r-command "add-dm-fct" chunk-definitions))

(defun add-dm-fct (chunk-definitions)
  (evaluate-act-r-command "add-dm-fct" chunk-definitions))

(defmacro pprint-chunks (&rest chunks)
  `(apply 'evaluate-act-r-command "pprint-chunks" ',chunks))

(defun pprint-chunks-fct (chunks)
  (apply 'evaluate-act-r-command "pprint-chunks" chunks))


(defun chunk-slot-value (chunk-name slot-name)
  (evaluate-act-r-command "chunk-slot-value" chunk-name slot-name))

(defun chunk-slot-value-fct (chunk-name slot-name)
  (evaluate-act-r-command "chunk-slot-value" chunk-name slot-name))

(defun set-chunk-slot-value (chunk-name slot-name new-value)
  (evaluate-act-r-command "set-chunk-slot-value" chunk-name slot-name new-value))

(defun set-chunk-slot-value-fct (chunk-name slot-name new-value)
  (evaluate-act-r-command "set-chunk-slot-value" chunk-name slot-name new-value))

(defun mod-focus (&rest modifications)
  (apply 'evaluate-act-r-command "mod-focus" modifications))

(defun mod-focus-fct (modifications)
  (apply 'evaluate-act-r-command "mod-focus" modifications))

(defun mod-chunk (chunk-name &rest modifications)
  (apply 'evaluate-act-r-command "mod-chunk" chunk-name modifications))

(defun mod-chunk-fct (chunk-name modifications)
  (apply 'evaluate-act-r-command "mod-chunk" chunk-name modifications))

(defun chunk-p (chunk-name?)
  (evaluate-act-r-command "chunk-p" chunk-name?))

(defun chunk-p-fct (chunk-name?)
  (evaluate-act-r-command "chunk-p" chunk-name?))

(defun copy-chunk (chunk-name?)
  (evaluate-act-r-command "copy-chunk" chunk-name?))

(defun copy-chunk-fct (chunk-name?)
  (evaluate-act-r-command "copy-chunk" chunk-name?))


(defun extend-possible-slots (slot-name &optional (warn t))
  (evaluate-act-r-command "extend-possible-slots" slot-name warn))

(defun set-buffer-chunk (buffer-name chunk-name &optional (requested t))
  (evaluate-act-r-command "set-buffer-chunk" buffer-name chunk-name requested))

(defun add-line-to-exp-window (window start end &optional color)
  (if color
      (evaluate-act-r-command "add-line-to-exp-window" window start end color)
    (evaluate-act-r-command "add-line-to-exp-window" window start end)))

(defun modify-line-for-exp-window (line start end &optional color)
  (if color
      (evaluate-act-r-command "modify-line-for-exp-window" line start end color)
    (evaluate-act-r-command "modify-line-for-exp-window" line start end)))

(defun start-hand-at-mouse ()
  (evaluate-act-r-command "start-hand-at-mouse"))
  
(defun schedule-simple-event (time action &optional params (module :none) (priority 0) (maintenance nil))
  (evaluate-act-r-command "schedule-simple-event" time action params module priority maintenance))

(defun schedule-simple-event-relative (time action &optional params (module :none) (priority 0) (maintenance nil))
  (evaluate-act-r-command "schedule-simple-event-relative" time action params module priority maintenance))

(defun mp-show-queue (&optional mark-traced?)
  (evaluate-act-r-command "mp-show-queue" mark-traced?))

(defun print-dm-finsts ()
  (evaluate-act-r-command "print-dm-finsts"))

(defun spp (&rest params)
  (apply 'evaluate-act-r-command "spp" (encode-keyword-names params)))

(defun spp-fct (params)
  (evaluate-act-r-command "spp" (encode-keyword-names params)))

(defun mp-models ()
  (evaluate-act-r-command "mp-models"))

(defun all-productions ()
  (evaluate-act-r-command "all-productions"))

(defun buffers()
  (evaluate-act-r-command "buffers"))

(defun printed-visicon ()
  (evaluate-act-r-command "printed-visicon"))

(defun printed-audicon ()
  (evaluate-act-r-command "printed-audicon"))

(defun print-audicon ()
  (evaluate-act-r-command "print-audicon"))

(defun printed-parameter-details (param)
  (evaluate-act-r-command "printed-parameter-details" param))

(defun sorted-module-names ()
  (evaluate-act-r-command "sorted-module-names"))

(defun modules-parameters (module)
  (evaluate-act-r-command "modules-parameters" module))

(defun modules-with-parameters ()
  (evaluate-act-r-command "modules-with-parameeters"))

(defun used-production-buffers ()
  (evaluate-act-r-command "used-production-buffers"))



(defun record-history (&rest names)
  (evaluate-act-r-command "record-history" names))

(defun stop-recording-history (&rest names)
  (evaluate-act-r-command "stop-recording-history" names))

(defun get-history-data (name &rest params)
  (evaluate-act-r-command "get-history-data" name params))


(defun process-history-data (name &optional file data-params processor-params)
  (evaluate-act-r-command "process-history-data" name file data-params processor-params))

(defun save-history-data (name file &optional comment &rest params)
  (evaluate-act-r-command "save-history-data" name file comment params))

(defmacro dm (&rest params)
  `(if ',params
       (dm-fct ',params)
     (evaluate-act-r-command "dm")))

(defun dm-fct (params)
  (apply 'evaluate-act-r-command (push "dm" params)))

(defmacro sdm (&rest params)
  `(if ',params
       (sdm-fct ',params)
     (evaluate-act-r-command "sdm")))
 
(defun sdm-fct (params)
  (apply 'evaluate-act-r-command (push "sdm" params)))


(defmacro sgp (&rest parameters)
  `(sgp-fct ',parameters))

(defun sgp-fct (parameters)
  (if (every 'keywordp parameters)
      (mapcar 'get-parameter-value (encode-keyword-names parameters))
    (if (evenp (length parameters))
        (let (v)
          (while parameters
            (push-last (set-parameter-value (encode-keyword-names (pop parameters)) (pop parameters)) v))
          v)
      (print-warning "Sgp requires either all keywords naming parameters or an even number of items."))))



(defun get-parameter-value (param)
  (evaluate-act-r-command "get-parameter-value" param))

(defun set-parameter-value (param value)
  (evaluate-act-r-command "set-parameter-value" param value))

(defmacro sdp (&rest params)
  `(if ',params
       (sdp-fct ',params)
     (evaluate-act-r-command "sdp")))
 
(defun sdp-fct (params)
  (apply 'evaluate-act-r-command (push "sdp" params)))

(defun simulate-retrieval-request (&rest params)
  (evaluate-act-r-command "simulate-retrieval-request" params))

(defun saved-activation-history ()
  (evaluate-act-r-command "saved-activation-history"))

(defun print-activation-trace (time &optional (ms t))
  (evaluate-act-r-command "print-activation-trace" time ms))

(defun print-chunk-activation-trace (chunk time &optional (ms t))
  (evaluate-act-r-command "print-chunk-activation-trace" chunk time ms))

(defmacro pp (&rest names)
  `(if ',names
       (pp-fct ',names)
     (evaluate-act-r-command "pp")))
   
(defun pp-fct (names)
  (apply 'evaluate-act-r-command (push "pp" names)))

(defun trigger-reward (reward &optional maintenance)
  (evaluate-act-r-command "trigger-reward" reward maintenance))

(defun define-chunk-spec (&rest spec)
  (apply 'evaluate-act-r-command (push "define-chunk-spec" spec)))

(define release-chunk-spec (chunk-spec)
  (evaluate-act-r-command "release-chunk-spec-id" chunk-spec))

(define chunk-spec-to-chunk-def (chunk-spec)
  (evaluate-act-r-command "chunk-spec-to-chunk-def" chunk-spec))


(defun schedule-simple-event-after-module (after-module action &optional params (module :none) (priority 0) (maintenance nil))
  (evaluate-act-r-command "schedule-simple-event-after-module" after-module action params module priority maintenance))

(defun schedule-simple-set-buffer-chunk (buffer-name chunk-name time-delta &optional (module :none) (priority 0) (requested t))
  (evaluate-act-r-command "schedule-simple-set-buffer-chunk" buffer-name chunk-name time-delta module priority requested))

(defun schedule-simple-mod-buffer-chunk (buffer-name mod-list-or-spec time-delta &optional (module :none) (priority 0))
  (evaluate-act-r-command "schedule-simple-set-buffer-chunk" buffer-name mod-list-or-spec time-delta module priority))


(define undefine-module (name)
  (evaluate-act-r-command "undefine-module" name))

(defun define-module (name buffer-list params version doc interface-list)
  (evaluate-act-r-command "define-module" name buffer-list params version doc interface-list))

(defun complete-request (spec-id)
  (evaluate-act-r-command "complete-request" spec-id))

(defun complete-all-buffer-requests (buffer-name)
  (evaluate-act-r-command "complete-all-buffer-requests" buffer-name))

(defun complete-all-module-requests (module-name)
  (evaluate-act-r-command "complete-all-module-requests" module-name))

(defun mp-time ()
  (evaluate-act-r-command "mp-time"))

(defun mp-time-ms ()
  (evaluate-act-r-command "mp-time-ms"))

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