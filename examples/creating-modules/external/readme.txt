There are two examples here of replacing the internal ACT-R goal module
with one that is implemented in Python instead.

The goal.py version requires the general ACT-R interface implemented in
the actr.py file (found in the tutorial/python directory) to operate.
The goal_complete.py version contains its own stripped down version of
the communication code such that it is self contained and does not 
require the actr.py file, but it is perhaps still a little more general
than it needs to be.

Both work very similar to the normal ACT-R goal module and also define
the goal-focus and mod-focus macros which allow most of the tutorial 
models to work as-is with this replacement.  The two primary differences
between these and the default goal module are: there are no goal-focus-fct
and mod-focus-fct functions available so you'd have to call the command
through the dispatcher e.g. (call-act-r-command "goal-focus" 'name) and
for simplicity of the example the mod-focus command doesn't properly
handle the situation when there's a pending goal-focus (it shouldn't
schedule the mod-buffer-chunk directly in that case and instead should
schedule a custom action to make the change).

Once you import either of those you should be able to load the tutorial
unit 1 models and run them just as before, and here's an example of
running the count model after importing the goal.py file:

>>> import goal
>>> actr.load_act_r_model('ACT-R:tutorial;unit1;count.lisp')
True
>>> actr.run(1)
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL FIRST-GOAL NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED START
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000   GOAL                   clear-delayed-goal COUNT
     0.050   PROCEDURAL             PRODUCTION-FIRED START
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST RETRIEVAL
     0.050   PROCEDURAL             CLEAR-BUFFER RETRIEVAL
     0.050   DECLARATIVE            start-retrieval
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.100   DECLARATIVE            RETRIEVED-CHUNK C
     0.100   DECLARATIVE            SET-BUFFER-CHUNK RETRIEVAL C
     0.100   PROCEDURAL             CONFLICT-RESOLUTION
     0.100   PROCEDURAL             PRODUCTION-SELECTED INCREMENT
     0.100   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.100   PROCEDURAL             BUFFER-READ-ACTION RETRIEVAL
     0.150   PROCEDURAL             PRODUCTION-FIRED INCREMENT
2
     0.150   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.150   PROCEDURAL             MODULE-REQUEST RETRIEVAL
     0.150   PROCEDURAL             CLEAR-BUFFER RETRIEVAL
     0.150   DECLARATIVE            start-retrieval
     0.150   PROCEDURAL             CONFLICT-RESOLUTION
     0.200   DECLARATIVE            RETRIEVED-CHUNK D
     0.200   DECLARATIVE            SET-BUFFER-CHUNK RETRIEVAL D
     0.200   PROCEDURAL             CONFLICT-RESOLUTION
     0.200   PROCEDURAL             PRODUCTION-SELECTED INCREMENT
     0.200   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.200   PROCEDURAL             BUFFER-READ-ACTION RETRIEVAL
     0.250   PROCEDURAL             PRODUCTION-FIRED INCREMENT
3
     0.250   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.250   PROCEDURAL             MODULE-REQUEST RETRIEVAL
     0.250   PROCEDURAL             CLEAR-BUFFER RETRIEVAL
     0.250   DECLARATIVE            start-retrieval
     0.250   PROCEDURAL             CONFLICT-RESOLUTION
     0.250   PROCEDURAL             PRODUCTION-SELECTED STOP
     0.250   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.300   DECLARATIVE            RETRIEVED-CHUNK E
     0.300   DECLARATIVE            SET-BUFFER-CHUNK RETRIEVAL E
     0.300   PROCEDURAL             PRODUCTION-FIRED STOP
4
     0.300   PROCEDURAL             CLEAR-BUFFER GOAL
     0.300   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   ------                 Stopped because no events left to process
[0.3, 50, None]

