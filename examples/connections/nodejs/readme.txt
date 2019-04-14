
There are two examples here, which were only tested on Windows with 
Node.js version 10.13.0. 

The first example, simple.js, just shows a simple connection to
the ACT-R server and can be run from a command prompt with:

C:\actr7.x\examples\connections\nodejs>node simple.js

It adds a command called "node-js-add" which takes two parameters and 
returns the sum of those items.  It's lacking some error handling,
so should only be called with numbers.

Here's a sample call:

(evaluate-act-r-command "node-js-add" 3 8)

The second example, relay-virtual-view.js, opens a web server on 
port 3000 which serves two pages.  The default page is found in
the index.html file and it displays a canvas which shows all the
text, buttons, and lines drawn by the AGI as well as the virtual
mouse cursor and models' visual attention marker for visible
virtual windows.  Those windows can be interacted with by a
person as well, and thus one can watch or perform all of the
tasks from the tutorial through a browser window.  The other
page it serves is /goal.html which displays a single button
that says Update.  Pressing that button will cause the window
to print the current contents of the model's goal buffer above 
the Update button.

The second example depends upon two additional libraries for node.js
which were installed using npm like this:

C:\actr7.x\examples\connections\nodejs>npm install --save express
...
C:\actr7.x\examples\connections\nodejs>npm install --save socket.io

and it can be run like this:

C:\actr7.x\examples\connections\nodejs>node relay-virtual-view.js

Then you can open a browser on the same machine and go to:

localhost:3000

or 

localhost:3000/goal.html


or on a different machine you could use the full IP address of the
computer running the example instead of localhost.


Extra note:

The package.json file can be used with the pkg tool to build
a self contained executable of the relay-virtual-view.js example.
If pkg is installed:

npm install -g pkg

then this would build Linux, Mac OSX, and Windows executables:

pkg .

