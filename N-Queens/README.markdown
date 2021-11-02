# queenapp

Problem Statement - https://en.wikipedia.org/wiki/Eight_queens_puzzle

I used a Constraint Satisfaction technique to find solutions for N-Queen problem.
AC-3 and backtracking algorithms were used to recursively find a solution. Caveman2 
with djula templates were used to create a web UI that allowed users to manually place 
queens and change the board size. 

The file containing the solving algorithm can be found in AI-Projects/N-Queens/src/n-queens.lisp

## Installation

Instructions :

Note: requires Quicklisp, caveman2, and alexandria

Replace PATH with the unzip directory and run the command below in SBCL Slime repl
(asdf:load-asd #P"PATH/queenapp/queenapp.asd")

Then run the following two commands
(ql:quickload :queenapp)
(queenapp:start :port 5000)

NOTE: The start command will produce an error about DLL's.
	  Just [ACCEPT] the errors and continue.
	  
	  
The server is now running and can be reached on
127.0.0.1:5000

## Author

* Brett Isken (BrettIsken@gmail.com)

## Copyright

Copyright (c) 2020 Brett Isken (BrettIsken@gmail.com)

