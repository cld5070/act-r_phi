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

;;;; By Christopher L. Dancy II
;;;;  Bucknell University
;;;; --------------------
;;;; Just some helper functions to be used w/ ACT-R/Phi

;;;Added by Chris Dancy for ACT-RPhi module
;;Added to add affect params to compiled production
(defun initialize-affect-for-compiled-production (new-p p1 p2)
 (let* ((sVal1 (production-sValue p1))
	    (sVal2 (production-sValue p2))
		(sVal (if (and (numberp sVal1) (numberp sVal2)) (max sVal1 sVal2)
			  (if (numberp sVal1) sVal1 (if (numberp sVal2) sVal2 nil))))
		(sFunc (if sVal 'cond nil))
		(fVal1 (production-fValue p1))
		(fVal2 (production-fValue p2))
		(fVal (if (and (numberp fVal1) (numberp fVal2)) (max fVal1 fVal2)
			(if (numberp fVal1) fVal1 (if (numberp fVal2) fVal2 nil))))
		(fFunc (if fVal 'cond nil)))
  (setf (production-sFunction new-p) sFunc)
  (setf (production-sValue new-p) sVal)
  (setf (production-fValue new-p) fVal)
  (setf (production-fFunction new-p) fFunc)))

  
