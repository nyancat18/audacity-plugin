;nyquist plug-in
;version 2
;type analyze
;name "Selection Duration..."
;action "Getting selection duration..."
;info "by David R. Sky, Steven Jones, Dominic Mazzoni\nReleased under terms of GNU Public License"

; Selection Duration by David R. Sky
; Code help from Steven jones, Dominic Mazzoni
; updated January 2, 2006
; useful for screen reader users
; Released under terms of the GNU Public license
; http://www.opensource.org/licenses/gpl-license.php

;control choice "Time or samples" int "0=time 1=samples" 0 0 1

; selection duration
(setf dur (/ len *sound-srate*))

; calculate minutes
(setf minutes (truncate (/ dur 60)))
(setf mins2secs (* minutes 60))

; calculate seconds
(setf seconds (- dur mins2secs))

(cond ((= choice 1) ; samples chosen

(cond ((> len 999999999) ; >= 1 billion samples
(setf len (/ len 1000000000.0))
(setf number 'billion))

((> len 999999) ; >= 1 million samples
(setf len (/ len 1000000.0))
(setf number 'million))

(t (setf number ""))) ; < 1 million samples

(format nil "Selection duration is ~a ~a samples. ~%" len number))

((= minutes 0)
(format nil "Selection duration is ~a seconds ~%" seconds))

((= minutes 1)
(format nil "Selection duration is 1 minute ~a seconds ~%"
seconds))

((> minutes 1)
(format nil "Selection duration is ~a minutes ~a seconds ~%"
minutes seconds)))

