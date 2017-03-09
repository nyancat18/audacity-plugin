;nyquist plug-in
;version 2
;type analyze
;name "Regular interval labels..."
;action "Generating equally-spaced labels to the label track..."
;info "equalabl.ny by David R. Sky www.shellworld.net/~davidsky/nyquist.htm \nNote: equalabl.ny does not over-write previous label track, it adds to it.\nChoosing to make final audio segment equal with others may slightly change your time setting.\nThanks to Alex S. Brown for example code from silencemarker.ny for setting labels in the label track.\nReleased under terms of the GNU Public License"

;control time "Label interval [seconds]" int "" 60 1 600
;control text "Label text" string "" "Label"
;control prepend "Prepend numbers to label text [0=no 1=yes]" int "" 1 0 1
;control include "Final label [0=exclude 1=include]" int "" 0 0 1
;control t-choice "Final audio segment duration equal with others [0=no 1=yes]" int "" 1 0 1

; Regular interval labels by David R. Sky, June 25, 2007.
; updated July 1 2007 to give an error message 
; if the specified label interval time is greater than the selection duration.
; thanks Dominic Mazzoni for this suggestion
; http://www.shellworld.net/~davidsky/nyquist.htm
; Thanks to Sami Jumppanen from the Audacity users list for plug-in suggestion
; Thanks to Gale Andrews from Audacity devel list for improvement suggestion
; Thanks to Alex S. Brown's silencemarker.ny 
; for example code of setting labels on the label track
; Released under terms of the GNU Public License
; http://www.opensource.org/licenses/gpl-license.php 

#|
equalabl.ny: Regular interval labels

thanks to Sami Jumppanen from the Audacity users group for
suggesting this plug-in: adding labels to the label track at
regular intervals. Thanks to leland Lucius from the Audacity development list
for code feedback which
helped wake me from late night programming! And thanks to Gale
Andrews from the Audacity development list for suggesting
improvements. 

Important note: equalabl.ny does not replace any previously-created
label track, it adds to it.

Start a new session of audacity. Load audio you want to add
regularly-spaced labels to. Select audio [control+a]. Open analyze
menu [alt+a]. Click on 'Regular interval labels'. use or change the
following five default variablees:

1. Label interval [seconds]
Default sixty seconds between labels, from one second to six
hundred seconds [ten minutes].

2. Label text
The text that will appear in each label, default is "label".

3. Prepend numbers to label text [0=no 1=yes]
Default is yes, so your labels would sequentially be "0label
1label..." and so on, using the default text.

4. Final label [0=exclude 1=include]
For example, if your selection is sixty seconds long, and your
label interval is ten seconds, the final label would be at the
exact end of your selection. By default, the above variable is to
exclude [not set] the final label.

5. Final audio segment duration equal with others [0=no 1=yes] 
Your label interval setting may result in the final segment of
audio being unequal with the preceding ones. By default, the above
variable is set to make all audio segments equal in duration with
each other. This might make the label interval slightly different
from your chosen one, depending on the size of the final audio
segment.

Note

People using a screen reader can view the label track in Audacity
1.3.3 beta by opening the track menu [alt+t] and clicking on 'edit
labels'. The labels and their time positions can be read by
cursoring up and down and left-right. press alt+f4 to return to the
main Audacity screen.

Written by David R. Sky.
http://www.shellworld.net/~davidsky/nyquist.htm
released to the Audacity community  June 25, 2007.
Thanks to Alex S. Brown for example code from his silencemarker.ny
plug-in for placing labels on the label track.
Released under terms of the GNU Public License
http://www.opensource.org/licenses/gpl-license.php 
|#

; get selection duration in seconds
(setf dur (/ len *sound-srate*))

; give an error message 
; if label time interval is greater than selection duration
(cond ; outermost cond
((> time dur)
(format nil
"Your requested label interval time of ~a seconds ~%
is greater than the duration of your selected audio ~a seconds. ~%
Please run this plug-in again using a smaller interval time. ~%" time dur))

(t ; label interval time is equal to or less than selection duration
; calculate number of labels in selection
; this includes label at start of selection
; which is label number 0 if numbers are prepended to label text
; if final label is unwanted, decrement number of labels by 1
(setf labels  (if (= include 0)
(truncate (/ dur time))
(+ 1 (truncate (/ dur time)))
) ; end if
) ; end setf labels

; setf check-labels: number of labels if user includes last label
; this is for checking purposes in the [setf time ...] section of code
(setf check-labels (+ 1 (truncate (/ dur time))))


; function to calculate new time value 
; if user value does not create equal duration final audio chunk
; returns new time value
(defun new-time (time dur check-labels)
; calculate duration of last audio chunk
(setf last-label (- dur (* time check-labels)))
(if (< last-label (* time 0.5)) ; last chunk is less than 1/2 time
(/ dur (- check-labels 1))
(/ dur check-labels)
) ; end if
) ; end defun new-time


(setf time ; depending whether user wants specified time used 
; or wants equal duration audio chunks including last chunk
; user time may create equal audio chunks even for last chunk
(cond ; 1
((= t-choice 0) time) ; use user time value...

(t ; ...otherwise calculate time for equal duration audio chunks
(cond ; 2 
; if user time value creates equal final audio segment duration anyway 
; then use user interval
((= dur (* time (- check-labels 1))) time)

; user time value does not create equal duration audio chunks
(t 
(new-time time dur check-labels)
) ; end t
) ; end cond 2
) ; end of calculation for equal duration audio chunks
) ; end cond1
) ; end setf time


; function to add labels to the label track
; from silencemarker.ny by Alex S. Brown
(defun add-label (l-time l-text)
 (setq l (cons (list l-time l-text) l)))


; function to prepend label number before label text
(defun prepend-number (i text)
(format nil "~a~a" i text))


; initialize blank label track
(setq l nil)

; add the labels
(dotimes (i labels)
(if (= prepend 1) ; prepend numbers to label text?
(add-label (* i time) (prepend-number i text)) ; yes
(add-label (* i time) text) ; no
) ; end if
) ; end dotimes i

; return label track
l

) ; close t of outermost cond
) ; end outermost cond
