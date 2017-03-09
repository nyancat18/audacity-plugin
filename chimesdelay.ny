;nyquist plug-in
;version 2
;type process
;name "Chimes delay (V2)..."
;action "Applying chimes delay..."
;info "Chimes delay by David R. Sky\nThanks to Steven Jones for Nyquist help\nIf chimes note list is blank, list is made from low to high notes\nReleased under terms of the GNU Public License"

;control chimeslist "Chimes note list" string "" "-24 -12 -5 0 4 7 12 14 19"
;control minnote "Minimum note" int "semitones" -12 -48 48
;control maxnote "Maximum note" int "semitones" 24 -48 48
;control maxdelay "Maximum delay time" real "seconds" 10.0 0.5 120.0
;control minvol "Minimum volume" int "%" 50.0 0.0 100.0
;control count "Number of chime delays" int "number" 20 1 100

; Chimes Delay by David R. Sky, January 30, 2005
; Version 2 plug-in, works in Audacity 1.2.3 and later
; Released under terms of the GNU Public License
; http://www.opensource.org/licenses/gpl-license.php
; 
; Thanks very much to Steven Jones -
; his 'Harmonic Noise' generator plug-in
; is the source of Nyquist code
; to handle string input note lists.
; Thanks too for range function from math.lsp.
; 
; For mono audio:
; generates random delay times, random volumes, random pitch changes
; (chooses randomly from user-defined or generated note list).
; For stereo audio:
; includes above plus random panning
; (best that audio is first panned to center for stereo).

; If input string is empty,
; generate list based on min and max notes.

; taking audio sample already loaded into Audacity
(setq sample (if (arrayp s)
(vector (aref s 0) (aref s 1)) s))

; making sure min and max really are min and max
(setf lownote (min minnote maxnote))
(setf highnote (max minnote maxnote))

; code from Steven Jones
; verbatim from 'Harmonic Noise' generator plug-in
; convert input string to semitone-shift list
(defun eval-string-to-list (symname str)
(let ((form (strcat "(setq " symname " (list " str "))")))    
(eval (read (make-string-input-stream form)))))

; evaluating chimes note list
(eval-string-to-list "notelist" chimeslist)

; range function by Steven Jones:
; used only when string note list is empty
(defun range (n start &optional (increment 1.0) (acc ()))
(if (plusp n)
(cons start (range (- n 1) (+ start increment) increment acc)) acc))

; if no chimes list, make list using lownote and highnote values 
; (if length of notelist is 0,
; generate list using range function)
(setf n (+ 1 (- highnote lownote)))
(setf notelist (if (= (length notelist) 0)
(range n lownote) notelist))

; random number generator
(defun get-random (min max mult div)
(setf min (* min mult))
(setf max (* max mult))
(setf rnd (/ (+ min (random (truncate max))) div)))

; panning function
(defun pan4 (where s)
   (vector (mult (aref s 0) (sum 1 (mult -1 where)))
       (mult (aref s 1) where)))

; convert randomly-generated semitone step shift value
; to value for use by stretch-abs
(defun convert (shift)
(expt 2.0 (/ shift -12.0)))

; generating chime delays
; comments appear above the line they describe
(if (arrayp s)
; generate stereo Chimes Delay
(sim 
; randomly change note of original audio
; but do not apply delay, volume or pan changes
(force-srate 44100 (stretch-abs 
(convert (nth (random (length notelist)) notelist))
(vector (sound (aref sample 0)) (sound (aref sample 1)))))
(simrep (i count)
; random pan
(pan4 (get-random 0.0 1.0 51.0 50.0)
; random amplitude
(mult (get-random minvol 101 1.0 100.0) 
; random note shift
(force-srate 44100 (stretch-abs 
(convert (nth (random (length notelist)) notelist))
(vector 
; random delay
(at-abs (get-random 0.0 maxdelay 1001.0 1000.0) 
(sound (aref sample 0)))  
(at-abs rnd 
(sound (aref sample 1))))))))))
; end stereo Chimes Delay


; generate mono Chimes Delay
(sim 
; randomly change note of original audio
; but do not apply delay or volume changes
(force-srate 44100 (stretch-abs 
(convert (nth (random (length notelist)) notelist))
(sound sample)))
(simrep (i count)
; random delay
(at-abs (get-random 0.0 maxdelay 1001.0 1000.0) 
; random note shift
(force-srate 44100 (stretch-abs 
(convert (nth (random (length notelist)) notelist))
(sound 
; random amplitude
(mult (get-random minvol 101 1.0 100.0) sample))))))))
; end mono chimes delay
