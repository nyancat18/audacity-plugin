;nyquist plug-in
;version 3
;type analyze
;name "ACX Check"
;action "Compute statistics about clip and compare to ACX requirements"
;author "Will McCown"

; will@ross-mccown.com
; 2015-07
; release 1.0

; based on code from the plugins:
; measurement.ny from
; endolith@gmail.com
; 2010-08
; and in stats.ny from
; Steve Daulton. http://audacity.easyspacepro.com 

; This tool provides a number of useful statisics about the selected
; audio.  Where appropriate values are reported both as linear numbers
; and in dBFS (dB relative to full scale).  For each of the left & right
; tracks of the selection it will report:

; Peak level - the greatest absolute value in the selection
; RMS level - the root mean square of the total selection
; NoiseFloor - the RMS level of the quietest 500 mS in the selection
; RMS (A) - The RMS level of the clip with an A-weighting filter applied
; NoiseFloor (A) - The noisefloor of the clip with an A-weighting filter applied
; DC offset - The DC (overall average) of the selection, reported as a percentage
;            of full scale.

; The length of the selection is reported both in samples and in seconds.
; The sample-rate is also reported.

; The Peak level, RMS level, and Noisefloor results are compared against
; the published ACX requirements:
; https://www.acx.com/help/acx-audio-submission-requirements/201456300

; This tool is intended only as an aid in acheiving ACX acceptance.
; A passing grade from this tool is NO guarantee of ACX acceptance.  This
; tool is very limited compared to the testing done by the ACX organization
; on submissions.  In particular the noise-floor measurement reported by
; this tool is the quietest 0.5 seconds found in the length of the clip.
; The intentional insertion of silence, or the overuse of noise-reduction 
; and compression tools will give you a passing number and yet will likely
; fail ACX acceptance.

; A test clip without a sufficiently long "room tone" section may yield
; a false indication of a high noise floor.

; Also beware that some noise sources are worse than others, and 
; noise such as the 1kHz whine that often happens in USB audio interfaces
; may result in an ACX rejection even though it is below the -60 dBFS
; noise floor requirement.

(setq max-len 100000000)  ; Empirically determined on my machine, may be hardware dependent.
(setq ACX-max-peak -3.0)  ; ACX maximum peak (db)
(setq ACX-max-rms -18.0)  ; ACX maximum rms (db)
(setq ACX-min-rms -23.0)  ; ACX minimum rms (db)
(setq ACX-max-nf  -60.0)  ; ACX maximum noise floor (db)

(setq FMT-db "%#.1f")
(setq FMT-val "%#.6f")

(defun fmt-pretty (FMT &rest stuff)
     (progv '(*float-format*) (list FMT)
	    (apply #'format stuff)
	    )
     )

(fmt-pretty FMT-val NIL "~a" 1.2345678)

(defun fmt-peak (value) ;string for peak value including ACX warning
  (strcat
   (fmt-db value)
   (if (> (linear-to-db value) ACX-max-peak)
       (format NIL "  << Exceeds ACX ~a dB max" ACX-max-peak)
     "  .. Passes ACX")
   )
 )

(defun fmt-nf (value) ;string for peak value including ACX warning
  (strcat
   (fmt-db value)
   (if (< value 0)
       (format NIL "  << selection too short")
     (if (> (linear-to-db value) ACX-max-nf)
	 (format NIL "  << Exceeds ACX ~a dB max" ACX-max-nf)
       "  .. Passes ACX")
     )
   )
 )

(defun fmt-rms (value) ;string for peak value including ACX warning
  (strcat
   (fmt-db value)
   (if (> (linear-to-db value) ACX-max-rms)
       (format NIL "  << Exceeds ACX ~a dB max" ACX-max-rms)
     (if (< (linear-to-db value) ACX-min-rms)
	 (format NIL "  << Less than ACX ~a dB min" ACX-min-rms)
       "  .. Passes ACX"))
   )
  )

       
(defun fmt-db (value)  ;print value and it's dB equivalent with appropriate formats
  (if (< value 0)
    (format NIL "N/A")
    (strcat
     (fmt-pretty FMT-val NIL "~a" value)
     (if (> value 0)
	 (fmt-pretty FMT-db NIL " (~a dB)" (linear-to-db value))
       (format NIL " (-inf dB)")
       )
     )
   )
  )

;the my-rms and my-avg routines compute the average value by
;two successive calls to snd-avg.  This is to get around the
;maximum block-size for the snd-avg function which is quite short
;compared to typical samples to be measured.

(defun my-rms (a len) ; compute the RMS of a sound
  (setq chunk 2000000)  ;emperically determined maximum blocksize for snd-avg
  (setq bsize (truncate (/ len chunk)))  ;blocksize to use for the first avg pass
  (setq result (mult a a))
  (when (> bsize 1)
    (setq result (snd-avg result bsize bsize OP-AVERAGE))
    )
  (setq bsize (snd-length result chunk))  ;probably should thow an error if bsize gets chunk here
  (setq result (snd-avg result bsize bsize OP-AVERAGE))
  (sqrt (snd-fetch result))
)

(defun my-avg (a len) ; compute the avg of a sound
  (setq chunk 2000000)  ;emperically determined maximum blocksize for snd-avg
  (setq bsize (truncate (/ len chunk)))  ;blocksize to use for the first avg pass
  (setq result (snd-copy a))
  (when (> bsize 1)
    (setq result (snd-avg result bsize bsize OP-AVERAGE))
    )
  (setq bsize (snd-length result chunk))  ;probably should thow an error if bsize gets chunk here
  (setq result (snd-avg result bsize bsize OP-AVERAGE))
  (snd-fetch result)
)

(defun my-noisefloor (s-in len) ;find min RMS
  (setq noise-win 0.5)      ; Noise level window (seconds)
  (setq noise-inc 5)        ; Noise level increment (fraction of a window will be shifted must be an int)
  (setq max-fact 0.0001)    ; min-max ratio at which we trust the min finder
  (setq min-fact 1e-20)     ; give up point...
  ;set block size to 500 mS
  (setq bsize (truncate (* noise-win (snd-srate s-in))))
  ;set step size to window/5
  (setq ssize (truncate (/ bsize noise-inc)))
  (if (<= len (* bsize 2)) -1
    (progn
	(setq s-samp (highpass8 s-in 10))
	(setq s-rms (mult s-samp s-samp))
	(setq s-rms (snd-avg s-rms bsize ssize OP-AVERAGE))
	(setq s-len (snd-length s-rms max-len))
					; trick to get max to find the min, find the peak and subract it
					;now the min is the max positive
	(setq max (peak s-rms s-len))
	(setq factor 1)
	(sqrt
	 (loop
	  (setq sa (sum (- 0 max) s-rms))
	  (setq min (- max (peak sa (- s-len (+ noise-inc 2)))))
	  (if (or (> min (* max max-fact)) (< max min-fact)) (return (max 0 min))) 
	  (setq max (* max max-fact))
	  (setq s-rms (clip s-rms max))
	  )
	 )
	)
    )
)

(defun analyze (s-in len) ; code that does the analysis and returns data as a string

  ; Measure DC
  (setq dc-offset (my-avg s-in len))

  ; Remove DC
  (setq s-in (diff s-in dc-offset))
  
  ; Should peak measurement be before or after removing DC?
  ; Peak samples should be measured before

  ; Calculate the maximum and RMS levels
  ; RMS of full scale square wave is 0 dBFS
  (setq lmax (snd-maxsamp s-in))
  (setq dbmax (linear-to-db lmax))

  (setq lrms (my-rms s-in len))
  (setq dbrms (linear-to-db lrms))
  
  ;measure noise floor
  (setq nfloor (my-noisefloor s-in len))
  (setq dbnfloor (linear-to-db nfloor))

  (setq acxfail(or (> dbmax ACX-max-peak) (> dbrms ACX-max-rms) (< dbrms ACX-min-rms) (> dbnfloor ACX-max-nf)))
  
  ; A-weighted version of sound - by Edgar (thanks!)
  (setq sa (lp (lp (hp (hp (hp (hp s-in 20.6) 20.6) 107.7) 737.9) 12200) 12200) )

  ; Calculate the RMS level of the A-weighted signal
  ; constant is a fudge factor to normalize to 0 dB at 1 kHz
  (setq smrate (snd-srate s-in))
  (setq fudge (if (<= smrate 44100) 1.344107 (if (<= smrate 48000) (+ 1.344107 (* (- 1.336392 1.344107 )(/ (- smrate 44100) 3900)))
				   (if (<= smrate 96000) (+ 1.336392 (* (- 1.296891 1.336392) (/ (- smrate 48000) 48000)))
				     (+ 1.296981 (* (- 1.277714 1.296981) (/ (- smrate 96000) 96000)))
					 ) )
					 ) )
  (setq larms (* fudge (my-rms sa len)))

  ;and get the noise floor of the A-weighted version
  (setq anfloor (* fudge (my-noisefloor sa len)))

  (format NIL "~a~%~a"
	  (format NIL 
		  "Peak level: ~a~%RMS level: ~a~%NoiseFloor: ~a~%~
                   RMS (A): ~a~%NoiseFloor (A): ~a~%DC offset: ~a%~%"
		  (fmt-peak lmax) (fmt-rms lrms) (fmt-nf nfloor) (fmt-db larms) (fmt-db anfloor) (fmt-pretty FMT-val NIL "~a" (* dc-offset 100)) )
	  (format NIL
		  (if acxfail (strcat
			       "Clip fails to meet ACX requirements~%"
			       (if (> dbmax ACX-max-peak) "Peak exceeds ACX specification of -3 dBFS~%" "")
			       (if (or (> dbrms ACX-max-rms) (< dbrms ACX-min-rms)) "RMS level is outside the ACX specification of -18 to -23 dBFS~%" "")
			       (if (> dbnfloor ACX-max-nf) "Noise floor exceeds ACX specification of -60 dBFS~%" "")
			       )
		    "Clip meets ACX requirements~%" )
		  ))
  )

(defun analyze-mono (input len) ; for mono tracks
  (format NIL "Mono track properties~%~%~a~%"
    (analyze input len))
  )

(defun analyze-stereo (input len) ; for stereo tracks
  (format NIL "Stereo track properties~%~%Left channel:~%~a~%~%Right channel:~%~a~%"
     (analyze (aref input 0) len)
     (analyze (aref input 1) len))
  )

(format NIL "~a~%Length of selection: ~a seconds.~%~a samples at ~a Hz.~%"
	(if (> len max-len) (format NIL "Selection too long for analysis, please select shorter section~%")
	  (if (arrayp s)(analyze-stereo s len)(analyze-mono s len)))
	(get-duration 1)(truncate LEN)(truncate *sound-srate*)
	)
