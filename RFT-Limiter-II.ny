;nyquist plug-in
;version 1
;type process
;name "Broadcast Limiter II"
;action "Broadcast Limiter II"
;info ""

;control vl "Threshold" real "" 1 0 1

; this plugin was written by edgar-rft@web.de and is released 
; under terms of the GNU General Public License on February 17th 2006

(if (< vl (db-to-linear -90))
  (setq vl (db-to-linear -90)))

(if (> vl 1.0) (setq vl 1.0))

(setf limit (linear-to-dB vl))

(scale (/ 1 (/ vl (db-to-linear 0.45))) (sim
  (scale 0.2 (clip s vl))
  (scale 0.2 (clip s (db-to-linear (- limit 0.25))))
  (scale 0.2 (clip s (db-to-linear (- limit 0.50))))
  (scale 0.2 (clip s (db-to-linear (- limit 0.75))))
  (scale 0.2 (clip s (db-to-linear (- limit 1.00))))
))

