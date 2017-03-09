;nyquist plug-in
;version 1
;type process
;name "Broadcast Limiter III"
;action "Broadcast Limiter III"
;info ""

;control hd "Exciter" real "" 0 0 10
;control vl "Threshold" real "" 1 0 1

; this plugin was written by edgar-rft@web.de and is released 
; under terms of the GNU General Public License on February 19th 2006

(cond ((< vl (db-to-linear -90)) (setq vl (db-to-linear -90)))
      ((> vl 1.0) (setq vl 1.0)))

(cond ((< hd 0.0) (setf hd 0.0))
      ((> hd 10.0) (setf hd 10.0)))

(setf limit (linear-to-dB vl))

(setf hd (db-to-linear (+ 1.0 (* 19.0 (/ hd 10.0)))))

(clip (sim

(scale hd (clip (hp s 10000) (/ 0.1 hd)))

(lp (scale (/ 0.95 (/ vl (db-to-linear 0.45))) (sim
  (scale 0.2 (clip s vl))
  (scale 0.2 (clip s (db-to-linear (- limit 0.25))))
  (scale 0.2 (clip s (db-to-linear (- limit 0.50))))
  (scale 0.2 (clip s (db-to-linear (- limit 0.75))))
  (scale 0.2 (clip s (db-to-linear (- limit 1.00))))
)) 10000)) 0.99)

