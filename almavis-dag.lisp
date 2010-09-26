(in-package #:almavis-dag)

;; Hack f�r att importera allt i almavis-paketet, eftersom det inte finns
;; n�got s�tt att importera alla symboler (�ven icke-exporterade), och inte
;; heller n�got s�tt att exportera alla symboler p�.
(do-symbols (symb :almavis) (import symb)) 

(defclass dag-view (view) ())
(defparameter +dag-vy+ (make-instance 'dag-view))

(defun rita-dag (frame pane)
  "Ritar ut en dag."
  (let*
    ((datah�mtare (slot-value frame 'datah�mtare))
     (plats (slot-value datah�mtare 'plats)) 
     (m�ten-att-visa (datah�mtare->clim-m�ten datah�mtare)))
    (terpri)
    (format t "Den ~A~A ~A~%"
	    (plats-dag plats)
	    (if (> 3 (plats-dag plats)) "a" "e") 
	    (plats-m�nad plats))
    (skriv-ut-datak�llor (slot-value frame 'datah�mtare)) 
    (skriv-ut-totala-m�tesl�ngder m�ten-att-visa)))
