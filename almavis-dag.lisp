(in-package #:almavis-dag)

;; Hack för att importera allt i almavis-paketet, eftersom det inte finns
;; något sätt att importera alla symboler (även icke-exporterade), och inte
;; heller något sätt att exportera alla symboler på.
(do-symbols (symb :almavis) (import symb)) 

(defclass dag-view (view) ())
(defparameter +dag-vy+ (make-instance 'dag-view))

(defun rita-dag (frame pane)
  "Ritar ut en dag."
  (let*
    ((datahämtare (slot-value frame 'datahämtare))
     (plats (slot-value datahämtare 'plats)) 
     (möten-att-visa (datahämtare->clim-möten datahämtare)))
    (terpri)
    (format t "Den ~A~A ~A~%"
	    (plats-dag plats)
	    (if (> 3 (plats-dag plats)) "a" "e") 
	    (plats-månad plats))
    (skriv-ut-datakällor (slot-value frame 'datahämtare)) 
    (if (alma-kan-längd-av-tp)
      (multiple-value-bind (timmar minuter)
	(truncate (räkna-ihop-möteslängder möten-att-visa) 60)
	(format t "Totalt bokat: ~d timmar, ~d minuter." timmar minuter)
	(terpri)))))
