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
    (skriv-ut-totala-möteslängder möten-att-visa)
    ;(rita-ut-tidslinjer frame) 
    (rita-ut-möten (partitionera-möten möten-att-visa) frame)))

#|(defun rita-ut-tidslinjer (ström)
  (loop
    for i from 0 below 24 do 
    with x = px-tidslinje-padding
    for y = (* i ) 
    (setf (stream-cursor-position ström)
	  (values ))))|# 

(defun partitionera-möten (möten &optional (resultat nil))
  (labels
    ((sortera-in-möte
       (möte möteslista)
       (cond ((null möteslista) (cons (list möte) möteslista))
	     ((finns-överlapp? möte (car möteslista))
	      (cons (car möteslista) (sortera-in-möte möte (cdr möteslista))))
	     (t (cons (cons möte (car möteslista)) (cdr möteslista))))))
    (if (null möten) resultat
      (partitionera-möten
	(cdr möten) 
	(sortera-in-möte (car möten) resultat))))) 

(defun rita-ut-möten (partitionerade-möten ström)
  (format t "Möten: ~A~%" partitionerade-möten))
