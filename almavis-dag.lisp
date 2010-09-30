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
    (terpri) 
    (formatting-table ;;formatting table är fulhack här för att skriva ut
      (pane) 	      ;;allt under info-texten ovan, istället för på den.
      (formatting-row
	(pane)
	(formatting-cell
	  (pane)
	  (rita-ut-tidslinjer pane) 
	  (rita-ut-möten (partitionera-möten möten-att-visa) pane))))))

;;TODO Kan optimeras, flera av värdena räknas ut varje iteration genom
;;loopen en kan istället räknas ut bara en gång innan loopen körs.
(defun rita-ut-tidslinjer (ström)
  (loop
    for i from 0 to 2400 by 100
    for tidstext = (skapa-tidstext i)
    for px-tidstext-längd = (sträng-px-längd tidstext ström) 
    with x-text-vänster = px-tidslinje-padding
    for x-linje-vänster = (+ px-tidslinje-padding
			      x-text-vänster
			      px-tidstext-längd)
    for x-linje-höger = (+ x-linje-vänster px-dagsbredd)
    for y-linje = (mappa-position 0 2400 i 0 px-dagshöjd) 
    for y-text = (- y-linje (/ px-bokstavshöjd 2)) 
    do
    (draw-line* ström
		x-linje-vänster
		y-linje
		x-linje-höger
		y-linje
		:line-style (make-line-style :dashes t)) 
    (setf (stream-cursor-position ström)
	  (values x-text-vänster y-text))
    (format ström "~A" tidstext))) 

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
  ;(loop for mötespartition in partitionerade-möten))
  (format t "Möten: ~A~%" partitionerade-möten))
