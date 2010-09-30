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
    (skriv-ut-totala-m�tesl�ngder m�ten-att-visa)
    (terpri) 
    (formatting-table ;;formatting table �r fulhack h�r f�r att skriva ut
      (pane) 	      ;;allt under info-texten ovan, ist�llet f�r p� den.
      (formatting-row
	(pane)
	(formatting-cell
	  (pane)
	  (rita-ut-tidslinjer pane) 
	  (rita-ut-m�ten (partitionera-m�ten m�ten-att-visa) pane))))))

;;TODO Kan optimeras, flera av v�rdena r�knas ut varje iteration genom
;;loopen en kan ist�llet r�knas ut bara en g�ng innan loopen k�rs.
(defun rita-ut-tidslinjer (str�m)
  (loop
    for i from 0 to 2400 by 100
    for tidstext = (skapa-tidstext i)
    for px-tidstext-l�ngd = (str�ng-px-l�ngd tidstext str�m) 
    with x-text-v�nster = px-tidslinje-padding
    for x-linje-v�nster = (+ px-tidslinje-padding
			      x-text-v�nster
			      px-tidstext-l�ngd)
    for x-linje-h�ger = (+ x-linje-v�nster px-dagsbredd)
    for y-linje = (mappa-position 0 2400 i 0 px-dagsh�jd) 
    for y-text = (- y-linje (/ px-bokstavsh�jd 2)) 
    do
    (draw-line* str�m
		x-linje-v�nster
		y-linje
		x-linje-h�ger
		y-linje
		:line-style (make-line-style :dashes t)) 
    (setf (stream-cursor-position str�m)
	  (values x-text-v�nster y-text))
    (format str�m "~A" tidstext))) 

(defun partitionera-m�ten (m�ten &optional (resultat nil))
  (labels
    ((sortera-in-m�te
       (m�te m�teslista)
       (cond ((null m�teslista) (cons (list m�te) m�teslista))
	     ((finns-�verlapp? m�te (car m�teslista))
	      (cons (car m�teslista) (sortera-in-m�te m�te (cdr m�teslista))))
	     (t (cons (cons m�te (car m�teslista)) (cdr m�teslista))))))
    (if (null m�ten) resultat
      (partitionera-m�ten
	(cdr m�ten) 
	(sortera-in-m�te (car m�ten) resultat))))) 

(defun rita-ut-m�ten (partitionerade-m�ten str�m)
  ;(loop for m�tespartition in partitionerade-m�ten))
  (format t "M�ten: ~A~%" partitionerade-m�ten))
