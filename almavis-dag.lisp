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
	  (cond ((alma-kan-jämför)
		 (rita-ledigheter pane (gemensam-ledighet datahämtare)))
		((and (alma-kan-ledigt)
		      (= 1 (antal-datakällor datahämtare)))
		 (rita-ledigheter
		   pane
		   (ledighet datahämtare))))
	  (let* 
	    ((px-möteskol-x (rita-ut-tidslinjer pane))) 
	    (rita-ut-möten
	      px-möteskol-x
	      (partitionera-möten möten-att-visa)
	      pane)))))))

;;TODO Kan optimeras, flera av värdena räknas ut varje iteration genom
;;loopen en kan istället räknas ut bara en gång innan loopen körs.
(defun rita-ut-tidslinjer (ström)
  "Ritar ut de horisontella linjer som indikerar tider för mötena.
  Returnerar x-koordinaten för första möteskolumnen"
  (loop
    for i from 0 to 2400 by 100
    for tidstext = (skapa-tidstext i)
    for px-tidstext-längd = (sträng-px-längd tidstext ström) 
    with px-text-vänster-x = px-tidslinje-padding
    for px-linje-vänster-x = (+ px-tidslinje-padding
			      px-text-vänster-x
			      px-tidstext-längd)
    for px-linje-höger-x = (+ px-linje-vänster-x px-dagsbredd)
    for px-linje-y = (mappa-position 0 2400 i 0 px-dagshöjd) 
    for px-tidstext-y = (- px-linje-y (/ px-bokstavshöjd 2)) 
    do
    (draw-line* ström
		px-linje-vänster-x
		px-linje-y
		px-linje-höger-x
		px-linje-y
		:line-style (make-line-style :dashes t)) 
    (setf (stream-cursor-position ström)
	  (values px-text-vänster-x px-tidstext-y))
    (format ström "~A" tidstext)
    finally (return px-linje-vänster-x))) 

(defun partitionera-möten (möten &optional (resultat nil))
  "Delar in en lista med clim-möten i flera listor så att inga möten
  i en underlista överlappar varandra. Vi delar in mötena i så få
  underlistor som möjligt. Resultatet har formen: ((clim-möte*)*)"
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

(defun rita-ut-möten (px-kolumn-ett-x partitionerade-möten ström)
  "Tar x-koordinaten för den första möteskolumnen, en lista med
  partitionerade möten på formen ((clim-möte*)*) och ritar ut
  mötena på en ström."
  (loop for mötespartition in partitionerade-möten
	for i from 0 to (1- max-antal-möteskolumner)
	for px-kolumn-vänster-x = (+ px-kolumn-ett-x
				     (* i (+ px-mellan-möteskol
					     px-möteskol-bredd)))
	do
	(loop for möte in mötespartition
	      do
	      (setf (stream-cursor-position ström)
		    (values px-kolumn-vänster-x 0)) 
	      (with-local-coordinates
		(ström px-kolumn-vänster-x 0) 
		(present möte
			 'clim-möte
			 :view +dag-vy+)))))

(define-presentation-method
  present
  (clim-möte (type clim-möte) stream (view dag-view) &key)
  (rita-möte clim-möte stream))

(defun rita-möte (clim-möte ström)
  (with-slots
    (alma-möte start-kl slut-kl almanacksnamn mötesinfo) 
    clim-möte
    (let*
      ((tidsperiod (tidsperioddel alma-möte))
       (starttid (alma-tp-starttid tidsperiod))
       (sluttid (alma-tp-sluttid tidsperiod))
       (px-start-y (tid-till-position starttid px-dagshöjd))
       (px-slut-y (tid-till-position sluttid px-dagshöjd))) 
      (with-drawing-options
	(ström :ink bokad-färg)
	(draw-rectangle*
	  ström
	  0
	  px-start-y
	  px-möteskol-bredd	
	  px-slut-y))
      (byt-cursor-position ström :y px-start-y) 
      (skriv-rader ström
		   px-möteskol-bredd
		   (- px-slut-y px-start-y)
		   (list mötesinfo
			 (format nil "~A - ~A  ~A"
						(skapa-tidstext start-kl)
						(skapa-tidstext slut-kl)
						(möteslängd-sträng clim-möte))
			 (format nil "~A" almanacksnamn))))))

(defun rita-ledigheter (ström clim-ledigheter)
  (mapc
    #'(lambda
	(clim-ledighet)
	(let*
	  ((tidsperiod (slot-value clim-ledighet 'tidsperiod))
	   (starttid (alma-tp-starttid tidsperiod))
	   (sluttid (alma-tp-sluttid tidsperiod))
	   (px-start-y (tid-till-position starttid px-dagshöjd))
	   (px-slut-y (tid-till-position sluttid px-dagshöjd))) 
	  (with-drawing-options
	    (ström :ink ledig-färg)
	    (draw-rectangle*
	      ström
	      0
	      px-start-y
	      (* 2 px-tidslinje-padding) 
	      px-slut-y))))
    clim-ledigheter)) 
